//! Difftastic is a syntactic diff tool.
//!
//! For usage instructions and advice on contributing, see [the
//! manual](http://difftastic.wilfred.me.uk/).
//!

// This tends to trigger on larger tuples of simple types, and naming
// them would probably be worse for readability.
#![allow(clippy::type_complexity)]
// == "" is often clearer when dealing with strings.
#![allow(clippy::comparison_to_empty)]
// It's common to have pairs foo_lhs and foo_rhs, leading to double
// the number of arguments and triggering this lint.
#![allow(clippy::too_many_arguments)]
// Has false positives on else if chains that sometimes have the same
// body for readability.
#![allow(clippy::if_same_then_else)]
// Purely stylistic, and ignores whether there are explanatory
// comments in the if/else.
#![allow(clippy::bool_to_int_with_if)]
// Good practice in general, but a necessary evil for Syntax. Its Hash
// implementation does not consider the mutable fields, so it is still
// correct.
#![allow(clippy::mutable_key_type)]

#[macro_use]
extern crate log;
extern crate pretty_env_logger;

use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use mimalloc::MiMalloc;
use owo_colors::OwoColorize;
use rayon::prelude::*;
use strum::IntoEnumIterator;
use typed_arena::Arena;

use difftastic::{diff, diff_file, diff_file_content};

use crate::parse::tree_sitter_parser as tsp;
use crate::conflicts::apply_conflict_markers;
use crate::conflicts::START_LHS_MARKER;
use crate::exit_codes::{EXIT_FOUND_CHANGES, EXIT_SUCCESS};
use crate::exit_codes::EXIT_BAD_ARGUMENTS;
use crate::files::{
    guess_content, ProbableFileKind, read_file_or_die,
    read_or_die,
};
use crate::options::{DiffOptions, DisplayMode, DisplayOptions, FileArgument, Mode};
use crate::parse::guess_language::{guess, Language, language_name, LanguageOverride};
use crate::parse::guess_language::language_globs;
use crate::parse::syntax;
use crate::parse::syntax::init_all_info;
use crate::summary::{DiffResult, FileContent, FileFormat};

mod conflicts;
mod constants;
mod diff;
mod display;
mod exit_codes;
mod files;
mod hash;
mod line_parser;
mod lines;
mod options;
mod parse;
mod summary;
mod version;

/// The global allocator used by difftastic.
///
/// Diffing allocates a large amount of memory, and `MiMalloc` performs
/// better.
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

/// Terminate the process if we get SIGPIPE.
#[cfg(unix)]
fn reset_sigpipe() {
    unsafe {
        libc::signal(libc::SIGPIPE, libc::SIG_DFL);
    }
}

#[cfg(not(unix))]
fn reset_sigpipe() {
    // Do nothing.
}

/// The entrypoint.
fn main() {
    pretty_env_logger::try_init_timed_custom_env("DFT_LOG")
        .expect("The logger has not been previously initalized");
    reset_sigpipe();

    match options::parse_args() {
        Mode::DumpTreeSitter {
            path,
            language_overrides,
        } => {
            let path = Path::new(&path);
            let bytes = read_or_die(path);
            let src = String::from_utf8_lossy(&bytes).to_string();

            let language = guess(path, &src, &language_overrides);
            match language {
                Some(lang) => {
                    let ts_lang = tsp::from_language(lang);
                    let tree = tsp::to_tree(&src, &ts_lang);
                    tsp::print_tree(&src, &tree);
                }
                None => {
                    eprintln!("No tree-sitter parser for file: {:?}", path);
                }
            }
        }
        Mode::DumpSyntax {
            path,
            ignore_comments,
            language_overrides,
        } => {
            let path = Path::new(&path);
            let bytes = read_or_die(path);
            let src = String::from_utf8_lossy(&bytes).to_string();

            let language = guess(path, &src, &language_overrides);
            match language {
                Some(lang) => {
                    let ts_lang = tsp::from_language(lang);
                    let arena = Arena::new();
                    let ast = tsp::parse(&arena, &src, &ts_lang, ignore_comments);
                    init_all_info(&ast, &[]);
                    println!("{:#?}", ast);
                }
                None => {
                    eprintln!("No tree-sitter parser for file: {:?}", path);
                }
            }
        }
        Mode::ListLanguages {
            use_color,
            language_overrides,
        } => {
            for (lang_override, globs) in language_overrides {
                let mut name = match lang_override {
                    LanguageOverride::Language(lang) => language_name(lang),
                    LanguageOverride::PlainText => "Text",
                }
                    .to_string();
                if use_color {
                    name = name.bold().to_string();
                }
                println!("{} (from override)", name);
                for glob in globs {
                    print!(" {}", glob.as_str());
                }
                println!();
            }

            for language in Language::iter() {
                let mut name = language_name(language).to_string();
                if use_color {
                    name = name.bold().to_string();
                }
                println!("{}", name);

                for glob in language_globs(language) {
                    print!(" {}", glob.as_str());
                }
                println!();
            }
        }
        Mode::DiffFromConflicts {
            display_path,
            path,
            diff_options,
            display_options,
            set_exit_code,
            language_overrides,
        } => {
            let diff_result = diff_conflicts_file(
                &display_path,
                &path,
                &display_options,
                &diff_options,
                &language_overrides,
            );

            print_diff_result(&display_options, &diff_result);

            let exit_code = if set_exit_code && diff_result.has_reportable_change() {
                EXIT_FOUND_CHANGES
            } else {
                EXIT_SUCCESS
            };
            std::process::exit(exit_code);
        }
        Mode::Diff {
            diff_options,
            display_options,
            set_exit_code,
            language_overrides,
            lhs_path,
            rhs_path,
            display_path,
            old_path,
        } => {
            // let encountered_changes = Arc::new(AtomicBool::new(false));

            let encountered_changes = Arc::new(AtomicBool::new(false));
            match (&lhs_path, &rhs_path) {
                (
                    FileArgument::NamedPath(lhs_path),
                    FileArgument::NamedPath(rhs_path),
                ) if lhs_path.is_dir() && rhs_path.is_dir() => {
                    let encountered_changes = encountered_changes.clone();
                    let diff_iter = diff(lhs_path, rhs_path, display_options, diff_options, language_overrides);

                    if matches!(display_options.display_mode, DisplayMode::Json) {
                        let results = diff_iter
                            .map(|diff_result| {
                                if diff_result.has_reportable_change() {
                                    encountered_changes.store(true, Ordering::Relaxed);
                                }

                                diff_result
                            })
                            .collect();

                        display::json::print_directory(results);
                    } else {
                        // We want to diff files in the directory in
                        // parallel, but print the results serially (to
                        // prevent display interleaving).
                        // https://github.com/rayon-rs/rayon/issues/210#issuecomment-551319338
                        let (send, recv) = std::sync::mpsc::sync_channel(1);

                        let encountered_changes = encountered_changes.clone();
                        let print_options = display_options.clone();

                        let printing_thread = std::thread::spawn(move || {
                            for diff_result in recv.into_iter() {
                                print_diff_result(&print_options, &diff_result);

                                if diff_result.has_reportable_change() {
                                    encountered_changes.store(true, Ordering::Relaxed);
                                }
                            }
                        });

                        diff_iter
                            .try_for_each_with(send, |s, diff_result| s.send(diff_result))
                            .expect("Receiver should be connected");

                        printing_thread
                            .join()
                            .expect("Printing thread should not panic");
                    }
                }
                _ => {
                    let diff_result = diff_file(
                        &display_path,
                        old_path,
                        &lhs_path,
                        &rhs_path,
                        &display_options,
                        &diff_options,
                        false,
                        &language_overrides,
                    );
                    if diff_result.has_reportable_change() {
                        encountered_changes.store(true, Ordering::Relaxed);
                    }

                    match display_options.display_mode {
                        DisplayMode::Inline
                        | DisplayMode::SideBySide
                        | DisplayMode::SideBySideShowBoth => {
                            print_diff_result(&display_options, &diff_result);
                        }
                        DisplayMode::Json => display::json::print(&diff_result),
                    }
                }
            }

            let exit_code = if set_exit_code && encountered_changes.load(Ordering::Relaxed) {
                EXIT_FOUND_CHANGES
            } else {
                EXIT_SUCCESS
            };
            std::process::exit(exit_code);
        }
    };
}

fn diff_conflicts_file(
    display_path: &str,
    path: &FileArgument,
    display_options: &DisplayOptions,
    diff_options: &DiffOptions,
    overrides: &[(LanguageOverride, Vec<glob::Pattern>)],
) -> DiffResult {
    let bytes = read_file_or_die(path);
    let mut src = match guess_content(&bytes) {
        ProbableFileKind::Text(src) => src,
        ProbableFileKind::Binary => {
            eprintln!("error: Expected a text file with conflict markers, got a binary file.");
            std::process::exit(EXIT_BAD_ARGUMENTS);
        }
    };

    if diff_options.strip_cr {
        src.retain(|c| c != '\r');
    }

    let conflict_files = match apply_conflict_markers(&src) {
        Ok(cf) => cf,
        Err(msg) => {
            eprintln!("error: {}", msg);
            std::process::exit(EXIT_BAD_ARGUMENTS);
        }
    };

    if conflict_files.num_conflicts == 0 {
        eprintln!(
            "warning: Expected a file with conflict markers {}, but none were found. See --help for usage instructions.\n",
            START_LHS_MARKER,
        );
    }

    let lhs_name = match conflict_files.lhs_name {
        Some(name) => format!("'{}'", name),
        None => "the left file".to_owned(),
    };
    let rhs_name = match conflict_files.rhs_name {
        Some(name) => format!("'{}'", name),
        None => "the right file".to_owned(),
    };

    let extra_info = format!(
        "Showing the result of replacing every conflict in {} with {}.",
        lhs_name, rhs_name
    );

    diff_file_content(
        display_path,
        Some(extra_info),
        path,
        path,
        &conflict_files.lhs_content,
        &conflict_files.rhs_content,
        display_options,
        diff_options,
        overrides,
    )
}

fn print_diff_result(display_options: &DisplayOptions, summary: &DiffResult) {
    match (&summary.lhs_src, &summary.rhs_src) {
        (FileContent::Text(lhs_src), FileContent::Text(rhs_src)) => {
            let hunks = &summary.hunks;

            if !summary.has_syntactic_changes {
                if display_options.print_unchanged {
                    println!(
                        "{}",
                        display::style::header(
                            &summary.display_path,
                            summary.extra_info.as_ref(),
                            1,
                            1,
                            &summary.file_format,
                            display_options,
                        )
                    );
                    match summary.file_format {
                        _ if summary.lhs_src == summary.rhs_src => {
                            println!("No changes.\n");
                        }
                        FileFormat::SupportedLanguage(_) => {
                            println!("No syntactic changes.\n");
                        }
                        _ => {
                            println!("No changes.\n");
                        }
                    }
                }
                return;
            }

            if summary.has_syntactic_changes && hunks.is_empty() {
                println!(
                    "{}",
                    display::style::header(
                        &summary.display_path,
                        summary.extra_info.as_ref(),
                        1,
                        1,
                        &summary.file_format,
                        display_options,
                    )
                );
                match summary.file_format {
                    FileFormat::SupportedLanguage(_) => {
                        println!("Has syntactic changes.\n");
                    }
                    _ => {
                        println!("Has changes.\n");
                    }
                }

                return;
            }

            match display_options.display_mode {
                DisplayMode::Inline => {
                    display::inline::print(
                        lhs_src,
                        rhs_src,
                        display_options,
                        &summary.lhs_positions,
                        &summary.rhs_positions,
                        hunks,
                        &summary.display_path,
                        &summary.extra_info,
                        &summary.file_format,
                    );
                }
                DisplayMode::SideBySide | DisplayMode::SideBySideShowBoth => {
                    display::side_by_side::print(
                        hunks,
                        display_options,
                        &summary.display_path,
                        summary.extra_info.as_ref(),
                        &summary.file_format,
                        lhs_src,
                        rhs_src,
                        &summary.lhs_positions,
                        &summary.rhs_positions,
                    );
                }
                DisplayMode::Json => unreachable!(),
            }
        }
        (FileContent::Binary, FileContent::Binary) => {
            if display_options.print_unchanged || summary.has_byte_changes {
                println!(
                    "{}",
                    display::style::header(
                        &summary.display_path,
                        summary.extra_info.as_ref(),
                        1,
                        1,
                        &FileFormat::Binary,
                        display_options,
                    )
                );
                if summary.has_byte_changes {
                    println!("Binary contents changed.");
                } else {
                    println!("No changes.");
                }
            }
        }
        (FileContent::Text(_), FileContent::Binary)
        | (FileContent::Binary, FileContent::Text(_)) => {
            // We're diffing a binary file against a text file.
            println!(
                "{}",
                display::style::header(
                    &summary.display_path,
                    summary.extra_info.as_ref(),
                    1,
                    1,
                    &FileFormat::Binary,
                    display_options,
                )
            );
            println!("Binary contents changed.");
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;

    use super::*;

    #[test]
    fn test_diff_identical_content() {
        let s = "foo";
        let res = diff_file_content(
            "foo.el",
            None,
            &FileArgument::from_path_argument(OsStr::new("foo.el")),
            &FileArgument::from_path_argument(OsStr::new("foo.el")),
            s,
            s,
            &DisplayOptions::default(),
            &DiffOptions::default(),
            &[],
        );

        assert_eq!(res.lhs_positions, vec![]);
        assert_eq!(res.rhs_positions, vec![]);
    }
}
