use std::env;
use std::path::{Path, PathBuf};

use glob::Pattern;
use humansize::{BINARY, format_size};
use log::info;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use typed_arena::Arena;

use crate::diff::changes::ChangeMap;
use crate::diff::dijkstra::{ExceededGraphLimit, mark_syntax};
use crate::diff::sliders::fix_all_sliders;
use crate::diff::unchanged;
use crate::display::context::opposite_positions;
use crate::display::hunks::{matched_pos_to_hunks, merge_adjacent};
use crate::files::{guess_content, ProbableFileKind, read_files_or_die, relative_paths_in_either};
use crate::lines::MaxLine;
use crate::options::{DiffOptions, DisplayOptions, FileArgument};
use crate::parse::{syntax, tree_sitter_parser as tsp};
use crate::parse::guess_language::{guess, language_name, LanguageOverride};
use crate::parse::syntax::init_next_prev;
use crate::summary::{DiffResult, FileContent, FileFormat};

pub mod conflicts;
pub mod constants;
pub mod diff;
pub mod display;
pub mod exit_codes;
pub mod files;
pub mod hash;
pub mod line_parser;
pub mod lines;
pub mod options;
pub mod parse;
pub mod summary;
pub mod version;

pub fn diff<'a>(lhs_path: &'a PathBuf, rhs_path: &'a PathBuf, display_options: DisplayOptions,
                diff_options: DiffOptions, language_overrides: Vec<(LanguageOverride, Vec<Pattern>)>) -> impl ParallelIterator<Item=DiffResult> + 'a {
    if lhs_path == rhs_path {
        let is_dir = lhs_path.is_dir();

        eprintln!(
            "warning: You've specified the same {} twice.\n",
            if is_dir { "directory" } else { "file" }
        );
    }

    let diff_iter = diff_directories(
        lhs_path,
        rhs_path,
        &display_options,
        &diff_options,
        &language_overrides,
    );

    diff_iter
}

/// Given two directories that contain the files, compare them
/// pairwise. Returns an iterator, so we can print results
/// incrementally.
///
/// When more than one file is modified, the hg extdiff extension passes directory
/// paths with the all the modified files.
fn diff_directories<'a>(
    lhs_dir: &'a Path,
    rhs_dir: &'a Path,
    display_options: &DisplayOptions,
    diff_options: &DiffOptions,
    overrides: &[(LanguageOverride, Vec<Pattern>)],
) -> impl ParallelIterator<Item=DiffResult> + 'a {
    let diff_options = diff_options.clone();
    let display_options = display_options.clone();
    let overrides: Vec<_> = overrides.into();

    // We greedily list all files in the directory, and then diff them
    // in parallel. This is assuming that diffing is slower than
    // enumerating files, so it benefits more from parallelism.
    let paths = relative_paths_in_either(lhs_dir, rhs_dir);

    paths.into_par_iter().map(move |rel_path| {
        info!("Relative path is {:?} inside {:?}", rel_path, lhs_dir);

        let lhs_path = Path::new(lhs_dir).join(&rel_path);
        let rhs_path = Path::new(rhs_dir).join(&rel_path);

        diff_file(
            &rel_path.display().to_string(),
            None,
            &FileArgument::NamedPath(lhs_path),
            &FileArgument::NamedPath(rhs_path),
            &display_options,
            &diff_options,
            true,
            &overrides,
        )
    })
}

/// Print a diff between two files.
pub fn diff_file(
    display_path: &str,
    extra_info: Option<String>,
    lhs_path: &FileArgument,
    rhs_path: &FileArgument,
    display_options: &DisplayOptions,
    diff_options: &DiffOptions,
    missing_as_empty: bool,
    overrides: &[(LanguageOverride, Vec<Pattern>)],
) -> DiffResult {
    let (lhs_bytes, rhs_bytes) = read_files_or_die(lhs_path, rhs_path, missing_as_empty);
    let (mut lhs_src, mut rhs_src) = match (guess_content(&lhs_bytes), guess_content(&rhs_bytes)) {
        (ProbableFileKind::Binary, _) | (_, ProbableFileKind::Binary) => {
            return DiffResult {
                extra_info,
                display_path: display_path.to_owned(),
                file_format: FileFormat::Binary,
                lhs_src: FileContent::Binary,
                rhs_src: FileContent::Binary,
                lhs_positions: vec![],
                rhs_positions: vec![],
                hunks: vec![],
                has_byte_changes: lhs_bytes != rhs_bytes,
                has_syntactic_changes: false,
            };
        }
        (ProbableFileKind::Text(lhs_src), ProbableFileKind::Text(rhs_src)) => (lhs_src, rhs_src),
    };

    if diff_options.strip_cr {
        lhs_src.retain(|c| c != '\r');
        rhs_src.retain(|c| c != '\r');
    }

    diff_file_content(
        display_path,
        extra_info,
        lhs_path,
        rhs_path,
        &lhs_src,
        &rhs_src,
        display_options,
        diff_options,
        overrides,
    )
}

pub fn diff_file_content(
    display_path: &str,
    extra_info: Option<String>,
    _lhs_path: &FileArgument,
    rhs_path: &FileArgument,
    lhs_src: &str,
    rhs_src: &str,
    display_options: &DisplayOptions,
    diff_options: &DiffOptions,
    overrides: &[(LanguageOverride, Vec<Pattern>)],
) -> DiffResult {
    let (guess_src, guess_path) = match rhs_path {
        FileArgument::NamedPath(path) => (&rhs_src, Path::new(path)),
        FileArgument::Stdin => (&rhs_src, Path::new(&display_path)),
        FileArgument::DevNull => (&lhs_src, Path::new(&display_path)),
    };

    let language = guess(guess_path, guess_src, overrides);
    let lang_config = language.map(|lang| (lang.clone(), tsp::from_language(lang)));

    if lhs_src == rhs_src {
        let file_format = match language {
            Some(language) => FileFormat::SupportedLanguage(language),
            None => FileFormat::PlainText,
        };

        // If the two files are completely identical, return early
        // rather than doing any more work.
        return DiffResult {
            extra_info,
            display_path: display_path.to_string(),
            file_format,
            lhs_src: FileContent::Text("".into()),
            rhs_src: FileContent::Text("".into()),
            lhs_positions: vec![],
            rhs_positions: vec![],
            hunks: vec![],
            has_byte_changes: false,
            has_syntactic_changes: false,
        };
    }

    let (file_format, lhs_positions, rhs_positions) = match lang_config {
        None => {
            let file_format = FileFormat::PlainText;
            if diff_options.check_only {
                return check_only_text(&file_format, display_path, extra_info, lhs_src, rhs_src);
            }

            let lhs_positions = line_parser::change_positions(lhs_src, rhs_src);
            let rhs_positions = line_parser::change_positions(rhs_src, lhs_src);
            (file_format, lhs_positions, rhs_positions)
        }
        Some((language, lang_config)) => {
            let arena = Arena::new();
            match tsp::to_tree_with_limit(diff_options, &lang_config, lhs_src, rhs_src) {
                Ok((lhs_tree, rhs_tree)) => {
                    match tsp::to_syntax_with_limit(
                        lhs_src,
                        rhs_src,
                        &lhs_tree,
                        &rhs_tree,
                        &arena,
                        &lang_config,
                        diff_options,
                    ) {
                        Ok((lhs, rhs)) => {
                            if diff_options.check_only {
                                let has_syntactic_changes = lhs != rhs;
                                return DiffResult {
                                    extra_info,
                                    display_path: display_path.to_string(),
                                    file_format: FileFormat::SupportedLanguage(language),
                                    lhs_src: FileContent::Text(lhs_src.to_owned()),
                                    rhs_src: FileContent::Text(rhs_src.to_owned()),
                                    lhs_positions: vec![],
                                    rhs_positions: vec![],
                                    hunks: vec![],
                                    has_byte_changes: true,
                                    has_syntactic_changes,
                                };
                            }

                            let mut change_map = ChangeMap::default();
                            let possibly_changed = if env::var("DFT_DBG_KEEP_UNCHANGED").is_ok() {
                                vec![(lhs.clone(), rhs.clone())]
                            } else {
                                unchanged::mark_unchanged(&lhs, &rhs, &mut change_map)
                            };

                            let mut exceeded_graph_limit = false;

                            for (lhs_section_nodes, rhs_section_nodes) in possibly_changed {
                                init_next_prev(&lhs_section_nodes);
                                init_next_prev(&rhs_section_nodes);

                                match mark_syntax(
                                    lhs_section_nodes.get(0).copied(),
                                    rhs_section_nodes.get(0).copied(),
                                    &mut change_map,
                                    diff_options.graph_limit,
                                ) {
                                    Ok(()) => {}
                                    Err(ExceededGraphLimit {}) => {
                                        exceeded_graph_limit = true;
                                        break;
                                    }
                                }
                            }

                            if exceeded_graph_limit {
                                let lhs_positions = line_parser::change_positions(lhs_src, rhs_src);
                                let rhs_positions = line_parser::change_positions(rhs_src, lhs_src);
                                (
                                    FileFormat::TextFallback {
                                        reason: "exceeded DFT_GRAPH_LIMIT".into(),
                                    },
                                    lhs_positions,
                                    rhs_positions,
                                )
                            } else {
                                fix_all_sliders(language, &lhs, &mut change_map);
                                fix_all_sliders(language, &rhs, &mut change_map);

                                let mut lhs_positions = syntax::change_positions(&lhs, &change_map);
                                let mut rhs_positions = syntax::change_positions(&rhs, &change_map);

                                if diff_options.ignore_comments {
                                    let lhs_comments =
                                        tsp::comment_positions(&lhs_tree, lhs_src, &lang_config);
                                    lhs_positions.extend(lhs_comments);

                                    let rhs_comments =
                                        tsp::comment_positions(&rhs_tree, rhs_src, &lang_config);
                                    rhs_positions.extend(rhs_comments);
                                }

                                (
                                    FileFormat::SupportedLanguage(language),
                                    lhs_positions,
                                    rhs_positions,
                                )
                            }
                        }
                        Err(tsp::ExceededParseErrorLimit(error_count)) => {
                            let file_format = FileFormat::TextFallback {
                                reason: format!(
                                    "{} {} parse error{}, exceeded DFT_PARSE_ERROR_LIMIT",
                                    error_count,
                                    language_name(language),
                                    if error_count == 1 { "" } else { "s" }
                                ),
                            };

                            if diff_options.check_only {
                                return check_only_text(
                                    &file_format,
                                    display_path,
                                    extra_info,
                                    lhs_src,
                                    rhs_src,
                                );
                            }

                            let lhs_positions = line_parser::change_positions(lhs_src, rhs_src);
                            let rhs_positions = line_parser::change_positions(rhs_src, lhs_src);
                            (file_format, lhs_positions, rhs_positions)
                        }
                    }
                }
                Err(tsp::ExceededByteLimit(num_bytes)) => {
                    let file_format = FileFormat::TextFallback {
                        reason: format!(
                            "{} exceeded DFT_BYTE_LIMIT",
                            &format_size(num_bytes, BINARY)
                        ),
                    };

                    if diff_options.check_only {
                        return check_only_text(
                            &file_format,
                            display_path,
                            extra_info,
                            lhs_src,
                            rhs_src,
                        );
                    }

                    let lhs_positions = line_parser::change_positions(lhs_src, rhs_src);
                    let rhs_positions = line_parser::change_positions(rhs_src, lhs_src);
                    (file_format, lhs_positions, rhs_positions)
                }
            }
        }
    };

    let opposite_to_lhs = opposite_positions(&lhs_positions);
    let opposite_to_rhs = opposite_positions(&rhs_positions);

    let hunks = matched_pos_to_hunks(&lhs_positions, &rhs_positions);
    let hunks = merge_adjacent(
        &hunks,
        &opposite_to_lhs,
        &opposite_to_rhs,
        lhs_src.max_line(),
        rhs_src.max_line(),
        display_options.num_context_lines as usize,
    );
    let has_syntactic_changes = !hunks.is_empty();

    DiffResult {
        extra_info,
        display_path: display_path.to_string(),
        file_format,
        lhs_src: FileContent::Text(lhs_src.to_owned()),
        rhs_src: FileContent::Text(rhs_src.to_owned()),
        lhs_positions,
        rhs_positions,
        hunks,
        has_byte_changes: true,
        has_syntactic_changes,
    }
}

fn check_only_text(
    file_format: &FileFormat,
    display_path: &str,
    extra_info: Option<String>,
    lhs_src: &str,
    rhs_src: &str,
) -> DiffResult {
    let has_changes = lhs_src != rhs_src;

    DiffResult {
        display_path: display_path.to_string(),
        extra_info,
        file_format: file_format.clone(),
        lhs_src: FileContent::Text(lhs_src.into()),
        rhs_src: FileContent::Text(rhs_src.into()),
        lhs_positions: vec![],
        rhs_positions: vec![],
        hunks: vec![],
        has_byte_changes: has_changes,
        has_syntactic_changes: has_changes,
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
