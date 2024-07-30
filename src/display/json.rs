use lazy_static::lazy_static;
use line_numbers::LineNumber;
use regex::Regex;
use serde::{ser::SerializeStruct, Serialize, Serializer};

use crate::{
    display::{
        context::{all_matched_lines_filled, opposite_positions},
        hunks::{matched_lines_indexes_for_hunk, matched_pos_to_hunks, merge_adjacent},
    },
    lines::MaxLine,
    parse::syntax::{self, MatchedPos, StringKind},
    summary::{DiffResult, FileContent, FileFormat},
};
use crate::display::side_by_side::lines_with_novel;
use crate::parse::syntax::NON_EXISTENT_PERMISSIONS;

lazy_static! {
    static ref FILE_PERMS_RE: Regex =
        Regex::new("^File permissions changed from (\\d+) to (\\d+).$").unwrap();
}

#[derive(Debug, Serialize, PartialEq)]
#[serde(rename_all = "lowercase")]
enum Status {
    Unchanged,
    Changed,
    Created,
    Deleted,
    Renamed,
}

#[derive(Debug)]
struct File<'f> {
    language: &'f FileFormat,
    path: &'f str,
    old_path: Option<&'f str>,
    chunks: Vec<Vec<Line<'f>>>,
    status: Status,
}

impl<'f> File<'f> {
    fn with_sections(
        language: &'f FileFormat,
        path: &'f str,
        extra_info: &'f Option<String>,
        chunks: Vec<Vec<Line<'f>>>,
    ) -> File<'f> {
        let old_path = Self::extract_old_path(extra_info);
        File {
            language,
            path,
            chunks,
            old_path,
            status: Status::Changed,
        }
    }

    fn extract_old_path(extra_info: &'f Option<String>) -> Option<&'f str> {
        match extra_info {
            None => None,
            Some(s) => {
                if s.starts_with("Renamed from ") {
                    let after_branch_name = s.find(" to ").unwrap();
                    Some(&s["Renamed from ".len()..after_branch_name])
                } else {
                    None
                }
            }
        }
    }

    fn extract_status(extra_info: &'f Option<String>) -> Option<(String, String)> {
        let temp = extra_info.clone().unwrap_or_default();
        let line = temp.lines().last();
        match line {
            None => None,
            Some(s) => {
                if let Some(captures) = crate::display::json::FILE_PERMS_RE.captures(s) {
                    let from = captures[1].to_string();
                    let to = captures[2].to_string();
                    Some((from, to))
                } else {
                    None
                }
            }
        }
    }

    fn with_status(
        language: &'f FileFormat,
        path: &'f str,
        extra_info: &'f Option<String>,
        status: Status,
    ) -> File<'f> {
        File {
            language,
            path,
            chunks: Vec::new(),
            old_path: Self::extract_old_path(extra_info),
            status,
        }
    }
}

impl<'f> From<&'f DiffResult> for File<'f> {
    fn from(summary: &'f DiffResult) -> Self {
        match (&summary.lhs_src, &summary.rhs_src) {
            (FileContent::Text(lhs_src), FileContent::Text(rhs_src)) => {
                // TODO: move into function as it is effectively duplicates lines 365-375 of main::print_diff_result
                let opposite_to_lhs = opposite_positions(&summary.lhs_positions);
                let opposite_to_rhs = opposite_positions(&summary.rhs_positions);

                let hunks = matched_pos_to_hunks(&summary.lhs_positions, &summary.rhs_positions);
                let hunks = merge_adjacent(
                    &hunks,
                    &opposite_to_lhs,
                    &opposite_to_rhs,
                    lhs_src.max_line(),
                    rhs_src.max_line(),
                    0,
                );

                if hunks.is_empty() {
                    let status = if File::extract_old_path(&summary.extra_info).is_some() {
                        Status::Renamed
                    } else if let Some((from, to)) = File::extract_status(&summary.extra_info) {
                        match (from.as_str(), to.as_str()) {
                            (NON_EXISTENT_PERMISSIONS, _) => Status::Created,
                            (_, NON_EXISTENT_PERMISSIONS) => Status::Deleted,
                            _ => {
                                if from == to {
                                    Status::Unchanged
                                } else {
                                    Status::Changed
                                }
                            }
                        }
                    } else {
                        Status::Unchanged
                    };
                    return File::with_status(
                        &summary.file_format,
                        &summary.display_path,
                        &summary.extra_info,
                        status,
                    );
                }

                if lhs_src.is_empty() {
                    return File::with_status(
                        &summary.file_format,
                        &summary.display_path,
                        &summary.extra_info,
                        Status::Created,
                    );
                }
                if rhs_src.is_empty() {
                    return File::with_status(
                        &summary.file_format,
                        &summary.display_path,
                        &summary.extra_info,
                        Status::Deleted,
                    );
                }
                let (lhs_lines_with_novel, rhs_lines_with_novel) =
                    lines_with_novel(&summary.lhs_positions, &summary.rhs_positions);

                let lhs_lines = lhs_src.split('\n').collect::<Vec<&str>>();
                let rhs_lines = rhs_src.split('\n').collect::<Vec<&str>>();

                let matched_lines = all_matched_lines_filled(
                    &summary.lhs_positions,
                    &summary.rhs_positions,
                    &lhs_lines,
                    &rhs_lines,
                );
                let mut matched_lines = &matched_lines[..];

                let mut chunks = Vec::with_capacity(hunks.len());
                for hunk in &hunks {
                    let mut lines = Vec::with_capacity(hunk.lines.len());

                    let (start_i, end_i) = matched_lines_indexes_for_hunk(matched_lines, hunk, 3);
                    let aligned_lines = &matched_lines[start_i..end_i];
                    matched_lines = &matched_lines[start_i..];

                    for (lhs_line_num, rhs_line_num) in aligned_lines {
                        let mut line =
                            Line::new(lhs_line_num.map(|l| l.0), rhs_line_num.map(|l| l.0));
                        if !lhs_lines_with_novel.contains(&lhs_line_num.unwrap_or(LineNumber(0)))
                            && !rhs_lines_with_novel
                                .contains(&rhs_line_num.unwrap_or(LineNumber(0)))
                        {
                            lines.push(line);
                            continue;
                        }

                        if let Some(line_num) = lhs_line_num {
                            line.status = Status::Changed;
                            add_changes_to_side(
                                line.lhs.as_mut().unwrap(),
                                *line_num,
                                &lhs_lines,
                                &summary.lhs_positions,
                            );
                        }
                        if let Some(line_num) = rhs_line_num {
                            line.status = Status::Changed;
                            add_changes_to_side(
                                line.rhs.as_mut().unwrap(),
                                *line_num,
                                &rhs_lines,
                                &summary.rhs_positions,
                            );
                        }
                        lines.push(line);
                    }

                    chunks.push(lines);
                }

                File::with_sections(
                    &summary.file_format,
                    &summary.display_path,
                    &summary.extra_info,
                    chunks,
                )
            }
            (FileContent::Binary, FileContent::Binary) => {
                let status = if summary.has_byte_changes {
                    Status::Changed
                } else {
                    Status::Unchanged
                };
                File::with_status(
                    &FileFormat::Binary,
                    &summary.display_path,
                    &summary.extra_info,
                    status,
                )
            }
            (_, FileContent::Binary) | (FileContent::Binary, _) => File::with_status(
                &FileFormat::Binary,
                &summary.display_path,
                &summary.extra_info,
                Status::Changed,
            ),
        }
    }
}

impl<'f> Serialize for File<'f> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // equivalent to #[serde(skip_serializing_if = "Vec::is_empty")]
        let mut file = if self.chunks.is_empty() {
            serializer.serialize_struct("File", 3)?
        } else {
            let mut file = serializer.serialize_struct("File", 4)?;
            file.serialize_field("chunks", &self.chunks)?;
            file
        };

        file.serialize_field("language", &format!("{}", self.language))?;
        file.serialize_field("path", &self.path)?;
        if let Some(old_path) = self.old_path {
            file.serialize_field("old_path", &format!("{}", old_path))?;
        }
        file.serialize_field("status", &self.status)?;

        file.end()
    }
}

#[derive(Debug, Serialize)]
struct Line<'l> {
    #[serde(skip_serializing_if = "Option::is_none")]
    lhs: Option<Side<'l>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    rhs: Option<Side<'l>>,
    status: Status,
}

impl<'l> Line<'l> {
    fn new(lhs_number: Option<u32>, rhs_number: Option<u32>) -> Line<'l> {
        Line {
            lhs: lhs_number.map(Side::new),
            rhs: rhs_number.map(Side::new),
            status: Status::Unchanged,
        }
    }
}

#[derive(Debug, Serialize)]
struct Side<'s> {
    line_number: u32,
    changes: Vec<Change<'s>>,
}

impl<'s> Side<'s> {
    fn new(line_number: u32) -> Side<'s> {
        Side {
            line_number,
            changes: Vec::new(),
        }
    }
}

#[derive(Debug, Serialize)]
struct Change<'c> {
    start: u32,
    end: u32,
    content: &'c str,
    highlight: Highlight,
    highlight_type: &'c str,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "snake_case")]
// TODO: use syntax::TokenKind and syntax::AtomKind instead of this merged enum,
// blocked by https://github.com/serde-rs/serde/issues/1402
enum Highlight {
    Delimiter,
    Normal,
    String,
    Type,
    Comment,
    Keyword,
    TreeSitterError,
}

impl Highlight {
    fn from_match(kind: &syntax::MatchKind) -> Self {
        use syntax::{AtomKind, MatchKind, TokenKind};

        let highlight = match kind {
            MatchKind::Ignored { highlight, .. } => highlight,
            MatchKind::UnchangedToken { highlight, .. } => highlight,
            MatchKind::Novel { highlight, .. } => highlight,
            MatchKind::NovelWord { highlight, .. } => highlight,
            MatchKind::NovelLinePart { highlight, .. } => highlight,
        };

        match highlight {
            TokenKind::Delimiter => Highlight::Delimiter,
            TokenKind::Atom(atom) => match atom {
                AtomKind::String(StringKind::StringLiteral) => Highlight::String,
                AtomKind::String(StringKind::Text) => Highlight::Normal,
                AtomKind::Keyword => Highlight::Keyword,
                AtomKind::Comment => Highlight::Comment,
                AtomKind::Type => Highlight::Type,
                AtomKind::Normal => Highlight::Normal,
                AtomKind::TreeSitterError => Highlight::TreeSitterError,
            },
        }
    }
}

pub(crate) fn print_directory(diffs: Vec<DiffResult>, print_unchanged: bool) {
    let files = diffs
        .iter()
        .map(File::from)
        .filter(|f| print_unchanged || f.status != Status::Unchanged)
        .collect::<Vec<File>>();
    println!(
        "{}",
        serde_json::to_string(&files).expect("failed to serialize files")
    );
}

pub(crate) fn print(diff: &DiffResult) {
    let file = File::from(diff);
    println!(
        "{}",
        serde_json::to_string(&file).expect("failed to serialize file")
    )
}

fn add_changes_to_side<'s>(
    side: &mut Side<'s>,
    line_num: LineNumber,
    src_lines: &[&'s str],
    all_matches: &'s [MatchedPos],
) {
    let src_line = src_lines[line_num.0 as usize];

    let matches = matches_for_line(all_matches, line_num);
    for m in matches {
        side.changes.push(Change {
            start: m.pos.start_col,
            end: m.pos.end_col,
            content: &src_line[(m.pos.start_col as usize)..(m.pos.end_col as usize)],
            highlight: Highlight::from_match(&m.kind),
            highlight_type: m.kind.as_ref(),
        })
    }
}

fn matches_for_line(matches: &[MatchedPos], line_num: LineNumber) -> Vec<&MatchedPos> {
    matches
        .iter()
        .filter(|m| m.pos.line == line_num)
        .filter(|m| m.kind.is_novel())
        .collect()
}
