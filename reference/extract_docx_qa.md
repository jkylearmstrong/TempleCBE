# Extract Questions, Answers and Cited Reports From a Word Document

Reads the body of a \`.docx\` that lists investigator questions, each
followed by the answer paragraphs written for it, and returns one row
per question with its answer and the report files the answer cites. This
is for tracking reviewer or PI questions across a project (a Q&A sheet
in Word turned into a deliverables table); it does not read Word's
comments or who made a tracked change, for which see
\[docx_review_extract()\].

## Usage

``` r
extract_docx_qa(
  docx_path,
  question_pattern = "^(What|Why|How|Summarize|Explain|Can|Is|Are)\\b|\\?$",
  citation_pattern = "(?i)\\b[[:alnum:]_.\\-]+\\.(pdf|docx|html)\\b",
  index_data = NULL,
  index_file_col = "file",
  index_path_col = "path"
)
```

## Arguments

- docx_path:

  Path to the \`.docx\` file.

- question_pattern:

  Regular expression (PCRE) that marks a question paragraph. The default
  matches paragraphs that begin with a question word (as a whole word,
  so "Isolated" is not "Is") or end with \`?\`.

- citation_pattern:

  Regular expression (PCRE) for a cited file name. The default matches
  names ending in \`.pdf\`, \`.docx\` or \`.html\`, in any letter case.

- index_data:

  Optional data frame of the deliverables, with one row per file.

- index_file_col, index_path_col:

  Columns of \`index_data\` with the file name and with the path or URL
  the link should point to (default \`"file"\` and \`"path"\`).

## Value

A tibble with one row per question: \`question_id\`, \`question\`,
\`answer\`, \`n_answer_paragraphs\`, \`citations\` (list of character
vectors), \`citation_files\` (the citations joined with \`"; "\`) and,
when \`index_data\` is given, \`links\` (list of HYPERLINK formulas, one
per resolved citation) and \`unresolved_citations\` (joined with \`";
"\`).

## Details

A paragraph is a \*\*question\*\* if it matches \`question_pattern\`;
every following paragraph up to the next question is part of its
\*\*answer\*\* (joined with a line break). Paragraphs before the first
question are ignored. The text is what the document currently reads:
tracked insertions count, tracked deletions do not. A table's paragraphs
count, in document order; the text of a text box is read once, as its
own paragraph. Empty paragraphs are dropped.

\*\*The default \`question_pattern\` is simple and can misfire\*\*: an
answer paragraph that begins with one of its words ("Can't say.", "How
we handled it: see a.pdf") or ends with \`?\` is taken for a new
question, and the answer it interrupts is cut short without a warning.
Check \`n_answer_paragraphs\`, or give a stricter pattern (for example
\`"^Q\[0-9\]+\[.:\]"\` when the questions are numbered).

\*\*Citations\*\* are the matches of \`citation_pattern\` in the answer.
With \`index_data\`, each is looked up (by file name, ignoring case and
folders) in the index; the column \`links\` then holds, per question,
the Excel formulas \`=HYPERLINK("\<path\>", "\<file\>")\` of the
citations the index knows, as character strings (write them to a
workbook cell with \`openxlsx::writeFormula()\`; Excel limits a string
in a formula to 255 characters, so longer paths are reported with a
warning and are not linked). A citation the index does not know is
listed in \`unresolved_citations\`; an index with the same file name
twice uses the first and warns.

The question and answer text is returned as it is in the document. If
you write it to a CSV or workbook, text that begins with \`=\`, \`+\`,
\`-\` or \`@\` is read by spreadsheet programs as a formula.

## See also

\[docx_review_extract()\], \[package_deliverables()\]

## Examples

``` r
if (FALSE) { # \dontrun{
qa <- extract_docx_qa("investigator_questions.docx")
qa$citations

index <- data.frame(file = c("model_report.pdf"), path = c("reports/model_report.pdf"))
extract_docx_qa("investigator_questions.docx", index_data = index)$links
} # }
```
