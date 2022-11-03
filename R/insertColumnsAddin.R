#' Insert Rmarkdown divs for a two-column layout
#'
#' Insert Rmarkdown divs for a two-column layout into the current
#' document. Uses the pandoc `:::` ... `:::` syntax for divs,
#' with `{.columns}` and `{.column}` tags for column sets and individual
#' columns.
#'
insertColumnsAddin <- function() {
  doc <- rstudioapi::getSourceEditorContext()
  row <- rstudioapi::primary_selection(doc)$range$start['row']
  col <- rstudioapi::primary_selection(doc)$range$start['column']
  rstudioapi::setCursorPosition(c(row, Inf), id = doc$id)
  new_pos <- rstudioapi::primary_selection(doc)
  if (new_pos$range$start['column'] > 1) {
    rstudioapi::insertText(new_pos$start, "\n", doc$id)
    row <- row + 1
  }
  rstudioapi::insertText(
    location = c(row, 1),
    text = paste(
      "::::::::: {.columns}",
      ":::::: {.column}",
      "",
      "::::::",
      ":::::: {.column}",
      "",
      "::::::",
      ":::::::::",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 2, 1), id = doc$id)
}

#' Insert Rmarkdown div for an extra column
#'
#' Insert Rmarkdown div for a single column into a multi-column layout.
#' Uses the pandoc `:::` ... `:::` syntax for divs, with a `{.column}` tag.
#'
insertOneColumnAddin <- function() {
  doc <- rstudioapi::getSourceEditorContext()
  row <- rstudioapi::primary_selection(doc)$range$start['row']
  col <- rstudioapi::primary_selection(doc)$range$start['column']
  rstudioapi::setCursorPosition(c(row, Inf), id = doc$id)
  new_pos <- rstudioapi::primary_selection(doc)
  if (new_pos$range$start['column'] > 1) {
    rstudioapi::insertText(new_pos$start, "\n", doc$id)
    row <- row + 1
  }
  rstudioapi::insertText(
    location = c(row, 1),
    text = paste(
      ":::::: {.column}",
      "",
      "::::::",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 1, 1), id = doc$id)
}

#' Insert Rmarkdown column break
#'
#' Insert Rmarkdown code to end a column div and start another
#'
insertColumnBreakAddin <- function() {
  doc <- rstudioapi::getSourceEditorContext()
  row <- rstudioapi::primary_selection(doc)$range$start['row']
  col <- rstudioapi::primary_selection(doc)$range$start['column']
  rstudioapi::setCursorPosition(c(row, Inf), id = doc$id)
  new_pos <- rstudioapi::primary_selection(doc)
  if (new_pos$range$start['column'] > 1) {
    rstudioapi::insertText(new_pos$start, "\n", doc$id)
    row <- row + 1
  }
  rstudioapi::insertText(
    location = c(row, 1),
    text = paste(
      "::::::",
      ":::::: {.column}",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 2, 1), id = doc$id)
}

#' Insert Rmarkdown max-listing div
#'
#' Insert div with `.maxlisting` class.
#'
insertMaxListingAddin <- function() {
  doc <- rstudioapi::getSourceEditorContext()
  row <- rstudioapi::primary_selection(doc)$range$start['row']
  col <- rstudioapi::primary_selection(doc)$range$start['column']
  rstudioapi::setCursorPosition(c(row, Inf), id = doc$id)
  new_pos <- rstudioapi::primary_selection(doc)
  if (new_pos$range$start['column'] > 1) {
    rstudioapi::insertText(new_pos$start, "\n", doc$id)
    row <- row + 1
  }
  rstudioapi::insertText(
    location = c(row, 1),
    text = paste(
      "::: {.max-listing}",
      ":::",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 1, 1), id = doc$id)
}

#' Insert LaTeX displaymath align block
#'
#' Insert LaTeX displaymath blosk with an align environment.
#'
insertDisplayAlignAddin <- function() {
  doc <- rstudioapi::getSourceEditorContext()
  row <- rstudioapi::primary_selection(doc)$range$start['row']
  col <- rstudioapi::primary_selection(doc)$range$start['column']
  rstudioapi::setCursorPosition(c(row, Inf), id = doc$id)
  new_pos <- rstudioapi::primary_selection(doc)
  if (new_pos$range$start['column'] > 1) {
    rstudioapi::insertText(new_pos$start, "\n", doc$id)
    row <- row + 1
  }
  rstudioapi::insertText(
    location = c(row, 1),
    text = paste(
      "$$",
      "\\begin{align}",
      "",
      "\\end{align}",
      "$$",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 2, 1), id = doc$id)
}
