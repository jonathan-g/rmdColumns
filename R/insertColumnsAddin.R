#' Insert Rmarkdown divs for a two-column layout
#'
#' Insert Rmarkdown divs for a two-column layout into the current
#' document. Uses the pandoc \code{:::} ... \code{:::} syntax for divs,
#' with \code{{.columns}} and \code{{.column}}
#' tags for column sets and individual
#' columns.
#'
#' @export
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
      "",
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
  rstudioapi::setCursorPosition(c(row + 3, 1), id = doc$id)
}
