#' Insert Rmarkdown div for an extra column
#'
#' Insert Rmarkdown div for a single column into a multi-column layout.
#' Uses the pandoc `:::` ... `:::` syntax for divs, with a
#' \code{{.column}} tag.
#'
#' @export
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
      "",
      ":::::: {.column}",
      "",
      "::::::",
      "",
      sep = "\n"
    ),
    id = doc$id
  )
  rstudioapi::setCursorPosition(c(row + 2, 1), id = doc$id)
}
