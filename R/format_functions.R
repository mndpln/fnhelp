
#' Write Markdown headers
#'
#' Writes the appropriate number of hash signs for the specified text level.
#' Intended for creating sections in an R Markdown document using for-loops.
#'
#' @param text Text to put as header.
#' @param level Header level.
#' @return A Markdown-formatted heading.
#' @export

cat_header = function(text = "", level = 3) {
  cat(paste0("\n\n",
             paste(rep("#", level), collapse = ""),
             " ", text, "\n"))
}
