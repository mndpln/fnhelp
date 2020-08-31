
#' Export data frame to Word document
#'
#' Exports a data frame as a Word document in the directory specified
#'
#' @param df The data frame to be exported
#' @param directory The directory where the Word file should be saved as a string.
#' @return Location of Word document created

word_saver = function(df, directory) {
  requireNamespace("officer", quietly = T)
  filename = deparse(substitute(df))

  x = df %>%
    flextable() %>%
    fontsize(size = 10, part = "all")

  read_docx() %>%
    body_add_flextable(x) %>%
    print(target = sprintf("%s/%s", directory, filename))
}
