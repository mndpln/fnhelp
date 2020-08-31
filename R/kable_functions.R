
#' Automatic kable formatting
#'
#' Formats data frames and tibbles as kables.
#'
#' @param df An object of class tbl.df, data.frame or tbl.
#' @param position Alignment of kable output as a string. Default value is \code{"center"}.
#' @return A kable. Output may be piped into specific formatting preferences for each column.

format_kable = function(df, position = "center") {
  requireNamespace("kableExtra", quietly = TRUE)
  requireNamespace("stringi", quietly = TRUE)
  align = stringi::stri_paste("l", paste(rep("c", ncol(df) - 1), collapse = ""),
                              sep = "")

  output = kableExtra::kable(
    df,
    format = "html",
    digits = 2,
    align = align,
    bold = T
  ) %>%
    kableExtra::kable_styling("striped", full_width = F, position = position) %>%
    kableExtra::collapse_rows(columns = 1, valign = "middle")

  return(output)
}
