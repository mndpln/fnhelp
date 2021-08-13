
#' Plotting categorical variables
#'
#' A function that generates either a boxplot or histogram.
#'
#' @param df An object of class data.frame or tbl.
#' @param plot_type Type of plot to be created (boxplot or histogram).
#' @param var1 Categorical variable in data frame to be used as x-axis in the
#'   boxplot, or as the grouping variable in the histogram.
#' @param var2 Categorical variable in data frame to be used as y-axis in the
#'   boxplot, or as x-axis in the histogram in the histogram.
#' @return A ggplot object. May add additional layers with a \code{+}.
#' @export

plot_category = function(df, plot_type, var1, var2) {
  requireNamespace("ggplot2", quietly = T)
  requireNamespace("rlang", quietly = T)

  var1 = rlang::enquo(var1)
  var2 = rlang::enquo(var2)
  plot_type = rlang::expr_text(rlang::enexpr(plot_type))

  if (tolower(plot_type) == "boxplot") {
    return(.gg.category.boxplot(df, !! var1, !! var2))
  } else if (tolower(plot_type) == "histogram") {
    return(.gg.category.histogram(df, !! var1, !! var2))
  }
}

.plot_category.boxplot = function(df, var1, var2) {
  requireNamespace("rlang", quietly = T)
  requireNamespace("tidyr", quietly = T)

  var1 = rlang::enquo(var1)
  var2 = rlang::enquo(var2)

  df_wrang = df %>%
    dplyr::select(!! var1, !! var2) %>%
    tidyr::drop_na()

  output = ggplot(df, aes(x = as.factor(!! var1), y = !! var2)) +
    geom_point() +
    xlab(quo_name(var1)) + ylab(quo_name(var2)) +
    theme_bw()

  return(output)
}

.plot_category.histogram = function(df, var1, var2) {
  requireNamespace("rlang", quietly = T)
  requireNamespace("tidyr", quietly = T)

  var1 = enquo(var1)
  var2 = enquo(var2)

  df_wrang = df %>%
    dplyr::select(!! var1, !! var2) %>%
    tidyr::drop_na()

  output = ggplot(df_wrang, aes(x = !! var2, stat(width * density))) +
    geom_histogram(aes(group = as.factor(!! var1), color = as.factor(!! var1), fill = as.factor(!! var1)),
                   position = "dodge", alpha = 0.70) +
    xlab(quo_name(var2)) +
    ylab("density") +
    guides(color = "none", fill = guide_legend(title = quo_name(var1))) +
    theme_bw()

  return(output)
}
