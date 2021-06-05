
#' Create bar charts for categorical variables
#'
#' @param df An object of class tbl.df, data.frame or tbl.
#' @param filter_var Variable you want to filter the dataset on.
#' @param filter_value Value to filter for.
#' @param facet Boolean. Add a facet to the plot or not.
#' @param facet_var Variable to facet on.
#' @param plot_title A string. Plot title.
#' @param aes_x The variable to be displayed on the x-axis.
#' @param aes_y The variable to be displayed on the y-axis.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @return A ggplot object. May add additional layers with a \code{+}.
#' @export

create_barplot = function(df,
                           filter_var = NULL,
                           filter_value = NULL,
                           facet = FALSE,
                           facet_var = NULL,
                           plot_title,
                           aes_x,
                           aes_y = ..prop..,
                           xlab,
                           ylab = "Proportion") {
  filter_var = enquo(filter_var)
  filter_value = enquo(filter_value)
  if (quo_is_null(filter_var)) {
    data = df
  } else {
    data = df %>%
      filter(!!filter_var == !!filter_value)
  }

  aes_x = ensym(aes_x)
  aes_y = ensym(aes_y)
  facet_var = enquo(facet_var)

  if (facet == TRUE) {
    label_df = data %>%
      group_by(!!facet_var, !!aes_x) %>%
      count() %>%
      group_by(!!facet_var) %>%
      mutate(group_sum = sum(n))

  } else {
    label_df = data %>%
      group_by(!!aes_x) %>%
      count()
  }

  if (facet == TRUE) {
    ggplot(data) +
      stat_count(aes(
        x = !!aes_x,
        y = !!aes_y,
        group = !!facet_var
      ), width = 0.5) +
      labs(title = plot_title,
           x = xlab,
           y = ylab) +
      geom_label(
        data = label_df,
        aes(
          x = !!aes_x,
          y = (n / group_sum) + 0.015,
          label = scales::comma(n, accuracy = 1)
        ),
        size = 3
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 9)) + facet_wrap(vars(!!facet_var))
  } else {
    ggplot(data) +
      stat_count(aes(
        x = !!aes_x,
        y = !!aes_y,
        group = 1
      ), width = 0.5) +
      labs(title = plot_title,
           x = xlab,
           y = ylab) +
      geom_label(
        data = label_df,
        aes(
          x = !!aes_x,
          y = (n / sum(n)) + 0.015,
          label = scales::comma(n, accuracy = 1)
        ),
        size = 3
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(size = 9))
  }
}
