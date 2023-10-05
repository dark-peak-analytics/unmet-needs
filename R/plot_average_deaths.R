#' @title Plot the average deaths by deprivation quintile
#' @details Plot the average deaths.
#'
#' @param average_deaths_ An object representing the outputs of the function
#' \link[UnmetNeeds]{calculate_average_deaths}.
#'
#' @return A list containing ggplot2 objects.
#'
#' @export
#' @examples
#' \dontrun{
#' average_deaths_bar_chart <- plot_average_deaths()
#' }
plot_average_deaths <- function(average_deaths_) {

  ## Sanity checks:

  ## Auxiliary data:
  average_deaths_plot_data <- average_deaths_[["data"]]
  average_deaths_plot_title <- average_deaths_[["title"]]
  average_deaths_plot_subtitle <- average_deaths_[["subtitle"]]
  average_deaths_plot_caption <- average_deaths_[["caption"]]

  ## Plot:
  `Deaths prevented` <- `Deprivation Quintile` <- NULL
  average_deaths_plot <- average_deaths_plot_data |>
    ggplot2::ggplot(
      data = _,
      ggplot2::aes(
        x = `Mortality Impact`,
        y = `Deprivation Quintile`
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title = average_deaths_plot_title,
      subtitle = average_deaths_plot_subtitle,
      caption = average_deaths_plot_caption
    ) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = ggplot2::element_text(
        hjust = 0
      ),
      panel.background = ggplot2::element_blank()
    )

  return(
    list(
      "title" = average_deaths_plot_title,
      "data" = average_deaths_plot
    )
  )
}
