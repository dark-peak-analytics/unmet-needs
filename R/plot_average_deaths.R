################################################################################
#
# Script Name:        plot_average_deaths.R
# Script Description: Defines the function plot_average_deaths(). This function
#                     plots the average deaths per 100,000 population by
#                     deprivation quintile.
#
################################################################################

#' @title Plot the average deaths by deprivation quintile.
#' @details Plot the average deaths.
#'
#' @param average_lives_saved_ An object representing the outputs of the function
#' \link[UnmetNeeds]{calculate_average_deaths}.
#'
#' @return A list containing ggplot2 objects.
#'
#' @export
#' @examples
#' \dontrun{
#' average_lives_saved_bar_chart <- plot_average_deaths()
#' }
plot_average_deaths <- function(
    average_lives_saved_ = UnmetNeeds::calculate_average_deaths()) {

  ## Sanity checks:

  ## Auxiliary data:
  average_lives_saved_plot_data <- average_lives_saved_[["data"]]
  average_lives_saved_plot_title <- average_lives_saved_[["title"]]
  average_lives_saved_plot_subtitle <- average_lives_saved_[["subtitle"]]
  average_lives_saved_plot_caption <- average_lives_saved_[["caption"]]

  ## Plot:
  average_lives_saved_plot <- average_lives_saved_plot_data |>
    ggplot2::ggplot(
      data = _,
      ggplot2::aes(
        x = `Deaths prevented`,
        y = `Deprivation Quintile`
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title = average_lives_saved_plot_title,
      subtitle = average_lives_saved_plot_subtitle,
      caption = average_lives_saved_plot_caption
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
      "title" = average_lives_saved_plot_title,
      "data" = average_lives_saved_plot
    )
  )
}
