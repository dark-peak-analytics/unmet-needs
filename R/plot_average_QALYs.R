################################################################################
#
# Script Name:        plot_average_QALYs.R
# Script Description: Defines the function plot_average_QALYs(). This
#                     function plots the average change in Quality-Adjusted Life
#                     Years (QALYs).
#
################################################################################

#' @title Plot the average change in Quality-Adjusted Life Years (QALYs).
#' @details This function takes ... inputs and then ... to plot the average
#' change in Quality-Adjusted Life Years (QALYs).
#'
#' @param average_QALYs_ An object representing the outputs of the function
#' \link[UnmetNeeds]{calculate_average_QALYs}.
#'
#' @return A list containing ggplot2 objects.
#'
#' @export
#' @examples
#' \dontrun{
#' average_QALYs_change_plot <- plot_average_QALYs()
#' }
plot_average_QALYs <- function(
    average_QALYs_ = UnmetNeeds::calculate_average_QALYs()) {

  ## Sanity checks:

  ## Auxiliary data:
  average_QALYs_plot_data <- average_QALYs_[["data"]]
  average_QALYs_plot_title <- average_QALYs_[["title"]]
  average_QALYs_plot_subtitle <- average_QALYs_[["subtitle"]]
  average_QALYs_plot_caption <- average_QALYs_[["caption"]]

  ## Plot:
  average_QALYs_plot <- average_QALYs_plot_data |>
    ggplot2::ggplot(
      data = _,
      ggplot2::aes(
        x = `Average change in QALYs`,
        y = `Deprivation Quintile`
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      title = average_QALYs_plot_title,
      subtitle = average_QALYs_plot_subtitle,
      caption = average_QALYs_plot_caption
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
      "title" = average_QALYs_plot_title,
      "data" = average_QALYs_plot
    )
  )
}
