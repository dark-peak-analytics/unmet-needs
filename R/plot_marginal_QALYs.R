#' @title Calculate the marginal QALYs
#' @description Estimates the the marginal Quality-Adjusted Life Expectancy
#' (QALE) per deprivation quintile.
#' @details This function takes ... inputs and then ... to estimate the marginal
#' QALE
#'
#' @param marginal_QALYs_ An object representing the outputs of the function
#' \link[UnmetNeeds]{calculate_marginal_QALYs}.
#'
#' @return A list containing ggplot2 objects.
#'
#' @export
#'
plot_marginal_QALYs <- function(marginal_QALYs_) {

  ## Sanity checks:

  ## Auxiliary data:
  marginal_QALYs_plot_data <- marginal_QALYs_[["data"]]
  marginal_QALYs_plot_title <- marginal_QALYs_[["title"]]
  marginal_QALYs_plot_caption <- marginal_QALYs_[["caption"]]

  ## Plot:
  `Marginal QALY` <- `Deprivation Quintile` <- NULL
  marginal_QALYs_plot_list <- lapply(
    X = 1:length(marginal_QALYs_plot_data),
    FUN = function(dataset_) {
      marginal_QALYs_plot_data[[dataset_]] |>
        base::subset(subset = `Deprivation Quintile` != "Average") |>
      ggplot2::ggplot(
        data = _,
        ggplot2::aes(
          x = `Marginal QALY`,
          y = `Deprivation Quintile`
        )
      ) +
        ggplot2::geom_col() +
        ggplot2::labs(
          title = paste(
            marginal_QALYs_plot_title
          ),
          caption = paste(
            marginal_QALYs_plot_caption[dataset_]
          )
        ) +
        ggplot2::theme(
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = ggplot2::element_text(
            hjust = 0
          ),
          panel.background = ggplot2::element_blank()
        )
    }
  )

  return(
    list(
      "title" = "Plots share of the marginal QALY from 1% increase in
      expenditure",
      "data" = marginal_QALYs_plot_list
    )
  )
}

