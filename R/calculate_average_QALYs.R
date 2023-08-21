################################################################################
#
# Script Name:        calculate_average_QALYs.R
# Script Description: Defines the function calculate_average_QALYs(). This
#                     function estimates the average Quality-Adjusted Life
#                     Years (QALY) gained by deprivation quintile.
#
################################################################################

#' @title Calculate Average Quality-Adjusted Life Years (QALYs) by Deprivation
#' Quintile.
#' @description Estimates the the average Quality-Adjusted Life Year (QALY) per
#' deprivation quintile.
#' @details This function takes ... inputs and then ... to estimate the average
#' QALYs.
#' @param absolute_QALYs_ Dataframe or data table representing the outputs of
#' the \link[UnmetNeeds]{calculate_absolute_QALYs}.
#' @param imd_population_ Dataframe with population distribution by deprivation.
#'
#' @return A dataframe.
#'
#' @export
#'
calculate_average_QALYs <- function(
    absolute_QALYs_ = UnmetNeeds::calculate_absolute_QALYs()$data,
    imd_population_ = CCG_IMD_population_2019) {

  ## Auxiliary data:
  quintile_names <- grep(
    pattern = "Q",
    x = colnames(absolute_QALYs_),
    ignore.case = TRUE,
    value = TRUE
  )
  total_pop_quintile <- colSums(imd_population_[, quintile_names])
  total_QALYs_change_quintile <- colSums(absolute_QALYs_[, ..quintile_names])

  ## Estimate average QALYs change per quintile:
  average_QALYs_change_quintile <-
    (total_QALYs_change_quintile / total_pop_quintile) * 1e5

  return(
    list(
      "title" = paste0(
        "Average Health Impact by Deprivation Quintile"
      ),
      "subtitle" = paste0(
        "Quality-Adjusted Life Years (QALYs) per 100,000 Population in Quintile"
      ),
      "caption" = paste0(
        "Average QALYs estimated assuming ",
        imd_population_$`Expenditure change (%)`[[1]],
        "% change in national healthcare expenditure."
      ),
      "data" = data.table::data.table(
        "Deprivation Quintile" = names(total_pop_quintile),
        "Average change in QALYs" = average_QALYs_change_quintile |>
          unname()
      )
    )
  )
}
