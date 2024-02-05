#' @title Calculate Average Quality-Adjusted Life Years (QALYs) by Deprivation
#' Quintile
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
#' @examples
#' \dontrun{
#' average_QALYs <- calculate_average_QALYs(
#'   absolute_QALYs_ = calculate_absolute_QALYs(
#'     target_maximum_health_ = input_data_mQALE$`Target maximum QALE`,
#'     baseline_health_ = input_data_mQALE$`Baseline health`[1:5],
#'     mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'     equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'     option_ = "estimated_mortality_elasticity",
#'     imd_population_ = CCG_IMD_population_2019,
#'     provider_ = "CCG"
#'   )$data[[1]],
#'   imd_population_ = CCG_IMD_population_2019
#' )
#' average_QALYs_equ_elas <- calculate_average_QALYs(
#'   absolute_QALYs_ = calculate_absolute_QALYs(
#'     target_maximum_health_ = input_data_mQALE$`Target maximum QALE`,
#'     baseline_health_ = input_data_mQALE$`Baseline health`[1:5],
#'     mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'     equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'     option_ = "equal_mortality_elasticity",
#'     imd_population_ = CCG_IMD_population_2019,
#'     provider_ = "CCG"
#'   )$data[[1]],
#'   imd_population_ = CCG_IMD_population_2019
#' )
#' }
calculate_average_QALYs <- function(
    absolute_QALYs_,
    imd_population_) {

  ## Auxiliary data:
  quintile_names <- grep(
    pattern = "Q",
    x = colnames(absolute_QALYs_),
    ignore.case = TRUE,
    value = TRUE
  )
  total_pop_quintile <- colSums(imd_population_[, quintile_names])
  total_QALYs_change_quintile <- colSums(
    absolute_QALYs_[, quintile_names, with = FALSE]
  )

  ## Estimate average QALYs change per quintile:
  average_QALYs_change_quintile <-
    (total_QALYs_change_quintile / total_pop_quintile)

  ## Estimate average percentage change in healthcare expenditure:
  average_percent_change <- mean(imd_population_$`Expenditure change (%)`)

  return(
    list(
      "title" = paste0(
        "Average Impact on QALE by Deprivation Quintile"
      ),
      "subtitle" = paste0(
        "Quality-Adjusted Life Expectancy (QALE) per Person in Quintile"
      ),
      "caption" = paste0(
        "Average QALYs estimated assuming ",
        round(average_percent_change, digits = 2),
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
