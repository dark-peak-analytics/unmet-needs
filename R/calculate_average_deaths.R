#' @title Calculate Average Number of Deaths Prevented by Deprivation Quintile
#' @description Estimates the the average Quality-Adjusted Life Year (QALY) per
#' deprivation quintile.
#' @details This function takes ... inputs and then ... to estimate the average
#' QALYs.
#' @param total_deaths_ Dataframe or data table representing the outputs of
#' the \link[UnmetNeeds]{calculate_absolute_QALYs}.
#' @param imd_population_ Dataframe with population distribution by deprivation.
#'
#' @return A dataframe.
#'
#' @export
#' @examples
#' \dontrun{
#' average_deaths <- calculate_average_deaths(
#'   total_deaths_ = calculate_total_deaths(
#'     mortality_rates_ = input_data_mQALE$`Mortality rate`[1:5],
#'     mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'     equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'     option_ = "estimated_mortality_elasticity",
#'     imd_population_ = CCG_IMD_population_2019,
#'     provider_ = "CCG"
#'   )$data,
#'   imd_population_ = CCG_IMD_population_2019
#' )
#' average_deaths_equ_elas <- calculate_average_deaths(
#'   total_deaths_ = calculate_total_deaths(
#'     mortality_rates_ = input_data_mQALE$`Mortality rate`[1:5],
#'     mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'     equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'     option_ = "equal_mortality_elasticity",
#'     imd_population_ = CCG_IMD_population_2019,
#'     provider_ = "CCG"
#'   )$data,
#'   imd_population_ = CCG_IMD_population_2019
#' )
#' }
calculate_average_deaths <- function(
    total_deaths_,
    imd_population_) {

  ## Auxiliary data:
  quintile_names <- grep(
    pattern = "Q",
    x = colnames(total_deaths_),
    ignore.case = TRUE,
    value = TRUE
  )
  total_pop_quintile <- colSums(imd_population_[, quintile_names])
  total_deaths_quintile <- colSums(
    total_deaths_[, quintile_names, with = FALSE]
  )

  ## Estimate average deaths per quintile:
  average_deaths_quintile <-
    (total_deaths_quintile / total_pop_quintile) * 1e5

  ## Estimate average percentage change in healthcare expenditure:
  average_percent_change <- mean(imd_population_$`Expenditure change (%)`)

  return(
    list(
      "title" = paste0(
        "Average Mortality Impact by Deprivation Quintile"
      ),
      "subtitle" = paste0(
        "Deaths per 100,000 Population in Quintile"
      ),
      "caption" = paste0(
        "Mortality impact estimated assuming ",
        round(average_percent_change, digits = 2),
        "% change in national healthcare expenditure."
      ),
      "data" = data.table::data.table(
        "Deprivation Quintile" = names(total_pop_quintile),
        "Mortality Impact" = average_deaths_quintile |>
          unname()
      )
    )
  )
}
