################################################################################
#
# Script Name:        calculate_average_deaths.R
# Script Description: Defines the function calculate_average_deaths(). This
#                     function estimates the average number of lives saved per
#                     100,000 population by deprivation quintile.
#
################################################################################

#' @title Calculate Average Number of Deaths Prevented by Deprivation Quintile.
#' @description Estimates the the average Quality-Adjusted Life Year (QALY) per
#' deprivation quintile.
#' @details This function takes ... inputs and then ... to estimate the average
#' QALYs.
#' @param total_lives_saved_ Dataframe or data table representing the outputs of
#' the \link[UnmetNeeds]{calculate_absolute_QALYs}.
#' @param imd_population_ Dataframe with population distribution by deprivation.
#'
#' @return A dataframe.
#'
#' @export
#' @examples
#' \dontrun{
#' average_deaths <- calculate_average_deaths(
#'   total_lives_saved_ = calculate_total_deaths(
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
#'   total_lives_saved_ = calculate_total_deaths(
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
    total_lives_saved_ = UnmetNeeds::calculate_total_deaths()$data,
    imd_population_ = CCG_IMD_population_2019) {

  ## Auxiliary data:
  quintile_names <- grep(
    pattern = "Q",
    x = colnames(total_lives_saved_),
    ignore.case = TRUE,
    value = TRUE
  )
  total_pop_quintile <- colSums(imd_population_[, quintile_names])
  total_lives_saved_quintile <- colSums(total_lives_saved_[, ..quintile_names])

  ## Estimate average deaths per quintile:
  average_deaths_quintile <-
    (total_lives_saved_quintile / total_pop_quintile) * 1e5

  return(
    list(
      "title" = paste0(
        "Average Health Impact by Deprivation Quintile"
      ),
      "subtitle" = paste0(
        "Lives saved per 100,000 Population in Quintile"
      ),
      "caption" = paste0(
        "Deaths prevented are estimated assuming ",
        imd_population_$`Expenditure change (%)`[[1]],
        "% change in national healthcare expenditure."
      ),
      "data" = data.table::data.table(
        "Deprivation Quintile" = names(total_pop_quintile),
        "Deaths prevented" = average_deaths_quintile |>
          unname()
      )
    )
  )
}
