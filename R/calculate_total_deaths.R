################################################################################
#
# Script Name:        calculate_total_deaths.R
# Script Description: Defines the function calculate_total_deaths(). This
#                     function estimates the number of lives saved per
#                     healthcare provider by deprivation quintile.
#
################################################################################

#' @title Calculate the Total Number of Deaths prevented by healthcare provider.
#' @description Estimates the total number of deaths per deprivation quintile in
#' each geographical area with a designated healthcare provider.
#' @details This function takes ... inputs and then ... to estimate the total
#' number of deaths.
#' @param mortality_rates_ Numeric vector specifying the mortality rates per
#' 100,000 population per deprivation quintile.
#' @param mortality_elasticity_ Numeric vector specifying the mortality
#' elasticity per deprivation quintile.
#' @param equal_mortality_elasticity_ Numeric scalar defining the value to be
#' used when the argument `option_` is set to `equal_mortality_elasticity`.
#' @param option_ Character scalar specifying whether to estimate the marginal
#' QALE based on actual or equal marginal elasticity,
#' `estimated_mortality_elasticity` or `equal_mortality_elasticity`,
#' respectively.
#' @param imd_population_ Dataframe with population distribution by deprivation.
#' @param provider_ String specifying whether the provider was CCG or ICS?ICB.
#'
#' @return A list containing dataframe(s).
#'
#' @importFrom data.table := .SD
#'
#' @export
#' @examples
#' \dontrun{
#' total_lives_saved <- calculate_total_deaths(
#'   mortality_rates_ = input_data_mQALE$`Mortality rate`[1:5],
#'   mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'   equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'   option_ = "estimated_mortality_elasticity",
#'   imd_population_ = CCG_IMD_population_2019,
#'   provider_ = "CCG"
#' )
#' total_lives_saved_equ_elas <- calculate_total_deaths(
#'   mortality_rates_ = input_data_mQALE$`Mortality rate`[1:5],
#'   mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'   equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'   option_ = "equal_mortality_elasticity",
#'   imd_population_ = CCG_IMD_population_2019,
#'   provider_ = "CCG"
#' )
#' }
calculate_total_deaths <- function(
    mortality_rates_ = input_data_mQALE$`Mortality rate`[1:5],
    mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
    equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
    option_ = "estimated_mortality_elasticity",
    imd_population_ = CCG_IMD_population_2019,
    provider_ = "CCG") {

  ## Sanity checks:
  assertthat::assert_that(
    is.numeric(mortality_rates_),
    is.numeric(mortality_elasticity_),
    assertthat::are_equal(
      length(mortality_rates_), length(mortality_elasticity_)
    ),
    assertthat::are_equal(
      length(mortality_rates_), 5
    ),
    all(mortality_elasticity_ > 0, mortality_elasticity_ < 1),
    msg = paste(
      "The vectors passed to mortality_rates_ and/or mortality_elasticity_",
      "arguments are not numeric, of equal length or of length 5. Please ensure",
      "that each object is a named numric vector of length five representing",
      "the values corresponding to each deprivation quintile, starting with Q1",
      "(the most deprived)."
    )
  )
  assertthat::assert_that(
    is.numeric(equal_mortality_elasticity_),
    length(equal_mortality_elasticity_) == 1,
    all(equal_mortality_elasticity_ > 0, equal_mortality_elasticity_ < 1),
    msg = paste(
      "equal_mortality_elasticity_ is not a numeric or a numeric scalar or",
      "between 0 and 1."
    )
  )

  ## Auxiliary data:
  quintile_names <- names(mortality_rates_)
  elasticities <- switch (
    EXPR = option_,
    estimated_mortality_elasticity = mortality_elasticity_,
    equal_mortality_elasticity = {
      rep(
        x = equal_mortality_elasticity_,
        length.out = length(mortality_elasticity_)
      ) |>
        `names<-`(quintile_names)
    }
  )
  mortality_rates <- c(mortality_rates_, "Average" = mean(mortality_rates_))
  elasticities <- c(elasticities, "Average" = mean(elasticities))

  ## Calculate total deaths per deprivation (IMD) quintile:
  lives_saved <- mortality_rates[quintile_names] * elasticities[quintile_names]

  if(is.null(imd_population_)) {

    return(
      list(
        "title" = "Lives saved per 100,000 per deprivation quintile",
        "data" = lives_saved
      )
    )
  } else {
    ## Estimate deaths resulting from expenditure change:
    imd_pop_df <- data.table::as.data.table(imd_population_)
    imd_pop_df[
      ,
      (quintile_names) := lapply(
        X = quintile_names,
        FUN = function(.x) {
          round(
            x = .SD[[.x]] *
              (lives_saved[[.x]] * imd_pop_df[["Expenditure change (%)"]]) /
              1e5,
            digits = 0
          )
        }
      ),
      .SDcols = quintile_names
    ][
      ,
      total_lives_saved := round(x = rowSums(.SD, na.rm = TRUE), digits = 0),
      .SDcols = quintile_names
    ][
      ,
      "Average lives saved (100,000 population)" :=
        round(x = (total_lives_saved / `Overall population`) * 1e5, digits = 0)
    ]

    imd_pop_df <- imd_pop_df |>
      replace(
        list = is.na(imd_pop_df),
        values = 0
      )

    return(
      list(
        "title" = paste0(
          "Results",
          if(!is.null(provider_)) paste0(" by ", provider_) else NULL
        ),
        "data" = imd_pop_df
      )
    )
  }
}
