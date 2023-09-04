################################################################################
#
# Script Name:        calculate_total_deaths.R
# Script Description: Defines the function calculate_total_deaths(). This
#                     function estimates the number of deaths per healthcare
#                     provider by deprivation quintile.
#
################################################################################

#' @title Calculate the Total Number of Deaths by healthcare provider.
#' @description Estimates the total number of deaths per deprivation quintile in
#' each geographical area with a designated healthcare provider.
#' @details This function takes ... inputs and then ... to estimate the total
#' number of deaths.
#' @param mortality_rates_ Numeric vector specifying the baseline health per
#' quintile. Default value from PAPER 1 representing EQ5D-5L results.
#' @param mortality_elasticity_ Numeric vector specifying the mortality
#' elasticity per deprivation quintile.
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
#' total_deaths <- calculate_total_deaths(
#'   mortality_rates_ = input_data_mQALE$`Mortality rate`,
#'   mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'   option_ = "estimated_mortality_elasticity",
#'   imd_population_ = CCG_IMD_population_2019,
#'   provider_ = "CCG"
#' )
#' total_deaths_equ_elas <- calculate_total_deaths(
#'   mortality_rates_ = input_data_mQALE$`Mortality rate`,
#'   mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'   option_ = "equal_mortality_elasticity",
#'   imd_population_ = CCG_IMD_population_2019,
#'   provider_ = "CCG"
#' )
#' }
calculate_total_deaths <- function(
    mortality_rates_ = input_data_mQALE$`Mortality rate`,
    mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
    option_ = "estimated_mortality_elasticity",
    imd_population_ = CCG_IMD_population_2019,
    provider_ = "CCG") {

  ## Sanity checks:
  assertthat::assert_that(
    is.numeric(baseline_health_),
    is.numeric(mortality_elasticity_),
    assertthat::are_equal(
      length(baseline_health_), length(mortality_elasticity_)
    ),
    assertthat::are_equal(
      length(baseline_health_), 5
    ),
    msg = paste(
      "The vectors passed to baseline_health_ and/or mortality_elasticity_",
      "arguments are not numeric, of equal length or of length 5. Please ensure",
      "that each object is a named numric vector of length five representing",
      "the values corresponding to each deprivation quintile, starting with Q1",
      "(the most deprived)."
    )
  )

  ## Auxiliary data:
  quintile_names <- names(baseline_health_)
  elasticities <- switch (
    EXPR = option_,
    estimated_mortality_elasticity = mortality_elasticity_,
    equal_mortality_elasticity = {
      rep(
        x = mean(mortality_elasticity_),
        length.out = length(mortality_elasticity_)
      ) |>
        `names<-`(quintile_names)
    }
  )
  baseline_health <- c(baseline_health_, "Average" = mean(baseline_health_))
  elasticities <- c(elasticities, "Average" = mean(elasticities))

  ## Calculate baseline health burden (estimated in QALE at birth):
  baseline_health_burden <- lapply(
    X = target_maximum_health_,
    FUN = function(max_QALE_) {
      values <- max_QALE_ - baseline_health[quintile_names]
      c(values, "Average" = mean(values))
    }
  )

  ## Calculate QALY gain per person per IMD quintile:
  QALY_gain <- lapply(
    X = baseline_health_burden,
    FUN = function(QALE_burden_) {
      values <- QALE_burden_[quintile_names] * elasticities[quintile_names]
      c(values, "Average" = mean(values))
    }
  )

  if(is.null(imd_population_)) {

    return(
      list(
        "title" = "Distribution of QALY change per IMD group",
        "data" = QALY_gain
      )
    )
  } else {
    ## Estimate gain due to expenditure change:
    QALY_gain_imd_pop <- data.table::as.data.table(imd_population_)
    QALY_gain_imd_pop_data <- lapply(
      X = QALY_gain,
      FUN = function(QALY_gain_) {
        QALY_gain_imd_pop[
          ,
          (quintile_names) := lapply(
            X = quintile_names,
            FUN = function(.x) {
              .SD[[.x]] * QALY_gain_[.x] *
                QALY_gain_imd_pop[["Expenditure change (%)"]]
            }
          ),
          .SDcols = quintile_names
        ][
          ,
          total_change := rowSums(.SD, na.rm = TRUE),
          .SDcols = quintile_names
        ][
          ,
          "Average change (100,000 population)" :=
            (total_change / `Overall population`) * 1e5
        ]

        QALY_gain_imd_pop <- QALY_gain_imd_pop |>
          replace(
            list = is.na(QALY_gain_imd_pop),
            values = 0
          )
      })

    return(
      list(
        "title" = paste0(
          "Results",
          if(!is.null(provider_)) paste0(" by ", provider_) else NULL
        ),
        "data" = QALY_gain_imd_pop_data
      )
    )
  }
}
