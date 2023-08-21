################################################################################
#
# Script Name:        calculate_absolute_QALYs.R
# Script Description: Defines the function calculate_absolute_QALYs(). This
#                     function estimates the absolute Quality-Adjusted Life
#                     Years (QALY).
#
################################################################################

#' @title Calculate Absolute Quality-Adjusted Life Years (QALYs).
#' @description Estimates the the absolute Quality-Adjusted Life Year (QALY) per
#' deprivation quintile.
#' @details This function takes ... inputs and then ... to estimate the absolute
#' QALYs.
#' @param baseline_health_ Numeric vector specifying the baseline health per
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
#' @return A dataframe.
#'
#' @importFrom data.table := .SD
#'
#' @export
#'
calculate_absolute_QALYs <- function(
    baseline_health_ = input_data_mQALE$`Baseline health`,
    mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
    option_ = "estimated_mortality_elasticity",
    imd_population_ = CCG_IMD_population_2019,
    provider_ = "CCG") {

  ## Sanity checks:
  ### Expect objects of equal length
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

  ## Calculate QALY gain per person per IMD quintile:
  QALY_gain <- baseline_health[quintile_names] * elasticities[quintile_names]
  QALY_gain <- c(QALY_gain, "Average" = mean(QALY_gain))

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
    QALY_gain_imd_pop[
      ,
      (quintile_names) := lapply(
        X = quintile_names,
        FUN = function(.x) {
          .SD[[.x]] * QALY_gain[.x] *
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

    return(
      list(
        "title" = paste0(
          "Results",
          if(!is.null(provider_)) paste0(" by ", provider_) else NULL
        ),
        "data" = QALY_gain_imd_pop
      )
    )
  }
}
