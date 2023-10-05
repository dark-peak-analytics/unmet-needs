#' @title Calculate Absolute Quality-Adjusted Life Years (QALYs)
#' @description Estimates the absolute Quality-Adjusted Life Year (QALY) per
#' deprivation quintile.
#' @details This function takes ... inputs and then ... to estimate the absolute
#' QALYs.
#' @param target_maximum_health_ Numeric vector specifying the target maximum QALE
#' for low (GBD study), mid and high (maximum lifespan).
#' @param baseline_health_ Numeric vector specifying the baseline health per
#' quintile. Default value from PAPER 1 representing EQ5D-5L results.
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
#' absolute_QALYs <- calculate_absolute_QALYs(
#'   target_maximum_health_ = input_data_mQALE$`Target maximum QALE`,
#'   baseline_health_ = input_data_mQALE$`Baseline health`[1:5],
#'   mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'   equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'   option_ = "estimated_mortality_elasticity",
#'   imd_population_ = CCG_IMD_population_2019,
#'   provider_ = "CCG"
#' )
#' absolute_QALYs_equ_elas <- calculate_absolute_QALYs(
#'   target_maximum_health_ = input_data_mQALE$`Target maximum QALE`,
#'   baseline_health_ = input_data_mQALE$`Baseline health`[1:5],
#'   mortality_elasticity_ = input_data_mQALE$`Mortality elasticity`,
#'   equal_mortality_elasticity_ = input_data_mQALE$`Equal mortality elasticity`,
#'   option_ = "equal_mortality_elasticity",
#'   imd_population_ = CCG_IMD_population_2019,
#'   provider_ = "CCG"
#' )
#' }
calculate_absolute_QALYs <- function(
    target_maximum_health_,
    baseline_health_,
    mortality_elasticity_,
    equal_mortality_elasticity_,
    option_ = "estimated_mortality_elasticity",
    imd_population_,
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
    all(mortality_elasticity_ > 0, mortality_elasticity_ < 1),
    msg = paste(
      "The vectors passed to baseline_health_ and/or mortality_elasticity_",
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
  quintile_names <- names(baseline_health_)
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

  ## Calculate QALY gain per person per deprivation (IMD) quintile:
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
    total_change <- `Overall population` <- NULL
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
          "QALE Impact Results",
          if(!is.null(provider_)) paste0(" by ", provider_) else NULL,
          if(!is.null(provider_)) paste0(" and Deprivation Quintile")
          else paste0(" Deprivation Quintile")
        ),
        "data" = QALY_gain_imd_pop_data
      )
    )
  }
}
