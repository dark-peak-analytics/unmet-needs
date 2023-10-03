#' @title Calculate the marginal QALYs
#' @description Estimates the per deprivation quintile marginal Quality-Adjusted
#' Life Years (QALYs) from the Quality-Adjusted Life Expectancy (QALE).
#' @details This function takes ... inputs and then ... to estimate the marginal
#' QALE per deprivation quintile and then the marginal QALYs.
#' @param target_maximum_health_ Numeric vector specifying the target maximum QALE
#' for low (GBD study), mid and high (maximum lifespan).
#' @param baseline_health_ Numeric vector specifying the baseline health per
#' quintile. Default value from PAPER 1 representing EQ5D-5L results.
#' @param mortality_elasticity_ Numeric vector specifying the mortality
#' elasticity per deprivation quintile.
#' @param option_ Character scalar specifying whether to estimate the marginal
#' QALE based on actual or equal marginal elasticity,
#' `estimated_mortality_elasticity` or `equal_mortality_elasticity`,
#' respectively.
#' @param expenditure_change_ Numeric specifying the percentage change for which
#' the QALY gain/loss is to be estimated.
#'
#' @return A dataframe.
#'
#' @export
#'
calculate_marginal_QALYs <- function(
    target_maximum_health_,
    baseline_health_,
    mortality_elasticity_,
    option_ = "estimated_mortality_elasticity",
    expenditure_change_ = 1) {

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

  ## Calculate QALE burden:
  QALE_burden <- lapply(
    X = target_maximum_health_,
    FUN = function(max_QALE_) {
      max_QALE_ - baseline_health
    }
  )

  ## Calculate effects on marginal QALE levels:
  ### [Per 1% increase in healthcare expenditure (about £10 pp)]:
  QALE_marginal_levels <- lapply(
    X = QALE_burden,
    FUN = function(QALE_burden_) {
      QALE_burden_ * elasticities
    }
  )

  ## Calculate corresponding shares of the marginal QALY:
  ### [Per 1% increase in healthcare expenditure (about £10 pp)]:
  QALY_marginal_levels <- lapply(
    X = QALE_marginal_levels,
    FUN = function(QALE_marginal_level_) {
      data.table::data.table(
        "Deprivation Quintile" = names(QALE_marginal_level_),
        "Marginal QALY" = (QALE_marginal_level_ /
          sum(QALE_marginal_level_[quintile_names])) *
          expenditure_change_
      )
    }
  )

  return(
    list(
      "title" = paste(
        "Distribution of a marginal QALY by deprivation quintile"
      ),
      "caption" = paste0(
        "Estimated asusming a ",
        expenditure_change_,
        "% change in expenditure, and maximum QALE = ",
        target_maximum_health_,
        "."
      ),
      "data" = QALY_marginal_levels
    )
  )
}
