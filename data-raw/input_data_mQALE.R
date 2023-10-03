## code for `input data for marginal QALE (input_data_mQALE)` dataset goes here

input_data_mQALE <-
  list(
    "Target maximum QALE" = c(
      "High (max lifespan)" = 120,
      "Mid" =  100,
      "Low (GBD study)" = 86.59
    ),
    "Baseline health" = c( # from paper 1 - EQ5D-5L results
      "Q1 - Most Deprived" = 66.37,
      "Q2" = 70.07,
      "Q3" = 73.74,
      "Q4" = 75.32,
      "Q5 - Least Deprived" = 77.63,
      "Overall" = 72.63
    ),
    "Mortality rate" = c( # from paper 3 - Adjusted mortality rate per 100,000
      "Q1 - Most Deprived" = 1379,
      "Q2" = 1125,
      "Q3" = 962,
      "Q4" = 877,
      "Q5 - Least Deprived" = 777,
      "Overall" = 1024
    ),
    "Mortality elasticity" = c( # from paper 2 (log-log model)
      "Q1 - Most Deprived" = 0.00433,
      "Q2" = 0.00768,
      "Q3" = 0.00909,
      "Q4" = 0.00941,
      "Q5 - Least Deprived" = 0.00763
    ),
    "Equal mortality elasticity" = 0.00763
  )

usethis::use_data(input_data_mQALE, overwrite = TRUE)

