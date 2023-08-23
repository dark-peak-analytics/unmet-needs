## code to prepare `ons_ccg_ics_regions_data` dataset goes here

ons_ccg_ics_regions_data <- data.frame(
  "table_year" = c(2019),
  "table_object_name" = c(
    "CCG19_NHSRLO19_NHSER19_EN_LU_0dcf1ec50b844f82b9204f1522e411b2"
  )
)

usethis::use_data(ons_ccg_ics_regions_data, overwrite = TRUE)
