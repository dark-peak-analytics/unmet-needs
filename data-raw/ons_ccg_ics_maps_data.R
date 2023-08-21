## code to prepare `ons_ccg_ics_maps_data` dataset goes here

ons_ccg_ics_maps_data <- data.frame(
  "map_year" = c(2018, 2019, 2020, 2021, 2022, 2023),
  "healthcare_entity" = c("ccg", "ccg", "ccg", "ccg", "ics", "ics"),
  "map_object_name" = c(
    "Clinical_Commissioning_Groups_April_2018_FCB_in_England_2022",
    "Clinical_Commissioning_Groups_April_2019_Boundaries_EN_BFC_2022",
    "Clinical_Commissioning_Groups_April_2020_FCB_EN_2022",
    "Clinical_Commissioning_Groups_April_2021_EN_BFC_2022",
    "Integrated_Care_Boards_July_2022_EN_BFC_2022",
    "Integrated_Care_Boards_April_2023_EN_BFC"
  )
)

usethis::use_data(ons_ccg_ics_maps_data, overwrite = TRUE)
