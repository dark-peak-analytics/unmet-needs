## code to prepare `ons_ccg_ics_names_data` dataset goes here

### From 1 July 2022, Integrated Care Boards (ICBs) have replaced Strategic
### Information Partnerships (STPs). Clinical Commissioning Groups (CCGs) are
### now Sub-ICB Locations (SICBLs).
### https://www.nhsbsa.nhs.uk/sicbls-icbs-and-other-providers/organisation-and-prescriber-changes/icbs
### https://www.nhsconfed.org/publications/integrated-care-systems-ics

ons_ccg_ics_names_data <- data.frame(
  "table_year" = c(2022, 2023),
  "table_object_name" = c(
    "SICBL22_ICB22_NHSER22_EN_LU",
    "SICBL23_ICB23_NHSER23_EN_LU"
  )
)

usethis::use_data(ons_ccg_ics_names_data, overwrite = TRUE)
