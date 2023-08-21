################################################################################
#
# Script Name:        inputs.R
# Module Name:        ui
#
################################################################################

# Define path for inputs UI elements -------------------------------------------

inputs_ui_path <- file.path("elements", "ui", "inputs")

# Create inputs page -----------------------------------------------------------

shinydashboard::tabItem(
  tabName = "inputs",
  source(
    file.path(inputs_ui_path, "HRQoL.R"),
    local = TRUE
  )$value,
  source(
    file.path(inputs_ui_path, "assumptions.R"),
    local = TRUE
  )$value,
  source(
    file.path(inputs_ui_path, "regional.R"),
    local = TRUE
  )$value,
  source(
    file.path(inputs_ui_path, "expenditure.R"),
    local = TRUE
  )$value
)
