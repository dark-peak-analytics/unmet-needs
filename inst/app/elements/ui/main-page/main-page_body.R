################################################################################
#
# Script Name:        main-page_body.R
# Module Name:        ui
#
################################################################################

# Create body ------------------------------------------------------------------

shinydashboard::dashboardBody(
  # Hack the shinydashboard heade
  shiny::tags$script(
    shiny::HTML(
      paste0(
        '$(document).ready(function() {',
        '$("header").find(',
        '"nav").append(',
        '\'<div ',
        'class="dashboardHeaderClass',
        '">',
        'NHS Formula Health Inequality Impact Calculator',
        '</div>\');',
        '})'
      )
    )
  ),
  shinydashboard::tabItems(
    # Create dashboard tabs
    source(
      file.path(ui_path, "inputs", "inputs.R"),
      local = TRUE
    )$value,
    source(
      file.path(ui_path, "qalys", "qalys.R"),
      local = TRUE
    )$value,
    source(
      file.path(ui_path, "mortality", "mortality.R"),
      local = TRUE
    )$value
  )
)
