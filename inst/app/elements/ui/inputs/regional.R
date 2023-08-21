################################################################################
#
# Script Name:        regional.R
# Module Name:        ui
#
################################################################################

# Create Assumptions box in the inputs page ------------------------------------

shinydashboard::box(
  id = "regional",
  title = shiny::tagList(
    shiny::actionButton(
      inputId = "regional_title",
      label = "NHS Organisations",
      icon = shiny::icon(
        name = "plus",
        style = "padding-right: 5px;"
      ),
      style = "float: left; margin-left: 0px; padding-left: 0px;"
    ),
    shiny::tags$div(
      title = paste0(
      "Selecting individual, region-specific or nation-wide NHS organisations"
      ),
      shiny::actionButton(
        inputId = "help_regional",
        label = "",
        icon = shiny::icon(
          name = "circle-question"
        ),
        style = "float: right!important; position: absolute; right: 10px;"
      )
    )
  ),
  status = "info",
  collapsible = TRUE,
  collapsed = TRUE,
  width = 12,
  shiny::column(
    style = "max-width: 100%; overflow: hidden;",
    width = 4,
    shinyWidgets::prettySwitch(
      inputId = "national",
      label = "All NHS England Organisations",
      value = TRUE,
      bigger = TRUE,
      status = "success"
    )
  ),
  shiny::conditionalPanel(
    condition = "!input.national",
    shiny::column(
      style = "max-width: 100%;",
      width = 4,
      shiny::selectizeInput(
        inputId = "regions",
        label = "English Region",
        choices = IMD_data$Region,
        multiple = TRUE
      )
    ),
    shiny::column(
      style = "max-width: 100%;",
      width = 4,
      shiny::selectizeInput(
        inputId = "ccgs",
        label = "Clinical Commissioning Group (CCG)",
        choices = IMD_data$CCG19NM,
        multiple = TRUE
      )
    )
  )
)
