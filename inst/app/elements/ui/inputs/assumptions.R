################################################################################
#
# Script Name:        assumptions.R
# Module Name:        ui
#
################################################################################

# Create Assumptions box in the inputs page ------------------------------------

shinydashboard::box(
  id = "assumptions",
  title = shiny::tagList(
    shiny::actionButton(
      inputId = "assumptions_title",
      label = "Basic Modelling Assumptions (Modifiable)",
      icon = shiny::icon(
        name = "plus",
        style = "padding-right: 5px;"
      ),
      style = "float: left; margin-left: 0px; padding-left: 0px;"
    ),
    shiny::tags$div(
      title = "Changing modelling assumptions",
      shiny::actionButton(
        inputId = "help_assumptions",
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
      inputId = "option_",
      label = "Assume equal elasticities",
      bigger = TRUE,
      status = "success"
    )
  ),
  shiny::column(
    style = "max-width: 100%; overflow: hidden;",
    width = 4,
    shinyWidgets::numericInputIcon(
      inputId = "pcnt_change",
      label = "Assumed change in healthcare expenditure",
      value = 1,
      min = 0,
      max = 100,
      icon = shiny::icon("percent")
    )
  ),
  shiny::column(
    style = "max-width: 100%;",
    width = 4,
    shiny::selectizeInput(
      inputId = "target_maximum_QALE",
      label = "Maximum Quality-adjusted Life Expectancy (QALE)",
      choices = "",
      multiple = FALSE
    )
  )
)
