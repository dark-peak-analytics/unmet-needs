################################################################################
#
# Script Name:        HRQoL.R
# Module Name:        ui
#
################################################################################

# Create HRQoL box in the inputs page ------------------------------------------

shinydashboard::box(
  id = "HRQoL",
  title = shiny::tagList(
    shiny::actionButton(
      inputId = "HRQoL_title",
      label = "Basic Health Impact Input Data (Editable)",
      icon = shiny::icon(
        name = "plus",
        style = "padding-right: 5px;"
      ),
      style = "float: left; margin-left: 0px; padding-left: 0px;"
    ),
    shiny::tags$div(
      title = "Understanding and editing HRQoL data",
      shiny::actionButton(
        inputId = "help_HRQoL",
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
    # style = "text-align: left;",
    width = 8,
    DT::dataTableOutput(
      outputId = "HRQoL_inputs1"
    )
  ),
  shiny::column(
    style = "max-width: 100%; overflow: hidden;",
    # style = "text-align: left;",
    width = 4,
    DT::dataTableOutput(
      outputId = "HRQoL_inputs2"
    )
  )
)
