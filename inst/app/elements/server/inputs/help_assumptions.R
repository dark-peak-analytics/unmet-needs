################################################################################
#
# Script Name:        help_assumptions.R
# Module Name:        server
#
################################################################################

# Call help modal when help button is clicked ----------------------------------

shiny::observeEvent(
  eventExpr = input$help_assumptions,
  handlerExpr = {
    shiny::showModal(
      source(
        file.path("elements", "ui", "inputs", "help_assumptions.R"),
        local = TRUE
      )$value
    )
  }
)
