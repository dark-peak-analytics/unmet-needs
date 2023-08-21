################################################################################
#
# Script Name:        help_HRQoL.R
# Module Name:        server
#
################################################################################

# Call help modal when help button is clicked ----------------------------------

shiny::observeEvent(
  eventExpr = input$help_HRQoL,
  handlerExpr = {
    shiny::showModal(
      source(
        file.path("elements", "ui", "inputs", "help_HRQoL.R"),
        local = TRUE
      )$value
    )
  }
)
