################################################################################
#
# Script Name:        help_expenditure.R
# Module Name:        server
#
################################################################################

# Call help modal when help button is clicked ----------------------------------

shiny::observeEvent(
  eventExpr = input$help_expenditure,
  handlerExpr = {
    shiny::showModal(
      source(
        file.path("elements", "ui", "inputs", "help_expenditure.R"),
        local = TRUE
      )$value
    )
  }
)
