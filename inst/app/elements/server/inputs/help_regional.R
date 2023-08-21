################################################################################
#
# Script Name:        help_regional.R
# Module Name:        server
#
################################################################################

# Call help modal when help button is clicked ----------------------------------

shiny::observeEvent(
  eventExpr = input$help_regional,
  handlerExpr = {
    shiny::showModal(
      source(
        file.path("elements", "ui", "inputs", "help_regional.R"),
        local = TRUE
      )$value
    )
  }
)
