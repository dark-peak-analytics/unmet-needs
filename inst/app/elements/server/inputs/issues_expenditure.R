################################################################################
#
# Script Name:        issues_expenditure.R
# Module Name:        server
#
################################################################################

# Call modal when issues are first detected button or button clicked -----------

shiny::observeEvent(
  eventExpr = input$issues_expenditure,
  handlerExpr = {
    shiny::showModal(
      source(
        file.path("elements", "ui", "inputs", "issues_expenditure.R"),
        local = TRUE
      )$value
    )
  }
)
