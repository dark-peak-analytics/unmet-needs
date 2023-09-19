################################################################################
#
# Script Name:        assumptions.R
# Module Name:        server
#
################################################################################

# Collapsible controls ---------------------------------------------------------

controls_rv[["assumptions"]] <- TRUE

shiny::observeEvent(
  eventExpr = input$assumptions_title,
  handlerExpr = {
    shinyjs::js$collapse("assumptions")
    if(shiny::isTruthy(controls_rv[["assumptions"]])) {
      shiny::updateActionButton(
        inputId = "assumptions_title",
        icon = shiny::icon(
          name = "minus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["assumptions"]] <- FALSE
    } else {
      shiny::updateActionButton(
        inputId = "assumptions_title",
        icon = shiny::icon(
          name = "plus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["assumptions"]] <- TRUE
    }
  }
)

# Update assumptions inputs ----------------------------------------------------

shiny::observe({
  shiny::updateSelectizeInput(
    inputId = "target_maximum_QALE",
    choices = names(inputs_rv[["target_maximum_QALE"]]),
    selected = inputs_rv[["target_maximum_QALE"]] |>
      names() |>
      _[2])
})

maximum_QALE_option <- shiny::reactive({
  ifelse(
    test = shiny::isTruthy(input$option_),
    yes = "equal_mortality_elasticity",
    no = "estimated_mortality_elasticity")
})
