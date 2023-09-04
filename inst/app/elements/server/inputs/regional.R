################################################################################
#
# Script Name:        regional.R
# Module Name:        server
#
################################################################################

# Collapsible controls ---------------------------------------------------------

controls_rv[["regional"]] <- TRUE

shiny::observeEvent(
  eventExpr = input$regional_title,
  handlerExpr = {
    shinyjs::js$collapse("regional")
    if(shiny::isTruthy(controls_rv[["regional"]])) {
      shiny::updateActionButton(
        inputId = "regional_title",
        icon = shiny::icon(
          name = "minus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["regional"]] <- FALSE
    } else {
      shiny::updateActionButton(
        inputId = "regional_title",
        icon = shiny::icon(
          name = "plus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["regional"]] <- TRUE
    }
  }
)

# Update regional inputs -------------------------------------------------------

shiny::observe({
  shiny::updateSelectizeInput(
    inputId = "ccgs",
    choices = if(is.null(input$regions)) {
      IMD_data |>
        subset(
          select = CCG19NM
        ) |>
        _[[1]]
    } else {
      IMD_data |>
        subset(
          subset = Region %in% input$regions,
          select = CCG19NM
        ) |>
        _[[1]]
    }
  )
})

shiny::observeEvent(
  eventExpr = input$ccgs,
  handlerExpr = {
    inputs_rv[["IMD_population"]] <- IMD_data |>
      subset(CCG19NM %in% input$ccgs)
  }
)

shiny::observeEvent(
  eventExpr = is.null(input$ccgs) | is.null(input$regions),
  handlerExpr = {
    if (length(input$ccgs) == 0 & length(input$regions) == 0) {

      inputs_rv[["IMD_population"]] <- IMD_data

    } else if (length(input$ccgs) == 0) {

      inputs_rv[["IMD_population"]] <- IMD_data |>
        subset(Region %in% input$regions)

    } else if (length(input$regions) == 0) {

      inputs_rv[["IMD_population"]] <- IMD_data |>
        subset(CCG19NM %in% input$ccgs)
    }
  }
)

shiny::observe({
  if(shiny::isTruthy(input$national)) {
    shinyjs::reset(id = "regions")
    shinyjs::reset(id = "ccgs")

    inputs_rv[["IMD_population"]] <- IMD_data
  }
})
