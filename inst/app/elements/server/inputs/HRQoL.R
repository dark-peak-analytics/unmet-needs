################################################################################
#
# Script Name:        HRQoL.R
# Module Name:        server
#
################################################################################

# Collapsible controls ---------------------------------------------------------

controls_rv[["HRQoL"]] <- TRUE

shiny::observeEvent(
  eventExpr = input$HRQoL_title,
  handlerExpr = {
    shinyjs::js$collapse("HRQoL")
    if(shiny::isTruthy(controls_rv[["HRQoL"]])) {
      shiny::updateActionButton(
        inputId = "HRQoL_title",
        icon = shiny::icon(
          name = "minus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["HRQoL"]] <- FALSE
    } else {
      shiny::updateActionButton(
        inputId = "HRQoL_title",
        icon = shiny::icon(
          name = "plus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["HRQoL"]] <- TRUE
    }
  }
)

# Convert input variables to reactive objects ----------------------------------

inputs_rv[["HRQoL_inputs_1"]] <- data.frame(
  names(UnmetNeeds::input_data_mQALE$`Baseline health`),
  UnmetNeeds::input_data_mQALE$`Baseline health` |>
    unname(),
  UnmetNeeds::input_data_mQALE$`Mortality rate` |>
    unname(),
  UnmetNeeds::input_data_mQALE$`Mortality elasticity` |>
    unname()
) |>
  `colnames<-`(
    c("Deprivation Quantile", "Baseline Health", "Mortality Rate",
      "Mortality Elasticity")
  )

shiny::observe({
  shiny::req(input$target_maximum_QALE)

  inputs_rv[["HRQoL_inputs_1"]]["Baseline Health Burden"] <- round(
    x = inputs_rv[["HRQoL_inputs_2"]][input$target_maximum_QALE |> unname(),] -
      inputs_rv[["HRQoL_inputs_1"]]["Baseline Health"],
    digits = 2
  )

  inputs_rv[["HRQoL_inputs_1"]] <- inputs_rv[["HRQoL_inputs_1"]][
    ,
    c("Deprivation Quantile", "Baseline Health", "Baseline Health Burden",
      "Mortality Rate", "Mortality Elasticity")
  ]

})

inputs_rv[["HRQoL_inputs_2"]] <- UnmetNeeds::input_data_mQALE$`Target maximum QALE` |>
  as.data.frame() |>
  `colnames<-`("Target Maximum QALE")

# Define outputs tables --------------------------------------------------------

output[["HRQoL_inputs1"]] <- DT::renderDataTable(
  expr = {
    inputs_rv[["HRQoL_inputs_1"]]
  },
  options = list(
    dom = 't', # only show the table
    ordering = FALSE,
    keys = TRUE
  ),
  rownames = FALSE,
  editable = list(
    target = 'column',
    disable = list(columns = c(0, 2))
  ),
  selection = 'none'
)

output[["HRQoL_inputs2"]] <- DT::renderDataTable(
  expr = {
    inputs_rv[["HRQoL_inputs_2"]]
  },
  options = list(
    dom = 't', # only show the table
    ordering = FALSE,
    keys = TRUE
  ),
  editable = list(
    target = 'column',
    disable = list(columns = 0)),
  selection = 'none'
)

# Update outputs underlying objects --------------------------------------------

shiny::observeEvent(
  eventExpr = input$HRQoL_inputs1_cell_edit,
  handlerExpr = {
    inputs_rv[["HRQoL_inputs_1"]] <- DT::editData(
      data = inputs_rv[["HRQoL_inputs_1"]],
      info = input$HRQoL_inputs1_cell_edit,
      proxy = "HRQoL_inputs1",
      rownames = FALSE
    )
  }
)

observeEvent(
  eventExpr = inputs_rv[["HRQoL_inputs_1"]],
  handlerExpr = {
    inputs_rv[["baseline_health"]] <- inputs_rv[["HRQoL_inputs_1"]] |>
      subset(select = `Baseline Health`) |>
      as.vector() |>
      unlist() |>
      `names<-`(names(UnmetNeeds::input_data_mQALE$`Baseline health`))

    inputs_rv[["mortality_rates"]] <- inputs_rv[["HRQoL_inputs_1"]] |>
      subset(select = `Mortality Rate`) |>
      as.vector() |>
      unlist() |>
      `names<-`(names(UnmetNeeds::input_data_mQALE$`Mortality rate`))

    inputs_rv[["mortality_elasticity"]] <- inputs_rv[["HRQoL_inputs_1"]] |>
      subset(select = `Mortality Elasticity`) |>
      as.vector() |>
      unlist() |>
      `names<-`(names(UnmetNeeds::input_data_mQALE$`Mortality elasticity`))
  }
)

shiny::observeEvent(
  eventExpr = input$HRQoL_inputs2_cell_edit,
  handlerExpr = {
    inputs_rv[["HRQoL_inputs_2"]] <- DT::editData(
      data = inputs_rv[["HRQoL_inputs_2"]],
      info = input$HRQoL_inputs2_cell_edit,
      proxy = "HRQoL_inputs2",
      rownames = TRUE
    )
  }
)

observeEvent(
  eventExpr = inputs_rv[["HRQoL_inputs_2"]],
  handlerExpr = {
    inputs_rv[["target_maximum_QALE"]] <- inputs_rv[["HRQoL_inputs_2"]] |>
      as.vector() |>
      unlist() |>
      `names<-`(names(UnmetNeeds::input_data_mQALE$`Target maximum QALE`))
  }
)
