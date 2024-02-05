################################################################################
#
# Script Name:        expenditure.R
# Module Name:        server
#
################################################################################

# Collapsible controls ---------------------------------------------------------

controls_rv[["expenditure"]] <- FALSE

shiny::observeEvent(
  eventExpr = input$expenditure_title,
  handlerExpr = {
    shinyjs::js$collapse("expenditure")
    if(shiny::isTruthy(controls_rv[["expenditure"]])) {
      shiny::updateActionButton(
        inputId = "expenditure_title",
        icon = shiny::icon(
          name = "minus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["expenditure"]] <- FALSE
    } else {
      shiny::updateActionButton(
        inputId = "expenditure_title",
        icon = shiny::icon(
          name = "plus",
          style = "padding-right: 5px;"
        )
      )

      controls_rv[["expenditure"]] <- TRUE
    }
  }
)

# Edit input dataframe for displaying ------------------------------------------

shiny::observe({
  inputs_rv[["expenditure_df"]] <- {
    tmp_df <- inputs_rv[["IMD_population"]]
    df_colnames <- colnames(tmp_df)

    regions_col <- grep(
      pattern = "region",
      x = df_colnames,
      ignore.case = TRUE,
      value = TRUE
    )

    names_col <- grep(
      pattern = "nm",
      x = df_colnames,
      ignore.case = TRUE,
      value = TRUE
    )

    quantile_col <- grep(
      pattern = "Q",
      x = df_colnames,
      ignore.case = FALSE,
      value = TRUE
    )

    pop_col <- grep(
      pattern = "population",
      x = df_colnames,
      ignore.case = TRUE,
      value = TRUE
    )

    expen_col <- grep(
      pattern = "expenditure",
      x = df_colnames,
      ignore.case = TRUE,
      value = TRUE
    )

    tmp_df <- tmp_df |>
      subset(
        select = c(regions_col, names_col, pop_col, quantile_col, expen_col)
      )

    names(tmp_df)[names(tmp_df) == names_col] <- inputs_rv[["entity"]]
    names(tmp_df)[names(tmp_df) == pop_col] <- "Total population"

    tmp_df
  }
})

# Define outputs title and table -----------------------------------------------

output[["expenditure_title_page"]] <- shiny::renderUI({
  shiny::tagList(
    shiny::actionButton(
      inputId = "expenditure_title",
      label = paste0(
        "Basic ",
        inputs_rv[["entity"]],
        " Population and Expenditure Change Data (Editable)"
      ),
      icon = shiny::icon(
        name = "minus",
        style = "padding-right: 5px;"
      ),
      style = "float: left; margin-left: 0px; padding-left: 0px;"
    ),
    shiny::conditionalPanel(
      condition = "output.issues_expenditure == true",
      shiny::tags$div(
        title = "Issues with uploaded data",
        shiny::actionButton(
          inputId = "issues_expenditure",
          label = "",
          icon = shiny::icon(
            name = "circle-xmark",
            class = "fa-regular fa-circle-xmark fa-beat-fade",
            style = "color:red"
          ),
          style = "float: right!important; position: absolute; right: 90px;"
        )
      )
    ),
    shiny::tags$div(
      title = controls_rv[["external_expenditure_title"]],
      shiny::actionButton(
        inputId = "external_expenditure",
        label = "",
        icon = shiny::icon(
          name = "upload"
        ),
        style = "float: right!important; position: absolute; right: 50px;"
      )
    ),
    shiny::tags$div(
      title = "Uploading and editing expenditure data",
      shiny::actionButton(
        inputId = "help_expenditure",
        label = "",
        icon = shiny::icon(
          name = "circle-question"
        ),
        style = "float: right!important; position: absolute; right: 10px;"
      )
    )
  )
})

output[["expenditure_inputs"]] <- DT::renderDataTable(
  expr = {
    # Create a vector of numeric column names
    tmp_df <- inputs_rv[["expenditure_df"]]

    numeric_cols <- names(tmp_df)[sapply(tmp_df, is.numeric)]

    # Round all numeric columns to 2 decimal places
    for (col in numeric_cols) {
      tmp_df[[col]] <- round(tmp_df[[col]], 0) |>
        format(big.mark = ",")
    }

    tmp_df[[inputs_rv[["entity"]]]] <- gsub(
      x =  tmp_df[[inputs_rv[["entity"]]]],
      pattern = "NHS | CCG| ICS| ICB",
      replacement = "",
      ignore.case = TRUE
    )

    tmp_df
  },
  options = list(
    # dom = 't', # only show the table
    ordering = TRUE,
    keys = TRUE
  ),
  rownames = FALSE,
  editable = list(
    target = 'column',
    disable = list(columns = 0:7)
  ),
  selection = 'none'
)

shiny::observeEvent(
  eventExpr = input$expenditure_inputs_cell_edit,
  handlerExpr = {
    inputs_rv[["expenditure_df"]] <- DT::editData(
      data = inputs_rv[["expenditure_df"]],
      info = input$expenditure_inputs_cell_edit,
      proxy = "expenditure_inputs",
      rownames = FALSE
    )
  }
)

shiny::observeEvent(
  eventExpr = input$pcnt_change,
  handlerExpr = {
    inputs_rv[["pcnt_change"]] <- input$pcnt_change
    inputs_rv[["expenditure_df"]][["Expenditure change (%)"]] <-
      input$pcnt_change
  }
)

# Uploading external data controls ---------------------------------------------

controls_rv[["external_expenditure"]] <- TRUE

controls_rv[["external_expenditure_title"]] <-
  "Upload external expenditure data"
output[["external_expenditure"]] <- shiny::reactive({

  return(controls_rv[["external_expenditure"]])
})

shiny::outputOptions(
  x = output,
  name = "external_expenditure",
  suspendWhenHidden = FALSE
)

shiny::observeEvent(
  eventExpr = input$external_expenditure,
  handlerExpr = {
    if(shiny::isTruthy(controls_rv[["external_expenditure"]])) {
      shiny::updateActionButton(
        inputId = "external_expenditure",
        icon = shiny::icon(
          name = "pen"
        )
      )

      controls_rv[["external_expenditure"]] <- FALSE
      controls_rv[["external_expenditure_title"]] <-
        "Edit existing table"

      output[["expenditure_inputs"]] <- DT::renderDataTable(
        expr = {
          # Create a vector of numeric column names
          tmp_df <- inputs_rv[["expenditure_df"]]

          numeric_cols <- names(tmp_df)[sapply(tmp_df, is.numeric)]

          # Round all numeric columns to 2 decimal places
          for (col in numeric_cols) {
            tmp_df[[col]] <- round(tmp_df[[col]], 2) |>
              format(big.mark = ",")
          }

          tmp_df[[inputs_rv[["entity"]]]] <- gsub(
            x =  tmp_df[[inputs_rv[["entity"]]]],
            pattern = "NHS | CCG| ICS| ICB",
            replacement = "",
            ignore.case = TRUE
          )

          tmp_df
        },
        options = list(
          # dom = 't', # only show the table
          ordering = TRUE,
          keys = TRUE
        ),
        rownames = FALSE,
        editable = list(
          target = 'column',
          disable = list(columns = 0:8)
        ),
        selection = 'none'
      )
    } else {
      shiny::updateActionButton(
        inputId = "external_expenditure",
        icon = shiny::icon(
          name = "upload"
        )
      )

      controls_rv[["external_expenditure"]] <- TRUE
      controls_rv[["external_expenditure_title"]] <-
        "Upload external expenditure data"
      controls_rv[["is_missing_nhs_org"]] <- NULL
      controls_rv[["is_incompatible_values"]] <- NULL

      output[["expenditure_inputs"]] <- DT::renderDataTable(
        expr = {
          # Create a vector of numeric column names
          tmp_df <- inputs_rv[["expenditure_df"]]

          numeric_cols <- names(tmp_df)[sapply(tmp_df, is.numeric)]

          # Round all numeric columns to 2 decimal places
          for (col in numeric_cols) {
            tmp_df[[col]] <- round(tmp_df[[col]], 2) |>
              format(big.mark = ",")
          }

          tmp_df[[inputs_rv[["entity"]]]] <- gsub(
            x =  tmp_df[[inputs_rv[["entity"]]]],
            pattern = "NHS | CCG| ICS| ICB",
            replacement = "",
            ignore.case = TRUE
          )

          tmp_df
        },
        options = list(
          # dom = 't', # only show the table
          ordering = TRUE,
          keys = TRUE
        ),
        rownames = FALSE,
        editable = list(
          target = 'column',
          disable = list(columns = 0:7)
        ),
        selection = 'none'
      )
    }
  }
)

# Set Download Handler ---------------------------------------------------------

## External expenditure data
output[["nhs_orgs"]] <- shiny::downloadHandler(
  filename = function() {
    paste0(
      "expenditure_change_by_",
      inputs_rv[["entity"]],
      "_template",
      ".csv"
    )
  },
  content = function(file) {
    write.csv(
      file = file,
      x = {
        inputs_rv[["expenditure_df"]]  |>
          base::subset(
            select = c(inputs_rv[["entity"]], "Expenditure change (%)")
          )
      }
    )
  }
)

# Set Upload Handler -----------------------------------------------------------

shiny::observeEvent(
  eventExpr = input$external_expenditure_data,
  handlerExpr = {
    shiny::req(input$external_expenditure_data)

    uploaded_data <- readr::read_csv(
      file = input$external_expenditure_data$datapath
    ) |>
      base::subset(
        select = c(inputs_rv[["entity"]], "Expenditure change (%)")
      )
    existing_data <- inputs_rv[["expenditure_df"]] |>
      subset(
        select = -`Expenditure change (%)`
      )
    tmp_numeric <- inputs_rv[["expenditure_df"]][["Expenditure change (%)"]]

    inputs_rv[["expenditure_df"]] <- merge(
      x = existing_data,
      y = uploaded_data,
      by = inputs_rv[["entity"]],
      all.x = TRUE,
      all.y = FALSE
    )

    # Check for missing NHS organisations:
    controls_rv[["is_missing_nhs_org"]] <- !all(
      existing_data[[inputs_rv[["entity"]]]] %in%
        uploaded_data[[inputs_rv[["entity"]]]]
    )
    if(controls_rv[["is_missing_nhs_org"]]) {
      missing_entries <-existing_data[[inputs_rv[["entity"]]]][
        !existing_data[[inputs_rv[["entity"]]]] %in%
          uploaded_data[[inputs_rv[["entity"]]]]
      ]

      controls_rv[["missing_entries"]] <- sapply(
        X = missing_entries,
        FUN= function(.x) {
          paste0(
            "<li>",
            .x,
            "</li>"
          )
        }
      ) |>
        unname() |>
        paste0(collapse = "")
    }

    # Check for incompatible values:
    controls_rv[["is_incompatible_values"]] <- any(
      is.na(
        as.numeric(
          uploaded_data[["Expenditure change (%)"]]
        )
      )
    )

    if(controls_rv[["is_incompatible_values"]]) {
      # NHS organisations with incompatible values:
      erroneous_entries <- uploaded_data[[inputs_rv[["entity"]]]][
        is.na(
          as.numeric(
            uploaded_data[["Expenditure change (%)"]]
          )
        )
      ]
      controls_rv[["erroneous_entries"]] <- sapply(
        X = erroneous_entries,
        FUN= function(.x) {
          paste0(
            "<li>",
            .x,
            "</li>"
          )
        }
      ) |>
        unname() |>
        paste0(collapse = "")
    }

    # Notify user:
    if(
      any(
        controls_rv[["is_missing_nhs_org"]],
        controls_rv[["is_incompatible_values"]]
      )
    ) {
      shiny::showModal(
        source(
          file.path("elements", "ui", "inputs", "issues_expenditure.R"),
          local = TRUE
        )$value
      )
    }

    # Override missing values with default ones:
    inputs_rv[["expenditure_df"]][["Expenditure change (%)"]] <- ifelse(
      test = is.na(
        as.numeric(
          inputs_rv[["expenditure_df"]][["Expenditure change (%)"]]
        )
      ),
      yes = tmp_numeric,
      no = inputs_rv[["expenditure_df"]][["Expenditure change (%)"]]
    )

    inputs_rv[["expenditure_df"]][["Expenditure change (%)"]] <-
      as.numeric(
        inputs_rv[["expenditure_df"]][["Expenditure change (%)"]]
      )
  }
)

output[["issues_expenditure"]] <- shiny::reactive({

  return(
    any(
      controls_rv[["is_missing_nhs_org"]],
      controls_rv[["is_incompatible_values"]]
    )
  )
})

shiny::outputOptions(
  x = output,
  name = "issues_expenditure",
  suspendWhenHidden = FALSE
)
