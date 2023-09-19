################################################################################
#
# Script Name:        qalys.R
# Module Name:        server
#
################################################################################

# Define a waiter object

waiter_qalys_plot <- waiter::Waiter$new(
  id = "plot_average_QALYs",
  html = waiter::spin_loaders(
    id = 1,
    color = "#00c0ef"
  ),
  hide_on_render  = TRUE
)

waiter_qalys_map <- waiter::Waiter$new(
  id = "map_absolute_QALYs",
  html = waiter::spin_loaders(
    id = 1,
    color = "#00c0ef"
  ),
  hide_on_render  = TRUE
)

waiter_qalys_table <- waiter::Waiter$new(
  id = "table_absolute_QALYs",
  html = waiter::spin_loaders(
    id = 1,
    color = "#00c0ef"
  ),
  hide_on_render  = TRUE
)

# Convert input variables to reactives -----------------------------------------

target_maximum_QALE <- shiny::reactive({
  inputs_rv[["HRQoL_inputs_2"]][input$target_maximum_QALE |> unname(),]
})

# Estimate Average QALYs -------------------------------------------------------

average_QALYs <- shiny::reactiveValues()

shiny::observe({
  imd_pop_data <- inputs_rv[["IMD_population"]]
  imd_pop_data[["Expenditure change (%)"]] <- input$pcnt_change

  average_QALYs[["data"]] <- UnmetNeeds::calculate_average_QALYs(
    absolute_QALYs_ = UnmetNeeds::calculate_absolute_QALYs(
      target_maximum_health_ = target_maximum_QALE(),
      baseline_health_ = inputs_rv[["baseline_health"]],
      mortality_elasticity_ = inputs_rv[["mortality_elasticity"]],
      option_ = maximum_QALE_option(),
      imd_population_ = imd_pop_data,
      provider_ = inputs_rv[["entity"]]
    )$data[[1]],
    imd_population_ = imd_pop_data
  )
})

shiny::observe({
  average_QALYs[["plot"]] <- UnmetNeeds::plot_average_QALYs(
    average_QALYs_ = average_QALYs[["data"]]
  )
})

# Render Average QALYs outputs -------------------------------------------------

output[["title_average_QALYs"]] <- shiny::renderUI(
  expr = {
    shiny::req(average_QALYs[["data"]])

    shiny::tagList(
      paste0(
        average_QALYs[["data"]][["title"]]
      ),
      shiny::tags$div(
        title = "Download Plot",
        shiny::downloadButton(
          outputId = "download_plot",
          label = "",
          icon = shiny::icon(
            name = "file-image"
          ),
          style = "float: right!important; position: absolute; right: 5px;
          top: 5px; border-color: #00c0ef; margin-right: 0px"
        )
      )
    )
  }
)

output[["subtitle_average_QALYs"]] <- shiny::renderText(
  expr = {
    shiny::req(average_QALYs[["data"]])

    paste0(
      average_QALYs[["data"]][["subtitle"]]
    )
  }
)

output[["plot_average_QALYs"]] <- shiny::renderPlot(
  expr = {
    shiny::req(average_QALYs[["plot"]])

    waiter_qalys_plot$show()

    average_QALYs[["plot"]]$data +
      ggplot2::labs(
        title = NULL,
        subtitle = NULL
      )

  },
  res = 96, # 148 120 are good values for resolution in shiny
  bg = "transparent"
)

# Estimate Absolute QALYs ------------------------------------------------------

absolute_QALYs <- shiny::reactiveValues()

imd_population <- shiny::reactive({

  entity <- grep(
    x =  colnames(inputs_rv[["IMD_population"]]),
    pattern = "nm",
    ignore.case = TRUE,
    value = TRUE
  )

  imd_population <- merge(
    x = subset(
      x = inputs_rv[["IMD_population"]],
      select = -`Expenditure change (%)`
    ),
    y = subset(
      x = inputs_rv[["expenditure_df"]],
      select = c(inputs_rv[["entity"]], "Expenditure change (%)")
    ),
    by.x = entity,
    by.y = inputs_rv[["entity"]]
  )

  imd_population
})

shiny::observe({
  absolute_QALYs[["national"]] <- UnmetNeeds::calculate_absolute_QALYs(
    target_maximum_health_ = target_maximum_QALE(),
    baseline_health_ = inputs_rv[["baseline_health"]],
    mortality_elasticity_ = inputs_rv[["mortality_elasticity"]],
    option_ = maximum_QALE_option(),
    imd_population_ = imd_population(),
    provider_ = inputs_rv[["entity"]]
  )
})

absolute_QALYs_map <- shiny::reactive({
  shiny::req(absolute_QALYs[["national"]])

  # Create_map() function expects the main outcome data to be under column Value
  value_df <- absolute_QALYs[["national"]]$data[[1]]
  names(value_df)[
    names(value_df) == "Average change (100,000 population)"] <- "Value"

  # Ensure SP data is the same as the value_df; i.e. subset as needed
  sp_df <- inputs_rv[["spdf_2019"]]
  common_col <- intersect(
    colnames(sp_df@data),
    colnames(value_df)
  ) |>
    grep(
      x = _,
      pattern = "nm",
      ignore.case = TRUE,
      value = TRUE
    )

  slotNames(sp_df)

  sp_df <- sp_df[
    methods::slot(sp_df, "data")[[common_col]] %in% value_df[[common_col]],
  ]

  slotNames(sp_df)

  sp_bounds <- sp_df |>
    sf::st_bbox() |>
    as.character()

  slotNames(sp_df)

  # Remove NHS CCG from names
  methods::slot(sp_df, "data")[[common_col]] <- gsub(
    x =  methods::slot(sp_df, "data")[[common_col]],
    pattern = "NHS | CCG| ICS| ICB",
    replacement = "",
    ignore.case = TRUE
  )

  slotNames(sp_df)

  value_df[[common_col]] <- gsub(
    x =  value_df[[common_col]],
    pattern = "NHS | CCG| ICS| ICB",
    replacement = "",
    ignore.case = TRUE
  )

  slotNames(sp_df)

  # Create the map
  UnmetNeeds::create_map_plot(
    sp_df_ = sp_df,
    value_df_ = value_df,
    var_nm_ = paste(
      inputs_rv[["outcome"]],
      "change"
    ),
    value_year_ = NA,
    value_digits = 0
  ) |>
    leaflet::fitBounds(
      sp_bounds[1], sp_bounds[2], sp_bounds[3], sp_bounds[4]
    )
})

absolute_QALYs_df_data <- shiny::reactive({
  shiny::req(absolute_QALYs[["national"]])

  df_colnames <- colnames(absolute_QALYs[["national"]][["data"]][[1]])

  tot_pop_names <-  df_colnames |>
    grep(
      x = _,
      pattern = "Overall population",
      ignore.case = TRUE,
      value = TRUE
    )

  nhs_org_names <-  df_colnames |>
    grep(
      x = _,
      pattern = "nm",
      ignore.case = TRUE,
      value = TRUE
    )

  quantile_names <- df_colnames |>
    grep(
      x = _,
      pattern = "Q",
      ignore.case = FALSE,
      value = TRUE
    )

  total_outcome_name <- df_colnames |>
    grep(
      x = _,
      pattern = "total",
      ignore.case = TRUE,
      value = TRUE
    )

  avg_outcome_name <- df_colnames |>
    grep(
      x = _,
      pattern = "Average",
      ignore.case = TRUE,
      value = TRUE
    )

  tmp_table <- absolute_QALYs[["national"]][["data"]][[1]] |>
    subset(
      select = c(nhs_org_names, tot_pop_names, avg_outcome_name,
                 total_outcome_name, quantile_names)
    )

  # Save summary values
  outputs_rv[["total_QALYs_gained"]] <- sum(tmp_table[[total_outcome_name]])

  # Create a vector of numeric column names
  numeric_cols <- names(tmp_table)[sapply(tmp_table, is.numeric)]

  # Round all numeric columns to 2 decimal places
  for (col in numeric_cols) {
    tmp_table[[col]] <- round(tmp_table[[col]], 0) |>
      format(big.mark = ",")
  }

  names(tmp_table) <- c(
    inputs_rv[["entity"]],
    "Total population",
    avg_outcome_name,
    "Total QALY change",
    quantile_names
  )

  tmp_table
})


# Render Absolute QALYs outputs ------------------------------------------------

output[["summary_absolute_QALYs"]] <- shiny::renderUI(
  expr = {
    shiny::req(absolute_QALYs_df_data)

    shiny::tagList(
      paste0(
        "Total Quality Adjusted Life Years Gained (QALYs) gained: ",
        round(outputs_rv[["total_QALYs_gained"]]),
        "."
      )
    )
  }
)

output[["title_map_absolute_QALYs"]] <- shiny::renderUI(
  expr = {
    shiny::req(absolute_QALYs[["national"]])

    shiny::tagList(
      paste0(
        "Average Health Impact by ",
        inputs_rv[["entity"]]
      ),
      shiny::tags$div(
        title = "Download Map",
        shiny::downloadButton(
          outputId = "download_map",
          label = "",
          icon = shiny::icon(
            name = "file-image"
          ),
          style = "float: right!important; position: absolute; right: 5px;
          top: 5px; border-color: #00c0ef; margin-right: 0px"
        )
      )
    )
  }
)

output[["subtitle_map_absolute_QALYs"]] <- shiny::renderText(
  expr = {
    shiny::req(absolute_QALYs[["national"]])

    paste0(
      "QALYs per 100,000 Population in ",
      inputs_rv[["entity"]]
    )
  }
)

output[["map_absolute_QALYs"]] <- leaflet::renderLeaflet(
  expr = {
    shiny::req(absolute_QALYs_map())

    waiter_qalys_map$show()

    absolute_QALYs_map()
  }
)

output[["title_table_absolute_QALYs"]] <- shiny::renderUI(
  expr = {
    shiny::req(absolute_QALYs[["national"]])

    shiny::tagList(
      paste0(
        absolute_QALYs[["national"]][["title"]]
      ),
      shiny::tags$div(
        title = "Download Table",
        shiny::downloadButton(
          outputId = "download_table",
          label = "",
          icon = shiny::icon(
            name = "file-csv"
          ),
          style = "float: right!important; position: absolute; right: 5px;
          top: 5px; border-color: #00c0ef; margin-right: 0px"
        )
      )
    )
  }
)

output[["table_absolute_QALYs"]] <- DT::renderDataTable(
  expr = {
    shiny::req(absolute_QALYs_df_data())

    waiter_qalys_table$show()

    tmp_df <- absolute_QALYs_df_data()

    tmp_df[[inputs_rv[["entity"]]]] <- gsub(
      x =  tmp_df[[inputs_rv[["entity"]]]],
      pattern = "NHS | CCG| ICS| ICB",
      replacement = "",
      ignore.case = TRUE
    )

    tmp_df
  },
  options = list(
    #dom = 't', # only show the table
    ordering = TRUE,
    keys = TRUE,
    scrollX = TRUE
  ),
  rownames = FALSE,
  selection = 'none'
)

# Set Download Handlers --------------------------------------------------------

## Plot
output[["download_plot"]] <- shiny::downloadHandler(
  filename = function() {
    paste0(
      "QALYs: ",
      average_QALYs[["data"]][["title"]],
      ".png"
    )
  },
  content = function(file) {
    ggplot2::ggsave(
      filename = file,
      units = "in",
      dpi = 300,
      width = 7,
      height = 4,
      plot = average_QALYs[["plot"]]$data
    )
  }
)

## Map
### Prepare title for downloadable map
tag_downloadable_map_title <- shiny::tags$style(
  shiny::HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    text-align: center;
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 18px;
  }
")
)

tag_downloadable_map_title_span <- shiny::tags$style(
  shiny::HTML("
  .leaflet-control.map-title-span {
    transform: translate(-50%,20%);
    position: fixed !important;
    margin-top: 40px;
    text-align: center;
    background: rgba(255,255,255,0.75);
    font-weight: normal;
    font-size: 16px;
    font-style: italic;
  }
")
)

downloadable_map_title_text <- shiny::reactive({
  paste0(
    "Average Health Impact by ",
    inputs_rv[["entity"]]
  )
})
downloadable_map_subtitle_text <- shiny::reactive({
  paste0(
    "QALYs per 100,000 Population in ",
    inputs_rv[["entity"]]
  )
})

downloadable_map_title <- shiny::reactive({
  shiny::tags$div(
    tag_downloadable_map_title,
    shiny::HTML(
      paste0(
        downloadable_map_title_text()
      )
    )
  )
})

downloadable_map_subtitle <- shiny::reactive({
  shiny::tags$div(
    tag_downloadable_map_title_span,
    shiny::HTML(
      paste0(
        downloadable_map_subtitle_text()
      )
    )
  )
})

### Set map download handler
output[["download_map"]] <- shiny::downloadHandler(
  filename = function() {
    paste0(
      "QALYs: ",
      "Average Health Impact by ",
      inputs_rv[["entity"]],
      ".png"
    )
  },
  content = function(file) {
    shinyjs::disable("download_map")
    on.exit(shinyjs::enable("download_map"))
    mapview::mapshot(
      file = file,
      x = absolute_QALYs_map() |>
        leaflet::addControl(
          html = downloadable_map_title(),
          position = "topleft",
          className = "map-title"
        ) |>
        leaflet::addControl(
          html = downloadable_map_subtitle(),
          position = "topleft",
          className = "map-title-span"
        )
    )
  }
)

## Table
output[["download_table"]] <- shiny::downloadHandler(
  filename = function() {
    paste0(
      "QALYs: ",
      absolute_QALYs[["national"]][["title"]],
      ".csv"
    )
  },
  content = function(file) {
    write.csv(
      file = file,
      x = {
        tmp_df <- absolute_QALYs_df_data()

        tmp_df[[inputs_rv[["entity"]]]] <- gsub(
          x =  tmp_df[[inputs_rv[["entity"]]]],
          pattern = "NHS | CCG| ICS| ICB",
          replacement = "",
          ignore.case = TRUE
        )

        tmp_df
      }
    )
  }
)
