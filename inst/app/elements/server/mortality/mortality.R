################################################################################
#
# Script Name:        mortality.R
# Module Name:        server
#
################################################################################

# Define a waiter object

waiter_deaths_plot <- waiter::Waiter$new(
  id = "plot_average_deaths",
  html = waiter::spin_loaders(
    id = 1,
    color = "#00c0ef"
  ),
  hide_on_render  = TRUE
)

waiter_deaths_map <- waiter::Waiter$new(
  id = "map_total_deaths",
  html = waiter::spin_loaders(
    id = 1,
    color = "#00c0ef"
  ),
  hide_on_render  = TRUE
)

waiter_deaths_table <- waiter::Waiter$new(
  id = "table_total_deaths",
  html = waiter::spin_loaders(
    id = 1,
    color = "#00c0ef"
  ),
  hide_on_render  = TRUE
)

# Estimate Average Lives Saved ---------------------------------------------------

average_lives_saved <- shiny::reactiveValues()

shiny::observe({
  imd_pop_data <- inputs_rv[["IMD_population"]]
  imd_pop_data[["Expenditure change (%)"]] <- input$pcnt_change

  average_lives_saved[["data"]] <- UnmetNeeds::calculate_average_deaths(
    total_lives_saved_ = UnmetNeeds::calculate_total_deaths(
      mortality_rates_ = inputs_rv[["mortality_rates"]],
      mortality_elasticity_ = inputs_rv[["mortality_elasticity"]],
      equal_mortality_elasticity_ = inputs_rv[["equal_mortality_elasticity"]],
      option_ = maximum_QALE_option(),
      imd_population_ = imd_pop_data,
      provider_ = inputs_rv[["entity"]]
    )$data,
    imd_population_ = imd_pop_data
  )
})

shiny::observe({
  average_lives_saved[["plot"]] <- UnmetNeeds::plot_average_deaths(
    average_lives_saved_ = average_lives_saved[["data"]]
  )
})

# Render Average Lives Saved outputs -------------------------------------------

output[["title_average_deaths"]] <- shiny::renderUI(
  expr = {
    shiny::req(average_lives_saved[["data"]])

    shiny::tagList(
      paste0(
        average_lives_saved[["data"]][["title"]]
      ),
      shiny::tags$div(
        title = "Download Plot",
        shiny::downloadButton(
          outputId = "download_deaths_plot",
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

output[["subtitle_average_deaths"]] <- shiny::renderText(
  expr = {
    shiny::req(average_lives_saved[["data"]])

    paste0(
      average_lives_saved[["data"]][["subtitle"]]
    )
  }
)

output[["plot_average_deaths"]] <- shiny::renderPlot(
  expr = {
    shiny::req(average_lives_saved[["plot"]])

    waiter_deaths_plot$show()

    average_lives_saved[["plot"]]$data +
      ggplot2::labs(
        title = NULL,
        subtitle = NULL
      )

  },
  res = 96, # 148 120 are good values for resolution in shiny
  bg = "transparent"
)

# Estimate Total Lives Saved ---------------------------------------------------

total_deaths <- shiny::reactiveValues()

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
  total_deaths[["national"]] <- UnmetNeeds::calculate_total_deaths(
    mortality_rates_ = inputs_rv[["mortality_rates"]],
    mortality_elasticity_ = inputs_rv[["mortality_elasticity"]],
    equal_mortality_elasticity_ = inputs_rv[["equal_mortality_elasticity"]],
    option_ = maximum_QALE_option(),
    imd_population_ = imd_population(),
    provider_ = inputs_rv[["entity"]]
  )
})

total_deaths_map <- shiny::reactive({
  shiny::req(total_deaths[["national"]])

  # Create_map() function expects the main outcome data to be under column Value
  value_df <- total_deaths[["national"]]$data
  names(value_df)[
    names(value_df) == "Average lives saved (100,000 population)"] <- "Value"

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

  slotNames(sp_df) # For some reason an error occurs without this line

  # Remove NHS CCG from names
  methods::slot(sp_df, "data")[[common_col]] <- gsub(
    x =  methods::slot(sp_df, "data")[[common_col]],
    pattern = "NHS | CCG| ICS| ICB",
    replacement = "",
    ignore.case = TRUE
  )

  slotNames(sp_df) # For some reason an error occurs without this line

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
    var_nm_ = paste("Lives saved"),
    value_year_ = NA,
    value_digits = 0
  ) |>
    leaflet::fitBounds(
      sp_bounds[1], sp_bounds[2], sp_bounds[3], sp_bounds[4]
    )
})

total_deaths_df_data <- shiny::reactive({
  shiny::req(total_deaths[["national"]])

  df_colnames <- colnames(total_deaths[["national"]][["data"]])

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

  tmp_table <- total_deaths[["national"]][["data"]] |>
    subset(
      select = c(nhs_org_names, tot_pop_names, avg_outcome_name,
                 total_outcome_name, quantile_names)
    )

  # Save summary values
  outputs_rv[["total_lives_saved"]] <- sum(tmp_table[[total_outcome_name]])
  outputs_rv[["average_lives_saved"]] <- 1e5 *
    (outputs_rv[["total_lives_saved"]] / sum(tmp_table[[tot_pop_names]]))

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
    "Total lives saved",
    quantile_names
  )

  tmp_table
})


# Render Total Lives Saved outputs ---------------------------------------------

output[["summary_total_lives_saved"]] <- shiny::renderUI(
  expr = {
    shiny::req(total_deaths_df_data())

    shiny::tagList(
      paste0(
        "Total lives saved in England: ",
        round(outputs_rv[["total_lives_saved"]])  |>
          format(big.mark = ","),
        "."
      ),
      shiny::br(),
      paste0(
        "Average lives saved per 100,000 population in England: ",
        round(outputs_rv[["average_lives_saved"]]) |>
          format(big.mark = ","),
        "."
      )
    )
  }
)

output[["title_map_total_deaths"]] <- shiny::renderUI(
  expr = {
    shiny::req(total_deaths[["national"]])

    shiny::tagList(
      paste0(
        "Average Health Impact by ",
        inputs_rv[["entity"]]
      ),
      shiny::tags$div(
        title = "Download Map",
        shiny::downloadButton(
          outputId = "download_deaths_map",
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

output[["subtitle_map_total_deaths"]] <- shiny::renderText(
  expr = {
    shiny::req(total_deaths[["national"]])

    paste0(
      "Lives saved per 100,000 Population in ",
      inputs_rv[["entity"]]
    )
  }
)

output[["map_total_deaths"]] <- leaflet::renderLeaflet(
  expr = {
    shiny::req(total_deaths_map())

    waiter_deaths_map$show()

    total_deaths_map()
  }
)

output[["title_table_total_deaths"]] <- shiny::renderUI(
  expr = {
    shiny::req(total_deaths[["national"]])

    shiny::tagList(
      paste0(
        total_deaths[["national"]][["title"]]
      ),
      shiny::tags$div(
        title = "Download Table",
        shiny::downloadButton(
          outputId = "download_deaths_table",
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

output[["table_total_deaths"]] <- DT::renderDataTable(
  expr = {
    shiny::req(total_deaths_df_data())

    waiter_deaths_table$show()

    tmp_df <- total_deaths_df_data()

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
output[["download_deaths_plot"]] <- shiny::downloadHandler(
  filename = function() {
    paste0(
      "Lives Saved: ",
      average_lives_saved[["data"]][["title"]],
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
      plot = average_lives_saved[["plot"]]$data
    )
  }
)

## Map
### Prepare title for downloadable map
tag_downloadable_deaths_map_title <- shiny::tags$style(
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

tag_downloadable_deaths_map_title_span <- shiny::tags$style(
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

downloadable_deaths_map_title_text <- shiny::reactive({
  paste0(
    "Average Health Impact by ",
    inputs_rv[["entity"]]
  )
})
downloadable_deaths_map_subtitle_text <- shiny::reactive({
  paste0(
    "Lives saved per 100,000 Population in ",
    inputs_rv[["entity"]]
  )
})

downloadable_deaths_map_title <- shiny::reactive({
  shiny::tags$div(
    tag_downloadable_deaths_map_title,
    shiny::HTML(
      paste0(
        downloadable_deaths_map_title_text()
      )
    )
  )
})

downloadable_deaths_map_subtitle <- shiny::reactive({
  shiny::tags$div(
    tag_downloadable_deaths_map_title_span,
    shiny::HTML(
      paste0(
        downloadable_deaths_map_subtitle_text()
      )
    )
  )
})

### Set map download handler
output[["download_deaths_map"]] <- shiny::downloadHandler(
  filename = function() {
    paste0(
      "Lives Saved: ",
      "Average Health Impact by ",
      inputs_rv[["entity"]],
      ".png"
    )
  },
  content = function(file) {
    shinyjs::disable("download_deaths_map")
    on.exit(shinyjs::enable("download_deaths_map"))
    mapview::mapshot(
      file = file,
      x = total_deaths_map() |>
        leaflet::addControl(
          html = downloadable_deaths_map_title(),
          position = "topleft",
          className = "map-title"
        ) |>
        leaflet::addControl(
          html = downloadable_deaths_map_subtitle(),
          position = "topleft",
          className = "map-title-span"
        )
    )
  }
)

## Table
output[["download_deaths_table"]] <- shiny::downloadHandler(
  filename = function() {
    paste0(
      "Lives Saved: ",
      total_deaths[["national"]][["title"]],
      ".csv"
    )
  },
  content = function(file) {
    write.csv(
      file = file,
      x = {
        tmp_df <- total_deaths_df_data()

        tmp_df <- tmp_df[, c(
          colnames(tmp_df)[colnames(tmp_df)!= "Total population"],
          "Total population"
        ), with = FALSE]

        quantile_names <- grep(
            x = colnames(inputs_rv[["IMD_population"]]),
            pattern = "Q",
            ignore.case = FALSE,
            value = TRUE
          )

        entity <- grep(
          x =  colnames(inputs_rv[["IMD_population"]]),
          pattern = "nm",
          ignore.case = TRUE,
          value = TRUE
        )

        pop_df <- inputs_rv[["IMD_population"]][, c(entity, quantile_names)]

        colnames(pop_df) <- c(inputs_rv[["entity"]],
                              paste0(quantile_names, " population"))
        tmp_df <- merge(
          x = tmp_df,
          y = pop_df,
          by = inputs_rv[["entity"]]
        )

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
