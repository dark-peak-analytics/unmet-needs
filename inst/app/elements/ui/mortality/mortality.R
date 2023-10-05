################################################################################
#
# Script Name:        mortality.R
# Module Name:        ui
#
################################################################################

# Create national impact results page ------------------------------------------

shinydashboard::tabItem(
  tabName = "mortality",
  shiny::fluidRow(
    shinydashboard::box(
      title = paste(
        "Annual mortality impact"
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 12,
      shiny::tagList(
        shiny::uiOutput(
          outputId = "summary_total_deaths"
        )
      )
    ),
    shinydashboard::box(
      title = shiny::tagList(
        shiny::uiOutput(
          outputId = "title_average_deaths"
        )
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 6,
      shiny::p(
        style = "float: left; padding-top: 0px; padding-left: 5px; margin-top: 0",
        shiny::textOutput(
          outputId = "subtitle_average_deaths"
        )
      ),
      shiny::plotOutput(
        outputId = "plot_average_deaths",
        height = 455,
        width = "100%"
      )
    ),
    shinydashboard::box(
      title = shiny::tagList(
        shiny::uiOutput(
          outputId = "title_map_total_deaths"
        ),
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 6,
      shiny::p(
        style = "float: left; padding-top: 0px; padding-left: 5px; margin-top: 0",
        shiny::textOutput(
          outputId = "subtitle_map_total_deaths"
        )
      ),
      leaflet::leafletOutput(
        outputId = "map_total_deaths",
        height = 455,
        width = "100%"
      )
    )
  ),
  shiny::fluidRow(
    shinydashboard::box(
      title = shiny::tagList(
        shiny::uiOutput(
          outputId = "title_table_total_deaths"
        )
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 12,
      DT::dataTableOutput(
        outputId = "table_total_deaths",
        height = 455,
        width = "100%"
      )
    )
  )
)
