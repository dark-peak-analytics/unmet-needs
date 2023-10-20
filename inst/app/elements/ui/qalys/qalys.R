################################################################################
#
# Script Name:        qalys.R
# Module Name:        ui
#
################################################################################

# Create national impact results page ------------------------------------------

shinydashboard::tabItem(
  tabName = "qalys",
  shiny::fluidRow(
    shinydashboard::box(
      title = paste(
        "Impact on Quality Adjusted Life Expectancy (QALE) of permanent",
        "lifetime expenditure change"
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 12,
      shiny::tagList(
        shiny::uiOutput(
          outputId = "summary_absolute_QALYs"
        )
      )
    ),
    shinydashboard::box(
      title = shiny::tagList(
        shiny::uiOutput(
          outputId = "title_average_QALYs"
        )
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 6,
      shiny::p(
        style = "float: left; padding-top: 0px; padding-left: 5px; margin-top: 0",
        shiny::textOutput(
          outputId = "subtitle_average_QALYs"
        )
      ),
      shiny::plotOutput(
        outputId = "plot_average_QALYs",
        height = 455,
        width = "100%"
      )
    ),
    shinydashboard::box(
      title = shiny::tagList(
        shiny::uiOutput(
          outputId = "title_map_absolute_QALYs"
        ),
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 6,
      shiny::p(
        style = "float: left; padding-top: 0px; padding-left: 5px; margin-top: 0",
        shiny::textOutput(
          outputId = "subtitle_map_absolute_QALYs"
        )
      ),
      leaflet::leafletOutput(
        outputId = "map_absolute_QALYs",
        height = 455,
        width = "100%"
      )
    )
  ),
  shiny::fluidRow(
    shinydashboard::box(
      title = shiny::tagList(
        shiny::uiOutput(
          outputId = "title_table_absolute_QALYs"
        )
      ),
      status = "info",
      collapsible = FALSE,
      collapsed = FALSE,
      width = 12,
      DT::dataTableOutput(
        outputId = "table_absolute_QALYs",
        height = 455,
        width = "100%"
      )
    )
  )
)
