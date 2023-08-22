################################################################################
#
# Script Name:        main-page_sidebar.R
# Module Name:        ui
#
################################################################################

# Define path for UI elements --------------------------------------------------

ui_path <- file.path("elements", "ui")

# Create dashboard with tab navigation bar -------------------------------------
shinydashboard::dashboardSidebar(
  width = 210,
  shinydashboard::sidebarMenu(
    shiny::br(),
    shiny::br(),
    shinydashboard::menuItem(
      text = "Inputs",
      tabName = "inputs"
    ),
    shinydashboard::menuItem(
      text = "Impact on QALYs",
      tabName = "qalys"
    ),
    shinydashboard::menuItem(
      text = "Impact on mortality",
      tabName = "mortality"
    )
  ),
  shiny::br(),
  shiny::br(),
  shiny::hr(),
  shiny::div(
    class = "nav_foot",
    # About the app modal
    shiny::actionLink(
      inputId = "show_about",
      label =  "About the tool",
      icon = shiny::icon(
        name = "info-circle",
        class = "fa-0.5x"
      )
    )
  ),
  shiny::fluidRow(
    width = 1,
    source(
      file.path(ui_path, "footer.R"),
      local = TRUE
    )$value
  )
)
