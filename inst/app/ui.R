################################################################################
#
# Script Name:        ui.R
#
################################################################################

# Define javascript function for collapsible tables ----------------------------

jscode <- "
  shinyjs.collapse = function(boxid) {
  $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
  }
  "

# Define main UI page ----------------------------------------------------------

ui <- shiny::fluidPage(
  title = "NHS Formula Health Inequality Impact Calculator",


  # Browser page logo ------------------------------------------------------

  shiny::tags$head(
    shiny::tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "york_mini.png"
    )
  ),

  # Setup html head tags (formatting metadata) -----------------------------

  # Load stylesheet specifying font formatting and layout options
  shiny::tags$head(
    shiny::includeCSS(file.path("css", "dpa_DB_stylesheet.css"))
  ),

  # Allow Javascript functionality
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    text = jscode,
    functions = "collapse"
  ),

  # Allow Waiter functionality
  waiter::use_waiter(),

  # Add page elements ------------------------------------------------------
  shinyjs::hidden(
    shiny::div(
      id = "main-ui",
      shinydashboard::dashboardPage(
        header =  source(
          file.path("elements", "ui", "main-page", "main-page_header.R"),
          local = TRUE
        )$value,
        sidebar = source(
          file.path("elements", "ui", "main-page", "main-page_sidebar.R"),
          local = TRUE
        )$value,
        body = source(
          file.path("elements", "ui", "main-page", "main-page_body.R"),
          local = TRUE
        )$value,
        skin = "blue" # this allows style (css) to be changed using 'skin-blue'
      )
    )
  )
)
