################################################################################
#
# Script Name:        main-page_header.R
# Module Name:        ui
#
################################################################################

# Create banner ----------------------------------------------------------------

shinydashboard::dashboardHeader(
  title = shiny::div(
    shiny::tags$a(
      href = "https://www.york.ac.uk/che/research/equity/",
      shiny::img(
        src = "logo.svg",
        # src = "https://www.york.ac.uk/static/stable/img/logo.svg",
        alt = "University of York",
        height = "60",
        width = "100",
        style = "float: center; margin-right: 10px; margin-top: 0px;"
      )
    )
  ),
  titleWidth = 228
)
