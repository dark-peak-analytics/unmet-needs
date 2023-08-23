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
        alt = "University of York",
        height = "75",
        width = "200",
        style = "margin-left: -14px; margin-right: 0px; margin-top: 5px;
        padding-left: 0px; padding-bottom: 16px; padding-top: 0px;
        margin-bottom: auto;"
      )
    )
  ),
  titleWidth = 190
)
