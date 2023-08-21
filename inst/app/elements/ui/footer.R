################################################################################
#
# Script Name:        footer.R
# Module Name:        ui
#
################################################################################

# Create copyright footer giving model overview -------------------------------------

shiny::div(
  class = "copyright nav_foot",
  shiny::column(
    width = 2,
    shiny::HTML(
      "<img src= dpa2.png width='60px'>"
      ),
    style = "margin:0; padding-left:5px;",
    align = "right"
  ),
  shiny::column(
    width = 9,
    offset = 1,
    style = "vertical-align: center; margin-top:5px;",
    align = "left",
    shiny::HTML("A shiny app by:<br> <b>Wael Mohammed</b>"),
    shiny::br(),
    shiny::HTML('<a href="https://www.darkpeakanalytics.com/"
    style="color: #00c0ef;">Dark Peak Analytics</a>')
  )
)
