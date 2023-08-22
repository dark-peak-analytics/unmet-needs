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
      "<a href= href='https://www.darkpeakanalytics.com/'>
      <img src= dpa2.png width='60px'>
      </a>"
      ),
    style = "margin:0; padding:0px; padding-left:10px;",
    align = "right"
  ),
  shiny::column(
    width = 8,
    offset = 2,
    style = "vertical-align: center; margin-top:13px;",
    align = "left",
    shiny::HTML("A shiny app by:<br> <b>Wael Mohammed</b>"),
    shiny::br(),
    shiny::HTML('<a href="https://www.darkpeakanalytics.com/"
    style="color: #00c0ef;">Dark Peak Analytics</a>')
  )
)
