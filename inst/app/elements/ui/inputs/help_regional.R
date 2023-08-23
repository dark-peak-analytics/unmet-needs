################################################################################
#
# Script Name:        help_regional.R
# Module Name:        ui
#
################################################################################

# Create help modal for regional inputs collapsible box ---------------------

shiny::modalDialog(
  id = "help_regional",

  shiny::fluidRow(
    shiny::column(
      offset = 1,
      width=10,
      shiny::br(),

      shiny::h2(
        "Selecting individual, region-specific or nation-wide NHS organisations"
      ),

      shiny::hr(),

      shiny::HTML(
        paste0(
          "In addition to presenting the health inequality impact on the ",
          "national level, the tool also allows switching between national, ",
          "region(s), or selected ", inputs_rv[["entity"]], "(s).",
          "<br>",
          "<br>",
          "By default, the tool presents the results of all NHS England ",
          inputs_rv[["entity"]], "s, as of ",
          inputs_rv[["IMD_population_year"]], ".", "To select specific a ",
          "region(s) or ", inputs_rv[["entity"]], "s, press on the green ",
          "button under the <b>NHS Organisations</b> section."
        )
      ),

      shiny::HTML(
        paste0(
          "<hr>",
          "<h3>Selecting NHS organisation in one or more region</h3>",
          "Clicking on the box labeled <b>English Region</b> opens a ",
          "dropdown menu naming the English Regions. One or more regions can ",
          "be selected by clicking on their names. Once selected, the region ",
          "name will be moved to the box, which updates the plot, map and ",
          "table in the <b>Impact on QALYs</b> tab to only show the ",
          inputs_rv[["entity"]], "s in the selected regions.",
          "<hr>",
          "<h3>Selecting one or more NHS organisation</h3>",
          "Clicking on the box labelled <b>Clinical Commissioning Group ",
          "(CCG)</b> lists NHS England.", inputs_rv[["entity"]], "s.",
          "Clicking on a ", inputs_rv[["entity"]],
          " name moves it to the box, which updates the plot, map and table ",
          "in the <b>Impact on QALYs</b> tab to only show the selected ",
          inputs_rv[["entity"]], "s."
        )
      )
    )
  ),

  style = "font-weight: 350; line-height: 22px;",
  size="l",
  easyClose = TRUE,
  footer = shiny::div(
    style = "text-align:center",
    shiny::modalButton("Dismiss")
  )
)
