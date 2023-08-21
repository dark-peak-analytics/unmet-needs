################################################################################
#
# Script Name:        help_assumptions.R
# Module Name:        ui
#
################################################################################

# Create help modal for assumptions inputs collapsible box ---------------------

shiny::modalDialog(
  id = "help_assumptions",

  shiny::fluidRow(
    shiny::column(
      offset = 1,
      width=10,
      shiny::br(),

      shiny::h2("Changing modelling assumptions"),

      shiny::hr(),

      shiny::HTML(
        paste0(
          "This tool utilises three modelling assumptions and allows users ",
          "to interrogate the sensitivity of these assumptions."
        )
      ),

      shiny::HTML(
        paste0(
          "<hr>",
          "<h3>Modelling elasticities</h3>",
          "The base-case analysis does not assume equal mortality across ",
          "deprivation quintiles. However, this assumption can be activated ",
          "by clicking the button labelled <b>Assume equal elasticities</b>. ",
          "<hr>",
          "<h3>Percentage change in healthcare expenditure</h3>",
          "The tool allows users to assign an overall percentage change in ",
          "annual healthcare expenditure. This value is set in the box titled ",
          "<b>Assumed change in healthcare expenditure</b>. ",
          "Changing this value instantaneously updates the values in the ",
          "`Expenditure change (%)` column in the table presented under the ",
          "<strong>Basic ", inputs_rv[["entity"]], " Population and ",
          "Expenditure Change Data (Editable)</strong>. ",
          "Moreover, this value is also set as the default value in the ",
          "downloadable <em>csv</em> template file.",
          "<hr>",
          "<h3>Maximum Quality-adjusted Life Expectancy (QALE)</h3>",
          "Users can choose between three maximum target QALE values, <em>",
          "Low, Mid, or High</em>. The values of the three levels are ",
          "modifiable from the <b>Basic Health Impact Input Data (Editable)",
          "</b> section.",
          "The base-case analysis assumes a `High` maximum target QALE which ",
          "is equal to <b>120</b>. Please remember that both the level of ",
          "maximum target QALE and its corresponding values are amendable."
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
