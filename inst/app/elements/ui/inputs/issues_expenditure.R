################################################################################
#
# Script Name:        issues_expenditure.R
# Module Name:        ui
#
################################################################################

# Create issues modal for expenditure inputs collapsible box -------------------

shiny::modalDialog(
  id = "external_expenditure_data_issues",

  shiny::fluidRow(
    shiny::column(
      offset = 1,
      width=10,
      shiny::br(),

      shiny::h2("Issues with uploaded expenditure data"),

      if(controls_rv[["is_incompatible_values"]]) {
        shiny::HTML(
          paste0(
            "<hr>",
            "<h3>",
            "Unacceptable values in `Expenditure change (%)`:",
            "</h3>",
            "<br>",
            "There was an issue with the values recorded under the ",
            "`Expenditure change (%)` column corresponding to the ",
            "following ",
            inputs_rv[["entity"]],
            "s:",
            "<br>",
            "<ol>",
            controls_rv[["erroneous_entries"]],
            "</ol>",
            "These values will be replaced with the default value ",
            "<strong>",
            input$pcnt_change,
            "</strong>. However, you can still edit these values in ",
            "the <strong> Population and Expenditure Change Data ",
            "Table</strong>. Remember to enable data editing by ",
            "clicking on the 'pen' ",
            shiny::icon(
              name = "pen"
            ),
            " icon."
          )
        )
      },

      if(controls_rv[["is_missing_nhs_org"]]) {
        shiny::HTML(
          paste0(
            "<hr>",
            "<h3>",
            "Missing ",
            inputs_rv[["entity"]],
            "s:",
            "</h3>",
            "<br>",
            "The uploaded file is either missing or has different ",
            "names for the following ",
            inputs_rv[["entity"]],
            "s:",
            "<br>",
            "<ol>",
            controls_rv[["missing_entries"]],
            "</ol>",
            "The values corresponding to these ",
            inputs_rv[["entity"]],
            "s will be replaced with the default value <strong>",
            input$pcnt_change,
            "</strong>. However, you can still change these values in ",
            "the <strong>Population and Expenditure Change ",
            "Data Table</strong>. Remember to enable data editing ",
            "by clicking on the 'pen' ",
            shiny::icon(
              name = "pen"
            ),
            " icon."
          )
        )
      },

      shiny::HTML(
        paste0(
          "<hr>",
          "<h3>",
          "Notes:",
          "</h3>",
          "<ul>",
          "<li>Download the provided template.</li>",
          "<li>In the template, only add the numeric value of the ",
          "percentage changes in healthcare expenditure under the ",
          "`Expenditure change (%)` column as needed. For example, ",
          "please enter 2 to indicate a 2% change.</li>",
          "<li>Do not change the ", inputs_rv[["entity"]],"s ",
          "names under the ", inputs_rv[["entity"]], " column.</li>",
          "<li>Once uploaded, you can edit the values under the ",
          "`Expenditure change (%)` column once the table editing ",
          "mode was activated.</li>",
          "<li>Data editing mode is activated by clicking the 'pen' ",
          shiny::icon(
            name = "pen"
          ),
          " icon.</li>",
          "<li>The editing mode is considered active if the 'pen' ",
          "icon disappeared.</li>",
          "</ul>"
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
