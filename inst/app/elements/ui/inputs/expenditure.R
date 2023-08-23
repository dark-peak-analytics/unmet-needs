################################################################################
#
# Script Name:        expenditure.R
# Module Name:        ui
#
################################################################################

# Create Expenditure box in the inputs page ------------------------------------

shinydashboard::box(
  id = "expenditure",
  title = shiny::uiOutput(
    outputId = "expenditure_title_page"
  ),
  status = "info",
  collapsible = TRUE,
  collapsed = FALSE,
  width = 12,
  shiny::conditionalPanel(
    condition = "output.external_expenditure == false",
    shiny::fluidRow(
      shinydashboard::box(
        style = "padding-left:0px; margin-left:0px;",
        width = 12,
        shiny::column(
          width = 4,
          shiny::div(
            shiny::downloadButton(
              outputId = "nhs_orgs",
              label = "Download template file with CCG names",
              icon = shiny::icon(
                "file-csv"
              )
            ),
            style = "float: left; padding-top:0px; padding-left:0px;
            margin-left:0px;"
          )
        ),
        shiny::column(
          width = 6,
          offset = 2,
          shiny::div(
            shiny::fileInput(
              inputId = "external_expenditure_data",
              label = "Please select a csv file",
              accept = ".csv"
            ),
            style = "float: left; padding-top:8px;"
          )
        )
      )
    )
  ),
  DT::dataTableOutput(
    outputId = "expenditure_inputs"
  )
)
