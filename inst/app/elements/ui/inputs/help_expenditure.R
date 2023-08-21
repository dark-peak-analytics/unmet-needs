################################################################################
#
# Script Name:        help_expenditure.R
# Module Name:        ui
#
################################################################################

# Create help modal for expenditure inputs collapsible box ---------------------

shiny::modalDialog(
  id = "help_expenditure",

  shiny::fluidRow(
    shiny::column(
      offset = 1,
      width=10,
      shiny::br(),

      shiny::h2("Understanding and editing expenditure data"),

      shiny::hr(),

      shiny::HTML(
        paste0(
          "The tool allows users to provide annual healthcare percentage ",
          "change for each ", inputs_rv[["entity"]],
          " either by editing the values under the `Expenditure change (%)` ",
          "column in the table presented under the <strong>Basic ",
          inputs_rv[["entity"]], " Population and Expenditure Change Data ",
          "(Editable)</strong> section or by uploading a <em>csv</em> file ",
          "containing the ", inputs_rv[["entity"]],"s' names and the ",
          "corresponding change in annual expenditure. A <em>csv</em> file ",
          "is available for download once the upload mode is activated.",
          "<br>",
          "<br>",
          "By default, the tool allows you to edit the values under the ",
          "`Expenditure change (%)` column. This mode is highlighted by the ",
          shiny::icon(name = "upload"), " icon. Press the ",
          shiny::icon(name = "upload"), " icon, to enable the upload of ",
          "external data. This mode is highlighted by the change of the ",
          shiny::icon(name = "upload"), " icon to ", shiny::icon(name = "pen"),
          ". This change is followed by the appearance of two fields above the ",
          "table."
        )
      ),

      shiny::hr(),

      shiny::h3("Table contents"),

      shiny::p("The table below shows "),
      shiny::tags$ul(
        shiny::tags$li(
          "the change in healthcare expenditure, and"
        ),
        shiny::tags$li(
          "the distribution of individuals by:",
          shiny::tags$ul(
            shiny::tags$li(
              "National Health Service (NHS) organisations, and"
            ),
            shiny::tags$li(
              "deprivation quintile."
            )
          )
        )
      ),

      shiny::hr(),

      shiny::HTML(
        paste0(
          "<h3>Uploading `Expenditure change (%)` data</h3>",
          "<ul>",
          "<li>Activate the upload mode by pressing on the ",
          shiny::icon(name = "upload"), " icon, which should then change to ",
          shiny::icon(name = "pen"), ".</li>",
          "<li>Download the provided <em>csv</em> template to upload ",
          "data.</li>",
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
          "</ul>",
          "If there were any issues in the uploaded file, the tool will ",
          "provide feedback. This feedback appears once when the external ",
          "data is uploaded, and can also be accessed by clicking on the ",
          shiny::icon(
            name = "circle-xmark",
            class = "fa-regular fa-circle-xmark fa-beat-fade",
            style = "color:red"
          ),
          " icon. Moreover, this feedback is removed once the editing mode ",
          "is activated or an error-free <em>csv</em> file is uploaded."
        )
      ),

      shiny::hr(),

      shiny::h3("Editing `Expenditure change (%)` data"),

      shiny::p(
        "To change values in the editable column(s)"
      ),
      shiny::tags$ul(
        shiny::tags$li(
          "point the mouse cursor to any cell in an editable column and",
          "double click,"
        ),
        shiny::tags$li(
          paste(
            "once the editable cells were activated, change the required",
            "values, as needed, and"
          )
        ),
        shiny::tags$li(
          "once done editing, press both buttons",
          shiny::strong("CTRL or COMMAND"), "and" , shiny::strong("ENTER"),
          "to submit and save the changes, or"
        ),
        shiny::tags$li(
          "press", shiny::strong("ESC"),
          "to cancel or reset the values to the last saved state."
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
