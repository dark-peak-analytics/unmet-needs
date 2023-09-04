################################################################################
#
# Script Name:        help_HRQoL.R
# Module Name:        ui
#
################################################################################

# Create help modal for HRQoL inputs collapsible box ---------------------------

shiny::modalDialog(
  id = "help_HRQoL",

  shiny::fluidRow(
    shiny::column(
      offset = 1,
      width=10,
      shiny::br(),

      shiny::h2("Understanding and editing HRQoL data"),

      shiny::h3("Table contents"),

      shiny::p("
        The table on the left shows, by deprivation quintile, the distribution of"
      ),
      shiny::tags$ul(
        shiny::tags$li(
          "the baseline health, in terms of Quality-adjusted Life Years (QALYs),"
        ),
        shiny::tags$li(
          "the baseline health burden, the shortfall from the assumed maximum",
          "Quality-adjusted Life Expectancy (QALE),"
        ),
        shiny::tags$li(
          "the mortality rate, per 100,000, and"
        ),
        shiny::tags$li(
          "the mortality elasticity by deprivation quintile.",
        )
      ),

      shiny::HTML(
        "The table on the right displays the assumed maximum QALE. Three levels",
        "of QALEs are available, <em>`Low`, `Mid`, and `High`</em>. The tool",
        "uses the maximum (`High`) QALE value by default, but this can be",
        "changed from the assumptions under the <b>Basic Modelling Assumptions",
        "(Modifiable)</b> tab."
      ),

      shiny::p(
        "The baseline health burden is dynamically updated when either the",
        "baseline health or the assumed maximum QALE changes."
      ),

      shiny::h3("Editable column(s)"),

      shiny::p(
        "All but the first column in the two tables below are editable."
      ),

      shiny::h3("Editing data"),

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
