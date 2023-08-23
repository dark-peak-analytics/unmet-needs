################################################################################
#
# Script Name:        about.R
# Module Name:        ui
#
################################################################################

# Create about modal giving model overview -------------------------------------

shiny::modalDialog(
  id = "about_model_modal",
  shiny::fluidRow(
    shiny::column(
      offset = 1,
      width=10,
      shiny::br(),

      shiny::h3("About this app"),
      shiny::br(),

      shiny::h4("Disclaimer"),
      shiny::p(
        shiny::HTML(
          paste0("Please note that this is a beta version.
                 In the meantime, please
                 <a href='mailto:richard.cookson@york.ac.uk'>contact us</a>
                 with any requests or queries."
          ))),
      shiny::br(),

      shiny::h4("Purpose"),
      shiny::p("
        The purpose of the app is ..."
      ),
      shiny::br(),

      shiny::h4("Package and code"),
      shiny::p("All source code is available, including:"),
      shiny::tags$ol(
        shiny::tags$li("The code for the app, hosted here ..."),
        shiny::br(),
        shiny::tags$li("The code for the underlying functions, are here ...")
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
