################################################################################
#
# Script Name:        server.R
#
################################################################################

# Define main server page ------------------------------------------------------

server <- function(input, output, session) {
  # Specify exit behaviour (when browser tab is closed) ------------------------

  session$onSessionEnded(function() {
    shiny::stopApp()
  })

  # Waiter ---------------------------------------------------------------------

  waiter <- waiter::Waiter$new(
    html = shiny::div(
      style = "
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content:center;
            color: white;
            width: 700px;
            opacity: 1 !important;
          ",
      shiny::div(
        style = "
            display: flex;
            align-items: center;
            justify-content:center;
            color: white;
            width: 700px;
            opacity: 1 !important;
          ",
        shiny::img(
          src = "logo.svg",
          alt = "University of York",
          height = "65",
          width = "130",
          style = "float: left; margin-right: 10px; margin-top: 0px;"
        ),
        shiny::HTML(
        '<h2 style="color: #000000; padding-top: 2px; margin-right: 0px;
        margin-left: 0px;"
        >NHS Formula Health Inequality Impact Calculator</h2>'
        ),
      ),
      shiny::HTML(
      '<h4 style="color: #5A5A5A;"
      >Loading ...</h4>'
      ),
      waiter::spin_loaders(
        id = 8,
        color = "#00c0ef",
        style = "
        height: 100px;
        width: 100px;
        "
      ),
      shiny::fluidRow(
        width = 1,
        source(
          file.path("elements", "ui", "footer.R"),
          local = TRUE
        )$value
      )
    ),
    hide_on_render  = FALSE
  )

  waiter$show()
  on.exit(waiter$hide())

  # Install phantomjs is not installed in deployment server --------------------

  if (is.null(suppressMessages(webshot:::find_phantom()))) {
    waiter$update(
      html = shiny::div(
        style = "
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content:center;
            color: white;
            width: 700px;
            opacity: 1 !important;
          ",
        shiny::div(
          style = "
            display: flex;
            align-items: center;
            justify-content:center;
            color: white;
            width: 700px;
            opacity: 1 !important;
          ",
          shiny::img(
            src = "logo.svg",
            # src = "https://www.york.ac.uk/static/stable/img/logo.svg",
            alt = "University of York",
            height = "60",
            width = "100",
            style = "float: left; margin-right: 10px; margin-top: 0px;"
          ),
          shiny::HTML(
            '<h2 style="color: #000000; padding-top:2px;"
        >NHS Formula Health Inequality Impact Calculator</h2>'
          ),
          # shiny::h1("NHS Formula Health Inequality Impact Calculator"),
        ),
        shiny::HTML(
          '<h4 style="color: #5A5A5A;"
      >Installing one more dependency ...</h4>'
        ),
        shiny::HTML(
          '<h5 style="color: #5A5A5A;"
      >Please stay tuned, this app will load soon!</h5>'
        ),
        waiter::spin_loaders(
          id = 8,
          color = "#00c0ef",
          style = "
        height: 100px;
        width: 100px;
        "
        ),
        shiny::fluidRow(
          width = 1,
          source(
            file.path("elements", "ui", "footer.R"),
            local = TRUE
          )$value
        )
      )
    )

    webshot::install_phantomjs()

    waiter$update(
      html = shiny::div(
        style = "
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content:center;
            color: white;
            width: 700px;
            opacity: 1 !important;
          ",
        shiny::div(
          style = "
            display: flex;
            align-items: center;
            justify-content:center;
            color: white;
            width: 700px;
            opacity: 1 !important;
          ",
          shiny::img(
            src = "logo.svg",
            # src = "https://www.york.ac.uk/static/stable/img/logo.svg",
            alt = "University of York",
            height = "60",
            width = "100",
            style = "float: left; margin-right: 10px; margin-top: 0px;"
          ),
          shiny::HTML(
            '<h2 style="color: #000000; padding-top:2px;"
        >NHS Formula Health Inequality Impact Calculator</h2>'
          ),
          # shiny::h1("NHS Formula Health Inequality Impact Calculator"),
        ),
        shiny::HTML(
          '<h4 style="color: #5A5A5A;"
      >Finished downloading the required dependency!.</h4>'
        ),
        shiny::HTML(
          '<h5 style="color: #5A5A5A;"
      >Loading app now ...</h5>'
        ),
        waiter::spin_loaders(
          id = 8,
          color = "#00c0ef",
          style = "
        height: 100px;
        width: 100px;
        "
        ),
        shiny::fluidRow(
          width = 1,
          source(
            file.path("elements", "ui", "footer.R"),
            local = TRUE
          )$value
        )
      )
    )
  }

  # Workaround to sort out phantomjs issues  -------------------------------------

  if(Sys.getenv(x = "OPENSSL_CONF") == "") {
    Sys.setenv(
      "OPENSSL_CONF" = "/dev/null" # also works: /etc/ssl/
    )
  }


  # Define reactive objects ----------------------------------------------------

  inputs_rv <- shiny::reactiveValues()
  controls_rv <- shiny::reactiveValues()
  outputs_rv <- shiny::reactiveValues()

  # Assign reactive values -------------------------------------------------------

  inputs_rv[["outcome"]] <- "QALYs"
  inputs_rv[["entity"]] <- "CCG"
  inputs_rv[["spdf_2019"]] <- UnmetNeeds::spdf_2019
  inputs_rv[["IMD_population"]] <- IMD_data
  inputs_rv[["IMD_population_year"]] <- "2019"
  inputs_rv[["pcnt_change"]] <- 1
  inputs_rv[["equal_mortality_elasticity"]] <- UnmetNeeds::input_data_mQALE$
    `Equal mortality elasticity`

  # Prepare server elements ----------------------------------------------------

  # Define file path to server elements
  server_path <- file.path("elements", "server")

  # Create server elements (tables/charts/interface functionality)
  files_server <- list.files(
    path = server_path,
    pattern = "\\.[rR]$",
    full.names = TRUE,
    recursive = TRUE
  )

  for (file in files_server) {
    source(file, local = TRUE)
  }

  # Source the about modal  when 'about tool' button is pressed ----------------
  shiny::observeEvent(
    eventExpr = input$show_about,
    handlerExpr = {
      shiny::showModal(
        source(
          file.path("elements", "ui", "about.R"),
          local = TRUE
        )$value
      )
    }
  )

  Sys.sleep(time = 1.5)
  # Show main ui page now that the server is ready -----------------------------
  shinyjs::show(
    id = "main-ui"
  )
}
