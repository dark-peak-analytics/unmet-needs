# Install old rsconnect to solve existing app error:
remotes::install_version("rsconnect", "0.8.29")

# Install private package:----
# devtools::install_github(
#   repo = paste0(
#     'W-Mohammed/ShinyNeeds@',
#     Sys.getenv('BRANCH_NAME')
#   ),
#   auth_token = Sys.getenv('PRIVATE_REPO_TOKEN')
# )
# Install public package:----
devtools::install_github(
  repo = paste0(
    'dark-peak-analytics/unmet-needs@',
    Sys.getenv('BRANCH_NAME')
  )
)

# Load two libraries:----
library(UnmetNeeds)
library(rsconnect)

# Authenticate:----
rsconnect::setAccountInfo(
  name = UnmetNeeds::error_on_missing_name("SHINY_ACC_NAME"),
  token = UnmetNeeds::error_on_missing_name("SHINY_ACC_TOKEN"),
  secret = UnmetNeeds::error_on_missing_name("SHINY_ACC_SECRET")
)

# Deploy the application:----
rsconnect::deployApp(
  appDir = file.path(
    here::here(),
    "app"
  ),
  appFiles = NULL,
  #c("app.R" #, you can specify which files to deploy,
  #or keep this NULL to deploy everything)

  appName = paste0(
    UnmetNeeds::error_on_missing_name("SHINY_APP_NAME"),
    "_",
    UnmetNeeds::error_on_missing_name("BRANCH_NAME")),
  appTitle = "Unmet Needs Tool",
  forceUpdate = TRUE
)
