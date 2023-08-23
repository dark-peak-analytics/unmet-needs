################################################################################
#
# Script Name:        global.R
#
################################################################################

# Set style options ------------------------------------------------------------

options(shiny.usecairo = TRUE)

# Output loading message to console --------------------------------------------

rlang::inform("Loading UnmetNeeds app")

# Load and prepare supporting data ---------------------------------------------

IMD_data <- data.table::setorderv(
  x = UnmetNeeds::CCG_IMD_population_2019,
  cols = "CCG19NM",
  order = 1,
  na.last = TRUE
)

for (colname in names(IMD_data)) {
  data.table::set(
    x = IMD_data,
    i = which(is.na(IMD_data[[colname]])),
    j = colname,
    value = 0
  )
}

# Cleanup on exit --------------------------------------------------------------

onStop(function() {
  # Remove objects added to the global Shiny variables
  rm(
    IMD_data, colname,
    envir = .GlobalEnv
  )
})
