################################################################################
#
# Script Name:        create_map_plot.R
# Script Description: Defines the function create_map_plot(). This function
#                     creates a map using Leaflet displaying the average values
#                     given by geographical areas representing areas served by
#                     certain organisation.
#
################################################################################

#' @title Create a landmap plot
#'
#' @description Creates a landmap plot of average values.
#' @details Creates a landmap plot using Leaflet displaying the average values
#' given by local authority in a dataframe
#'
#' @param sp_df_ A SpatialPointsDataFrame defining the boundaries within
#' the land map.
#' @param value_df_ A dataframe containing average values and scaled average
#' values (between 0 and 1) for each area within the land map.
#' @param var_nm_ A string specifying the name of the variable being displayed.
#' @param value_year_ (Optional) A numeric specifying the year of the variable
#' being displayed.
#' @param value_digits_ A numeric scalar specifying the number of digits to
#' which the outcome in the data table or dataframe `value_df_` is to be rounds.
#'
#' @return A Leaflet object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' map_data = sp_df_2023 |>
#'   slot("data") |>
#'   subset(x = _, select = ICB23CD) |>
#'   cbind(
#'   "Value" = runif(nrow(sp_df_2023@data))
#'   )
#'
#' create_map_plot(sp_df_ = sp_df_2023,
#'                 value_df_ = map_data,
#'                 var_nm_ = "QALYs",
#'                 value_year_ = 2023)
#'
#' map_data2 = sp_df_2018 |>
#'   slot("data") |>
#'   subset(x = _, select = ccg18cd) |>
#'   cbind(
#'   "Value" = runif(nrow(sp_df_2018@data))
#'   )
#'
#' create_map_plot(sp_df_ = sp_df_2018,
#'                 value_df_ = map_data2,
#'                 var_nm_ = "QALYs",
#'                 value_year_ = 2018)
#' }
#'
create_map_plot <- function(
    sp_df_,
    value_df_,
    var_nm_,
    value_year_ = NA,
    value_digits_ = 0) {
  # Identify geographical level to use -----------------------------------------

  data_cols <- colnames(methods::slot(sp_df_, "data"))

  geo_lvl <- data_cols |>
    grep(
      x = _,
      pattern = "cd",
      ignore.case = TRUE,
      value = TRUE
    )

  geo_lvl_nm <- data_cols |>
    grep(
      x = _,
      pattern = "nm",
      ignore.case = TRUE,
      value = TRUE
    )

  if(!is.null(value_df_[[geo_lvl_nm]])) {
    rlang::inform(
      message = paste(
        "Droping the",
        geo_lvl_nm,
        "column from map data to avoid errors! This should not affect the",
        "results."
      )
    )
    value_df_[[geo_lvl_nm]] <- NULL
  }

  # Add visualisation data to empty landmap ------------------------------------

  ## color scale calculaion
  map_color <- leaflet::colorNumeric(
    palette = "viridis",
    domain = c(min(value_df_$Value), max(value_df_$Value))
  )

  value_df_ <- value_df_ |>
    cbind(
      "FillColour" = map_color(value_df_$Value), #"#FF0000" "#7F7F7F"
      "FillOpacity" = 1 # value_df_$Value / max(value_df_$Value) # 0.5
    )

  ## merge outcome and maping data
  landmap_data <- sp_df_
  methods::slot(landmap_data, "data") <- landmap_data |>
    methods::slot("data") |>
    merge(
      x = _,
      y = value_df_,
      by = geo_lvl
    )

  # Create labels --------------------------------------------------------------
  if (is.na(value_year_)) {
    landmap_labels <- sprintf(
      paste(
        "<strong>%s</strong>",
        "%s: %s",
        sep = "<br/>"
      ),
      methods::slot(landmap_data, "data") |>
        subset(select = {{geo_lvl_nm}}) |>
        _[[1]],
      as.character(var_nm_),
      methods::slot(landmap_data, "data") |>
        subset(select = "Value") |>
        _[[1]] |>
        round(
          digits = value_digits_
        ) |>
        format(
          big.mark   = ","
        )
    ) |>
      lapply(shiny::HTML)
  } else {
    landmap_labels <- sprintf(
      paste(
        "<strong>%s</strong>",
        "%s (%s): %s",
        sep = "<br/>"
      ),
      methods::slot(landmap_data, "data") |>
        subset(select = {{geo_lvl_nm}}) |>
        _[[1]],
      as.character(var_nm_),
      as.character(value_year_),
      methods::slot(landmap_data, "data") |>
        subset(select = "Value") |>
        _[[1]] |>
        round(
          digits = value_digits_
        ) |>
        format(
          big.mark   = ","
        )
    ) |>
      lapply(shiny::HTML)
  }

  # Create landmap visualisation -----------------------------------------------

  landmap_plot <- leaflet::leaflet(
    data = landmap_data,
    options = leaflet::leafletOptions(
      zoomControl = FALSE,
      # dragging = FALSE,
      minZoom = 6,
      maxZoom = 9
    )
  ) |>
    leaflet::addPolygons(
      # Add coloring for local authorities
      fillColor = landmap_data$FillColour,
      fillOpacity = landmap_data$FillOpacity,
      # Add label text
      label = landmap_labels,
      # Add coloring for boundaries
      color = "#1978BB",
      weight = 1,
      opacity = 1,
      # Add coloring for selected LTLA boundary
      highlightOptions = leaflet::highlightOptions(
        color = "#FAB819",
        weight = 2,
        bringToFront = TRUE
      ),
      # Add label formatting
      labelOptions = leaflet::labelOptions(
        style = list(
          "font-family" = "Arial, sans-serif",
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
        textsize = "12pt",
        direction = "auto"
      )
    ) |>
    # Set default map view
    leaflet::setView(
      lng = -1.8, #-1.464854,
      lat = 52.96, #52.56193,
      zoom = 6
    ) |>
    leaflet.extras::setMapWidgetStyle(
      list(
        background = "white"
      )
    ) |>
    leaflet::addLegend(
      position = "topright",
      pal = map_color,
      values = ~ Value,
      title = var_nm_,
      labFormat = leaflet::labelFormat(
        digits = value_digits_
      ),
      opacity = 1
    )
    # |>
    # leaflet.extras::suspendScroll(
    #   hoverToWake = FALSE #TRUE
    # )

  # Return landmap plotting data -----------------------------------------------

  return(landmap_plot)
}
