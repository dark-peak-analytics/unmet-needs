################################################################################
#
# Script Name:        get_map_data.R
# Script Description: Defines the function get_map_data(). This function connects
#                     to the Open Geography Portal provided by the ONS and
#                     performs an API query to get a boundary data shape file
#                     for a user specified layer.
#
################################################################################

#' @title Get Map API Query for Open Geography Portal
#'
#' @description Performs an API query for data from the Open Geography Portal
#' (OGP).
#' @details Connects to the Open Geography Portal (OGP), an application
#' programming interface (API) operated by or behalf of the Office of National
#' Statistics (ONA), and performs an API query to get a boundary data shape file
#' for healthcare organisations, Integrated care systems/boards (ICSs)/(ICBs)
#' or NHS Clinical Commissioning Groups (CCGs), (with codes and names). If the
#' API query is not successful, this function will stop and return an error.
#' The API call was developed by interrogating the API explorer provided by
#' ONS's OGP.
#' https://geoportal.statistics.gov.uk/datasets/integrated-care-boards-july-2022-en-bfc-3/api
#'
#' @param healthcare_entity_ A string scalar, either "ccg" or "ics" representing
#' the healthcare organisation for which the map data is to be queried.
#' @param map_year_ A numeric scalar, specifying the year for which map data is
#' to be queried. Setting this parameter to any value but `NULL` will lead the
#' function to ignore the parameter `healthcare_entity_`. Should users request
#' map data before or after the currently provided data, then the function will
#' query the earliest or latest available data, respectively.
#' @param outFields_ A string vector specifying the names of the columns the user
#' wants the API to return. For example "ICB22CD", "ICB22NM", "LONG", "LAT",
#' "Shape__Area", "Shape__Length". If all contents were required, then users
#' should set this parameter to `"*"`.
#' @param simplify_shape_ A logical scalar, default is TRUE, indicating whether
#' to reduce the size of the shape file using \link[rmapshaper]{ms_simplify}.
#'
#' @return A shapefile.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data_2018 <- get_map_data(map_year_ = 2018)
#' data_2023 <- get_map_data(healthcare_entity_ = "ics")
#' }
#'
get_map_data <- function(
    healthcare_entity_ = "ics",
    map_year_ = 2023,
    outFields_ = "*",
    simplify_shape_ = TRUE) {
  # Sanity checks:
  ## Assert inputs:
  assertthat::assert_that(
    assertthat::is.string(healthcare_entity_),
    tolower(healthcare_entity_) %in% c("ics", "ccg"),
    msg = paste(
      "The parameter 'healthcare_entity_' only accepts 'ics' or 'ccg'"
    )
  )
  assertthat::assert_that(
    assertthat::is.count(map_year_),
    msg = paste(
      "The parameter 'map_year_' is a scalar positive integer.",
      "Please enter a valid year, for example, 2023"
    )
  )
  ## Override map_year_ to earliest/latest value if needed:
  query_year <- if(!is.null(map_year_)) {
    if(map_year_ < min(ons_ccg_ics_maps_data$map_year)) {
      rlang::inform(
        message = paste(
          "Data from the year",
          map_year_,
          "is not currently available via this function. Querying data from the",
          "earliest possible year:",
          min(ons_ccg_ics_maps_data$map_year)
        )
      )
      min(ons_ccg_ics_maps_data$map_year)
    } else if (map_year_ > max(ons_ccg_ics_maps_data$map_year)) {
      rlang::inform(
        message = paste(
          "Data from the year",
          map_year_,
          "is not currently available via this function. Querying data from the",
          "latest possible year:",
          max(ons_ccg_ics_maps_data$map_year)
        )
      )
      max(ons_ccg_ics_maps_data$map_year)
    }  else {
      map_year_
    }
  } else {
    map_year_
  }

  # Grab the correct ONS object name:
  map_object <- if(!is.null(query_year)) {
    ons_ccg_ics_maps_data |>
      subset(
        subset = map_year == query_year,
        select = map_object_name
      ) |>
      _[[1]][1]
  } else {
    ons_ccg_ics_maps_data[
      order(
        ons_ccg_ics_maps_data$map_year,
        decreasing = TRUE
      ),
    ] |>
      subset(
        x = _,
        subset = healthcare_entity %in% tolower(healthcare_entity_),
        select = map_object_name
      ) |>
      _[[1]][1]
  }

  # Get the data from the OGP:
  ## Put together the API query path:
  path <- paste(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services",
    map_object,
    "FeatureServer",
    "0",
    "query?",
    sep = "/"
  )
  ## Add query details:
  query_list <- list(
    where = "1=1",
    outFields = outFields_,
    outSR = "4326",
    f = "json"
  )
  ## Make the API query/request:
  request <- httr::GET(
    url = path,
    query = query_list,
    httr::timeout(60)
  )
  ## Check that the query was successful:
  if (request$status_code != 200L) {
    rlang::abort("Cannot access Open Geography Portal!")
  }
  ## Convert the response to text:
  response <- httr::content(
    x = request,
    as = "text",
    encoding = "UTF-8"
  )

  # Convert response to map data:
  projection <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84")
  map_data <- response |>
    sf::read_sf() |>
    sf::as_Spatial() |>
    sp::spTransform(projection)

  if(all(simplify_shape_,"SpatialPolygonsDataFrame" %in% class(map_data))) {
    rlang::inform(
      message = paste(
        "Completed downloading,
        Simplifying Shape data..."
      )
    )

    map_data <- map_data |>
      rmapshaper::ms_simplify()
  } else {
    map_data
  }

  return(map_data)
}
