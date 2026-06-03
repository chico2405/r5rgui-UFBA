#' @title Run the r5rgui Shiny Application
#'
#' @description This function launches a Shiny application that provides a graphical
#' user interface for the 'r5r' package, allowing for interactive transit routing.
#'
#' @param r5r_network A pre-built 'r5r' network object, or a named list of such objects for comparison.
#'   If a list is provided, the list names will be used as labels in the GUI. If a single object is provided, its variable name will be used as the label.
#' @param center A numeric vector of length 2, specifying the initial longitude and latitude for the map's center. If `NULL` (the default), the map will be centered on the bounding box of the `r5r_network`. If `{r5r}` is below version 2.4.0, calculating the bounding box may be slow.
#' @param zoom An integer specifying the initial zoom level of the map. If `NULL` (the default), the zoom level will be automatically calculated to fit the bounding box of the `r5r_network`. If `{r5r}` is below version 2.4.0, calculating the bounding box may be slow.
#' @param departure_date A Date object specifying the initial departure date for the trip. Defaults to the current system date.
#' @param mode A character vector specifying the initial transport modes. This is passed directly to the `mode` argument in [detailed_itineraries()][r5r::detailed_itineraries] (and other functions of [`r5r`][r5r::r5r]). Defaults to `c("WALK", "TRANSIT")`.
#' @param basemaps A named list of MapLibre style URLs (or style generator functions like [mapgl::carto_style()]). The names are used in the GUI selector. Defaults to a set of Carto styles (Voyager, Positron, Dark Matter). See [mapgl styling helpers](https://walker-data.com/mapgl/reference/index.html#styling-helpers) for options.
#' @param location_choices Optional data frame with the possible origins and destinations. When provided, the GUI uses dropdowns instead of free-form coordinate fields.
#' @param location_id_col Name of the column in `location_choices` that stores the internal identifier used in the dropdowns. Defaults to `"id"`.
#' @param location_label_col Name of the column in `location_choices` that stores the visible label used in the dropdowns. Defaults to `"name"`.
#' @param location_lon_col Name of the longitude column in `location_choices`. Defaults to `"lon"`.
#' @param location_lat_col Name of the latitude column in `location_choices`. Defaults to `"lat"`.
#' @param unit_polygons Optional `sf` object with the unit polygons to draw on the map. If provided, the polygons will be clickable and show hover labels.
#' @param unit_polygon_id_col Name of the polygon column that stores the internal unit identifier. Defaults to `"id"`.
#' @param unit_polygon_label_col Name of the polygon column that stores the visible unit name. Defaults to `"name"`.
#'
#' @return This function does not return a value; it launches a Shiny application.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # First, build the r5r network
#'   library(r5r)
#'
#'   # Note: This requires a valid r5r network.
#'   # Using the sample data included in the r5r package:
#'   data_path <- system.file("extdata/poa", package = "r5r")
#'   r5r_network <- setup_r5(data_path = data_path)
#'
#'   # Launch the application without specifying center and zoom
#'   # The map will be automatically centered and zoomed to the network's extent
#'   r5r_gui(r5r_network)
#'
#'   # Launch with a specific departure date with auto-zoom and center
#'   r5r_gui(r5r_network, departure_date = as.Date("2019-05-13"))
#'
#'   # Launch with specific transport modes
#'   r5r_gui(r5r_network, mode = c("WALK", "BUS"))
#'
#'   # Manually define map center and zoom
#'   map_center <- c(-51.22, -30.05)
#'   map_zoom <- 11
#'   r5r_gui(r5r_network, center = map_center, zoom = map_zoom)
#'
#'   # Compare two networks
#'   # Note: For this example, we use the same network object twice.
#'   # In a real scenario, you would use two different networks (e.g. current vs future).
#'   r5r_gui(list("Baseline" = r5r_network, "Scenario A" = r5r_network))
#' }
r5r_gui <- function(
  r5r_network,
  center = NULL,
  zoom = NULL,
  departure_date = Sys.Date(),
  mode = c("WALK", "TRANSIT"),
  basemaps = list(
    "Positron" = mapgl::carto_style("positron"),
    "Dark Matter" = mapgl::carto_style("dark-matter"),
    "Voyager" = mapgl::carto_style("voyager")
  ),
  location_choices = NULL,
  location_id_col = "id",
  location_label_col = "name",
  location_lon_col = "lon",
  location_lat_col = "lat",
  unit_polygons = NULL,
  unit_polygon_id_col = "osm_id",
  unit_polygon_label_col = "name"
) {
  if (!check_r5r_available()) {
    stop(
      "The 'r5r' package is required to run this demo. Please install it first.",
      call. = FALSE
    )
  }
  # Get the name of the r5r_network object as a string
  r5r_network_name <- deparse(substitute(r5r_network))

  # Normalization of basemaps (ensure they are URL strings)
  # Some might be passed as functions without being evaluated
  basemaps <- lapply(basemaps, function(x) {
    if (is.character(x)) {
      return(x)
    }
    # If it's a list/json from mapgl, it should be fine as well,
    # but for style selector, strings are safer.
    return(x)
  })

  # Normalize r5r_network to a list
  if (
    inherits(r5r_network, "list") && !inherits(r5r_network, "TransportNetwork")
  ) {
    # If it's a list (and not a single TransportNetwork object which might inherit from list?)
    # r5r objects are typically environment-based or java pointers, but let's be safe.
    # Usually r5r objects class is "TransportNetwork".

    # Ensure names
    if (is.null(names(r5r_network))) {
      names(r5r_network) <- paste("Graph", seq_along(r5r_network))
    }
    # Fill missing names
    unnamed <- names(r5r_network) == ""
    if (any(unnamed)) {
      names(r5r_network)[unnamed] <- paste("Graph", seq_along(r5r_network))[
        unnamed
      ]
    }
    primary_network <- r5r_network[[1]]
  } else {
    primary_network <- r5r_network
    # Use the object name passed by the user
    r5r_network <- stats::setNames(list(r5r_network), r5r_network_name)
  }

  normalized_location_choices <- NULL
  if (!is.null(location_choices)) {
    if (!inherits(location_choices, "data.frame")) {
      stop(
        "'location_choices' must be a data frame with label and coordinate columns.",
        call. = FALSE
      )
    }

    required_location_cols <- c(
      location_id_col,
      location_label_col,
      location_lon_col,
      location_lat_col
    )
    missing_location_cols <- setdiff(required_location_cols, names(location_choices))
    if (length(missing_location_cols) > 0) {
      stop(
        sprintf(
          "'location_choices' is missing the following columns: %s",
          paste(missing_location_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    normalized_location_choices <- data.frame(
      id = as.character(location_choices[[location_id_col]]),
      label = as.character(location_choices[[location_label_col]]),
      lon = as.numeric(location_choices[[location_lon_col]]),
      lat = as.numeric(location_choices[[location_lat_col]]),
      stringsAsFactors = FALSE
    )
  }

  normalized_unit_polygons <- unit_polygons
  if (!is.null(unit_polygons)) {
    if (!inherits(unit_polygons, "sf")) {
      stop(
        "'unit_polygons' must be an sf object when provided.",
        call. = FALSE
      )
    }

    missing_polygon_cols <- setdiff(
      c(unit_polygon_id_col, unit_polygon_label_col),
      names(unit_polygons)
    )
    if (length(missing_polygon_cols) > 0) {
      stop(
        sprintf(
          "'unit_polygons' is missing the following columns: %s",
          paste(missing_polygon_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    normalized_unit_polygons <- unit_polygons
    names(normalized_unit_polygons)[
      names(normalized_unit_polygons) == unit_polygon_id_col
    ] <- "unit_id"
    names(normalized_unit_polygons)[
      names(normalized_unit_polygons) == unit_polygon_label_col
    ] <- "unit_name"
  }

  # if center or zoom are not provided, calculate them from the network bbox
  if (is.null(center) || is.null(zoom)) {
    if (utils::packageVersion("r5r") >= "2.3.0.999") {
      street_network_bbox_fun <- get("street_network_bbox", asNamespace("r5r"))
      bbox <- street_network_bbox_fun(primary_network, output = "vector")
    } else {
      message(
        "Calculating network bounding box with a legacy method. This is slow."
      )
      message(
        "Please update 'r5r' to version 2.4.0 or newer for better performance."
      )
      message(
        "Alternatively, provide 'center' and 'zoom' arguments manually to skip this calculation."
      )
      bbox <- sf::st_bbox(r5r::street_network_to_sf(primary_network)$edges)
    }
    center <- c(
      (bbox["xmin"] + bbox["xmax"]) / 2,
      (bbox["ymin"] + bbox["ymax"]) / 2
    )
    center <- unname(center)

    lon_range <- bbox["xmax"] - bbox["xmin"]
    lat_range <- bbox["ymax"] - bbox["ymin"]
    max_range <- max(lon_range, lat_range)
    zoom <- floor(log2(360 / max_range))
    if (zoom > 18) zoom <- 18
  }

  # Add resource path to serve logo from the man/figures directory
  assets_path <- system.file("assets", package = "r5rgui")
  shiny::addResourcePath("r5rgui_assets", assets_path)

  # Load the UI from its file
  ui_path <- system.file("shiny_app", "ui.R", package = "r5rgui")
  ui <- source(ui_path, local = TRUE)$value

  # Load the server factory from its file
  server_factory_path <- system.file(
    "shiny_app",
    "server.R",
    package = "r5rgui"
  )
  server_factory <- source(server_factory_path, local = TRUE)$value

  # Create a list of arguments to pass to the server
  app_args <- list(
    r5r_network = r5r_network, # This is now always a list
    r5r_network_name = r5r_network_name,
    center = center,
    zoom = zoom,
    departure_date = departure_date,
    mode = mode,
    basemaps = basemaps,
    location_choices = normalized_location_choices,
    unit_polygons = normalized_unit_polygons,
    unit_polygon_id_col = "unit_id",
    unit_polygon_label_col = "unit_name"
  )

  # Use the factory to create the final server function
  server <- server_factory(app_args)

  # Launch the application using shinyApp
  shiny::shinyApp(ui = ui, server = server)
}
