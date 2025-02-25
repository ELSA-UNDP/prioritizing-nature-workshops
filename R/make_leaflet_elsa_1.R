#' Create an Interactive Leaflet Map with ELSA Prioritization
#'
#' This function generates an interactive `leaflet` map that visualizes spatial prioritization results
#' from the ELSA (Environmental and Landscape Sustainability Assessment) tool. It can handle multiple themes
#' or a single theme, and it visualizes the relative contribution of each planning unit to the overall prioritization.
#' The map includes base layers, raster layers for prioritization, heatmaps, and optional protected and restoration areas.
#'
#' @param lockin_scenario Character. Specifies the lock-in scenario. If "run_all_lockin", separate prioritizations 
#'        are displayed for each theme; otherwise, a single prioritization applies.
#' @param elsa_hm A `SpatRaster` or list of `SpatRaster` objects. Contains normalized input feature data
#'        for the entire country, representing the relative contribution of each planning unit to the
#'        maximum possible net benefit.
#' @param heatm_lst A list of `SpatRaster` objects. Contains normalized input feature data per theme for the country,
#'        indicating the relative contribution of each planning unit to the maximum possible net benefit for each theme.
#' @param rast A `SpatRaster`. The output of the spatial prioritization, representing the spatial distribution of priority areas.
#' @param theme_tbl A `tbl_df`. Contains information on the themes, including the names of the data in each theme
#'        and the types of data layers used.
#'
#' @return A `leaflet` map visualizing the ELSA prioritization, including the prioritization results, heatmaps, 
#'         and optional layers such as protected and restoration areas.
#' @export
#' @examples
#' # Example usage:
#' # map <- fun_leaflet_elsa_1(multi_theme = TRUE, elsa_hm = my_elsa_hm, heatm_lst = my_heatmaps, rast = my_rast, theme_tbl = my_theme_tbl)
#' # map
make_leaflet_elsa_1 <- function(multi_theme = NULL,
                               elsa_hm = NULL,
                               heatm_lst = NULL,
                               rast = NULL,
                               theme_tbl = NULL) {
  
  # Initialize the default Leaflet basemap
  map <- get_leaflet_basemap()
  
  # Add ELSA prioritization layers based on the lock-in scenario
  if (multi_theme) {
    # For multiple themes, add a separate raster for each theme
    theme_group <- c(
      glue("ELSA {get_translation('action')}"),
      glue("{theme_tbl$theme} {get_translation('action')}")
    )
    
    # Loop through each layer in the raster stack and add to map
    for (n in 1:nlyr(rast)) {
      # Get color and category information for each layer
      raster_attributes <- as_tibble(terra::cats(rast[[n]])[[1]]) |> 
        filter(value %in% unique(terra::values(rast[[n]])))
      
      # Add raster image and legend for each scenario to map
      map <- map |>
        addRasterImage(
          rast[[n]],
          colors = raster_attributes$colour,
          opacity = 0.8,
          group = theme_group[n],
          options = tileOptions(zIndex = 1000)
        ) |>
        addLegend(
          "topright",
          colors = filter(raster_attributes, category != "Do_Nothing")$colour,
          values = filter(raster_attributes, category != "Do_Nothing")$category,
          title = theme_group[n],
          labels = filter(raster_attributes, category != "Do_Nothing")$label,
          group = theme_group[n]
        )
    }
  } else {
    # Handle single prioritization case
    raster_attributes <- as_tibble(terra::cats(rast)[[1]]) |> 
      filter(value %in% unique(terra::values(rast)))
    
    # Add single raster and legend to map
    map <- map |>
      addRasterImage(
        rast,
        colors = raster_attributes$colour,
        opacity = 0.8,
        group = glue(
          "ELSA {get_translation('action')}"
        ),
        options = tileOptions(zIndex = 1000)
      ) |>
      addLegend(
        "topright",
        colors = filter(raster_attributes, category != "Do_Nothing")$colour,
        values = filter(raster_attributes, category != "Do_Nothing")$category,
        title = glue(
          "ELSA {get_translation('action')}"
        ),
        labels = filter(raster_attributes, category != "Do_Nothing")$label
      )
  }
  
  # Add ELSA heatmap layer based on data availability
  if (terra::minmax(elsa_hm)[2] == 0) { # Case for single input layer heatmap
    map <- map |>
      addRasterImage(
        elsa_hm,
        colors = pal.hm[1],
        opacity = 0.8,
        group = "ELSA HM",
        options = tileOptions(zIndex = 1000)
      ) |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM",
        group = "ELSA HM"
      )
  } else { # Case for multiple input layers
    map <- map |>
      addRasterImage(
        elsa_hm,
        colors = pal.hm,
        opacity = 0.8,
        group = "ELSA HM",
        options = tileOptions(zIndex = 1000)
      ) |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM",
        group = "ELSA HM"
      )
  }
  
  # Loop over each theme and add corresponding heatmap layers
  for (ii in 1:nrow(theme_tbl)) {
    name <- glue("{theme_tbl$theme[ii]} HM")
    if (terra::minmax(heatm_lst[[ii]])[2] == 0) { # Case for single input layer
      map <- map |>
        addRasterImage(
          heatm_lst[[ii]],
          colors = pal.hm[1],
          opacity = 0.8,
          group = name,
          options = tileOptions(zIndex = 1000)
        ) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm[1], domain = NULL),
          values = terra::values(heatm[[ii]]),
          title = name,
          group = name
        )
    } else { # Case for multiple input layers
      map <- map |>
        addRasterImage(
          heatm_lst[[ii]],
          colors = pal.hm,
          opacity = 0.8,
          group = name,
          options = tileOptions(zIndex = 1000)
        ) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm, domain = NULL),
          values = terra::values(heatm_lst[[ii]]),
          title = name,
          group = name
        )
    }
  }
  
  # Add protected areas layer to the map
  map <- map |>
    addRasterImage(
      PA,
      colors = "#005a32",
      opacity = 0.8,
      group = get_translation("protected_areas"),
      options = tileOptions(zIndex = 800)
    ) |>
    addLegend(
      "topright",
      colors = "#005a32",
      labels = get_translation("protected_areas"),
      group = get_translation("protected_areas")
    )
  
  oecm_true <- !is.null(OECMS) && !is.na(terra::global(OECMS, sum, na.rm = TRUE)) && 
    terra::global(OECMS, sum, na.rm = TRUE) > 0
  
  # Add potential OECM areas layer if it exists
  if (oecm_true) {
    map <- map |>
      addRasterImage(
        OECMS,
        colors = "#8e06ff",
        opacity = 0.8,
        group = get_translation("oecm_areas"),
        options = tileOptions(zIndex = 800)
      ) |>
      addLegend(
        "topright",
        colors = "#8e06ff",
        labels = get_translation("oecm_areas"),
        group = get_translation("oecm_areas")
      )
  } 
  
  rest_true <- !is.null(Rest) && !is.na(terra::global(Rest, sum, na.rm = TRUE)) && 
    terra::global(Rest, sum, na.rm = TRUE) > 0
  
  # Add restoration areas layer if enabled
  if (rest_true) {
    map <- map |>
      addRasterImage(
        Rest,
        colors = "#fcae16",
        opacity = 0.8,
        group = get_translation("restore_areas"),
        options = tileOptions(zIndex = 1000)
      ) |>
      addLegend(
        "topright",
        colors = "#fcae16",
        labels = get_translation("restore_areas"),
        group = get_translation("restore_areas")
      )
  } 
  
  # Define overlay groups for layers control based on lock-in scenario
  overlay_groups <- if (multi_theme) {
    c(
      glue("ELSA {get_translation('action')}"),
      glue("{theme_tbl$theme} {get_translation('action')}"),
      "ELSA HM",
      glue("{theme_tbl$theme} HM"),
      get_translation("protected_areas")
    )
  } else {
    c(
      glue("ELSA {get_translation('action')}"),
      "ELSA HM",
      glue("{theme_tbl$theme} HM"),
      get_translation("protected_areas")
    )
  }
  
  # Add potential OECMS areas group to overlay groups
  if (oecm_true) {
    overlay_groups <- c(
      overlay_groups,
      get_translation("oecm_areas")
    )
  }
  
  # Add optional restoration areas group to overlay groups
  if (rest_true) {
    overlay_groups <- c(
      overlay_groups,
      get_translation("restore_areas")
    )
  } 
  
  # Configure layer control settings
  map <- map |>
    addLayersControl(
      overlayGroups = overlay_groups,
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) |>
    hideGroup(c(
      overlay_groups[-1],
      "ELSA HM",
      glue("{theme_tbl$theme} HM"),
      get_translation("protected_areas")
    ))
  
  # Optionally hide OECMS restoration group if they exist
  if (oecm_true) {
    map <- map |>
      hideGroup(get_translation("oecm_areas"))
  } 
  
  # Hide restoration areas group if enabled
  if (rest_true) {
    map <- map |>
      hideGroup(get_translation("restore_areas"))
  } 
  
  return(map)
}
