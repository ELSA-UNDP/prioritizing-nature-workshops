#' Generate an interactive Leaflet map for ELSA visualizations
#'
#' This function creates a Leaflet map for visualizing ELSA (Environmental and Land Sensitivity Analysis) heatmaps.
#' It adds base layers, ELSA HM, optional theme-specific heatmaps, and protected/restoration areas with legends.
#'
#' @param elsa_hm RasterLayer, ELSA heatmap layer to be visualized
#' @param heatm_lst List of RasterLayers, heatmap layers for additional themes
#' @param theme_tbl Data frame, contains theme information for heatmap layers
#'
#' @return A Leaflet map object with ELSA and theme-specific layers
#' @export
#'
#' @examples
make_leaflet_elsa_0 <- function(elsa_hm = NULL,
                               heatm_lst = NULL,
                               theme_tbl = NULL) {
  # Initialize the Leaflet basemap
  map <- get_leaflet_basemap()
  
  # Add the main ELSA HM layer with appropriate legend and styling
  if (terra::minmax(elsa_hm)[2] == 0) {  # Check if layer max value is 0
    map <- map |>
      addRasterImage(
        elsa_hm,
        colors = pal.hm[1],  # Use single color if the layer is empty or has low variation
        opacity = 0.8,
        group = "ELSA HM",
        options = tileOptions(zIndex = 800)
      ) |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM"
      )
  } else {
    map <- map |>
      addRasterImage(
        elsa_hm,
        colors = pal.hm,
        opacity = 0.8,
        group = "ELSA HM",
        options = tileOptions(zIndex = 800)
      ) |>
      addLegend(
        "topright",
        pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
        values = terra::values(elsa_hm),
        title = "ELSA HM"
      )
  }
  
  # Loop through each theme in theme_tbl and add heatmap layers
  for (ii in 1:nrow(theme_tbl)) {
    name <- glue("{theme_tbl$theme[ii]} HM")
    if (terra::minmax(heatm_lst[[ii]])[2] == 0) {  # Check if layer max value is 0
      map <- map |>
        addRasterImage(
          heatm_lst[[ii]],
          colors = pal.hm[1],
          opacity = 0.8,
          group = name,
          options = tileOptions(zIndex = 800)
        ) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm[1], na.color = NA, domain = NULL),
          values = terra::values(heatm_lst[[ii]]),
          title = name,
          group = name
        )
    } else {
      map <- map |>
        addRasterImage(
          heatm_lst[[ii]],
          colors = pal.hm,
          opacity = 0.8,
          group = name,
          options = tileOptions(zIndex = 800)
        ) |>
        addLegend(
          "topright",
          pal = colorNumeric(pal.hm, na.color = NA, domain = NULL),
          values = terra::values(heatm_lst[[ii]]),
          title = name,
          group = name,
          bins = c(0, 0.25, 0.5, 0.75, 1)
        )
    }
  }
  
  # Add the protected areas layer
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
  
  # Add restoration areas layer if restoration lock is enabled
  if (rest_true) {
    map <- map |>
      addRasterImage(
        Rest,
        colors = "#fcae16",
        opacity = 0.8,
        group = get_translation("restore_areas"),
        options = tileOptions(zIndex = 800)
      ) |>
      addLegend(
        "topright",
        colors = "#fcae16",
        labels = get_translation("restore_areas"),
        group = get_translation("restore_areas")
      )
  } 
  
  # Define overlay groups for layer control panel and add the control
  overlay_groups <- c(
    "ELSA HM",
    glue("{theme_tbl$theme} HM"),
    get_translation("protected_areas")
  )
  
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
  
  # Finalize map with layer control options
  map <- map |>
    addLayersControl(
      overlayGroups = overlay_groups,
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) |>
    hideGroup(c(
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
  
  # Return the completed Leaflet map
  return(map)
}
