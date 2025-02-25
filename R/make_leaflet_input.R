#' Create an Interactive Leaflet Map with Input Layers
#'
#' This function generates an interactive `leaflet` map, adding raster input layers and 
#' associated legends. The input layers are downsampled for faster rendering and organized 
#' into groups for easy toggling in the map's control panel. It also includes predefined
#' zone layers (e.g., protect, restore, manage, green) and provides the ability to customize
#' zones like the "green zone" based on the `urb_green` condition.
#'
#' @param layers A list of `SpatRaster` objects representing the layers to be displayed on the map.
#' @param labels A character vector containing the names of each input layer for the map legend and group control.
#'
#' @return A `leaflet` map object containing the input layers, zone layers, legends, and controls.
#' @export
#'
#' @examples
#' # Example usage:
#' # map <- fun_leaflet_input(layers = list(raster1, raster2), labels = c("Layer 1", "Layer 2"))
#' # map
make_leaflet_input <- function(layers, labels) {
  
  # Initialize the leaflet map with a default basemap
  map <- get_leaflet_basemap()
  
  # Add input layers to the map with color palettes and legends
  for (ii in 1:length(labels)) {
    name <- labels[ii]
    
    # Downsample the raster layer for faster rendering using regular sampling
    map <- map |>
      addRasterImage(
        spatSample(layers[[ii]], 100000, method = "regular", as.raster = TRUE), # Downsample to 100k points
        colors = pal.in,  # Predefined color palette for input layers
        group = name,     # Assign the layer to a named group
        opacity = 0.8,    # Set opacity for the layer
        options = tileOptions(zIndex = 800)  # Ensure the layer is always rendered on top
      ) |>
      addLegend(
        position = "topright",
        pal = colorNumeric(pal.in, na.color = NA, domain = NULL), # Color palette for the legend
        values = values(layers[[ii]]),  # Values for the legend
        title = name,  # Set the title for the legend
        group = name,  # Assign the legend to the same group as the layer
        opacity = 0.8
      )
  }
  
  # Add base zone layers (protect, restore, manage) to the map
  map <- map |>
    addRasterImage(
      pu1[[1]],  # Protected zone raster
      group = get_translation("protect_zone"),  # Group for the protected zone
      opacity = 0.8,
      colors = pal.zone,  # Predefined color palette for zone layers
      options = tileOptions(zIndex = 800)
    ) |>
    addLegend(
      position = "topright",
      colors = pal.zone,  # Color for the protected zone legend
      values = values(pu1[[1]]),
      labels = get_translation("protect_zone"),  # Legend label for the protected zone
      group = get_translation("protect_zone"),
      opacity = 0.8
    ) |>
    
    # Add restore zone
    addRasterImage(
      pu1[[2]],  # Restore zone raster
      group = get_translation("restore_zone"),
      opacity = 0.8,
      colors = pal.zone,
      options = tileOptions(zIndex = 800)
    ) |>
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[2]]),
      labels = get_translation("restore_zone"),
      group = get_translation("restore_zone"),
      opacity = 0.8
    ) |>
    
    # Add manage zone
    addRasterImage(
      pu1[[3]],  # Manage zone raster
      group = get_translation("manage_zone"),
      opacity = 0.8,
      colors = pal.zone,
      options = tileOptions(zIndex = 800)
    ) |>
    addLegend(
      position = "topright",
      colors = pal.zone,
      values = values(pu1[[3]]),
      labels = get_translation("manage_zone"),
      group = get_translation("manage_zone"),
      opacity = 0.8
    )
  
  # Optionally add the green zone if `urb_green` is TRUE
  if (exists("urb_green") && urb_green) {
    map <- map |>
      addRasterImage(
        pu1[[4]],  # Green zone raster
        group = get_translation("green_zone"),
        opacity = 0.8,
        colors = pal.zone,
        options = tileOptions(zIndex = 800)
      ) |>
      addLegend(
        position = "topright",
        colors = pal.zone,
        values = values(pu1[[4]]),
        labels = get_translation("green_zone"),
        group = get_translation("green_zone"),
        opacity = 0.8
      )
  } else NULL
  
  # Add layer control for toggling between layers
  overlay_groups <- c(
    labels,
    get_translation("protect_zone"),
    get_translation("restore_zone"),
    get_translation("manage_zone"),
    if (exists("urb_green") && urb_green) get_translation("green_zone") else NULL
  )
  
  map <- map |>
    addLayersControl(
      baseGroups = c("UNBL", "UNBL Dark", "Satellite"),  # Basemap options
      overlayGroups = overlay_groups,  # Allow toggling of all input and zone layers
      options = layersControlOptions(collapsed = TRUE),
      position = "topleft"
    ) |>
    
    # Hide some layers by default
    hideGroup(labels[-1]) |>
    hideGroup(get_translation("protect_zone")) |>
    hideGroup(get_translation("restore_zone")) |>
    hideGroup(get_translation("manage_zone"))
  
  # Optionally hide the green zone if `urb_green` is TRUE
  if (exists("urb_green") && urb_green) {
    map <- map |>
      hideGroup(get_translation("green_zone"))
  } else NULL

  return(map)
}
