#' Create a Basic Leaflet Map with Multiple Basemap Layers
#'
#' This function creates a basic `leaflet` map with several basemap layers that can be used as the foundation 
#' for building more complex maps. It includes a satellite imagery layer from Esri, as well as two different 
#' styles from the UN Biodiversity Lab (UNBL), one of which is a dark theme.
#'
#' @return A `leaflet` map object with preconfigured basemap layers.
#'         This map can be further customized by adding additional layers, markers, or shapes.
#' @export
#'
#' @examples
#' # Example of how to create and view the basic leaflet map
#' map <- get_leaflet_basemap()
#' map  # display the map
get_leaflet_basemap <- function() {
  
  # Initialize the leaflet map
  map <- leaflet() |>
    
    # Add the Esri World Imagery layer as a satellite basemap
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    
    # Add a custom UNBL basemap layer (UN Biodiversity Lab)
    addTiles(
      urlTemplate = "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl07p7r84000b15mw1ctxlqwo/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2xvZmJ5eHNkMDlqNjJxdWhjYjVlcG5sMSJ9.ATqw6HfibevC5ov5y6VTOQ",
      group = "UNBL",  # Name of the group in the layer control
      attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Geodata'
    ) |>
    
    # Add a dark-themed UNBL basemap layer
    addTiles(
      urlTemplate = "https://api.mapbox.com/styles/v1/unbiodiversitylab/cl5vpldzt000614qc8qnjutdy/tiles/{z}/{x}/{y}?access_token=pk.eyJ1IjoidW5iaW9kaXZlcnNpdHlsYWIiLCJhIjoiY2xvZmJ5eHNkMDlqNjJxdWhjYjVlcG5sMSJ9.ATqw6HfibevC5ov5y6VTOQ",
      group = "UNBL Dark",  # Name of the group in the layer control
      attribution = '© <a href="https://www.mapbox.com/contribute/">Mapbox</a> © <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | UN Geodata'
    )
  
  # Return the configured leaflet map object
  return(map)
}
