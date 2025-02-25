#' Function to create a categorical raster based on an input raster
#'
#' This function converts an input `SpatRaster` into a categorical raster. The input raster 
#' is processed to remove zero values, and categorical data are derived based on the number 
#' of layers and layer names. Additionally, the categories are linked to a global color 
#' palette and associated labels.
#'
#' @param in_rast A `SpatRaster` that is to be divided into categories.
#'
#' @return A `SpatRaster` with categorical data that includes category values, colors, and labels.
#' @export
make_elsa_categorical_raster <- function(in_rast) {
  
  # Create a tibble mapping each layer in the raster to a category
  elsa_categories <- tibble(
    value = seq(1, nlyr(in_rast), 1),  # Assign a value for each layer from 1 to the number of layers
    action = names(in_rast)  # Assign the names of the raster layers as actions/categories
  )
  
  # Create a working copy of the input raster
  r_in <- in_rast
  
  # Replace any zero values in the raster with NA to exclude them from analysis
  r_in[r_in == 0] <- NA
  
  # Convert the input raster into a categorical raster for mapping, using the prioritizr function
  elsa_categorical_raster <- prioritizr::category_layer(r_in)
  
  # Extract unique categories (layer names)
  unique_vals <- unique(elsa_categories$action)
  
  # Ensure all unique values from the raster have corresponding colors in the palette `pal.elsa`
  if (!all(unique_vals %in% pal.elsa$category)) {
    stop("Some categories in the raster do not have corresponding colors in `pal.elsa`.")  # Error if mismatch
  }
  
  # Create raster attributes by matching the categories to the global palette and text labels
  raster_attributes <- pal.elsa |>
    filter(category %in% unique_vals) |>  
    arrange(factor(category, levels = unique_vals)) |>
    mutate(category_lower = gsub(' ', '_', tolower(category))) |>  # Convert category names to lowercase for consistent matching
    left_join(translations, by = c("category_lower" = "var")) |>
    left_join(elsa_categories, by = c("category" = "action")) |>
    select(value, colour, category, label = !!sym(language)) |> 
    data.frame()  # Convert to a data frame for compatibility with raster functions
  
  # Assign the attributes (values, colors, labels) to the categorical raster
  levels(elsa_categorical_raster) <- raster_attributes
  
  # Set the active category to the label field (2nd column), to use the required language labels
  activeCat(elsa_categorical_raster) <- 2
  
  # Return the processed categorical raster
  return(elsa_categorical_raster)
}
