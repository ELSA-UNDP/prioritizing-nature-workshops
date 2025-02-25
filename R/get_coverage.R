#' Calculate the Percentage Area Coverage for a Given Zone
#'
#' This function calculates the percentage of the total area covered by a specific zone (e.g., protected areas, managed areas) 
#' relative to the entire planning unit (PU) layer. It returns the areal coverage as a percentage.
#' 
#' The function uses `terra::global()` to calculate the sum of values in both the zone and PU layers, 
#' ignoring missing values (`NA`s), and computes the percentage of the zone's total area relative to the PU's total area.
#'
#' @param zone_layer A `terra` raster object representing the specific zone (e.g., protected or managed areas) for which 
#'        the coverage percentage will be calculated.
#' @param pu_layer A `terra` raster object representing the total planning units (PU) layer that defines the entire area of interest.
#'
#' @return A numeric value representing the percentage coverage of the `zone_layer` relative to the `pu_layer`.
#'
#' @seealso [terra::global()] which is used to sum the values in the raster layers.
#' @export
#' @examples
#' # Calculate coverage for protected areas
#' get_coverage(PA, pu)
#' # Calculate coverage for protected zones
#' get_coverage(zone_protect, pu)
#' # Calculate coverage for managed zones
#' get_coverage(zone_manage, pu)
get_coverage <- function(zone_layer, pu_layer) {
  
  # Calculate the total area (sum of all values) in the `zone_layer`, ignoring NA values
  zone_sum <- terra::global(zone_layer, sum, na.rm = TRUE)$sum
  
  # Calculate the total area (sum of all values) in the `pu_layer`, ignoring NA values
  pu_sum <- terra::global(pu_layer, sum, na.rm = TRUE)$sum
  
  # Return the percentage coverage of the `zone_layer` relative to the `pu_layer`
  (zone_sum / pu_sum) * 100
}
