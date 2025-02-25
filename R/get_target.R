#' Get country-specific target value based on the total area of planning units (PU)
#'
#' This function calculates a target value by taking a percentage of the total area 
#' represented by the planning units (PU). The result is rounded to the nearest whole number, 
#' and if the calculated target is zero, a small value (1e-4) is returned to avoid issues with zero targets.
#'
#' @param PU A `terra` raster or object representing the planning units, where the total area is calculated.
#' @param target A numeric value representing the percentage target (e.g., 10 for 10%).
#'
#' @return A numeric value representing the target area. If the calculated target is zero, 
#' the function returns a very small value (1e-4) to ensure a non-zero target.
#' @export
#'
#' @examples
#' # Example usage with a raster of planning units and a target of 10%
#' get_target(PU = my_planning_units_raster, target = 10)
get_target <- function(PU = NULL, target = NULL) {
  
  # Calculate the total area of the planning units and determine the target based on the given percentage.
  # 'terra::global' is used to calculate the total sum of the values in the PU raster (ignoring NA values).
  # The target is calculated as the specified percentage of this total.
  tar <- round(terra::global(PU, "sum", na.rm = TRUE) / 100 * target, 0)$sum
  
  # If the calculated target is zero, return a small value (1e-4) to avoid zero targets in the optimization process.
  # Otherwise, return the calculated target value.
  ifelse(tar == 0, 1e-4, tar)
}
