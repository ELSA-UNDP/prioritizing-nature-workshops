#' Get Minimum Lock-in Targets for Zones
#'
#' This function calculates the minimum lock-in targets for different zones (e.g., protect, restore, manage, and optionally green).
#' It compares the input targets with the minimum coverage needed for each zone and ensures that the target is not set below this threshold.
#'
#' @param lockin A raster or list of rasters representing the locked-in areas for each zone.
#' @param input A list or dataframe containing the target values for each zone (e.g., `zone_1_target`, `zone_2_target`, etc.).
#' @param pu A raster layer representing the planning unit used for calculating coverage.
#' @return A `tibble` containing the adjusted target values for each zone.
#' @export
get_min_lockin_target <- function(lockin, input, pu) {
  
  # Calculate the minimum coverage of locked-in areas for each zone
  min_coverage <- get_coverage(lockin, pu)
  
  # Check if the 4th zone (e.g., green zone) is part of the input
  if (!is.null(input$zone_4_target)) {
    # Create a tibble with adjusted targets for all four zones (including the green zone)
    targets <- tibble::tibble(
      zone_1_target = ifelse(
        input$zone_1_target < min_coverage[1],
        min_coverage[1],  # Ensure zone_1 target is at least the minimum coverage
        input$zone_1_target
      ),
      zone_2_target = ifelse(
        input$zone_2_target < min_coverage[2],
        min_coverage[2],  # Ensure zone_2 target is at least the minimum coverage
        input$zone_2_target
      ),
      zone_3_target = ifelse(
        input$zone_3_target < min_coverage[3],
        min_coverage[3],  # Ensure zone_3 target is at least the minimum coverage
        input$zone_3_target
      ),
      zone_4_target = ifelse(
        input$zone_4_target < min_coverage[4],
        min_coverage[4],  # Ensure zone_4 target is at least the minimum coverage
        input$zone_4_target
      )
    )
  } else {
    # Create a tibble with adjusted targets for only three zones (no green zone)
    targets <- tibble::tibble(
      zone_1_target = ifelse(
        input$zone_1_target < min_coverage[1],
        min_coverage[1],  # Ensure zone_1 target is at least the minimum coverage
        input$zone_1_target
      ),
      zone_2_target = ifelse(
        input$zone_2_target < min_coverage[2],
        min_coverage[2],  # Ensure zone_2 target is at least the minimum coverage
        input$zone_2_target
      ),
      zone_3_target = ifelse(
        input$zone_3_target < min_coverage[3],
        min_coverage[3],  # Ensure zone_3 target is at least the minimum coverage
        input$zone_3_target
      )
    )
  }
  
  # Return the tibble with the adjusted targets
  return(targets)
}
