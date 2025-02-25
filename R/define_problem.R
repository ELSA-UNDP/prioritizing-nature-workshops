#' Define spatial prioritization problems with optional locked-in constraints
#'
#' These functions define spatial prioritization problems using the `prioritizr` package.
#' Each function allows customization of locked-in constraints for specific areas,
#' affecting how zones are allocated (protect, restore, manage, etc.).
#' For example, areas may be freely allocated, or restricted based on OECMs (Other
#' Effective Conservation Measures) or degraded areas within protected areas.
#'
#' @param input A list or dataframe containing input parameters for the prioritization.
#' @param prob.ta An existing `prioritizr` problem object to be updated with constraints.
#' @param pu_temp A list of raster layers representing different zones (protect, restore, manage, etc.).
#' @param new_protect_zone Logical; if `TRUE`, modifies the protect zone to exclude degraded and/or unprotected areas.
#' @return A `prioritizr` problem object with updated constraints based on the input.
#' @export
#' 
#### Define Problem with No Locked-In Areas ####
define_problem_avail <- function(input, prob.ta, pu_temp) {
  
  target <- get_min_lockin_target(c(PAOECM, Rest0, PA0), input, pu) # Use Rest0 in place of Rest since no restoration project lock-ins exist in Peru  
  
  prob.ta <- prob.ta |> 
    prioritizr::add_max_utility_objective(terra::global(pu0, "sum", na.rm = TRUE)[[1]]) |>
    prioritizr::add_locked_out_constraints(pas_locked_out) |> 
    # prioritizr::add_locked_in_constraints(c(Rest0, Rest, Rest0, Rest0, Rest0)) |> # No restoration project lock-ins exist in Peru
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[1]),
      sense = "<=",
      data = c(
        pu_temp[[1]], # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[2]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]], # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) %>%
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[3]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]], # manage_zone
        pu_temp[[4]] * 0, # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) %>%
    prioritizr::add_mandatory_allocation_constraints()
  
  return(prob.ta)
}

#### Define Problem with OECM Locked-In ####
define_problem_oecmLI <- function(input, 
                                  prob.ta, pu_temp,
                                  new_protect_zone = TRUE) {
  
  if (new_protect_zone) {
    # protect_zone doesn't have all OECM cells #
    protect_zone_new <- pu_temp[[1]]
    protect_zone_new[PAOECM == 1] <- 1
    protect_zone_new[Rest > 0] <- NA
    pu_temp <- c(protect_zone_new, pu_temp[[2]], pu_temp[[3]], pu_temp[[4]], pu_temp[[5]])
    prob.ta <- prioritizr::problem(pu_temp, zns, run_checks = FALSE) |> 
      prioritizr::add_gurobi_solver(gap = 0.05, threads = 8)
  }
  
  target <- get_min_lockin_target(c(PAOECM, Rest0, PA0), input, pu) # Use Rest0 in place of Rest since no restoration project lock-ins exist in Peru
  
  prob.ta <- prob.ta |> 
    prioritizr::add_max_utility_objective(terra::global(pu0, "sum", na.rm = TRUE)[[1]]
    ) |> 
    # prioritizr::add_locked_in_constraints(c(Rest0, Rest, Rest0, Rest0, Rest0)) |> # No restoration project lock-ins exist in Peru 
    prioritizr::add_locked_in_constraints(c(PAOECM1, PAOECM0, PAOECM0, PAOECM0, PAOECM0)) |> 
    # prioritizr::add_locked_out_constraints(PAOECM_locked_out) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[1]),
      sense = "<=",
      data = c(
        pu_temp[[1]], # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[2]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]], # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[3]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]], # manage_zone
        pu_temp[[4]] * 0, # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_mandatory_allocation_constraints()
  
  return(prob.ta)
}

#### Define Problem with Degraded Areas inside Protected Areas Locked-In ####
define_problem_restoreLI <- function(input, prob.ta, pu_temp) {
  target <- get_min_lockin_target(c(PA, Rest_DEGR_PA, PA0), input, pu)
  
  prob.ta <- prob.ta |> 
    prioritizr::add_max_utility_objective(terra::global(pu0, "sum", na.rm = TRUE)[[1]])  |> 
    # prioritizr::add_locked_in_constraints(c(Rest0, Rest, Rest0, Rest0, Rest0)) |> # No restoration project lock-ins exist in Peru 
    prioritizr::add_locked_in_constraints(c(DEGR_PA0, DEGR_PA0, DEGR_PA0, DEGR_PAN, DEGR_PA0)) |> 
    prioritizr::add_locked_in_constraints(c(
      PA_wo_DEGR_PA1,
      PA_wo_DEGR_PA0,
      PA_wo_DEGR_PA0,
      PA_wo_DEGR_PA0,
      PA_wo_DEGR_PA0
    )) |>  # if we lock Degraded PAs and PA in, need to use PAs that don't include Degraded area
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[1]),
      sense = "<=",
      data = c(
        pu_temp[[1]], # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[2]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]], # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[3]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]], # manage_zone
        pu_temp[[4]] * 0, # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_mandatory_allocation_constraints()
  
  return(prob.ta)
}

#### Define Problem with Both OECM and Degraded Areas inside Protected Areas Locked-In ####
define_problem_oecmRestoreLI <- function(input, 
                                         prob.ta, pu_temp,
                                         new_protect_zone = TRUE) {
  if (new_protect_zone) {
    # protect_zone doesn't have all OECM cells #
    protect_zone_new <- pu_temp[[1]]
    protect_zone_new[PAOECM_wo_DEGR_PA1 == 1] <- 1
    protect_zone_new[Rest > 0] <- NA
    pu_temp <- c(protect_zone_new, pu_temp[[2]], pu_temp[[3]], pu_temp[[4]], pu_temp[[5]])
    prob.ta <- prioritizr::problem(pu_temp, zns, run_checks = FALSE) |>
      prioritizr::add_gurobi_solver(gap = 0.05,  threads = 8)
  }
  
  target <- get_min_lockin_target(c(PAOECM, Rest_DEGR_PA, PA0), input, pu)
  
  prob.ta <- prob.ta |> 
    prioritizr::add_max_utility_objective(
      terra::global(pu0, "sum", na.rm = TRUE)[[1]]
    ) |>
    prioritizr::add_locked_in_constraints(c(
      PAOECM_wo_DEGR_PA1,
      PAOECM_wo_DEGR_PA0,
      PAOECM_wo_DEGR_PA0,
      PAOECM_wo_DEGR_PA0,
      PAOECM_wo_DEGR_PA0
    )) |> 
    # prioritizr::add_locked_in_constraints(c(Rest0, Rest, Rest0, Rest0, Rest0)) |> # No restoration project lock-ins exist in Peru 
    prioritizr::add_locked_in_constraints(c(DEGR_PA0, DEGR_PA0, DEGR_PA0, DEGR_PAN, DEGR_PA0)) |>
    # prioritizr::add_locked_out_constraints(PAOECM_wo_DEGR_PA_locked_out) |> 
    # prioritizr::add_locked_out_constraints(DEGR_PA_locked_out) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[1]),
      sense = "<=",
      data = c(
        pu_temp[[1]], # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[2]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]], # restore_zone
        pu_temp[[3]] * 0, # manage_zone
        pu_temp[[4]], # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_linear_constraints(
      threshold = get_target(pu0, target[3]),
      sense = "<=",
      data = c(
        pu_temp[[1]] * 0, # protect_zone
        pu_temp[[2]] * 0, # restore_zone
        pu_temp[[3]], # manage_zone
        pu_temp[[4]] * 0, # pr_zone
        pu_temp[[5]] * 0 # do_nothing_zone
      )
    ) |> 
    prioritizr::add_mandatory_allocation_constraints()
  
  return(prob.ta)
} 
