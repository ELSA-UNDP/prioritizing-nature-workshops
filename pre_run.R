#!/usr/bin/env Rscript
source("packages.R")
purrr::walk(list.files(path = "R", pattern = "R", full.names = TRUE), source, echo = FALSE)

`%nin%` <- Negate(`%in%`)

terra::terraOptions(tempdir = here::here(""), steps = 4, todisk = TRUE)

terra::tmpFiles(remove = TRUE) # Clean existing temp layers

iso3 <- "ECU" # not used - local testing only
country <- "Ecuador"
language <- "es" # not used - local testing only
input_spreadsheet <- "prioritizing-nature-database-master.xlsx"
sheetname <- "ECU_data_stack"
blm <- 0
reducedres <- FALSE
weight_cal <- FALSE

if (reducedres) {
  resol <- 4 # factor to aggregate raster by for local testing (speed up!)
}

ELSA_df <- readxl::read_excel(
  path = input_spreadsheet,
  sheet = sheetname
  ) |>
  janitor::clean_names()

# Load Translation File ####
translations <-
  readxl::read_excel(
    path = "translation-matrix.xlsx",
    sheet = "tool_master"
  )

readr::write_rds(translations, here::here(".", "translations.rds"), compress = "gz")

protect_budget <-
  ELSA_df[ELSA_df$groups == "Budgets", "protect"] |>
  tidyr::drop_na()  |> 
  dplyr::pull() |>
  as.numeric() |>
  plyr::round_any(1e-4, ceiling)
manage_budget <-
  ELSA_df[ELSA_df$groups == "Budgets", "manage"] |>
  tidyr::drop_na() |>
  dplyr::pull() |>
  as.numeric() |>
  plyr::round_any(1e-4, ceiling)
restore_budget <-
  ELSA_df[ELSA_df$groups == "Budgets", "restore"] |>
  tidyr::drop_na() |>
  dplyr::pull() |>
  as.numeric() |>
  plyr::round_any(1e-4, ceiling)

feat_df <- ELSA_df |>
  dplyr::filter(groups == "Features" & !is.na(protect)) |>
  dplyr::mutate(
    feat_name = file_name,
    protect = as.numeric(protect),
    manage = as.numeric(manage),
    restore = as.numeric(restore),
    label = case_when(
      language != "en" ~ label_name_translated,
      language == "en" ~ label_name
    ),
    theme = case_when(
      language != "en" ~ label_theme_translated,
      language == "en" ~ label_theme
    ),
    policy_num = ifelse(
      is.na(secondary_target),
      primary_target,
      glue::glue("{primary_target},{secondary_target}")
    )
  ) |>
  dplyr::select(
    groups,
    label,
    theme,
    protect,
    manage,
    restore,
    weight_stakeholder,
    weight_calibration,
    weight_final,
    policy_num = primary_target, # NBSAP policy in Peru
    policy_short,
    policy_long,
    descriptions = layer_description,
    citation,
    citation_short,
    feat_name
  )

lockin_df <- ELSA_df |>
  dplyr::filter(groups == "Lock-in") |>
  dplyr::select(
    name = if_else(language != "en", "label_name_translated", "label_name"),
    short = policy_short,
    description = layer_description,
    file_name
  )

zones_df <- ELSA_df |>
  dplyr::filter(groups == "Zones-restrictions") |>
  dplyr::select(
    name = if_else(language != "en", "label_name_translated", "label_name"),
    short = policy_short,
    description = layer_description,
    file_name
  ) |>
  bind_rows(lockin_df[nrow(lockin_df),])

rm(ELSA_df)

# Data layers ####
feat_stack <-
  terra::rast(here::here("data/elsa_inputs", feat_df$feat_name))
names(feat_stack) <- feat_df$feat_name
PA <-
  terra::rast(here::here("data/elsa_inputs", lockin_df$file_name[1]))
Rest <- terra::rast(here::here("data/elsa_inputs", lockin_df$file_name[2]))
OECMS <- terra::rast(here::here("data/elsa_inputs", lockin_df$file_name[3]))
DEGR_PA <- terra::rast(here::here("data/elsa_inputs", lockin_df$file_name[4]))

# Zones ####
prot_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[1])) # adapted to get right data
man_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[2]))
rest_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[3]))
# New Protect + Restore Zone ####
pr_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[4]))

# Reduce resolution for testing
if (reducedres) {
  feat_stack <- terra::aggregate(feat_stack, fact = resol)
  PA <- terra::aggregate(PA, fact = resol)
  Rest <- terra::aggregate(Rest, fact = resol)
  OECMS <- terra::aggregate(OECMS, fact = resol)
  DEGR_PA <- terra::aggregate(DEGR_PA, fact = resol)
  prot_zone <- terra::aggregate(prot_zone, fact = resol)
  man_zone <- terra::aggregate(man_zone, fact = resol)
  rest_zone <- terra::aggregate(rest_zone, fact = resol)
  pr_zone <- terra::aggregate(pr_zone, fact = resol)
}

# Locked in Constraints ####
PA <- PA > 0.5
OECMS <- OECMS > 0.5
PA[Rest == 1] <- NA # because we want to lock in Rest (if it exists) in zone2 but some Rest vals might be in PA
PAOECM <- PA | OECMS
PAN <- PA
PAN[PAN == 1] <- NA
PA0 <- PA
PA0[PA0] <- NA
PA[PA == 0] <- NA
OECMS[OECMS == 0] <- NA

Rest <- Rest > 0
Rest_DEGR_PA <- Rest | DEGR_PA > 0.5
Rest_DEGR_PA[Rest == 1] <- NA # because we want to lock in Rest (if it exists) in zone 2 but Rest_DEGR_PA in pr zone
RestN <- Rest
RestN[RestN == 1] <- NA
Rest0 <- Rest
Rest0[Rest0] <- NA
Rest[Rest == 0] <- NA
Rest[PA > 0] <- NA

# Planning unit information ####
pu0 <- prot_zone >= 0
pu <- pu0
pu[is.na(prot_zone)] <- NA

PAOECM_Rest_DEGR_PA <- PAOECM | Rest_DEGR_PA

#### PA locked-in ####
non_logical_DEGR_PA <- Rest_DEGR_PA*1
DEGR_PA0 <- terra::subst(non_logical_DEGR_PA, 1, NA)
DEGR_PAN <- terra::subst(non_logical_DEGR_PA, 0, NA)

##### OECM locked-in #####
non_logical_PAOECM <- PAOECM * 1
non_logical_PAOECM[Rest == 1] <- 0 #always with Rest locked in (if it exists), so always need to exclude rest
PAOECM0 <- terra::subst(non_logical_PAOECM, 1, NA)
PAOECM1 <- terra::subst(non_logical_PAOECM, 0, NA)

##### OECM locked-in without PAs #####
non_logical_PAOECM_wo_DEGR_PA <- PAOECM*1
non_logical_PAOECM_wo_DEGR_PA[Rest == 1] <- 0 #always with Rest locked in (if it exists), so always need to exclude rest
non_logical_PAOECM_wo_DEGR_PA[non_logical_PAOECM_wo_DEGR_PA == non_logical_DEGR_PA] <- 0 # because these will be assigned to different zones: SNAP to pr and PAOECM to protect, so CANT have overlap, otherwise error bc PUs assigned to several zones

PAOECM_wo_DEGR_PA0 <- terra::subst(non_logical_PAOECM_wo_DEGR_PA, 1, NA)
PAOECM_wo_DEGR_PA1 <- terra::subst(non_logical_PAOECM_wo_DEGR_PA, 0, NA)

##### PAs without Degraded PAs ####
PA_wo_DEGR_PA <- PA
PA_wo_DEGR_PA[DEGR_PAN == 1] <- 0
PA_wo_DEGR_PA0 <- terra::subst(PA_wo_DEGR_PA, 1, NA)
PA_wo_DEGR_PA1 <- terra::subst(PA_wo_DEGR_PA, 0, NA)

# Locked out Constraints ####
# PAs ####
# only allow PUs for current PAs to be assigned to protect or pr zone
PA_logical <- as.logical(PA) # to lock out (locked_out cell = 1)
not_locked_out <- as.logical(terra::subst(
  terra::subst(PA, 1, 0), # also need not locked out cells for zones where PAs are included
  NA, 1
) * terra::subst(PA0, NA, 1))
# needs to be in right order for zones so that:
# zone 1(protect): NOT locked_out,
# zone 2(restore): locked out
# zone 3(manage): locked out,
# zone 4(pr): NOT locked_out,
# zone 5(do nothing): locked out
pas_locked_out <- c(
  not_locked_out, PA_logical, PA_logical,
  not_locked_out, PA_logical
) # need to have as many inputs as we have zones

Rest_logical <- as.logical(Rest)

not_locked_out <- as.logical(terra::ifel(!is.na(Rest), 0, 1) * terra::ifel(!is.na(Rest0), 1, 1))
Rest_not_locked_out <- c(Rest_logical, not_locked_out, Rest_logical, not_locked_out, Rest_logical)

# OECMS (for palock) ####
PAOECM_logical <- as.logical(PAOECM)

PAOECM_locked_out <- c(
  not_locked_out, 
  PAOECM_logical, 
  PAOECM_logical,
  not_locked_out, 
  PAOECM_logical
)

# Rest_DEGR_PA ####
DEGR_PA_logical <- Rest_DEGR_PA*1
DEGR_PA_logical <- as.logical(terra::subst(DEGR_PA_logical, 0, NA))

DEGR_PA_locked_out <- c(
  DEGR_PA_logical,
  DEGR_PA_logical,
  DEGR_PA_logical,
  not_locked_out,
  DEGR_PA_logical
  )

# OECMS and degraded areas in PAs (for restorelock and palock) ####
PAOECM_wo_DEGR_PA <- PAOECM
PAOECM_wo_DEGR_PA[PAOECM_wo_DEGR_PA == Rest_DEGR_PA] <- 0 # because these will be assigned to different zones: SNAP to pr and PAOECM to protect, so CANT have overlap, otherwise error bc PUs assigned to several zones
PAOECM_wo_DEGR_PA_logical <- as.logical(PAOECM_wo_DEGR_PA)

PAOECM_wo_DEGR_PA_locked_out <- c(
  not_locked_out,
  PAOECM_wo_DEGR_PA_logical,
  PAOECM_wo_DEGR_PA_logical,
  PAOECM_wo_DEGR_PA_logical,
  PAOECM_wo_DEGR_PA_logical
)

# Zones ####
zone_protect <- prot_zone
zone_protect[zone_protect < 1] <- NA
zone_protect[PA == 1] <- 1 #Patch in existing PAs because budget errors?
zone_protect[Rest > 0] <- NA # Restoration areas are locked in, so excluded from the protect zone
zone_protect_PAOECM <- ifel(zone_protect | PAOECM, 1, NA)
zone_protect_PAOECM[Rest > 0] <- NA # Restoration areas are locked in, so excluded from the protect zone
zone_protect_Rest_DEGR_PA <- zone_protect
zone_protect_Rest_DEGR_PA[Rest_DEGR_PA > 0] <- NA
zone_protect_PAOECMS_Rest_DEGR_PA <- zone_protect_Rest_DEGR_PA

# Restore zone
zone_restore <- rest_zone
zone_restore[zone_restore < 1] <- NA
zone_restore[Rest > 0] <- 1 # Restoration areas are locked in, so always included in the restore zone
zone_restore_PAOECM <- zone_restore
zone_restore_PAOECM[PAOECM == 1] <- NA
zone_restore_PAOECM[Rest > 0] <- 1 # Restoration areas are locked in, so always included every restore zone
zone_restore_Rest_DEGR_PA <- ifel(zone_restore | Rest_DEGR_PA, 1, NA)
zone_restore_Rest_DEGR_PA[Rest > 0] <- 1 # Restoration areas are locked in, so always included every restore zone
zone_restore_PAOECMS_Rest_DEGR_PA <- zone_restore_Rest_DEGR_PA
zone_restore_PAOECMS_Rest_DEGR_PA[PAOECM > 0] <- NA
zone_restore_PAOECMS_Rest_DEGR_PA[Rest > 0] <- 1 # Restoration areas are locked in, so always included every restore zone

# Manage zone
zone_manage <- man_zone
zone_manage[zone_manage < 1] <- NA
zone_manage[Rest > 0] <- NA # Restoration areas are locked in, so excluded from the manage zone
zone_manage_PAOECM <- zone_manage
zone_manage_PAOECM[PAOECM == 1] <- NA
zone_manage_PAOECM[Rest > 0] <- NA # Restoration areas are locked in, so excluded from all manage zones
zone_manage_Rest_DEGR_PA <- zone_manage
zone_manage_Rest_DEGR_PA[Rest_DEGR_PA > 0] <- NA
zone_manage_PAOECMS_Rest_DEGR_PA <- zone_manage_Rest_DEGR_PA
zone_manage_PAOECMS_Rest_DEGR_PA[PAOECM | Rest_DEGR_PA] <- NA

# New PR zone ####
zone_pr <- pr_zone
zone_pr[zone_pr < 1] <- NA
zone_pr_PAOECM <- zone_pr
zone_pr_Rest_DEGR_PA <- zone_pr 
zone_pr_Rest_DEGR_PA[Rest > 0] <- NA # exclude current restoration projects because they are locked in the restore zone
zone_pr_PAOECMS_Rest_DEGR_PA <- zone_pr_Rest_DEGR_PA

# New Do Nothing zone ####
do_nothing_zone <- terra::subst(pu0, TRUE, 0) # set cost to 0
zone_dn <- do_nothing_zone
zone_dn_PAOECM <- do_nothing_zone
zone_dn_Rest_DEGR_PA <- do_nothing_zone # don't need to exclude anything because this won't have any features
zone_dn_PAOECMS_Rest_DEGR_PA <- do_nothing_zone # don't need to exclude anything because this won't have any features

pu1 <- c(zone_protect, zone_restore, zone_manage, zone_pr, zone_dn)
pu1_oecm <- c(zone_protect_PAOECM, zone_restore_PAOECM, zone_manage_PAOECM, zone_pr_PAOECM, zone_dn_PAOECM)
names(pu1) <- names(pu1_oecm) <- c("protect", "restore", "manage", "pr", "do_nothing")
pu1_rest <-
  c(
    zone_protect_Rest_DEGR_PA,
    zone_restore_Rest_DEGR_PA,
    zone_manage_Rest_DEGR_PA,
    zone_pr_Rest_DEGR_PA,
    zone_dn_Rest_DEGR_PA
  )
names(pu1_rest) <- c("protect", "restore", "manage", "pr", "do_nothing")
pu1_oecmrest <-
  c(
    zone_protect_PAOECMS_Rest_DEGR_PA,
    zone_restore_PAOECMS_Rest_DEGR_PA,
    zone_manage_PAOECMS_Rest_DEGR_PA,
    zone_pr_PAOECMS_Rest_DEGR_PA,
    zone_dn_PAOECMS_Rest_DEGR_PA
  )
names(pu1_oecmrest) <- c("protect", "restore", "manage", "pr", "do_nothing")
pu_all <- list(area = list(
  avail = pu1,
  locked = pu1_oecm,
  restore = pu1_rest,
  pa_restore = pu1_oecmrest
))

# Impacts setup
protect_impacts <- feat_df$protect
restore_impacts <- feat_df$restore
manage_impacts <- feat_df$manage
# New PR zone impacts ######
pr_impacts <- feat_df$restore

impacts <- data.frame(
  Name = feat_df$label,
  Theme = feat_df$theme,
  feature = names(feat_stack),
  Protect = as.numeric(protect_impacts),
  Restore = as.numeric(restore_impacts),
  Manage = as.numeric(manage_impacts),
  PR = as.numeric(pr_impacts)
)

# features
zn1 <- feat_stack * impacts$Protect
zn2 <- feat_stack * impacts$Restore
zn3 <- feat_stack * impacts$Manage

# New protect and restore zone ####
# zone_pr <- pr_zone
# zone_pr[zone_pr < 1] <- NA
# names(zone_pr) <- "pr"

zn4 <- zn2 * pr_zone # use restore values but filter for areas of relevance

# New Do Nothing zone ####
# do_nothing_zone <- terra::subst(pu0, TRUE, 0) # set cost to 0
# names(do_nothing_zone) <- "do_nothing"

zn5 <- zn1 * 0 # set all features to 0

### Create Zone file
zns <- prioritizr::zones(
  "Protect" = zn1,
  "Restore" = zn2,
  "Manage" = zn3,
  "PR" = zn4,
  "Do_Nothing" = zn5,
  feature_names = names(zn1)
)

################################################################################
# Weight calibration #####
# The maximum utility objective function does not have targets for features, but
# features can be given weights based on their perceived importance.
# In the ELSA pipeline, weights are determined in stakeholder workshops, but initial
# weights are calculated based on the analysis below. Pre-calibration aims to ensure
# that all conservation features achieve a similar level of representation relative
# to their maximum possible representation within the constraints of the budgets.
################################################################################
if (weight_cal) {
  # Planning unit information with cost information per zone (area based, so 1 where selection is possible for a zone, 0 where selection isn't possible)
  pu_temp <- pu_all[["area"]][["locked"]]
  
  # Create data frame for representation calculations ####
  # Calculate the sum of each layer
  layer_sums <- terra::global(feat_stack, sum, na.rm = TRUE)
  # Extract the names of each layer
  layer_names <- terra::names(feat_stack)
  
  # Create a data frame with layer names and their corresponding sums
  overall_raw_df <- tibble(
    feature = layer_names,
    total_amount = layer_sums[, 1] # First column of the global output
  )
  
  rm(layer_sums, layer_names)
  
  #### Create Problem ####
  prob.ta <-
    prioritizr::problem(pu_temp, zns, run_checks = FALSE) |> 
    prioritizr::add_gurobi_solver(gap = 0.05, threads = 8)
  
  # Create a temporary table called input as required for the define_problem* functions
  input <- list()
  input$zone_1_target <- protect_budget
  input$zone_2_target <- restore_budget
  input$zone_3_target <- manage_budget
  
  # Use PA lock-in scenario - in BEZOS tool this is the 'avail' scenario (PAs and Rest projects area locked in)
  prob.ta <- define_problem_avail(
    input = input, 
    prob.ta = prob.ta,
    pu_temp = pu_temp
  )
  
  s.ta <- solve.ConservationProblem(prob.ta, force = TRUE)
  
  freq <-
    prioritizr::eval_feature_representation_summary(prob.ta, s.ta)
  
  # Prepare weight matrix with zeros
  wgt <- matrix(0, ncol = length(zns), nrow = terra::nlyr(feat_stack))
  
  # Initialize data frame to store maximum representations
  rep_max <- tibble(feature = names(feat_stack), max_representation = 0)
  
  for (ii in 1:terra::nlyr(feat_stack)) {
    wgt2 <- wgt
    wgt2[ii, ] <- 1  # Set weight for the current feature
    
    prob_all <- prob.ta |>
      prioritizr::add_feature_weights(wgt2)
    
    result <- solve.ConservationProblem(prob_all, force = TRUE)
    
    # Calculate representation using your new method
    overall_rep <- prioritizr::eval_feature_representation_summary(prob_all, result) |>
      dplyr::rename(zone = summary) |>
      filter(zone == "overall") |>
      dplyr::select("feature", "absolute_held") |>
      dplyr::left_join(overall_raw_df, by = "feature") |>
      dplyr::mutate(relative_held_overall = absolute_held / total_amount)
    
    # Store the maximum representation for the current feature
    rep_max$max_representation[ii] <- overall_rep$relative_held_overall[ii]
    
    rm(prob_all, result, overall_rep)
    gc()
  }
  
  gc()
  
  # All features weighted equally
  wgt_equal <- matrix(1, ncol = length(zns), nrow = terra::nlyr(feat_stack))
  
  prob_all_equal <- prob.ta |> 
    prioritizr::add_feature_weights(wgt_equal)
  
  result_equal <- solve.ConservationProblem(prob_all_equal, force = TRUE)
  
  overall_rep_equal <- prioritizr::eval_feature_representation_summary(prob_all_equal, result_equal) |>
    dplyr::rename(zone = summary) |>
    filter(zone == "overall") |>
    dplyr::select("feature", "absolute_held") |>
    dplyr::left_join(overall_raw_df, by = "feature") |>
    dplyr::mutate(relative_held_overall = absolute_held / total_amount)
  
  # Combine into a data frame
  dd <- data.frame(
    feature = overall_rep_equal$feature,
    max_representation = rep_max$max_representation,
    max_utility = overall_rep_equal$relative_held_overall
  ) |> 
    dplyr::mutate(
      delta_mu = max_utility - max_representation,
      delta_mu_perc = ifelse(max_representation == 0, 0, (max_utility - max_representation) / max_representation * 100)
    )
  
  summary(dd$delta_mu_perc)
  
  # calculate addition
  calib <- TRUE
  wgt_scale <- 2
  it <- 1
  wgta <- wgt_equal  # Start with equal weights
  
  while (calib) {
    adj <- wgt_scale - (dd$delta_mu_perc - min(dd$delta_mu_perc)) / (max(dd$delta_mu_perc) - min(dd$delta_mu_perc)) * wgt_scale
    wgtb <- wgta + adj
    
    print(it)
    flush.console()
    
    p1 <- prob.ta |>
      prioritizr::add_feature_weights(wgtb)
    
    s1 <- solve.ConservationProblem(p1, force = TRUE)
    
    # Calculate representation using new method
    overall_rep_iter <- prioritizr::eval_feature_representation_summary(p1, s1) |>
      dplyr::rename(zone = summary) |>
      filter(zone == "overall") |>
      dplyr::select("feature", "absolute_held") |>
      dplyr::left_join(overall_raw_df, by = "feature") |>
      dplyr::mutate(relative_held_overall = absolute_held / total_amount)
    
    # Create dd1 data frame with updated representations
    dd1 <- data.frame(
      feature = overall_rep_iter$feature,
      max_representation = rep_max$max_representation,
      max_utility = overall_rep_iter$relative_held_overall
    ) |>
      dplyr::mutate(
        delta_mu = max_utility - max_representation,
        delta_mu_perc = ifelse(max_representation == 0, 0, (max_utility - max_representation) / max_representation * 100)
      )
    
    # Check if the spread of delta_mu_perc has decreased
    delta1 <- max(dd1$delta_mu_perc) - min(dd1$delta_mu_perc)
    delta0 <- max(dd$delta_mu_perc) - min(dd$delta_mu_perc)
    
    if (delta1 < delta0) {
      wgta <- wgtb
      dd <- dd1
      it <- it + 1
    } else {
      calib <- FALSE
    }
  }
  
  rm(
    pu_temp,
    input,
    prob.ta,
    result_equal,
    freq,
    prob_all_equal,
    overall_rep_equal,
    overall_rep_iter,
    p1,
    s1,
    dd1,
    ELSA_text
  )
  
  wgta[, 1] |> readr::write_rds(glue::glue("wgta_{tolower(iso3)}.rds"), compress = "gz")
} else {
  wgta <- as.numeric(feat_df$weight_calibration)
}

wgts <- tibble::tibble(
  name = feat_df$label,
  theme = feat_df$theme,
  feature = names(feat_stack),
  weight = ifelse(
    is.na(feat_df$weight_final),
    5,
    as.numeric(feat_df$weight_final)
    ),
  policy = feat_df$policy_num
) # Policy Targets

################################################################################
# END Weight calibration
################################################################################

# Process feature theme information ####
# (specific category for a feature, either Biodiversity, Climate Mitigation or Human Well-being)
themes <- unique(feat_df$theme)
theme_names <- list()
theme_layers <- list()

for (ii in 1:length(themes)) {
  theme_names[[ii]] <-
    names(feat_stack)[grep(themes[ii], feat_df$theme, ignore.case = T)]
  theme_layers[[ii]] <- feat_stack[[theme_names[[ii]]]]
}

theme_tbl <- tibble(
  theme = themes,
  names = theme_names,
  layers = theme_layers
)

gc()

# Get minimum budget values per each lock-in scenario
default_protect_min_budget <- round(coalesce(get_coverage(PA, pu), 0), 2)
default_restore_min_budget <- round(coalesce(get_coverage(Rest, pu), 0), 2)
paoecm_lockin_min_budget <- round(coalesce(get_coverage(PAOECM, pu), 0), 2)
restore_snap_lockin_min_budget <- round(coalesce(get_coverage(Rest | Rest_DEGR_PA, pu), 0), 2)

PA <- terra::wrap(PA)
Rest <- terra::wrap(Rest)
Rest0 <- terra::wrap(Rest0)
OECMS <- terra::wrap(OECMS)
PAOECM <- terra::wrap(PAOECM)
Rest_DEGR_PA <- terra::wrap(Rest_DEGR_PA)
PAOECM_Rest_DEGR_PA <- terra::wrap(PAOECM_Rest_DEGR_PA)
PAN <- terra::wrap(PAN)
PA0 <- terra::wrap(PA0)
PA_wo_DEGR_PA1 <- terra::wrap(PA_wo_DEGR_PA1)
PA_wo_DEGR_PA0 <- terra::wrap(PA_wo_DEGR_PA0)
DEGR_PAN <- terra::wrap(DEGR_PAN)
DEGR_PA0 <- terra::wrap(DEGR_PA0)
PAOECM1 <- terra::wrap(PAOECM1)
PAOECM0 <- terra::wrap(PAOECM0)
PAOECM_wo_DEGR_PA1 <- terra::wrap(PAOECM_wo_DEGR_PA1)
PAOECM_wo_DEGR_PA0 <- terra::wrap(PAOECM_wo_DEGR_PA0)
pu1 <- terra::wrap(pu1)
pu1_oecm <- terra::wrap(pu1_oecm)
pas_locked_out <- terra::wrap(pas_locked_out)
PAOECM_locked_out <- terra::wrap(PAOECM_locked_out)
DEGR_PA_locked_out <- terra::wrap(DEGR_PA_locked_out)
PAOECM_wo_DEGR_PA_locked_out <- terra::wrap(PAOECM_wo_DEGR_PA_locked_out)
pu1_rest <- terra::wrap(pu1_rest)
pu1_oecmrest <- terra::wrap(pu1_oecmrest)
pu0 <- terra::wrap(pu0)
pu <- terra::wrap(pu)

zn1 <- terra::wrap(zn1)
zn2 <- terra::wrap(zn2)
zn3 <- terra::wrap(zn3)
zn4 <- terra::wrap(zn4)
zn5 <- terra::wrap(zn5)

save.image(here::here("prioritizing_nature.RData"), compress = "gzip")

gc()
# terra::tmpFiles(remove = TRUE)
