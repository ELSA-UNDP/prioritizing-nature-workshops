# Load required packages
source("packages.R")

# Load pre-computed global data
load("prioritizing_nature.RData")

# Load the ELSA text data (for different languages)
translations <- readr::read_rds(here::here(".", "translations.rds"))

# Initialize key variables from previously loaded data
language <- language  # Language of the interface
country <- country    # Country of interest (e.g., "Liberia")
reducedres <- reducedres # Flag indicating if you want to use reduced resolution rasters for testing

# Define a negation of `%in%` for later use
`%nin%` <- Negate(`%in%`)

# Create a list of lock-in categories
prot_lst <- list(
  "avail", # Areas available for allocation
  "locked", # Areas locked in for protection
  "restore", # Areas allocated for restoration
  "pa_restore") # Areas for protection and restoration

# Assign translated names from ELSA_text based on the current language
names(prot_lst) <- c(
    get_translation("nolock_txt"),
    get_translation("prot_txt_bezos"),
    get_translation("restlock_txt_bezos"),
    get_translation("prot_rest_txt_bezos")
  )

# Create a list for area calculation and assign a name based on the current language
area_lst <- list("area")
names(area_lst) <- get_translation("area")

#### Colour Palettes for the Map ####
pal.elsa <- tibble(
  colour = c("#4daf4a", "#984ea3", "#377eb8", "#adaf4a" , "#FFFFFF00"),
  category = c("Protect", "Restore", "Manage", "PR" , "Do_Nothing")
  )
pal.hm <- c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725")  # Heatmap colors
pal.in <- c("#0D0887", "#7E03A8", "#CC4678", "#F89441", "#F0F921")  # Input data colors
pal.zone <- "#6afdfa"  # Color for zone layers

#### Load spatial data layers ####
# Data layers ####
feat_stack <-
  terra::rast(here::here("data/elsa_inputs", feat_df$feat_name))
names(feat_stack) <- feat_df$feat_name
# Zones ####
prot_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[1]))
man_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[2]))
rest_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[3]))
# New Protect + Restore Zone ####
pr_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[4]))

# If optionally reduce resolution for testing
if (reducedres) {
  feat_stack <- terra::aggregate(feat_stack, fact = resol)
  prot_zone <- terra::aggregate(prot_zone, fact = resol)
  man_zone <- terra::aggregate(man_zone, fact = resol)
  rest_zone <- terra::aggregate(rest_zone, fact = resol)
  pr_zone <- terra::aggregate(pr_zone, fact = resol)
}

zn1 <- terra::rast(zn1)
zn2 <- terra::rast(zn2)
zn3 <- terra::rast(zn3)
zn4 <- terra::rast(zn4)
zn5 <- terra::rast(zn5)

### Create a Zone File for Prioritization ###
zns <- prioritizr::zones(
  "Protect" = zn1,
  "Restore" = zn2,
  "Manage" = zn3,
  "PR" = zn4,
  "Do_Nothing" = zn5,
  feature_names = names(zn1)
)

# Create a data frame for feature representation calculations
layer_sums <- terra::global(feat_stack, sum, na.rm = TRUE)  # Calculate total values for each feature
layer_names <- terra::names(feat_stack)  # Get feature names

# Create a data frame containing the feature names and their total values
overall_raw_df <- data.frame(
  feature = layer_names,
  total_amount = layer_sums[, 1]  # Use the first column for totals
)

# Remove temporary variables to clean up memory
rm(layer_sums, layer_names)

# Load other raster layers for protected areas, restoration, and management
PA <- terra::rast(PA)
Rest <- terra::rast(Rest)
Rest0 <- terra::rast(Rest0)
PAOECM <- terra::rast(PAOECM)
OECMS <- terra::rast(OECMS)
Rest_DEGR_PA <- terra::rast(Rest_DEGR_PA)
PACOEM_Rest_DEGR_PA <- terra::rast(PAOECM_Rest_DEGR_PA)
PAN <- terra::rast(PAN)
PA0 <- terra::rast(PA0)
PA_wo_DEGR_PA1 <- terra::rast(PA_wo_DEGR_PA1)
PA_wo_DEGR_PA0 <- terra::rast(PA_wo_DEGR_PA0)
DEGR_PAN <- terra::rast(DEGR_PAN)
DEGR_PA0 <- terra::rast(DEGR_PA0)
PAOECM1 <- terra::rast(PAOECM1)
PAOECM0 <- terra::rast(PAOECM0)
PAOECM_wo_DEGR_PA1 <- terra::rast(PAOECM_wo_DEGR_PA1)
PAOECM_wo_DEGR_PA0 <- terra::rast(PAOECM_wo_DEGR_PA0)
pas_locked_out <- terra::rast(pas_locked_out)
PAOECM_locked_out <- terra::rast(PAOECM_locked_out)
DEGR_PA_locked_out <- terra::rast(DEGR_PA_locked_out)
PAOECM_wo_DEGR_PA_locked_out <- terra::rast(PAOECM_wo_DEGR_PA_locked_out)
pu1 <- terra::rast(pu1)
pu1_oecm <- terra::rast(pu1_oecm)
pu1_rest <- terra::rast(pu1_rest)
pu1_oecmrest <- terra::rast(pu1_oecmrest)
pu0 <- terra::rast(pu0)
pu <- terra::rast(pu)

# Create a list of planning units based on restorelock and palock status
pu_all <- list(area = list(
  avail = pu1,
  locked = pu1_oecm,
  restore = pu1_rest,
  pa_restore = pu1_oecmrest
))

# Organize feature data by themes
themes <- unique(feat_df$theme)
theme_names <- list()
theme_layers <- list()

# Group layers by themes and create corresponding lists
for (ii in 1:length(themes)) {
  theme_names[[ii]] <- names(feat_stack)[grep(themes[ii], feat_df$theme, ignore.case = TRUE)]
  theme_layers[[ii]] <- feat_stack[[theme_names[[ii]]]]
}

# Create a table with theme information, including layer names and data
theme_tbl <- tibble(
  theme = themes,
  names = theme_names,
  layers = theme_layers
)

scenario_names <- c(
  get_translation("all_run_nolock_txt"),
  get_translation("all_run_oecmlock_txt"),
  get_translation("all_run_restorelock_txt"),
  get_translation("all_run_alllock_txt")
)
