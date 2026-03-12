# Created: 2026-03-11
# Updated: 2026-03-12

# Purpose: Gather veg cover columns needed for PSM from LDC geoindicators.

# To install terradactyl: remotes::install_github(repo = 'Landscape-Data-Commons/terradactyl')

# Downloaded LDC geoindicators data again on 2026-03-11 because the 2026-01 download was missing
#   the columns Total Foliar Cover and Perennial Forb and Graminoid Cover. The points are the same
#   in terms of Primary Key and coordinates.

library(tidyverse)
library(terradactyl)

# Load data ---------------------------------------------------------------

ldc009.raw <- read_csv("data/GIS-exports/009_LDCpts009_export.csv")
geoindicators1.raw <- read_csv("data/raw/downloaded/ldc-data-2026-01-15/geoindicators.csv")
geoindicators2.raw <- read_csv("data/raw/downloaded/ldc-data-2026-03-11/geoindicators.csv")


# Data wrangling ----------------------------------------------------------

# Check for matching primary keys between LDC versions 1 & 2 (Jan vs March download)
setdiff(geoindicators1.raw$`Primary Key`, geoindicators2.raw$`Primary Key`)
setdiff(geoindicators2.raw$`Primary Key`, geoindicators1.raw$`Primary Key`)
unique(geoindicators1.raw$`Primary Key` == geoindicators2.raw$`Primary Key`)

# Check for matching coordinates
unique(geoindicators1.raw$`Longitude (decimal degrees, NAD83)` == geoindicators2.raw$`Longitude (decimal degrees, NAD83)`)
unique(geoindicators1.raw$`Latitude (decimal degrees, NAD83)` == geoindicators2.raw$`Latitude (decimal degrees, NAD83)`)


# Check for differing columns
setdiff(colnames(geoindicators2.raw), colnames(geoindicators1.raw))
colnames(geoindicators2.raw)


# Adjust column names of geoindicators
col_rename_map <- c(
  "Project Key" = "ProjKey",
  "Primary Key" = "PrimaryKey",
  "Date Visited" = "DateVisted",
  "Ecological Site ID" = "EcoSiteID",
  "Latitude (decimal degrees, NAD83)" = "Latitude",
  "Longitude (decimal degrees, NAD83)" = "Longitude",
  "Location Status" = "LocationStatus",
  "Location Type" = "LocationType",
  "Latitude, Actual (decimal degrees, NAD83)" = "LatActual",
  "Longitude, Actual (decimal degrees, NAD83)" = "LonActual",
  "Bare Soil (% First Hit)" = "BareSoil_FH",
  "Total Foliar Cover (%)" = "TotalFoliarCover",
  "Annual Forb Cover (% Any Hit)" = "AnnForbCover_AH",
  "Annual Graminoid Cover (% Any Hit)" = "AnnGramCover_AH",
  "Forb Cover (% Any Hit)" = "ForbCover_AH",
  "Annual Forb and Graminoid Cover (% Any Hit)" = "AnnForbGramCover_AH",
  "Graminoid Cover (% Any Hit)" = "GramCover_AH",
  "Perennial Forb Cover (% Any Hit)" = "PerForbCover_AH",
  "Perennial Forb and Graminoid Cover (% Any Hit)" = "PerForbGramCover_AH",
  "Perennial Graminoid Cover (% Any Hit)" = "PerGramCover_AH",
  "Shrub Cover (% Any Hit)" = "ShrubCover_AH",
  "FH Cyanobacteria Cover (% First Hit)" = "CyanobacteriaCover_FH",
  "Deposited Soil Cover (% First Hit)" = "DepositedSoilCover_FH",
  "Duff Cover (% First Hit)" = "DuffCover_FH",
  "Embedded Litter Cover (% First Hit)" = "EmbeddedLitterCover_FH",
  "Herbaceous Litter Cover (% First Hit)" = "HerbLitterCover_FH",
  "Lichen Cover (% First Hit)" = "LichenCover_FH",
  "Moss Cover (% First Hit)" = "MossCover_FH",
  "Rock Cover (% First Hit)" = "RockCover_FH",
  "Total Litter Cover (% First Hit)" = "TotalLitterCover_FH",
  "Vagrant Lichen Cover (% First Hit)" = "VagrantLichenCover_FH",
  "Water Cover (% First Hit)" = "WaterCover_FH",
  "Woody Litter Cover (% First Hit)" = "WoodyLitterCover_FH",
  "Canopy Gaps 25 - 50 cm (%)" = "Gap25_50",
  "Canopy Gaps 51-100 cm (%)" = "Gap51_100",
  "Canopy Gaps 101 - 200 cm (%)" = "Gap101_200",
  "Canopy Gaps > 200 cm (%)" = "Gap200plus",
  "Canopy Gaps > 25 cm (%)" = "Gap25plus",
  "Mean Forb Height (cm)" = "MeanForbHgt",
  "Mean Graminoid Height (cm)" = "MeanGramHgt",
  "Mean Herbaceous Plant Height (cm)" = "MeanHerbHgt",
  "Mean Perennial Forb Height (cm)" = "MeanPerForbHgt",
  "Mean Perennial Forb Graminoid Height (cm)" = "MeanPFbGrHgt",
  "Mean Perennial Graminoid Height (cm)" = "MeanPerGramHgt",
  "Mean Woody Plant Height (cm)" = "MeanWoodyHgt",
  "Total Annual Production (Rangeland Health)" = "TotAnnualProduction_RH",
  "Bare Ground (Rangeland Health)" = "BareGround_RH",
  "Biotic Integrity (Rangeland Health)" = "BioticIntegrity_RH",
  "Comments: Biotic Integrity (Rangeland Health)" = "BioticIntegrity_comments",
  "Comments: Hydrologic Function (Rangeland Health)" = "HydrologicFunction_comments",
  "Comments: Soil and Site Stability (Rangeland Health)" = "SoilSiteStability_comments",
  "Compaction (Rangeland Health)" = "Compaction_RH",
  "Proportion of Dead or Dying Plant Parts (Rangeland Health)" = "PropDeadDyingPlants_RH",
  "Functional/Sructural Groups (Rangeland Health)" = "FunctionalStructuralGroups_RH",
  "Gullies (Rangeland Health)" = "Gullies_RH",
  "Hydrologic Function (Rangeland Health)" = "HydrologicFunction_RH",
  "Invasive Plants (Rangeland Health)" = "InvasivePlants_RH",
  "Litter Amount (Rangeland Health)" = "LitterAmount_RH",
  "Litter Movement (Rangeland Health)" = "LitterMovement_RH",
  "Pedestals/Terracettes (Rangeland Health)" = "Pedestals_RH",
  "Plant Community Composition (Rangeland Health)" = "PlantCommunityComposition_RH",
  "Perennial Reproductive Capability (Rangeland Health)" = "PerReproCapactiy_RH",
  "Rills (Rangeland Health)" = "Rills_RH",
  "Soil Site Stability (Rangeland Health)" = "SoilSiteStability_RH",
  "Soil Surface Loss/Degradation (Rangeland Health)" = "SoilSurfaceLoss_RH",
  "Soil Surface Erosion Resistance (Rangeland Health)" = "SoilErosionResistance_RH",
  "Water Flow Patterns (Rangeland Health)" = "WaterFlowPatterns_RH",
  "Wind Scoured Areas (Rangeland Health)" = "WindScouredAreas_RH",
  "Mean Soil Stability: Surface" = "MeanSoilStability_Surface",
  "Mean Soil Stability: Protected Samples" = "MeanSoilStability_Protected",
  "Mean Soil Stability: Unprotected Samples" = "MeanSoilStabilityUnprotected",
  "MLRA Description" = "MLRADesc",
  "MLRA Symbol" = "MLRASym",
  "Ecoregion Level I" = "EcoLvl1",
  "Ecoregion Level II" = "EcoLvl2",
  "Ecoregion Level III" = "EcoLvl3",
  "Ecoregion Level IV" = "EcoLvl4",
  "State" = "State",
  "MODIS IGBP Name" = "MODISName",
  "Database Key" = "DBKey",
  "Date Loaded in Database" = "DateLoad",
  "Total Horizontal Flux" = "TotalHorizontalFlux",
  "Total Vertical Flux" = "TotalVerticalFlux",
  "PM 2.5 Vertical Flux" = "PM25Flux",
  "PM 10 Vertical Flux" = "PM10Flux",
  "Long-Term Mean Precipitation" = "LongTermMeanPrecip",
  "Long-Term Mean Runoff" = "LongTermMeanRunoff",
  "Long-Term Mean Sediment Yield" = "LongTermMeanSedimentYield",
  "Long-Term Mean Soil Loss" = "LongTermMeanSoilLoss"
)

geoindicators <- geoindicators2.raw %>% 
  rename(!!!setNames(names(col_rename_map), col_rename_map))

# Identify completely empty columns
empty_cols <- geoindicators %>%
  summarise(across(everything(), ~ all(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "is_empty") %>%
  filter(is_empty) %>%
  pull(column)

geoindicators <- geoindicators %>% 
  select(-all_of(empty_cols))


# Columns of interest -----------------------------------------------------

psm.col <- geoindicators %>% 
  select(PrimaryKey, BareSoil_FH, TotalFoliarCover, 
         ForbCover_AH, GramCover_AH, ShrubCover_AH, 
         AnnForbCover_AH, AnnGramCover_AH, PerForbCover_AH, PerGramCover_AH,
         Gap101_200, Gap200plus)



