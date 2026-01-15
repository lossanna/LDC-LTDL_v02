# Created: 2026-01-15
# Updated: 2026-01-15

# Purpose: Write the LDC points to shapefiles.

# From 2025-01-15 download of geoindicators.csv.


library(tidyverse)
library(sf)

# Load data ---------------------------------------------------------------

geoindicators.raw <- read_csv("data/raw/downloaded/ldc-data-2026-01-15/geoindicators.csv")

# Data wrangling ----------------------------------------------------------

# Adjust column names to be ESRI-friendly
col_rename_map <- c(
  "Project Key" = "ProjKey",
  "Primary Key" = "PrimaryKey",
  "Date Visited" = "DateVisted",
  "Ecological Site Id" = "EcoSiteID",
  "Latitude (decimal degrees, NAD83)" = "Latitude",
  "Longitude (decimal degrees, NAD83)" = "Longitude",
  "Location Status" = "LocatStatus",
  "Location Type" = "LocatType",
  "Latitude, Actual (decimal degrees, NAD83)" = "LatAct",
  "Longitude, Actual (decimal degrees, NAD83)" = "LonAct",
  "Bare Soil (% First Hit)" = "BareSoil",
  "Annual Forb Cover (% Any Hit)" = "AnnForbC",
  "Annual Graminoid Cover (% Any Hit)" = "AnnGramC",
  "Forb Cover (% Any Hit)" = "ForbC",
  "Annual Forb and Graminoid Cover (% Any Hit)" = "AnnFGC",
  "Graminoid Cover (% Any Hit)" = "GramC",
  "Perennial Forb Cover (% Any Hit)" = "PerForbC",
  "Perennial Graminoid Cover (% Any Hit)" = "PerGramC",
  "Shrub Cover (% Any Hit)" = "ShrubC",
  "FH Cyanobacteria Cover (% First Hit)" = "CyanoC",
  "Deposited Soil Cover (% First Hit)" = "DepstSoilC",
  "Duff Cover (% First Hit)" = "DuffC",
  "Embedded Litter Cover (% First Hit)" = "EmbLitC",
  "Herbaceous Litter Cover (% First Hit)" = "HerbLitC",
  "Lichen Cover (% First Hit)" = "LichenC",
  "Moss Cover (% First Hit)" = "MossC",
  "Rock Cover (% First Hit)" = "RockC",
  "Total Litter Cover (% First Hit)" = "TotLitC",
  "Vagrant Lichen Cover (% First Hit)" = "VagrtLichC",
  "Water Cover (% First Hit)" = "WaterC",
  "Woody Litter Cover (% First Hit)" = "WoodyLitC",
  "Canopy Gaps 25 - 50 cm (%)" = "Gap25_50",
  "Canopy Gaps 51-100 cm (%)" = "Gap51_100",
  "Canopy Gaps 101 - 200 cm (%)" = "Gap101_200",
  "Canopy Gaps > 200 cm (%)" = "Gap200plus",
  "Canopy Gaps > 25 cm (%)" = "Gap25plus",
  "Mean Forb Height (cm)" = "ForbHgt",
  "Mean Graminoid Height (cm)" = "GramHgt",
  "Mean Herbaceous Plant Height (cm)" = "HerbHgt",
  "Mean Perennial Forb Height (cm)" = "PForbHgt",
  "Mean Perennial Forb Graminoid Height (cm)" = "PFbGrHgt",
  "Mean Perennial Graminoid Height (cm)" = "PGramHgt",
  "Mean Woody Plant Height (cm)" = "WoodyHgt",
  "Total Annual Production (Rangeland Health)" = "TotProd",
  "Bare Ground (Rangeland Health)" = "BareGrd",
  "Biotic Integrity (Rangeland Health)" = "BioInt",
  "Comments: Biotic Integrity (Rangeland Health)" = "CmtBioInt",
  "Comments: Hydrologic Function (Rangeland Health)" = "CmtHydFn",
  "Comments: Soil and Site Stability (Rangeland Health)" = "CmtSoilSS",
  "Compaction (Rangeland Health)" = "Compact",
  "Proportion of Dead or Dying Plant Parts (Rangeland Health)" = "DeadProp",
  "Functional/Sructural Groups (Rangeland Health)" = "FuncGrp",
  "Gullies (Rangeland Health)" = "Gullies",
  "Hydrologic Function (Rangeland Health)" = "HydFn",
  "Invasive Plants (Rangeland Health)" = "InvasPl",
  "Litter Amount (Rangeland Health)" = "LitAmt",
  "Litter Movement (Rangeland Health)" = "LitMov",
  "Pedestals/Terracettes (Rangeland Health)" = "Pedestals",
  "Plant Community Composition (Rangeland Health)" = "CommComp",
  "Perennial Reproductive Capability (Rangeland Health)" = "PerRepro",
  "Rills (Rangeland Health)" = "Rills",
  "Soil Site Stability (Rangeland Health)" = "SoilSS",
  "Soil Surface Loss/Degradation (Rangeland Health)" = "SoilLoss",
  "Soil Surface Erosion Resistance (Rangeland Health)" = "SoilEroRes",
  "Water Flow Patterns (Rangeland Health)" = "WatFlow",
  "Wind Scoured Areas (Rangeland Health)" = "WindScd",
  "Mean Soil Stability: Surface" = "SoilSurf",
  "Mean Soil Stability: Protected Samples" = "SoilProt",
  "Mean Soil Stability: Unprotected Samples" = "SoilUnp",
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
  "Total Horizontal Flux" = "TotHorizFl",
  "Total Vertical Flux" = "TotVertFlx",
  "PM 2.5 Vertical Flux" = "PM25Flux",
  "PM 10 Vertical Flux" = "PM10Flux",
  "Long-Term Mean Precipitation" = "LTPrecip",
  "Long-Term Mean Runoff" = "LTRunoff",
  "Long-Term Mean Sediment Yield" = "LTSedYield",
  "Long-Term Mean Soil Loss" = "LTSoilLoss"
)

geoindicators <- geoindicators.raw %>% 
  rename(!!!setNames(names(col_rename_map), col_rename_map))

# Identify completely empty columns
empty_cols <- geoindicators %>%
  summarise(across(everything(), ~ all(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "is_empty") %>%
  filter(is_empty) %>%
  pull(column)
empty_cols

geoindicators <- geoindicators %>% 
  select(-all_of(empty_cols))


# Convert to shapefile ----------------------------------------------------

ldc.points <- st_as_sf(geoindicators, coords = c("Longitude", "Latitude"),
                       crs = 4269)

st_write(ldc.points, "data/LDC-points/03_LDC-points.shp",
         delete_layer = TRUE)
