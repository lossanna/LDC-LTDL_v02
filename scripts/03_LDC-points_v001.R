# Created: 2026-01-15
# Updated: 2026-01-22

# Purpose: Create LDC_v001, which is the most recent monitoring instance for each
#   unique point in space.

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.pts <- read_csv("data/GIS-exports/001_LDCpts_export.csv")
ldc.countoverlapping <- read_csv("data/GIS-exports/001_LDCpts-CountOverlapping_export.csv")
ldc.overlaptable <- read_csv("data/GIS-exports/001_LDCpts-OverlapTable_export.csv")

# Join tables -------------------------------------------------------------

# Join with OverlapTable
ldc.join <- ldc.pts %>% 
  left_join(ldc.overlaptable)

#   Look for NAs
apply(ldc.join, 2, anyNA)

# Join with CountOverlapping
ldc.join <- ldc.join %>% 
  left_join(ldc.countoverlapping)

#   Look for NAs
apply(ldc.join, 2, anyNA)

# Remove unnecessary cols and rename COUNT_
ldc.join <- ldc.join %>% 
  select(-COUNT_FC, -ORIG_NAME) %>% 
  rename(ldc_count = COUNT_)

# Reformat DateVisted col
ldc.join <- ldc.join %>% 
  mutate(DateVisted = as.Date(as.POSIXct(DateVisted, format = "%m/%d/%Y %H:%M:%S")))


# Extract rows with no DateVisted -----------------------------------------

# NA for DateVisted
ldc.date.na <- ldc.join %>% 
  filter(is.na(DateVisted))
# For now, these rows should just be deleted.


# Extract rows of most recent monitoring ----------------------------------

# Extract the most recent point for LDC plots that were monitored multiple times
most.recent <- ldc.join %>%
  group_by(OVERLAP_OID) %>%
  filter(DateVisted == max(DateVisted)) %>%
  ungroup()
length(unique(most.recent$OVERLAP_OID)) == nrow(most.recent) # FALSE
#   this means that there are some points that have the same DateVisted, so multiple rows
#     for those cases are created

# Separate out points where there is only one most recent date for DateVisted
#   these ones are fine and don't need fixing
most.recent.single <- most.recent %>%
  group_by(OVERLAP_OID) %>%
  filter(n() == 1) %>%
  ungroup()


## Multiple points/rows for DateVisted ------------------------------------

# Separate out points where DateVisted is the same for multiple rows
most.recent.multiple <- most.recent %>%
  group_by(OVERLAP_OID) %>%
  filter(n() > 1) %>%
  ungroup() %>% 
  arrange(OVERLAP_OID)

# OUTPUT: LDC points with multiple DateVisted of the same value
write_csv(most.recent.multiple,
          file = "data/data-wrangling-intermediate/03a_output1_LDC_multiple-same-DateVisted.csv")

# Because this version of the LDC data has more flux data included, I am just going
#   to take the first instance of every duplicate instead of going through manually
#   like I did before, because it is taking too long. (The edited version is abandoned.)

# Retain only the first instance of duplicate rows
most.recent.multiple.fixed <- most.recent.multiple %>% 
  group_by(OVERLAP_OID) %>% 
  slice_head(n = 1) %>% 
  ungroup()


## Combine all with corrections -------------------------------------------

# Remove rows with NA for DateVisted and use correction when there are multiple most recent rows
most.recent.combined <- most.recent.single %>% 
  bind_rows(most.recent.multiple.fixed) %>% 
  filter(!is.na(DateVisted))



# Separate out columns for GIS join ---------------------------------------

most.recent.gisjoin <- most.recent.combined %>% 
  select(ORIG_OID, OVERLAP_OID, ProjKey, PrimaryKey, DateVisted, EcoLvl3, EcoSiteID, 
         MLRADesc, MLRASym)


# Write LDC001 to CSV -----------------------------------------------------

# All columns
write_csv(most.recent.combined,
          file = "data/versions-from-R/03_LDC-points_v001.csv")

# GIS join
write_csv(most.recent.gisjoin,
          file = "data/versions-from-R/03_LDC-points_v001-gisjoin.csv")


save.image("RData/03_LDC-points_v001.RData")
