# Created: 2026-01-15
# Updated: 2026-01-16

# Purpose: Remove instances of LDC points that overlap with treatment polygons but have a 
#   comp_est_date after the LDC DateVisted to create LDC_points_v002.

# Filter out rows where comp_est_date > DateVisted.
# Filter out "Cultural Protection" and "Other" treatments (based on Trt_Type_Major).

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

ldc.trt.sjoin.raw <- read_csv("data/GIS-exports/002_LDC001-Trt001_SpatialJoin_export.csv")


# Preliminary data wrangling ----------------------------------------------

# Convert date cols
ldc.trt.sjoin <- ldc.trt.sjoin.raw %>% 
  mutate(DateVisted = as.Date(DateVisted, format = "%m/%d/%Y"),
         init_date_est = as.Date(init_date_est, format = "%m/%d/%Y"),
         comp_date_est = as.Date(comp_date_est, format = "%m/%d/%Y"))

# Separate out points that overlap with treatment polygons
ldc.trt.overlap <- ldc.trt.sjoin %>% 
  filter(!is.na(TrtPolyID))

  
# Find instances where comp_est_date > DateVisted -------------------------

# Find instances where comp_date_est <= Date_Visted
trt.pre <- ldc.trt.overlap %>% 
  filter(comp_date_est <= DateVisted)



# Filter out "Other" and "Cultural Protection" ----------------------------

# Remove because these treatments are unlikely to affect veg, or categories are too broad
trt.pre.filtered <- trt.pre %>% 
  filter(!Trt_Type_Major %in% c("Cultural Protection", "Other"))



# Treatment based on single most recent date ------------------------------

## Extract rows of most recent treatment ----------------------------------

# Separate out the rows with most recent treatment polygon(s)
most.recent <- trt.pre.filtered %>% 
  group_by(PrimaryKey) %>% 
  filter(comp_date_est == max(comp_date_est)) %>% 
  mutate(most_recent_trt_count = n()) %>% 
  ungroup()
  
count(most.recent, most_recent_trt_count) %>% 
  arrange(desc(n))

# Separate points that have overlapping polygons with multiple/same comp_date_est 
#   and different Trt_Type_Sub
most.recent.multiple <- most.recent %>% 
  arrange(Trt_Type_Sub) %>% 
  group_by(PrimaryKey, LDCpointID) %>%
  summarise(
    treatments_sub = paste(unique(Trt_Type_Sub), collapse = ", "),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(sub_count = str_count(treatments_sub, ",") + 1) %>% 
  filter(sub_count > 1)

# Examine possible Trt_Type_Sub combos
most.recent.multiple.types <- most.recent.multiple %>% 
  select(-PrimaryKey, -LDCpointID) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(treatments_sub) %>% 
  arrange(sub_count)

count(most.recent.multiple, treatments_sub) %>% 
  arrange(desc(n)) %>% 
  print(n = 20)



# Treatments within 1 year of most recent ---------------------------------

## Extract rows of recent treatment combos --------------------------------

# Extract the most recent treatment polygon(s) for each LDC point, and polygons within 1 year
#   of most recent
recent.combo <- trt.pre.filtered %>% 
  group_by(PrimaryKey, LDCpointID) %>%
  mutate(most_recent_comp = max(comp_date_est)) %>%
  filter(comp_date_est >= most_recent_comp - years(1)) %>%
  mutate(recent_trt_count = n()) %>%
  ungroup() %>%
  arrange(Trt_Type_Sub) %>% 
  arrange(desc(comp_date_est)) %>% 
  arrange(LDCpointID)

# Separate out polygons with no overlaps of recent treatments 
#   (treatment info is fine as is)
recent.no.overlap <- recent.combo %>% 
  filter(recent_trt_count == 1)

# Separate out polygons with overlaps (multiple recent treatments)
recent.overlaps <- recent.combo %>% 
  filter(recent_trt_count > 1) 


## Create treatment combinations ------------------------------------------

# Build hierarchy table (all most recent)
recent.trt.table.all <- recent.combo %>% 
  distinct(Trt_Type_Major, Trt_Type_Sub, Treatment_Type) %>% 
  arrange(Treatment_Type) %>% 
  arrange(Trt_Type_Sub) %>% 
  arrange(Trt_Type_Major)

#   Check for matching lengths to verify hierarchy (should have only one row per Treatment_Type)
length(unique(recent.combo$Treatment_Type)) == nrow(recent.trt.table.all)

# Build hierarchy table (overlapping treatments only)
recent.trt.table.overlap <- recent.overlaps %>% 
  distinct(Trt_Type_Major, Trt_Type_Sub, Treatment_Type) %>% 
  arrange(Treatment_Type) %>% 
  arrange(Trt_Type_Sub) %>% 
  arrange(Trt_Type_Major)


# Begin assembling treatment combos based on Trt_Type_Sub
#   (most recent treatment listed first; multiple most recent treatments are alphabetized)
recent.combo.sub <- recent.overlaps %>% 
  select(LDCpointID, PrimaryKey, Trt_Type_Sub, comp_date_est, most_recent_comp, recent_trt_count) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Trt_Type_Sub) %>% 
  arrange(desc(comp_date_est)) %>% 
  group_by(LDCpointID, PrimaryKey) %>%
  summarise(
    combo_sub = paste(unique(Trt_Type_Sub), collapse = ", "),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(sub_count = str_count(combo_sub, ",") + 1) %>% 
  filter(sub_count > 1) %>% 
  arrange(LDCpointID)

# Examine combo options
combo.sub.types <- recent.combo.sub %>% 
  select(-PrimaryKey, -LDCpointID) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(combo_sub) %>% 
  arrange(sub_count)

count(recent.combo.sub, combo_sub) %>% 
  arrange(desc(n)) %>% 
  print(n = 20)



# Apply combo selection ---------------------------------------------------

# Based on the combinations with the highest number of points both when considering
#   recent treatments (with one year buffer) and most recent treatment based on single date,
#   I have picked 8 treatment combinations.

# Will consider the following combos:
#   (most recent treatment listed first; multiple most recent treatments are alphabetized)
# 1. Aerial Seeding, Drill Seeding
# 2. Drill Seeding, Aerial Seeding
# 3. Aerial Seeding, Soil Disturbance
# 4. Drill Seeding, Soil Disturbance
# 5. Ground Seeding, Soil Disturbance
# 6. Seeding, Soil Disturbance
# 7. Aerial Seeding, Herbicide
# 8. Drill Seeding, Herbicide

combo.selection <- c("Aerial Seeding, Drill Seeding", "Drill Seeding, Aerial Seeding",
                     "Aerial Seeding, Soil Disturbance", "Drill Seeding, Soil Disturbance", 
                     "Ground Seeding, Soil Disturbance", "Seeding, Soil Disturbance", 
                     "Aerial Seeding, Herbicide", "Drill Seeding, Herbicide")


## Separate out points with selected combos -------------------------------

# Separate out points with selected combos (of points with any combo)
combos.select <- recent.combo.sub %>% 
  filter(combo_sub %in% combo.selection) 

# Assign combo_major
combos.select <- combos.select %>% 
  mutate(combo_major = case_when(
    combo_sub %in% c("Aerial Seeding, Drill Seeding", 
                     "Drill Seeding, Aerial Seeding")  ~ "Seeding",
    combo_sub %in% c("Aerial Seeding, Soil Disturbance",
                     "Drill Seeding, Soil Disturbance", 
                     "Ground Seeding, Soil Disturbance",
                     "Seeding, Soil Disturbance") ~ " Seeding, Vegetation/Soil Manipulation",
    combo_sub %in% c("Aerial Seeding, Herbicide", 
                     "Drill Seeding, Herbicide") ~ "Seeding, Herbicide/Weeds/Chemical"
  ))

# Join to get other cols
combos.select <- combos.select %>% 
  left_join(recent.overlaps)

# Save version that retains all rows
combos.select.allrows <- combos.select 

# Retain only combo cols and remove instances of multiple rows
#   (now ready for eventual bind_rows())
combos.select <- combos.select %>% 
  select(-TrtPolyID, -Prj_ID, -Trt_ID, -init_date_est, -comp_date_est,
         -Trt_Type_Major, -Trt_Type_Sub, -Treatment_Type, -recent_trt_count) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Trt_Type_Sub = combo_sub,
         Trt_Type_Major = combo_major,
         recent_trt_count = sub_count) %>% 
  select(LDCpointID, PrimaryKey, ProjKey, DateVisted, EcoSiteID, MLRADesc, MLRASym,
         Trt_Type_Major, Trt_Type_Sub, most_recent_comp, recent_trt_count)



## Combine all other points without selected combos -----------------------

# Get list of all other points without selected combos
non.combo <- trt.pre.filtered %>% 
  filter(!PrimaryKey %in% combos.select$PrimaryKey)

# Extract rows with most recent treatment polygon(s)
non.combo.mr <- non.combo %>% 
  group_by(PrimaryKey) %>% 
  filter(comp_date_est == max(comp_date_est)) %>% 
  mutate(most_recent_trt_count = n()) %>% 
  ungroup()

# Separate out points with only one most recent treatment
#   (treatment info is fine as is)
non.combo.mr.single <- non.combo.mr %>% 
  filter(most_recent_trt_count == 1)

# Standardize cols for eventual bind_rows()
#   (now ready for eventual bind_rows())
non.combo.mr.single <- non.combo.mr.single %>% 
  mutate(most_recent_comp = comp_date_est,
         recent_trt_count = 1) %>% 
  select(LDCpointID, PrimaryKey, ProjKey, DateVisted, EcoSiteID, MLRADesc, MLRASym,
         Trt_Type_Major, Trt_Type_Sub, most_recent_comp, recent_trt_count)   


# Separate out points with multiple most recent treatments
non.combo.mr.multiple <- non.combo.mr %>% 
  filter(most_recent_trt_count > 1)


### Combinations from single most recent date -----------------------------

#### Same Trt_Type_Sub ----------------------------------------------------

# Separate points that have overlapping polygons with multiple/same comp_date_est 
#   and the same Trt_Type_Sub
non.combo.same <- non.combo.mr.multiple %>% 
  arrange(Trt_Type_Sub) %>% 
  group_by(PrimaryKey, LDCpointID) %>%
  summarise(
    treatments_sub = paste(unique(Trt_Type_Sub), collapse = ", "),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(sub_count = str_count(treatments_sub, ",") + 1) %>% 
  filter(sub_count == 1)


# Join to get other cols
non.combo.same <- non.combo.same %>% 
  left_join(non.combo.mr.multiple)

# Remove some cols and instances of multiple rows
#   (now ready for eventual bind_rows())
non.combo.same <- non.combo.same %>% 
  mutate(most_recent_comp = comp_date_est,
         recent_trt_count = 1) %>% 
  select(LDCpointID, PrimaryKey, ProjKey, DateVisted, EcoSiteID, MLRADesc, MLRASym,
         Trt_Type_Major, Trt_Type_Sub, most_recent_comp, recent_trt_count) %>% 
  distinct(.keep_all = TRUE)


#### Different Trt_Type_Sub -----------------------------------------------

# Separate points that have overlapping polygons with multiple/same comp_date_est 
#   and different Trt_Type_Sub
non.combo.diffsub <- non.combo.mr.multiple %>% 
  arrange(Trt_Type_Sub) %>% 
  group_by(PrimaryKey, LDCpointID) %>%
  summarise(
    treatments_sub = paste(unique(Trt_Type_Sub), collapse = ", "),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(sub_count = str_count(treatments_sub, ",") + 1) %>% 
  filter(sub_count > 1)

# Examine possible Trt_Type_Sub combos
non.combo.types <- non.combo.diffsub %>% 
  select(-PrimaryKey, -LDCpointID) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(treatments_sub) %>% 
  arrange(sub_count)

count(non.combo.diffsub, treatments_sub) %>% 
  arrange(desc(n)) %>% 
  print(n = 20)


# Apply (arbitrary) selection preference for diff sub 

# OUTPUT: Treatment combos that need to be narrowed to just 1 treatment
write_csv(non.combo.types,
          file = "data/data-wrangling-intermediate/05a_output1_conflicting-multiple-treatments.csv")

# EDITED: Combinations resolved to a single treatment
#   see notes tab for preference rules (mostly they are arbitrary)
diffsub.resolved <- read_xlsx("data/data-wrangling-intermediate/05b_edited1_conflicting-multiple-treatments_fixed.xlsx",
                              sheet = "05a_output1_conflicting-multipl")

# Join to get other cols
non.combo.diffsub.resolved <- non.combo.diffsub %>% 
  left_join(diffsub.resolved) %>% 
  left_join(non.combo.mr.multiple)

# Remove some cols and instances of multiple rows
non.combo.diffsub.resolved <- non.combo.diffsub.resolved %>% 
  mutate(most_recent_comp = comp_date_est,
         recent_trt_count = 1) %>% 
  select(-TrtPolyID, -Prj_ID, -Trt_ID, -init_date_est, -comp_date_est,
         -Trt_Type_Major, -Trt_Type_Sub, -Treatment_Type, -sub_count) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Trt_Type_Sub = sub_selected)


# Create Trt_Type_Major col and reorder cols
#   (now ready for eventual bind_rows())
major.join <- recent.trt.table.all %>% 
  select(-Treatment_Type) %>% 
  distinct(.keep_all = TRUE)

non.combo.diffsub.resolved <- non.combo.diffsub.resolved %>% 
  left_join(major.join) %>% 
  select(LDCpointID, PrimaryKey, ProjKey, DateVisted, EcoSiteID, MLRADesc, MLRASym,
         Trt_Type_Major, Trt_Type_Sub, most_recent_comp, recent_trt_count)


## Combine all ------------------------------------------------------------

# Combine all
with.combos <- combos.select %>% 
  bind_rows(non.combo.mr.single) %>% 
  bind_rows(non.combo.same) %>% 
  bind_rows(non.combo.diffsub.resolved) %>% 
  arrange(LDCpointID)

# Check for single instance of primary keys
length(unique(with.combos$PrimaryKey)) == nrow(with.combos)

# Check for missing primary keys from trt.pre.filtered
setdiff(trt.pre.filtered$PrimaryKey, with.combos$PrimaryKey)


# Additional primary keys -------------------------------------------------

# Identify missing primary keys from original df of points with treatment polygons
setdiff(ldc.trt.overlap$PrimaryKey, with.combos$PrimaryKey)
setdiff(ldc.trt.overlap$PrimaryKey, trt.pre.filtered$PrimaryKey)

unique(setdiff(ldc.trt.overlap$PrimaryKey, with.combos$PrimaryKey) ==
         setdiff(ldc.trt.overlap$PrimaryKey, trt.pre.filtered$PrimaryKey))
# The same primary keys missing from with.combos are missing from trt.pre.filtered;
#   these are points that only overlapped with a "Cultural Protection" or
#   "Other" treatment polygon, or where the treatment was applied after the LDC DateVisted.


# Assign these points all NAs for treatment cols, and standardize cols for bind_rows()
other.cultural.post <- ldc.trt.overlap %>% 
  filter(PrimaryKey %in% setdiff(ldc.trt.overlap$PrimaryKey, with.combos$PrimaryKey)) %>% 
  mutate(Trt_Type_Major = NA,
         Trt_Type_Sub = NA,
         most_recent_comp = NA,
         recent_trt_count = NA) %>% 
  select(LDCpointID, PrimaryKey, ProjKey, DateVisted, EcoSiteID, MLRADesc, MLRASym,
         Trt_Type_Major, Trt_Type_Sub, most_recent_comp, recent_trt_count) %>% 
  distinct(.keep_all = TRUE)


# Make df of LDC points that don't overlap with treatment polygons with 
#   standardized cols for bind_rows()
ldc.control <- ldc.trt.sjoin %>% 
  filter(is.na(TrtPolyID)) %>% 
  mutate(most_recent_comp = NA,
         recent_trt_count = NA) %>% 
  select(LDCpointID, PrimaryKey, ProjKey, DateVisted, EcoSiteID, MLRADesc, MLRASym,
         Trt_Type_Major, Trt_Type_Sub, most_recent_comp, recent_trt_count)

#   Check there are no duplicate primary keys
count(ldc.control, PrimaryKey) %>% 
  arrange(desc(n))


# bind_rows() to make LDC_points_v002 -------------------------------------

# Combine
ldcpts002 <- with.combos %>% 
  bind_rows(other.cultural.post) %>% 
  bind_rows(ldc.control) %>% 
  arrange(LDCpointID)

# Check that all primary keys are there
setdiff(ldc.trt.sjoin.raw$PrimaryKey, ldcpts002$PrimaryKey)

# Check there are no duplicate primary keys
count(ldcpts002, PrimaryKey) %>% 
  arrange(desc(n))


# Write v002 to CSV -------------------------------------------------------

write_csv(ldcpts002,
          file = "data/versions-from-R/05_LDC-points_v002.csv")


save.image("RData/05_LDC-points_v002.RData")
