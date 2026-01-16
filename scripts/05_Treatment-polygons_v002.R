# Created: 2026-01-15
# Updated: 2026-01-15

# Purpose: Identify treatment polygons that overlap with LDC points, but have a 
#   comp_est_date after the LDC DateVisted to create Treatment_polygons_v002.

# Filter out rows where comp_est_date > DateVisted.
# Filter out "Cultural Protection" and "Other" treatments (based on Trt_Type_Major).

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.trt.sjoin <- read_csv("data/GIS-exports/002_LDC001-Trt001_SpatialJoin_export.csv")


# Find instances where comp_est_date > DateVisted -------------------------

# Convert date cols
ldc.trt.sjoin <- ldc.trt.sjoin %>% 
  mutate(DateVisted = as.Date(DateVisted, format = "%m/%d/%Y"),
         init_date_est = as.Date(init_date_est, format = "%m/%d/%Y"),
         comp_date_est = as.Date(comp_date_est, format = "%m/%d/%Y"))

# Find instances where comp_date_est <= Date_Visted
trt.pre <- ldc.trt.sjoin %>% 
  filter(comp_date_est <= DateVisted)



# Filter out "Other" and "Cultural Protection" ----------------------------

# Remove because these treatments are unlikely to affect veg, or categories are too broad
ldc.trt <- trt.pre %>% 
  filter(!Trt_Type_Major %in% c("Cultural Protection", "Other"))



# Treatment based on single most recent date ------------------------------

## Extract rows of most recent treatment ----------------------------------

# Separate out the rows with most recent overlapping treatment polygon(s)
most.recent <- trt.pre %>% 
  group_by(PrimaryKey) %>% 
  filter(comp_date_est == max(comp_date_est)) %>% 
  mutate(most_recent_trt_count = n()) %>% 
  ungroup()
  
count(most.recent, most_recent_trt_count) %>% 
  arrange(desc(n))

# Separate overlapping polygons with multiple/same comp_date_est and different Trt_Type_Sub
most.recent.multiple <- most.recent %>% 
  arrange(Trt_Type_Sub) %>% 
  group_by(PrimaryKey) %>%
  summarise(
    treatments_sub = paste(unique(Trt_Type_Sub), collapse = ", "),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  mutate(sub_count = str_count(treatments_sub, ",") + 1) %>% 
  filter(sub_count > 1)

# Examine possible Trt_Type_Sub combos
most.recent.multiple.types <- most.recent.multiple %>% 
  select(-PrimaryKey) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(treatments_sub) %>% 
  arrange(sub_count)

count(most.recent.multiple, treatments_sub) %>% 
  arrange(desc(n)) %>% 
  print(n = 20)

