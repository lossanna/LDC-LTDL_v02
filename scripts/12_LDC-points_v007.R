# Created: 2026-02-19
# Updated: 2026-03-05

# Purpose: Create LDC points v007, which is ecoregion 3 defined by EPA shapefiles,
#   and includes only the treatments with 30+ points per treatment.

# Am opting for only the 30+ version (not 25+), because I anticipate some points will be
#   lost through propensity score matching.

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.epa3.sj.raw <- read_csv("data/GIS-exports/007_LDC004-EcoEPA3_SpatialJoin_export.csv")


# Create v007 -------------------------------------------------------------

# Rename cols for EPA-defined ecoregions
ldc.007 <- ldc.epa3.sj.raw %>% 
  rename(EcoEPA1 = NA_L1NAME,
         EcoEPA2 = NA_L2NAME,
         EcoEPA3 = NA_L3NAME)

# Remove treatment info for post-burn control situations of TRT -> Fire -> LDC
ldc.007 <- ldc.007 %>% 
  mutate(
    Trt_Type_Major = case_when(
      Category == "Control, post-burn" ~ NA,
      TRUE ~ Trt_Type_Major),
    Trt_Type_Sub = case_when(
      Category == "Control, post-burn" ~ NA,
      TRUE ~ Trt_Type_Sub),
    MR_trt_comp = case_when(
      Category == "Control, post-burn" ~ NA,
      TRUE ~ MR_trt_comp),
    recent_trt_count = case_when(
      Category == "Control, post-burn" ~ NA,
      TRUE ~ recent_trt_count)
  )

# Convert DateVisted to date col
ldc.007 <- ldc.007 %>% 
  mutate(DateVisted = as.Date(DateVisted, format = "%m/%d/%Y"))


# Count tables ------------------------------------------------------------

# By Ecoregion Level 3 (EPA defined)
level3 <- ldc.007 %>% 
  group_by(EcoEPA1, EcoEPA2, EcoEPA3) %>% 
  count(Category)

level3.trt <- ldc.007 %>% 
  group_by(EcoEPA1, EcoEPA2, EcoEPA3) %>% 
  count(Trt_Type_Sub)

level3.trt30 <- ldc.007 %>% 
  group_by(EcoEPA1, EcoEPA2, EcoEPA3, Category) %>% 
  count(Trt_Type_Sub) %>% 
  filter(n >= 30,
         !is.na(Trt_Type_Sub)) %>% 
  ungroup()
length(unique(level3.trt30$EcoEPA3)) # 14



# LDC points by Ecoregion 3 -----------------------------------------------

# LDC points with at least 30 points per treatment group
eco3.trt30 <- level3.trt30 %>% 
  left_join(ldc.007)

#   Control equivalents
eco3.trt30.ctrl.count <- ldc.007 %>% 
  filter(EcoEPA3 %in% eco3.trt30$EcoEPA3 & str_detect(Category, "Control")) %>% 
  group_by(EcoEPA3, Category) %>% 
  summarise(n = n(),
            .groups = "keep") %>% 
  ungroup()

eco3.trt30.ctrl <- ldc.007 %>% 
  filter(EcoEPA3 %in% eco3.trt30$EcoEPA3 & str_detect(Category, "Control")) %>% 
  left_join(eco3.trt30.ctrl.count)

#   Combine & order cols
eco3.trt30.all <- eco3.trt30 %>% 
  bind_rows(eco3.trt30.ctrl) %>% 
  select(EcoEPA3, Category, Trt_Type_Sub, n, MR_trt_comp, LDCpointID, DateVisted, 
         ProjKey, PrimaryKey, EcoSiteID, MLRADesc, MLRASym, EcoEPA1, EcoEPA2, 
         EcoLvl1, EcoLvl2, EcoLvl3, EcoLvl4, State, MODIS_IGBP,
         Trt_Type_Major, recent_trt_count, FirePolyID, USGS_Assigned_ID, MR_wildfire,
         Fire_freq, Fire_freq_post_trt) %>% 
  rename(Treatment_count = n) %>% 
  arrange(LDCpointID)
nrow(eco3.trt30.all) / nrow(ldc.007) # 89.6% of points used



# Write to CSV ------------------------------------------------------------

# Points
write_csv(eco3.trt30.all,
          file = "data/versions-from-R/12_LDC-points-eco3-trt30_v007.csv",
          na = "")

# Count table
write_csv(level3.trt30,
          file = "data/versions-from-R/12_treatment-count-table.csv")


save.image("RData/12_LDC-points_v007.RData")
