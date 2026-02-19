# Created: 2026-02-17
# Updated: 2026-02-18

# Purpose: Check LDC ecoregion categorization against spatial analysis. Also compare
#   EPA vs Esri layer versions of ecoregions.

# Ecoregion layers from EPA vs. Esri are the same.

# All treatment types are the same for both ecoregion versions (EPA vs. LDC) when looking  
#   at groups with at least 25 points or more (some counts vary slightly, but treatment
#   types are still the same).

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.epa3.sj.raw <- read_csv("data/GIS-exports/007_LDC004-EcoEPA3_SpatialJoin_export.csv")
ldc.esri3.sj.raw <- read_csv("data/GIS-exports/007_LDC004-EcoEsri3_SpatialJoin_export.csv")
load("RData/09_LDC-points_v006.RData")


# Compare EPA and Esri layers ---------------------------------------------

# Create Esri version for joining
esri.join <- ldc.esri3.sj.raw %>% 
  select(LDCpointID, NA_L1NAME, NA_L2NAME, NA_L3NAME) %>% 
  rename(NA_L1NAME_esri = NA_L1NAME,
         NA_L2NAME_esri = NA_L2NAME,
         NA_L3NAME_esri = NA_L3NAME)

# Join
ldc.epa.esri <- ldc.epa3.sj.raw %>% 
  left_join(esri.join)

# Level 3
ldc.epa.esri %>% 
  filter(NA_L3NAME_esri != NA_L3NAME) # EPA and ESRI versions are the same


# Compare Level 1 ---------------------------------------------------------

level1.conflicting <- ldc.epa3.sj.raw %>% 
  filter(EcoLvl1 != NA_L1NAME) %>% 
  select(LDCpointID, PrimaryKey, EcoLvl1, NA_L1NAME)

level1.conflicting %>% 
  select(EcoLvl1, NA_L1NAME) %>% 
  distinct(.keep_all = TRUE)


# Compare Level 2 ---------------------------------------------------------

level2.conflicting <- ldc.epa3.sj.raw %>% 
  filter(EcoLvl2 != NA_L2NAME) %>% 
  select(LDCpointID, PrimaryKey, EcoLvl2, NA_L2NAME)

level2.conflicting %>% 
  select(EcoLvl2, NA_L2NAME) %>% 
  distinct(.keep_all = TRUE)


# Compare Level 3 ---------------------------------------------------------

level3.conflicting <- ldc.epa3.sj.raw %>% 
  filter(EcoLvl3 != NA_L3NAME) %>% 
  select(LDCpointID, PrimaryKey, EcoLvl3, NA_L3NAME)

level3.conflicting %>% 
  select(EcoLvl3, NA_L3NAME) %>% 
  distinct(.keep_all = TRUE) %>% 
  print(n = 46)



# Create LDC v007 ---------------------------------------------------------

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



# Count tables ------------------------------------------------------------

# By Ecoregion Level 3 (EPA-defined from spatial analysis)
level3.epa <- ldc.007 %>% 
  group_by(EcoEPA1, EcoEPA2, EcoEPA3) %>% 
  count(Category)

level3.epa.trt <- ldc.007 %>% 
  group_by(EcoEPA1, EcoEPA2, EcoEPA3) %>% 
  count(Trt_Type_Sub)

level3.epa.trt25 <- ldc.007 %>% 
  group_by(EcoEPA1, EcoEPA2, EcoEPA3, Category) %>% 
  count(Trt_Type_Sub) %>% 
  filter(n >= 25,
         !is.na(Trt_Type_Sub)) %>% 
  ungroup()
length(unique(level3.epa.trt25$EcoEPA3)) # 15

level3.epa.trt30 <- ldc.007 %>% 
  group_by(EcoEPA1, EcoEPA2, EcoEPA3, Category) %>% 
  count(Trt_Type_Sub) %>% 
  filter(n >= 30,
         !is.na(Trt_Type_Sub)) %>% 
  ungroup()
length(unique(level3.epa.trt30$EcoEPA3)) # 14

setdiff(level3.epa.trt25, level3.epa.trt30)
setdiff(level3.epa.trt25$EcoEPA3, level3.epa.trt30$EcoEPA3)


# Compare with LDC-assigned Ecoregion 3
level3.epa.trt25$Trt_Type_Sub == level3.trt25$Trt_Type_Sub # all treatment types are the same
level3.epa.trt25$n == level3.trt25$n # a few counts differ
level3.epa.trt25$n - level3.trt25$n
