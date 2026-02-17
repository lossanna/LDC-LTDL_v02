# Created: 2026-01-26
# Updated: 2026-01-29

# Purpose: Create LDC points v006, with versions that both have 25+ points per treatment,
#   and 30+ points per treatment

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.004 <- read_csv("data/versions-from-R/07.3_LDC-points_v004.csv")


# Create v006 -------------------------------------------------------------

# Change instances of "UPPER GILA MOUNTAINS (?)" to "UPPER GILA MOUNTAINS"
#   (I have confirmed in ArcGIS Pro that these points are in fact within the 
#     Upper Gila Mountains ecoregion)
ldc.006 <- ldc.004 %>% 
  mutate(
    EcoLvl2 = str_replace(EcoLvl2,
                          "UPPER GILA MOUNTAINS \\(\\?\\)",
                          "UPPER GILA MOUNTAINS")
  )


# Remove treatment info for post-burn control situations of TRT -> Fire -> LDC
ldc.006 <- ldc.006 %>% 
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

# By Ecoregion Level 3
level3 <- ldc.006 %>% 
  group_by(EcoLvl1, EcoLvl2, EcoLvl3) %>% 
  count(Category)

level3.trt <- ldc.006 %>% 
  group_by(EcoLvl1, EcoLvl2, EcoLvl3) %>% 
  count(Trt_Type_Sub)

level3.trt25 <- ldc.006 %>% 
  group_by(EcoLvl1, EcoLvl2, EcoLvl3, Category) %>% 
  count(Trt_Type_Sub) %>% 
  filter(n >= 25,
         !is.na(Trt_Type_Sub)) %>% 
  ungroup()
length(unique(level3.trt25$EcoLvl3)) # 15

level3.trt30 <- ldc.006 %>% 
  group_by(EcoLvl1, EcoLvl2, EcoLvl3, Category) %>% 
  count(Trt_Type_Sub) %>% 
  filter(n >= 30,
         !is.na(Trt_Type_Sub)) %>% 
  ungroup()
length(unique(level3.trt30$EcoLvl3)) # 15

setdiff(level3.trt25, level3.trt30)
setdiff(level3.trt25$EcoLvl3, level3.trt30$EcoLvl3)



# LDC points by Ecoregion 3 -----------------------------------------------

# LDC points with at least 30 points per treatment group
eco3.trt30 <- level3.trt30 %>% 
  left_join(ldc.006)

#   Control equivalents
eco3.trt30.ctrl.count <- ldc.006 %>% 
  filter(EcoLvl3 %in% eco3.trt30$EcoLvl3 & str_detect(Category, "Control")) %>% 
  group_by(EcoLvl3, Category) %>% 
  summarise(n = n(),
            .groups = "keep") %>% 
  ungroup()

eco3.trt30.ctrl <- ldc.006 %>% 
  filter(EcoLvl3 %in% eco3.trt30$EcoLvl3 & str_detect(Category, "Control")) %>% 
  left_join(eco3.trt30.ctrl.count)

#   Combine & order cols
eco3.trt30.all <- eco3.trt30 %>% 
  bind_rows(eco3.trt30.ctrl) %>% 
  select(EcoLvl3, Category, Trt_Type_Sub, n, MR_trt_comp, LDCpointID, DateVisted, 
         ProjKey, PrimaryKey, EcoSiteID, MLRADesc, MLRASym, EcoLvl1, EcoLvl2, EcoLvl4, 
         State, MODIS_IGBP,
         Trt_Type_Major, recent_trt_count, FirePolyID, USGS_Assigned_ID, MR_wildfire,
         Fire_freq, Fire_freq_post_trt) %>% 
  rename(Treatment_count = n) %>% 
  arrange(LDCpointID)
nrow(eco3.trt30.all) / nrow(ldc.006) # 90% of points used


# LDC points with at least 25 points per treatment group
eco3.trt25 <- level3.trt25 %>% 
  left_join(ldc.006)

#   Control equivalents
eco3.trt25.ctrl.count <- ldc.006 %>% 
  filter(EcoLvl3 %in% eco3.trt25$EcoLvl3 & str_detect(Category, "Control")) %>% 
  group_by(EcoLvl3, Category) %>% 
  summarise(n = n(),
            .groups = "keep") %>% 
  ungroup()

eco3.trt25.ctrl <- ldc.006 %>% 
  filter(EcoLvl3 %in% eco3.trt25$EcoLvl3 & str_detect(Category, "Control")) %>% 
  left_join(eco3.trt25.ctrl.count)

#   Combine & order cols
eco3.trt25.all <- eco3.trt25 %>% 
  bind_rows(eco3.trt25.ctrl) %>% 
  select(EcoLvl3, Category, Trt_Type_Sub, n, MR_trt_comp, LDCpointID, DateVisted, 
         ProjKey, PrimaryKey, EcoSiteID, MLRADesc, MLRASym, EcoLvl1, EcoLvl2, EcoLvl4, 
         State, MODIS_IGBP,
         Trt_Type_Major, recent_trt_count, FirePolyID, USGS_Assigned_ID, MR_wildfire,
         Fire_freq, Fire_freq_post_trt) %>% 
  rename(Treatment_count = n) %>% 
  arrange(LDCpointID)
nrow(eco3.trt25.all) / nrow(ldc.006) # 91% of points used




# Write to CSV ------------------------------------------------------------

write_csv(eco3.trt25.all,
          "data/versions-from-R/09_LDC-points-eco3-trt25_v006.csv")

write_csv(eco3.trt30.all,
          "data/versions-from-R/09_LDC-points-eco3-trt30_v006.csv")


save.image("RData/09_LDC-points_v006.RData")
