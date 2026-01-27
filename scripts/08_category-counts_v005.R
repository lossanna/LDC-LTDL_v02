# Created: 2026-01-26
# Updated: 2026-01-26

# Purpose:

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.004 <- read_csv("data/versions-from-R/07.3_LDC-points_v004.csv")



# Create v005 -------------------------------------------------------------

# Change instances of "UPPER GILA MOUNTAINS (?)" to "UPPER GILA MOUNTAINS"
#   (I have confirmed in ArcGIS Pro that these points are in fact within the 
#     Upper Gila Mountains ecoregion)
ldc.005 <- ldc.004 %>% 
  mutate(
    EcoLvl2 = str_replace(EcoLvl2,
                          "UPPER GILA MOUNTAINS \\(\\?\\)",
                          "UPPER GILA MOUNTAINS")
  )



# Count tables ------------------------------------------------------------

# All
count(ldc.005, Category)

# By Ecoregion Level 1
level1 <- ldc.005 %>% 
  group_by(EcoLvl1) %>% 
  count(Category)
length(unique(ldc.005$EcoLvl1)) # 6

# By Ecoregion Level 2
level2 <- ldc.005 %>% 
  group_by(EcoLvl1, EcoLvl2) %>% 
  count(Category)
length(unique(ldc.005$EcoLvl2)) # 8

level2.trt <- ldc.005 %>% 
  group_by(EcoLvl1, EcoLvl2) %>% 
  count(Trt_Type_Sub)

level2.trt3 <- ldc.005 %>% 
  group_by(EcoLvl1, EcoLvl2) %>% 
  count(Trt_Type_Sub) %>% 
  filter(n >= 3,
         !is.na(Trt_Type_Sub))

# By Ecoregion Level 3
level3 <- ldc.005 %>% 
  group_by(EcoLvl1, EcoLvl2, EcoLvl3) %>% 
  count(Category)
length(unique(ldc.005$EcoLvl3)) # 32

# By MODIS
modis <- ldc.005 %>% 
  group_by(MODIS_IGBP) %>% 
  count(Category)
length(unique(ldc.005$MODIS_IGBP)) # 12

# By State
state <- ldc.005 %>% 
  group_by(State) %>% 
  count(Category)
length(unique(ldc.005$State)) # 14

# By MLRA
mlra <- ldc.005 %>% 
  group_by(MLRADesc) %>% 
  count(Category)
length(unique(ldc.005$MLRADesc)) # 69
