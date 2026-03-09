# Created: 2026-03-04
# Updated: 2026-03-04

# Purpose: Create a table with the extra treatment info cols for LDC points v007.

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.007.raw <- read_csv("data/versions-from-R/12_LDC-points-eco3-trt30_v007.csv")
trtpolyid.004 <- read_csv("data/versions-from-R/07.2_TrtPolyID-for-treatment-info-v004.csv")
treatment.info.001 <- read_csv("data/versions-from-R/04_Treatment-info_v001.csv")

# Create Treatment info v007 ----------------------------------------------

# Separate out relevant treatments
treatment.info.007 <- treatment.info.001 %>% 
  filter(Trt_ID %in% trtpolyid.004$Trt_ID)

# Replace instances of "\r\n" with space
treatment.info.007[] <- lapply(treatment.info.007, function(col) {
  if (is.character(col)) {
    gsub("\\\\r\\\\n", " ", col)
  } else {
    col
  }
})

# Treatment info cols for join
trt007.join <- treatment.info.007 %>% 
  select(-Prj_ID, -Plan_Imp, -Dates_Confirmed, -Init_Date, -Comp_Date, -Units,
         -Num_Units, -Trt_Feature_Type, -Feature_Status, -How_Feature_Created, 
         -Feature_Creation_Date, -Total_Acres, -BLM_Acres, -GIS_Notes, -Initiated_By,
         -User_Updated, -init_m, -init_d, -init_y, -init_m_est, -init_d_est,
         -init_est, -comp_m, -comp_d, -comp_y, -comp_m_est, -comp_d_est, -comp_est,
         -init_comp_elapsed)


# Join points with treatment info cols ------------------------------------

## Multiple most recent treatments ----------------------------------------

# Separate out points with multiple most recent treatments
ldc.007.trt.multiple <- ldc.007.raw %>% 
  filter(recent_trt_count >= 2) %>% 
  select(EcoEPA3, Category, Trt_Type_Sub, MR_trt_comp, LDCpointID, recent_trt_count) %>% 
  rename(Trt_Type_Sub_new = Trt_Type_Sub)

# Join to get TrtPolyID
ldc.007.trt.multiple <- ldc.007.trt.multiple %>% 
  left_join(trtpolyid.004)

# Join treatment info cols
ldc.007.trt.multiple <- ldc.007.trt.multiple %>% 
  left_join(trt007.join) %>% 
  select(-LDCpointID, -PrimaryKey) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Trt_ID) %>% 
  arrange(Trt_Type_Sub_new) %>% 
  arrange(Category) %>% 
  arrange(EcoEPA3)



# Single most recent treatment --------------------------------------------

# Separate out points with single most recent treatment
ldc.007.trt.single <- ldc.007.raw %>% 
  filter(recent_trt_count == 1) %>% 
  select(EcoEPA3, Category, Trt_Type_Sub, MR_trt_comp, LDCpointID, recent_trt_count) %>% 
  rename(Trt_Type_Sub_new = Trt_Type_Sub)

# Join to get TrtPolyID
ldc.007.trt.single <- ldc.007.trt.single %>% 
  left_join(trtpolyid.004)

# Join treatment info cols
ldc.007.trt.single <- ldc.007.trt.single %>% 
  left_join(trt007.join) %>% 
  select(-LDCpointID, -PrimaryKey) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Trt_ID) %>% 
  arrange(Trt_Type_Sub_new) %>% 
  arrange(Category) %>% 
  arrange(EcoEPA3)


## Combine ----------------------------------------------------------------

ldc.007.trt <- ldc.007.trt.multiple %>% 
  bind_rows(ldc.007.trt.single) %>% 
  arrange(Trt_ID) %>% 
  arrange(Trt_Type_Sub_new) %>% 
  arrange(Category) %>% 
  arrange(EcoEPA3)


# Write to CSV ------------------------------------------------------------

write_csv(ldc.007.trt,
          file = "data/versions-from-R/13_Treatment-info-extra-cols-for-LDC007.csv",
          na = "")
