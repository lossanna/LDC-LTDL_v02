# Created: 2026-02-17
# Updated: 2026-02-18

# Purpose: Examine other columns from Treatment_info for relevant treatments.

library(tidyverse)

# Load data ---------------------------------------------------------------

ldc.eco3.sj.raw <- read_csv("data/GIS-exports/007_LDC004-EcoEPA3_SpatialJoin_export.csv")
trtpolyid.004 <- read_csv("data/versions-from-R/07.2_TrtPolyID-for-treatment-info-v004.csv")
treatment.info.001 <- read_csv("data/versions-from-R/04_Treatment-info_v001.csv")


# Create LDC v007 ---------------------------------------------------------

# Rename cols for EPA-defined ecoregions
ldc.007 <- ldc.eco3.sj.raw %>% 
  rename(EcoEPA1 = NA_L1NAME,
         EcoEPA2 = NA_L2NAME,
         EcoEPA3 = NA_L3NAME) %>% 
  select(-Join_Count, -JOIN_FID, -TARGET_FID)

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


# Break down by Ecoregion 3 (EPA defined) ---------------------------------

# Treatment info cols to join
trt007.join <- treatment.info.007 %>% 
  select(-Prj_ID, -Plan_Imp, -Dates_Confirmed, -Init_Date, -Comp_Date, -Units,
         -Num_Units, -Trt_Feature_Type, -Feature_Status, -How_Feature_Created, 
         -Feature_Creation_Date, -Total_Acres, -BLM_Acres, -GIS_Notes, -Initiated_By,
         -User_Updated, -init_m, -init_d, -init_y, -init_m_est, -init_d_est,
         -init_est, -comp_m, -comp_d, -comp_y, -comp_m_est, -comp_d_est, -comp_est,
         -init_comp_elapsed)

# LDC points with EPA ecoregions
ldc.join <- ldc.007 %>% 
  select(LDCpointID, EcoEPA1, EcoEPA2, EcoEPA3, Category, EcoLvl1, EcoLvl2, EcoLvl3)

# Join to match EPA ecoregion with detailed treatment info cols
ldc.trt.007 <- trtpolyid.004 %>% 
  left_join(ldc.join) %>% 
  left_join(trt007.join)

trt.007.joined <- ldc.trt.007 %>% 
  select(-LDCpointID, -PrimaryKey) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Trt_Type_Sub) %>% 
  arrange(Trt_Type_Major) %>% 
  arrange(Category) %>% 
  arrange(EcoEPA3) %>% 
  arrange(EcoEPA2) %>% 
  arrange(EcoEPA1) %>% 
  filter(Category != "Control, post-burn") %>% 
  filter(!is.na(Trt_Type_Sub)) # does not include missing prescribed burns that were later added



# Write to CSV ------------------------------------------------------------

write_csv(trt.007.joined,
          file = "data/versions-from-R/11_Treatment-info-with-ecoregion_v007.csv",
          na = "")
