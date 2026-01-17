# Created: 2026-01-16
# Updated: 2026-01-16

# Purpose: Create the equivalent of a Treatment_info table (GIS join cols only) for the 
#   prescribed fires identified in the USGS Combined Wildland Fire Dataset that are
#   missing from the LTDL dataset (similar to 05.6.R from Project v01).

library(tidyverse)

# Load data ---------------------------------------------------------------

pf.missing.raw <- read_csv("data/GIS-exports/003_Prescribed-fires-missing_export.csv")
treatment.info.001.gisjoin <- read_csv("data/versions-from-R/04_Treatment-info_v001-gisjoin.csv")
treatment.info.001 <- read_csv("data/versions-from-R/04_Treatment-info_v001.csv")


# Assign Trt_ID and TrtPolyID ---------------------------------------------

# Figure out range of current Trt_ID from LTDL
range(treatment.info.001$Trt_ID)

# Assign Trt_ID beyond current range
#   Use ID of PrescribedFires003, because that is the ID of original polygons
pf.missing <- pf.missing.raw %>% 
  mutate(Trt_ID = FID_PrescribedFires003 + 73350)


# Based on TrtPoly001, the max TrtPolyID value is 30397

# Assign Trt_ID beyond current range
#   Use ID of PrescribedFires003, because that is the ID of original polygons
pf.missing <- pf.missing.raw %>% 
  mutate(TrtPolyID = FID_PrescribedFires003 + 30397)


# Assign treatment --------------------------------------------------------

# Major
treatment.info.001 %>% 
  filter(str_detect(Trt_Type_Major, "Burn")) %>% 
  select(Trt_Type_Major) %>% 
  unique()

# Sub
treatment.info.001 %>% 
  filter(str_detect(Trt_Type_Sub, "Burn")) %>% 
  select(Trt_Type_Sub) %>% 
  unique()

# Treatment_Type
treatment.info.001 %>% 
  filter(str_detect(Treatment_Type, "Burn")) %>% 
  select(Treatment_Type) %>% 
  unique()

# Assign Major, Sub, and Treatment_Type
pf.missing <- pf.missing %>% 
  mutate(Trt_Type_Major = "Prescribed Burn",
         Trt_Type_Sub = "Prescribed Burn",
         Treatment_Type = "Prescribed Burn")

# Save intermediate
pf.missing1 <- pf.missing


# Inspect date columns ----------------------------------------------------

# Inspect Listed_Fire_Dates
unique(pf.missing$Listed_Fire_Dates)

# Split into multiple columns
pf.missing <- pf.missing1 %>% 
  separate_wider_delim(Listed_Fire_Dates, 
                       delim = " | ",
                       names = c("DateA", "DateB", "DateC"),
                       too_few = "align_start",
                       cols_remove = FALSE)

# Split again to separate date vs type of date
pf.missing <- pf.missing %>% 
  separate_wider_delim(cols = c(DateA, DateB, DateC),
                       delim = ": ",
                       names_sep = "")

# Inspect A, B, and C
unique(pf.missing$DateA1)
unique(pf.missing$DateB1)
unique(pf.missing$DateC1) # do not need listed upload date


# Fires with end date in col A
end.date.A <- pf.missing %>% 
  filter(DateA1 == "Listed Prescribed Fire End Date(s)")

#   Remove other date cols and rename A
end.date.A <- end.date.A %>% 
  select(-DateA1, -DateB1, -DateB2, -DateC1, -DateC2) %>% 
  rename(Rx_End_Date = DateA2)

# Fires with end date in col B
end.date.B <- pf.missing %>% 
  filter(DateB1 == "Listed Prescribed Fire End Date(s)")

#   Remove other date cols and rename B
end.date.B <- end.date.B %>% 
  select(-DateA1, -DateA2, -DateB1, -DateC1, -DateC2) %>% 
  rename(Rx_End_Date = DateB2)

# Combine A & B
end.date <- end.date.A %>% 
  bind_rows(end.date.B)

# Fires with a single end date
end.date.single <- end.date %>% 
  filter(!str_detect(Rx_End_Date, ",")) %>% 
  mutate(Rx_End_Date = str_sub(Rx_End_Date, 1, 10)) %>% 
  mutate(Rx_End_Date = as.Date(Rx_End_Date))

# Fires with multiple end dates
end.date.multiple <- end.date %>% 
  filter(str_detect(Rx_End_Date, ","))


# Fires without an end date
end.date.missing <- pf.missing %>% 
  filter(!TrtPolyID %in% end.date$TrtPolyID)
