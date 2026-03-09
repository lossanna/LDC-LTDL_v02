# Created: 2026-03-09
# Updated: 2026-03-09

# Purpose: Collate the results of the bulk point query service, and ultimately assign
#   an elevation value to each LDC point.

library(tidyverse)


# Load data ---------------------------------------------------------------

# Original list
ldc008.raw <- read_csv("data/data-wrangling-intermediate/14.1_LDC-points-v008-coordinates.csv")

# Results CSVs
path <- "data/data-wrangling-intermediate/14.2_results_bulk-point-query-service"

files <- list.files(path,
                    pattern = "bulk-pqs_chunk.*\\.csv$",
                    full.names = TRUE)

names(files) <- str_extract(files, "chunk\\d+-\\d+") %>%
  str_replace("-", ".")

combined.chunks.csvs <- files %>%
  map(read_csv) 


# Data wrangling ----------------------------------------------------------

# Without duplicates
combined.chunks <- combined.chunks.csvs %>% 
  map(\(df) filter(df, `Elev(ft)` != -9999)) %>%
  split(str_extract(names(files), "\\d+") %>% 
          str_pad(width = 3, side = "left", pad = "0") %>% 
          paste0("chunk", .)) %>%
  map(bind_rows) %>%
  map(\(df) df %>% 
        select(-ID, -`Elev(ft)`) %>% 
        rename(Elev_m = `Elev(m)`,
               InputLon = `Input Lon`,
               InputLat = `Input Lat`) %>% 
        distinct(.keep_all = TRUE) %>% 
        group_by(InputLon, InputLat) %>% 
        slice_head(n = 1))


# With duplicates
combined.chunks.dup <- combined.chunks.csvs %>% 
  map(\(df) filter(df, `Elev(ft)` != -9999)) %>%
  split(str_extract(names(files), "\\d+") %>% 
          str_pad(width = 3, side = "left", pad = "0") %>% 
          paste0("chunk", .)) %>%
  map(bind_rows) %>%
  map(\(df) df %>% 
        select(-ID, -`Elev(ft)`) %>% 
        rename(Elev_m = `Elev(m)`,
               InputLon = `Input Lon`,
               InputLat = `Input Lat`) %>% 
        distinct(.keep_all = TRUE))

combined.dup <- bind_rows(combined.chunks.dup) %>% 
  summarise(difference = max(Elev_m) - min(Elev_m),
            .by = c(InputLon, InputLat))
summary(combined.dup$difference)





