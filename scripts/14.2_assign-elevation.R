# Created: 2026-03-09
# Updated: 2026-03-10

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
  map(~ vroom::vroom(.x, show_col_types = FALSE))


# Combine chunks ----------------------------------------------------------

# Without duplicates
combined.chunks <- combined.chunks.csvs %>% 
  split(str_extract(names(files), "\\d+") %>% 
          str_pad(width = 3, side = "left", pad = "0") %>% 
          paste0("chunk", .)) %>%
  imap(\(x, nm) bind_rows(x) %>% mutate(chunk = nm)) %>%
  map(\(df) df %>% 
        filter(`Elev(ft)` != -9999) %>% 
        select(-`Elev(ft)`) %>% 
        rename(Elev_m = `Elev(m)`,
               InputLon = `Input Lon`,
               InputLat = `Input Lat`) %>% 
        distinct(.keep_all = TRUE) %>% 
        group_by(chunk, ID) %>% 
        slice_head(n = 1) %>% 
        ungroup())


# With duplicates
combined.chunks.dup <- combined.chunks.csvs %>% 
  split(str_extract(names(files), "\\d+") %>% 
          str_pad(width = 3, side = "left", pad = "0") %>% 
          paste0("chunk", .)) %>%
  imap(\(x, nm) bind_rows(x) %>% mutate(chunk = nm)) %>%
  map(\(df) df %>% 
        filter(`Elev(ft)` != -9999) %>% 
        select(-`Elev(ft)`) %>% 
        rename(Elev_m = `Elev(m)`,
               InputLon = `Input Lon`,
               InputLat = `Input Lat`) %>% 
        distinct(.keep_all = TRUE))


# Join with LDC008 --------------------------------------------------------

# Add ID and chunk cols to LDC008
ldc008.join <- ldc008.raw %>%
  mutate(
    ID = ((row_number() - 1) %% 500) + 1,
    chunk = paste0(
      "chunk",
      str_pad(((row_number() - 1) %/% 500) + 1, width = 3, pad = "0")
    )
  )

# Calculate differences in elevation for the same point
combined.dup.all <- bind_rows(combined.chunks.dup) %>% 
  left_join(ldc008.join) 

combined.dup <- combined.dup.all %>% 
  summarise(difference = max(Elev_m) - min(Elev_m),
            .by = c(LDCpointID, chunk, ID)) %>% 
  ungroup()
summary(combined.dup$difference)

#   Examine differences >3 m
combined.dup.inspect.id <- combined.dup %>% 
  filter(difference > 3)
combined.dup.inspect <- combined.dup %>% 
  filter(LDCpointID %in% combined.dup.inspect.id$LDCpointID) %>% 
  left_join(combined.dup.all) %>% 
  arrange(desc(difference)) %>% 
  select(-InputLon, -InputLat)
count(combined.dup.inspect, chunk) %>% 
  arrange(desc(n)) %>% 
  print(n = 30)


# Join without duplicates
combined.all <- bind_rows(combined.chunks) %>% 
  left_join(ldc008.join)

# Points still missing
points.missing <- ldc008.raw %>% 
  left_join(combined.all) %>% 
  filter(is.na(Elev_m))
