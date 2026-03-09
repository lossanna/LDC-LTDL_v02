# Created: 2026-03-09
# Updated: 2026-03-09

# Purpose: Create a table of the LDC points v008 with their coordinates. Write multiple 
#   separate CSVs with the coordinates (max of 500 rows) to prepare for bulk point query service
#   (108 CSVs in total).

# Bulk point query service for national map: https://apps.nationalmap.gov/bulkpqs/ 


library(tidyverse)

# Load data ---------------------------------------------------------------

ldc008.raw <- read_csv("data/GIS-exports/009_LDC008-with-WGS1984-coordinates_export.csv")

# Coordinates for LDC points v008 -----------------------------------------

# with LDCpointID and PrimaryKey
ldc008.coord <- ldc008.raw %>% 
  select(LDCpointID, PrimaryKey, X_WSG1984, Y_WSG1984) %>% 
  arrange(LDCpointID)

# Coordinates only
ldc008.coord.only <- ldc008.coord %>% 
  select(X_WSG1984, Y_WSG1984)

# As list
coord.list <- split(ldc008.coord.only, ceiling(seq_len(nrow(ldc008.coord.only)) / 500))


# Write to CSV ------------------------------------------------------------

# With LDCpointID and PrimaryKey
write_csv(ldc008.coord,
          file = "data/data-wrangling-intermediate/14.1_LDC-points-v008-coordinates.csv")

# Coordinates only, formatted for bulk query as separate CSVs
for(i in seq_along(coord.list)){
  write_csv(coord.list[[i]], 
            file = paste0("data/data-wrangling-intermediate/14.1_LDC008-coordinate-lists/chunk_", i, ".csv"),
            col_names = FALSE)
}
