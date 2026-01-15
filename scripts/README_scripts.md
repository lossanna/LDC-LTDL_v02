# README for `scripts/`

Created: 2026-01-15  
Updated: 2026-01-15

A list of scripts, including their purpose, output files (includes entire file path), and any input files created from ArcGIS geoprocessing (from `data/GIS-exports/` folder).


## Directory
### `01_treatment-info_v001.R`
- <u>Purpose:</u> Write out Treatment Info v000, which has fixed the issues created by overflow text of a single cell (a problem of Excel).
- <u>Outputs:</u>
    - `data/versions-from-R/01_Treatment-info_v000.csv`


### `02_LDC-to-shapefile.R`
- <u>Purpose:</u> Write the LDC points to shapefiles.
- <u>Outputs:</u>
    - Entire `data/LDC-points/` folder, which includes `03_LDC-points.shp`


### `03_treatment-info_v001.R`
- <u>Purpose:</u> Complete initial data cleaning to create Treatment Info v001.
-  Filtered for polygons, implemented plans, confirmed features only; also cleaned dates.
- <u>Outputs:</u>
    - `data/versions-from-R/03_Treatment-info_v001.csv`
    - `data/versions-from-R/03_Treatment-info_v001-gisjoin.csv`
    - `data/data-wrangling-intermediate/03_treatment-hierarchy-table_v001.csv`


