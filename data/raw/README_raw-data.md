# README for `data/raw/`

Created: 2026-01-15  
Updated: 2026-01-15

## Notes
### LDC 
- I downloaded the geoindicators data for all of the plots in the western continental US on 2026-01-15.

### LTDL 
Same as Project v01.
- There were issues with the LTDL `Treatment_Info.csv` file because it was probably a table taken directly from ArcGIS Pro, but in Excel there is a limit on the number of a characters a single cell can have, so in a couple of instances when there was too much text for a single cell, a new row was created (and then cells were thereafter delinated by commas in the text).
    - I had to create a new version of the file where the extra row was deleted (`Treatment_Info_R.csv`), as well as additional files to handle the manual fixes (located in the `data-wrangling-intermediate/` folder).

## Data downloads
- Land Treatment Digital Library:
    - Downloaded from: https://doi.org/10.5066/P98OBOLS on 2025-06-24 (v7.0, released Sept 2024; download `LTDL_Sept_2024_Release.zip` to get folder of CSVs).
- Landscape Data Commons:
    - Downloaded from https://landscapedatacommons.org/ldc-map on 2026-01-15.
         - Download of geoindicators for all plots in the western continental US.
            - Select plots on map -> Download -> Indicators


## Directory
- `downloaded/`
    - Raw downloaded files, not altered in any way.
    - `ldc-data-2026-01-15/`
        - Direct download of geoindicators data for all plots, downloaded directly from LDC website as single file.
        - `geoindicators.csv`
        - `table.schema.csv`
    - `LTDL_data_csvs/`
        - `Treatment_Info.csv` (not used because overflow text causes extra rows)
    -   `2026-01-05_Screenshot-of-LDC-plots.png`
- `Treatment_Info_columns.xlsx`
    - A spreadsheet to describe each of the columns in the `Treatment_Info` table.
    - Copied from Project v01.
- `Treatment_Info_R.csv`
    - Manually edited version of `Treatment_Info.csv` (from LTDL CSVs) to delete extra rows created from overflow text cells; created so it is easier to read into R (see `data/data-wrangling-intermediate/01_treatment-info_fix-rows.xlsx`). Still needs to be fixed in R with the `01_treatment-info_fix-rows.csv` file.
    - Copied from Project v01.