# This script downloads Google Sheets for the first time (unless overwritten)
# and stores log of files 


######################################################################
# DO NOT RUN THE SCRIPT ##############################################
# THIS WAS DESIGNED FOR INITIATING LOCAL COPY OF DATA ################
######################################################################


source('00_project_settings.R')

##### METADATA #####

# Download Metadata for the first time ------------------------------------

GetMetadata()


# Create Log of Google Sheets for Site Metadata ---------------------------

# store Google Sheet names, keys and last update info
if (file.exists('Original_Data/Metadata/key_metadata.txt')) {
  print('This file already exists. Overwriting is not allowed!')
} else {
  gs_ls("^[1-3]. ") %>%
    filter(
      sheet_title %in% c(
        "1. Site Information",
        "2. One-Time Plot Data",
        "3. Yearly Treatment Data"
      )
    ) %>%
    select(sheet_title, updated, sheet_key) %>%
    write_csv('Original_Data/Metadata/key_metadata.txt')
}



##### RESEARCH DATA #####

# Creat list of site IDs
read_excel('Original_Data/Metadata/1. Site Information.xlsx', 
           range = cell_cols('A')) %>% 
  slice(-1) %>%
  pull() -> SITES


# Download Research Data for the first time -------------------------------

GetResearchData()


# Create Log of Google Sheets for Research Data ---------------------------

# store Google Sheet names, keys and last update info
if (file.exists('Original_Data/key_data.txt')) {
  print('This file already exists. Overwriting is not allowed!')
} else {
  gs_ls(" Data - ") %>%
    filter(str_detect(sheet_title, paste(SITES, collapse = '|'))) %>%
    select(sheet_title, updated, sheet_key) %>%
    write_csv('Original_Data/key_data.txt')
}


# Download ATEMP files
gs_ls('ATEMP') %>%
  select(1, 5, 6) -> ATEMP_gs

for (i in seq_along(ATEMP_gs$sheet_title)) {
  gs_key(ATEMP_gs$sheet_key[i]) %>%
    gs_download(to = paste0('Original_Data/', 
                            word( ATEMP_gs$sheet_title[i]), '/', 
                            ATEMP_gs$sheet_title[i], '_', 
                            Sys.Date(), '.xlsx'))
  }
