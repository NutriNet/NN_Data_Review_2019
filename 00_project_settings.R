# Load the main packages
library(readxl)
library(tidyverse)
library(lubridate)
library(ggforce)
library(googlesheets)
library(cowplot)



# download from the google drive and save in the site folder
# this function checks if you have an updated version of Google Sheet before downloading one
DownloadGoogleSheet <- 
  function(GOOGLESHEET, TYPE = 'Data') {
    if (TYPE %in% c('Data', 'Metadata')) {
      if (TYPE == "Data") {
        pass <- 'Original_Data/key_data.txt'
        subfolder <- word(GOOGLESHEET)
        folder <- paste0('Original_Data/', subfolder, '/')
      } else {
        pass <- 'Original_Data/Metadata/key_metadata.txt'
        folder <- 'Original_Data/Metadata/'
      }
      suppressMessages(read_csv(pass)) -> metadata
      metadata %>%
        filter(sheet_title == GOOGLESHEET) -> sheet_metadata
      if (nrow(sheet_metadata) == 1) {
        sheet_key <- gs_key(sheet_metadata$sheet_key)
        if (sheet_key$updated == sheet_metadata$updated) {
          print('You have the latest version')
        } else {
          # downloads file and assigns download date
          sheet_key %>%
            gs_download(
              to = paste0(folder, GOOGLESHEET, '_', Sys.Date(), '.xlsx'),
              overwrite = TRUE
            )
          # updates file that stores info about version dates
          new_update <- sheet_key$updated
          metadata %>%
            mutate(
              updated = ifelse(sheet_title == GOOGLESHEET, new_update, updated),
              updated = as_datetime(updated)
            ) %>%
            write_csv(pass)
        }
      } else {
        print('No matching Google Sheet found')
      }
    } else {
      print('Invalid entry for TYPE')
    }
  }


# Download Google Sheets to update data
UpdateData <- function(DATA_TYPE) {
  SITES <- c("DOUGLAS", "DUDLEY", "KELLEY", "MUDS2", 
             "NWRF", "SUBSURF", "WQFS", "ONT_4R", "TRO")
  DATA_TYPES <- c("Crop", "Soil", "Water", "GHG", "Weather")
  if (DATA_TYPE %in% DATA_TYPES) {
    for (i in seq_along(SITES)) {
      sheet <- paste0(DATA_TYPE, ' Data - ', SITES[i])
      print(paste0('Looking for ', sheet, '...'))
      DownloadGoogleSheet(GOOGLESHEET = sheet)
    }
  } else if (DATA_TYPE %in% c('All', 'ALL', 'all')) {
    for (j in DATA_TYPES) {
      for (i in seq_along(SITES)) {
        sheet <- paste0(j, ' Data - ', SITES[i])
        print(paste0('Looking for ', sheet, '...'))
        DownloadGoogleSheet(GOOGLESHEET = sheet)
      }
    }
    print('good boy')
  } else {
    print('Please enter one of the options: Crop, Soil, Water, GHG, Weather, All')
  }
}


# Download Google Sheets with Metadata of interest for the FIRST time
GetMetadata <-
  function(OVERWRITE = FALSE) {
    # get keys and names of Google Sheets of interest
    gs_ls("^[1-3]. ") %>%
      filter(
        sheet_title %in% c(
          "1. Site Information",
          "2. One-Time Plot Data",
          "3. Yearly Treatment Data"
        )
      ) -> sheet_info
    keys <- sheet_info$sheet_key
    titles <- sheet_info$sheet_title
    
    for (i in seq_along(keys)) {
      pass <- paste0('Original_Data/Metadata/', titles[i], '.xlsx')
      # check if files exist and overwrite is allowed
      if (OVERWRITE == FALSE && file.exists(pass)) {
        text <- paste(titles[i], '- file exists and overwrite is FALSE')
        print(text)
      } else {
        text <- paste(titles[i], '- file is downloding ...')
        print(text)
        gs_key(keys[i]) %>%
          gs_download(
            to = pass,
            overwrite = OVERWRITE
          )
      }
    }
  }


# Download Google Sheets with Research Data of interest for the FIRST time 
GetResearchData <-
  function(OVERWRITE = FALSE) {
    # Creat list of site IDs
    read_excel('Original_Data/Metadata/1. Site Information.xlsx', 
               range = cell_cols('A')) %>%
      filter(!is.na(SiteID)) %>%
      pull() -> SITES
    # get keys and names of Google Sheets of interest
    gs_ls(" Data - ") %>%
      filter(str_detect(sheet_title, paste(SITES, collapse = '|'))) -> sheet_info
    keys <- sheet_info$sheet_key
    titles <- sheet_info$sheet_title
    
    for (i in seq_along(keys)) {
      subfolder <- word(titles[i])
      pass <- paste0('Original_Data/', subfolder, '/', titles[i], '_', Sys.Date(), '.xlsx')
      # check if files exist and overwrite is allowed
      if (OVERWRITE == FALSE && file.exists(pass)) {
        text <- paste(titles[i], '- file exists and overwrite is FALSE')
        print(text)
      } else {
        text <- paste(titles[i], '- file is downloding ...')
        print(text)
        gs_key(keys[i]) %>%
          gs_download(
            to = pass,
            overwrite = OVERWRITE
          )
      }
    }
  }

# read local copy of Google Sheets 
ReadExcelSheets <-
  function(PATH, GUESS = 10000){
    sheets <- excel_sheets(PATH)
    dl <- vector('list', length = length(sheets))
    for (i in seq_along(sheets)){
      column_names <- read_excel(path = PATH, sheet = i, n_max = 2) %>%
        names()
      dl[[i]] <- read_excel(path = PATH,
                            sheet = i, 
                            col_names = column_names, 
                            cell_limits(c(3, 1), c(NA, length(column_names))), 
                            guess_max = GUESS,
                            na = c('n/a', 'NA', 'did not collect')) %>%
        mutate(sheet = sheets[i])
    }
    return(dl)
  }


# reads data from the last version of files in a folder
ReadLatestData <-
  function(DATA_TYPE) {
    if (DATA_TYPE %in% c("Crop", "Soil", "Water", "GHG", "Weather")) {
      folder <- paste0('Original_Data/', DATA_TYPE, '/')
      # get list of all files in the folder
      files_all <- dir(folder)
      # get list of the latest versions of files
      files_all %>%
        tibble('file_name' = .) %>%
        separate(file_name,
                 into = c('type', 'file'),
                 sep = ' - ',
                 extra = 'merge',
                 remove = FALSE) %>% 
        mutate(file = str_remove(file, '.xlsx'),
               date = str_sub(file, -10, -1) %>% ymd()) %>%
        separate(file, into = 'siteid', sep = "_20", extra = 'drop') %>%
        group_by(siteid) %>%
        filter(date == max(date)) %>%
        ungroup() %>%
        pull(file_name) ->
        files_latest
      # aggregate all data into one list object
      dl <- vector('list', length = length(files_latest))
      for (i in seq_along(dl)) {
        path <- paste0(folder, files_latest[i])
        dl[i] <- ReadExcelSheets(PATH = path)
      }
      return(dl)
    } else {
      ANSWER <- readline('>>> Enter data type to read (Crop, Soil, Water, GHG, Weather): ')
      if (ANSWER %in% c("Crop", "Soil", "Water", "GHG", "Weather")) {
        ReadLatestData(DATA_TYPE = ANSWER)
      } else {
        writeLines('You have entered incorrect DATA TYPE.\nTry again!')
      }
    }
  }


# Combining Soil data
ReadSoilData <-
  function(){
    ReadLatestData('Soil') %>%
      # make sure that SiteID is entered in all rows
      map(~ .x %>% fill(SiteID)) %>%
      # define type of Key variables & gather all non-key variables
      map(~ .x %>%
            mutate(SiteID = as.character(SiteID),
                   PlotID = as.character(PlotID),
                   SoilSampleDate = as.Date(SoilSampleDate),
                   Laboratory = as.character(Laboratory),
                   SoilDepth = as.character(SoilDepth),
                   Subsample = as.character(Subsample)) %>%
            gather(key, value, -(SiteID:Subsample))) %>%
      bind_rows() %>%
      # added in 2019-04-24 to resolve duplicate problems when spreading data
      filter(!is.na(SoilDepth)) %>%
      spread(key, value) %>%
      mutate(BulkDensity = as.numeric(BulkDensity),
             PercentSand = as.numeric(PercentSand),
             PercentSilt = as.numeric(PercentSilt),
             PercentClay = as.numeric(PercentClay),
             SoilTexture = as.character(SoilTexture),
             SoilpH = as.numeric(SoilpH),
             FallSOC = as.numeric(FallSOC),
             FallSoilTN = as.numeric(FallSoilTN),
             FallSoilNO3 = as.numeric(FallSoilNO3),
             FallSoilNH4 = as.numeric(FallSoilNH4),
             FallSOC_calc = as.numeric(FallSOC_calc),
             FallSoilTN_calc = as.numeric(FallSoilTN_calc),
             FallSoilNO3_calc = as.numeric(FallSoilNO3_calc),
             FallSoilNH4_calc = as.numeric(FallSoilNH4_calc),
             SpringSoilNO3 = as.numeric(SpringSoilNO3),
             SpringSoilNH4 = as.numeric(SpringSoilNH4), # NEW
             SoilCa = as.numeric(SoilCa),
             SoilMg = as.numeric(SoilMg),
             SoilP = as.numeric(SoilP),
             SoilK = as.numeric(SoilK),
             CommentsSoilSamples = as.character(CommentsSoilSamples)) %>%
      select(SiteID:Subsample, 
             BulkDensity,
             PercentSand,
             PercentSilt,
             PercentClay,
             SoilTexture,
             SoilpH,
             FallSOC,
             FallSoilTN,
             FallSoilNO3,
             FallSoilNH4,
             FallSOC_calc,
             FallSoilTN_calc,
             FallSoilNO3_calc,
             FallSoilNH4_calc,
             SpringSoilNO3,
             SpringSoilNH4, # NEW
             SoilCa,
             SoilMg,
             SoilP,
             SoilK,
             everything()) %>%
      # remove the ATEMP before outputting
      filter(SiteID != 'ATEMP') -> df
    return(df)
  }


# Combining Crop data
ReadCropData <- 
  function() {
    ReadLatestData('Crop') %>%
      # make sure that SiteID is entered in all rows
      map(~ .x %>% fill(SiteID)) %>%
      # define type of Key variables & gather all non-key variables
      map(~ .x %>%
            mutate(SiteID = as.character(SiteID),
                   PlotID = as.character(PlotID),
                   Subsample = as.character(Subsample),
                   ExperimentYear = as.numeric(ExperimentYear),
                   Crop = as.character(Crop)) %>%
            gather(key, value, -(SiteID:Crop))) %>%
      bind_rows() %>%
      spread(key, value) %>%
      mutate(CoverCropBiomass = as.numeric(CoverCropBiomass),
             CoverCropStubbleHeight = as.numeric(CoverCropStubbleHeight),
             CoverCropBiomassRemoved = as.numeric(CoverCropBiomassRemoved),
             CoverCropN = as.numeric(CoverCropN),
             CoverCropP = as.numeric(CoverCropP),
             CoverCropK = as.numeric(CoverCropK),
             EmergenceDate = as.Date(as_datetime(as.numeric(EmergenceDate))),
             CornLSNTBiomass = as.numeric(CornLSNTBiomass),
             CornLSNT_N = as.numeric(CornLSNT_N),
             SilkingDate = as.Date(as_datetime(as.numeric(SilkingDate))),
             SilkingEarLeafN = as.numeric(SilkingEarLeafN),
             SilkingEarLeafP = as.numeric(SilkingEarLeafP),
             SilkingEarLeafK = as.numeric(SilkingEarLeafK),
             SPAD = as.numeric(SPAD),
             SoybeanR6Biomass = as.numeric(SoybeanR6Biomass),
             SoybeanR6N = as.numeric(SoybeanR6N),
             SoybeanR6P = as.numeric(SoybeanR6P),
             SoybeanR6K = as.numeric(SoybeanR6K),
             `CornR6Biomass_non-grain` = as.numeric(`CornR6Biomass_non-grain`),
             `CornPlantN_non-grain` = as.numeric(`CornPlantN_non-grain`),
             `CornPlantP_non-grain` = as.numeric(`CornPlantP_non-grain`),
             `CornPlantK_non-grain` = as.numeric(`CornPlantK_non-grain`),
             CornGrainYield_Subsample = as.numeric(CornGrainYield_Subsample),
             StalkNitrate = as.numeric(StalkNitrate),
             SeasonEndPopulation = as.numeric(SeasonEndPopulation),
             CropYield = as.numeric(CropYield),
             MoistureAtHarvest = as.numeric(MoistureAtHarvest),
             CornGrainN_Subsample = as.numeric(CornGrainN_Subsample),
             CornGrainP_Subsample = as.numeric(CornGrainP_Subsample),
             CornGrainK_Subsample = as.numeric(CornGrainK_Subsample),
             CornGrainN_Combine = as.numeric(CornGrainN_Combine),
             CornGrainP_Combine = as.numeric(CornGrainP_Combine),
             CornGrainK_Combine = as.numeric(CornGrainK_Combine),
             # SoybeanR8Biomass = as.numeric(SoybeanR8Biomass),
             SoybeanGrainN_Subsample = as.numeric(SoybeanGrainN_Subsample),
             SoybeanGrainP_Subsample = as.numeric(SoybeanGrainP_Subsample),
             SoybeanGrainK_Subsample = as.numeric(SoybeanGrainK_Subsample),
             SoybeanGrainN_Combine = as.numeric(SoybeanGrainN_Combine),
             SoybeanGrainP_Combine = as.numeric(SoybeanGrainP_Combine),
             SoybeanGrainK_Combine = as.numeric(SoybeanGrainK_Combine),
             StoverRemoved = as.numeric(StoverRemoved)) %>%
      select(SiteID:Crop, 
             CoverCropBiomass,
             CoverCropStubbleHeight,
             CoverCropBiomassRemoved,
             CoverCropN,
             CoverCropP,
             CoverCropK,
             EmergenceDate,
             CornLSNTBiomass,
             CornLSNT_N,
             SilkingDate,
             SilkingEarLeafN,
             SilkingEarLeafP,
             SilkingEarLeafK,
             SPAD,
             SoybeanR6Biomass,
             SoybeanR6N,
             SoybeanR6P,
             SoybeanR6K,
             CornR6Biomass_NonGrain = `CornR6Biomass_non-grain`,
             CornPlantN_NonGrain = `CornPlantN_non-grain`,
             CornPlantP_NonGrain = `CornPlantP_non-grain`,
             CornPlantK_NonGrain = `CornPlantK_non-grain`,
             CornGrainYield_Subsample,
             StalkNitrate,
             SeasonEndPopulation,
             CropYield,
             MoistureAtHarvest,
             CornGrainN_Subsample,
             CornGrainP_Subsample,
             CornGrainK_Subsample,
             CornGrainN_Combine,
             CornGrainP_Combine,
             CornGrainK_Combine,
             # SoybeanR8Biomass,
             SoybeanGrainN_Subsample,
             SoybeanGrainP_Subsample,
             SoybeanGrainK_Subsample,
             SoybeanGrainN_Combine,
             SoybeanGrainP_Combine,
             SoybeanGrainK_Combine,
             everything()) %>%
      # remove the ATEMP before outputting
      filter(SiteID != 'ATEMP') -> df
    return(df)
  }


# Combining Water data
ReadWaterData <- 
  function() {
    ReadLatestData('Water') %>%
      # make sure that SiteID is entered in all rows
      map(~ .x %>% fill(SiteID)) %>%
      # make sure that there are no empty dates (they create problems with spread)
      map(~ .x %>% filter(!is.na(MeasurementDate))) %>%
      # define type of Key variables & gather all non-key variables
      map(~ .x %>%
            mutate(SiteID = as.character(SiteID),
                   PlotID = as.character(PlotID),
                   MeasurementDate = as.Date(MeasurementDate)) %>%
            gather(key, value, -(SiteID:MeasurementDate))) %>%
      bind_rows() %>%
      spread(key, value) %>%
      mutate(DailyDrainage = as.numeric(DailyDrainage),
             CommentsDailyDrainage = as.character(CommentsDailyDrainage),
             DailyRunoff = as.numeric(DailyRunoff),
             DrainageTotalNConc = as.numeric(DrainageTotalNConc),
             CommentsTotalN = as.character(CommentsTotalN),
             DrainageNO3Conc = as.numeric(DrainageNO3Conc),
             CommentsNO3 = as.character(CommentsNO3),
             DrainageNH3Conc = as.numeric(DrainageNH3Conc),
             CommentsNH3 = as.character(CommentsNH3),
             DrainageTotalPConc = as.numeric(DrainageTotalPConc),
             CommentsTotalP = as.character(CommentsTotalP),
             DrainageReactivePConc = as.numeric(DrainageReactivePConc),
             CommentsReactiveP = as.character(CommentsReactiveP),
             DrainageTotalKConc = as.numeric(DrainageTotalKConc),
             CommentsTotalK = as.character(CommentsTotalK),
             DrainageDissolvedKConc = as.numeric(DrainageDissolvedKConc),
             CommentsDissolvedK = as.character(CommentsDissolvedK),
             RunoffTotalNConc = as.numeric(RunoffTotalNConc),
             RunoffNO3Conc = as.numeric(RunoffNO3Conc),
             RunoffTotalPConc = as.numeric(RunoffTotalPConc),
             RunoffReactivePConc = as.numeric(RunoffReactivePConc)) %>%
      select(SiteID:MeasurementDate,
             DailyDrainage,
             DrainageTotalNConc,
             DrainageNO3Conc,
             DrainageNH3Conc,
             DrainageTotalPConc,
             DrainageReactivePConc,
             DrainageTotalKConc,
             DrainageDissolvedKConc,
             CommentsDailyDrainage,
             CommentsTotalN,
             CommentsNO3,
             CommentsNH3,
             CommentsTotalP,
             CommentsReactiveP,
             CommentsTotalK,
             CommentsDissolvedK,
             DailyRunoff,
             RunoffTotalNConc,
             RunoffNO3Conc,
             RunoffTotalPConc,
             RunoffReactivePConc,
             everything()) %>%
      # remove the ATEMP before outputting
      filter(SiteID != 'ATEMP') -> df
    return(df)
  }


# Combining GHG data
ReadGHGData <- 
  function() {
    ReadLatestData('GHG') %>%
      # make sure that SiteID is entered in all rows
      map(~ .x %>% fill(SiteID)) %>%
      # define type of Key variables & gather all non-key variables
      map(~ .x %>%
            mutate(SiteID = as.character(SiteID),
                   PlotID = as.character(PlotID),
                   MeasurementDate = as.Date(MeasurementDate),
                   TimeOfSample = as_datetime(TimeOfSample)) %>%
            gather(key, value, -(SiteID:TimeOfSample))) %>%
      bind_rows() %>%
      spread(key, value) %>%
      mutate(TimeOfSample = update(MeasurementDate, 
                                   hour = hour(TimeOfSample),
                                   minute = minute(TimeOfSample)),
             NH3Emissions = as.numeric(NH3Emissions),
             N2OEmissions = as.numeric(N2OEmissions),
             CommentsEmissions = as.character(CommentsEmissions),
             SamplingTemperature = as.numeric(SamplingTemperature),
             SamplingSoilMoisture = as.numeric(SamplingSoilMoisture),
             SamplingSoilNO3 = as.numeric(SamplingSoilNO3),
             SamplingSoilNH4 = as.numeric(SamplingSoilNH4),
             SamplingBulkDensity = as.numeric(SamplingBulkDensity),
             CommentsGHGSampling = as.character(CommentsGHGSampling)) %>%
      select(SiteID:TimeOfSample,
             NH3Emissions,
             N2OEmissions,
             CommentsEmissions,
             SamplingTemperature,
             SamplingSoilMoisture,
             SamplingSoilNO3,
             SamplingSoilNH4,
             SamplingBulkDensity,
             everything()) %>%
      # remove the ATEMP before outputting
      filter(SiteID != 'ATEMP') -> df
    return(df)
  }


# Combining Weather data
ReadWeatherData <- 
  function() {
    ReadLatestData('Weather') %>%
      # make sure that SiteID is entered in all rows
      map(~ .x %>% fill(SiteID)) %>%
      # define type of Key variables & gather all non-key variables
      map(~ .x %>%
            mutate(SiteID = as.character(SiteID),
                   MeasurementDate = as.Date(MeasurementDate)) %>%
            gather(key, value, -(SiteID:MeasurementDate))) %>%
      bind_rows() %>%
      spread(key, value) %>%
      mutate(MeasurementDate = as.Date(MeasurementDate),
             DailyPrecipitation = as.numeric(DailyPrecipitation),
             DailySnow = as.numeric(DailySnow),
             RelativeHumidity = as.numeric(RelativeHumidity),
             SolarRadiation = as.numeric(SolarRadiation),
             MaxAirTemperature = as.numeric(MaxAirTemperature),
             MinAirTemperature = as.numeric(MinAirTemperature),
             AveAirTemperature = as.numeric(AveAirTemperature),
             SoilTemperature = as.numeric(SoilTemperature),
             WindSpeed = as.numeric(WindSpeed),
             WindDirection = as.numeric(WindDirection),
             CommentsWeather = as.character(CommentsWeather)) %>%
      select(SiteID,
             MeasurementDate,
             DailyPrecipitation,
             DailySnow,
             RelativeHumidity,
             SolarRadiation,
             MaxAirTemperature,
             MinAirTemperature,
             AveAirTemperature,
             SoilTemperature,
             WindSpeed,
             WindDirection,
             everything()) %>%
      # remove the ATEMP before outputting
      filter(SiteID != 'ATEMP') -> df
    return(df)
  }



# Setting ggplot theme -----------------------------------

# Set up a theme for plotting
theme_gio <-
  theme_light() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(colour = "#666666", size = rel(2), hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "#666666", size = rel(1.5), hjust = 0.5, face = "plain", lineheight = rel(1.1)), 
        plot.caption = element_text(colour = "#666666", size = rel(1.2), hjust = 0.5, face = "plain"),
        axis.title = element_text(colour = "#666666", size = rel(1.2)),
        axis.text.x = element_text(colour = "#757575", size = rel(1)), 
        axis.text.y = element_text(colour = "#757575", size = rel(0.9)), 
        legend.title =  element_text(colour = "#757575", size = rel(1.2)),
        legend.text = element_text(colour = "#757575", size = rel(1)),
        strip.text = element_text(colour = "#666666", hjust = 0.5, face = "bold", size = rel(1)),
        strip.background = element_rect(colour = NA, fill = NA),
        panel.grid.minor = element_blank())

theme_gio2 <-
  theme_light() +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(colour = "#666666", size = rel(2), hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(colour = "#666666", size = rel(1.5), hjust = 0.5, face = "plain", lineheight = rel(1.1)), 
        plot.caption = element_text(colour = "#666666", size = rel(1.2), hjust = 0.5, face = "plain"), 
        axis.title = element_text(colour = "#666666", size = rel(1.2)),
        axis.text = element_text(colour = "#757575", size = rel(1)), 
        legend.title =  element_text(colour = "#757575", size = rel(1.2)),
        legend.text = element_text(colour = "#757575", size = rel(1)),
        strip.text = element_text(colour = "#666666", hjust = 0.5, face = "bold", size = rel(1)),
        panel.grid.minor = element_blank())
