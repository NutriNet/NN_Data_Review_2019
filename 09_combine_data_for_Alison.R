# preparing data for Alison



crop %>%
  # select only 2018 data
  filter(ExperimentYear < 2019) %>%
  # select variables of interest
  select(SiteID:CoverCropBiomass, CoverCropN:CoverCropK, SoybeanR6Biomass:CornGrainYield_Subsample,
         CropYield, CornGrainN_Subsample:CornGrainK_Subsample, SoybeanGrainN_Subsample:SoybeanGrainK_Subsample) %>%
  gather(key, value, CoverCropBiomass:SoybeanGrainK_Subsample) %>%
  # replace -9999 with NA
  mutate(value = ifelse(value == -9999, NA_real_, value)) %>%
  # get rid missing data
  filter(!is.na(value)) %>%
  # add Water Year
  mutate(WaterYear = ExperimentYear) %>%
  mutate(Crop = tolower(Crop)) %>%
  # average over plot (get rid of subsample)
    # DOUGLAS has some variables reported for 2 Subsamples
  group_by(SiteID, PlotID, WaterYear, Crop, key) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>%
  spread(key, value) %>% 
  filter(WaterYear == 2018) %>%
  ungroup() %>% 
  mutate(PlotID = str_remove(PlotID, 'Field A_')) -> crop_annual # yeilds are in kg/ha
  

water %>%
  # get rid of comments
  select(-contains('Comment')) %>%
  # get rid of runoff data
  select(-contains('Runoff')) %>%
  gather(key, value, -(SiteID:MeasurementDate)) %>%
  # get rid missing data
  filter(!is.na(value)) %>%
  # replace -9999 with NA
  mutate(value = ifelse(value == -9999, NA_real_, value)) %>%
  spread(key, value) %>%
  # add Water Year
  mutate(Year = year(MeasurementDate),
         WaterYear = ifelse(month(MeasurementDate) > 9, Year + 1, Year),
         DailyDrainage = as.numeric(DailyDrainage)) %>%
  # calculate daily losses
  mutate(NO3_loss = DailyDrainage * DrainageNO3Conc/100,
         TN_loss = DailyDrainage * DrainageTotalNConc/100,
         RP_loss = DailyDrainage * DrainageReactivePConc/100,
         TP_loss = DailyDrainage * DrainageTotalPConc/100,
         TK_loss = DailyDrainage * DrainageTotalKConc/100) %>%
  # transform data and filter only exisiting measurements
  select(SiteID:MeasurementDate, Year, WaterYear, ends_with('loss')) %>%
  gather(key, value, ends_with('loss')) %>%
  filter(!is.na(value)) %>%
  # aggregate annual losses
  group_by(SiteID, PlotID, WaterYear, key) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  spread(key, value) %>% 
  ungroup() %>%
  # standartize Plot names (in particulat at NWRF)
  mutate(PlotID = str_remove(PlotID, 'Plot ')) %>%
  filter(WaterYear == 2018) -> water_annual_loss # losses are in kg/ha

ghg %>%
  # get rid of comments
  select(-contains('Comment')) %>%
  # select only emission data
  select(SiteID:MeasurementDate, contains('Emissions')) %>%
  gather(key, value, contains('Emissions')) %>%
  # get rid missing data
  filter(!is.na(value)) %>%
  # replace -9999 with NA
  mutate(value = ifelse(value == -9999, NA_real_, value)) %>%
  # add Water Year
  mutate(Year = year(MeasurementDate),
         WaterYear = ifelse(month(MeasurementDate) > 9, Year + 1, Year)) %>%
  # calculate annual losses
  group_by(SiteID, PlotID, WaterYear, key) %>%
  summarise(value = sum(value, na.rm = TRUE)/1000) %>%
  {. ->> ghg_ids} %>%
  spread(key, value) %>%
  rename(N2O_loss = N2OEmissions, NH3_loss = NH3Emissions) %>%
  filter(WaterYear == 2018) -> ghg_annual_loss # lossed in kg/ha

# calculated linearly interpolated GHG 
library(zoo)

# incorporate detrilizer application date into GHG interpolation
# make GHG concentration constant from the last pre-banding 
# to the banding date (band = spring fertilizer application)
gs_title('Management DataStore (NN backend for mandata)') %>%
  gs_read(ws = 'Fertilizer Applications') -> fertilizer_app

# get plot IDs
ReadExcelSheets('Original_Data/Metadata/2. One-Time Plot Data.xlsx') %>%
  purrr::pluck(1) %>%
  select(1:4) %>%
  mutate(PlotID = str_remove(PlotID, '.0$')) %>%
  mutate(PlotIDList = 'ALL PLOTS') %>%
  select(SiteID, PlotID, PlotIDList) -> plotids

# combine fertilizer application and plot ids
fertilizer_app %>%
  filter(!is.na(SiteID)) %>%
  filter(ExperimentYear %in% 2018:2019,
         FertSource == 'fertilizer_synthetic') %>%
  select(SiteID, PlotIDList, ApplicationDate, Crop, FertilizerName, FertFormulation, ApplicationMethod) %>%
  mutate(PlotIDList = str_remove(PlotIDList, 'PLOTS: ')) %>%
  # remove 'Plot ' from TRO's plot names
  mutate(PlotIDList = str_remove_all(PlotIDList, 'Plot ')) %>%
  mutate(PlotIDList = str_replace_all(PlotIDList, ';', ',')) %>%
  mutate(PlotIDs = str_split(PlotIDList, pattern = ', ')) %>%
  unnest(PlotIDs) %>% 
  left_join(plotids, by = c('SiteID', 'PlotIDList')) %>%
  mutate(PlotID = ifelse(is.na(PlotID), PlotIDs, PlotID)) %>%
  mutate(ApplicationDate = as.Date(ApplicationDate)) %>%
  select(SiteID, PlotID, ApplicationDate) %>%
  distinct() %>%
  mutate(DUMMY = 1) %>%
  full_join(tibble(key = c('N2OEmissions', 'NH3Emissions'),
                   DUMMY = 1), by = 'DUMMY') %>% 
  full_join(tibble(SiteID = 'KELLEY', Subsample = c('between_row', 'in_row')),
            by = "SiteID")  %>%
  mutate(MeasurementDate = ApplicationDate) %>%
  # remove plots with no GHG data in 2018
  right_join(ghg_ids %>% filter(WaterYear == 2018) %>% distinct(SiteID, PlotID, key),
            by = c('SiteID', 'PlotID', 'key')) %>%
  filter(year(ApplicationDate) < 2019) %>%
  select(-DUMMY, -WaterYear) -> ferts_2018

ghg %>%
  # get rid of comments
  select(-contains('Comment')) %>%
  # select only emission data
  select(SiteID:MeasurementDate, Subsample, contains('Emissions')) %>%
  gather(key, value, contains('Emissions')) %>%
  # replace -9999 with NA
  mutate(value = ifelse(value == -9999, NA_real_, value)) %>% 
  # get rid missing data
  filter(!is.na(value)) %>%
  full_join(ferts_2018, by = c('SiteID', 'PlotID', 'Subsample', 'MeasurementDate', 'key')) %>%
  arrange(SiteID, PlotID, Subsample, key, MeasurementDate) %>%
  group_by(SiteID, PlotID, Subsample, key) %>%
  fill(value) %>%
  nest() %>%
  # mutate(period = list(tibble(MeasurementDate = seq(ymd(20170101), ymd(20181231), by = 'day')))) %>%
  # mutate(data2 = map2(.x = data, .y = period, .f = full_join, by = "MeasurementDate"))
  mutate(data2 = map(.x = data, 
                     .f = ~ .x  %>% 
                       full_join(tibble(MeasurementDate = seq(ymd(20170101), ymd(20181231), by = 'day')), 
                                 by = "MeasurementDate") %>%
                       arrange(MeasurementDate) %>%
                       mutate(value_interp = na.approx(value, na.rm = FALSE)))) %>%
  unnest(data2) %>%
  filter(!(is.na(value_interp) & year(MeasurementDate) < 2018)) %>%
  mutate(comments = ifelse(is.na(value) & !is.na(value_interp), 'interpolated (linear)', NA),
         value = value_interp) %>%
  mutate(comments = ifelse(!is.na(ApplicationDate), 'fertilizer application', comments)) %>%
  select(-value_interp, -ApplicationDate) -> ghg_interpolated

ghg_interpolated %>%
  # add Water Year
  mutate(Year = year(MeasurementDate),
         WaterYear = ifelse(month(MeasurementDate) > 9, Year + 1, Year)) %>%
  # calculate annual losses
  group_by(SiteID, PlotID, WaterYear, key) %>%
  summarise(value = sum(value, na.rm = TRUE)/1000) %>%
  spread(key, value) %>%
  rename(N2O_loss = N2OEmissions, NH3_loss = NH3Emissions) %>%
  filter(WaterYear == 2018) -> ghg_interpolated_annual_loss # lossed in kg/ha

  
# included SOC, texture, and Spring NO3
soil %>%
  # get rid of comments
  select(-contains('Comment')) %>%
  # select variables of interest
  select(SiteID:Subsample, SoilTexture, FallSOC, SpringSoilNO3) %>%
  gather(key, value, FallSOC:SpringSoilNO3) %>%
  # replace -9999 with NA
  mutate(value = ifelse(value == -9999, NA_real_, value)) %>%
  # get rid missing data
  filter(!is.na(value)) %>%
  # standartize depths
  mutate(SoilDepth = ifelse(SoilDepth == '60-80***', '60-90', SoilDepth)) %>%
  # average Subsample readings (at DOUGLAS, KELLEY, TRO)
  group_by(SiteID, PlotID, SoilSampleDate, Laboratory, SoilDepth, SoilTexture, key) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  select(-Laboratory) %>%
  # add Water Year
  mutate(Year = year(SoilSampleDate),
         WaterYear = ifelse(month(SoilSampleDate) > 9, Year + 1, Year)) %>%
  filter(WaterYear == 2018) %>%
  select(-Year) -> soil_long

# arrange soil data
soil_long %>%
  filter(str_detect(key, 'NO3')) %>%
  select(-SoilTexture) %>%
  unite(var, key, SoilDepth, sep = '_') %>%
  spread(var, value) %>%
  rename(SoilNO3SampleDate = SoilSampleDate) -> soil_NO3
soil_long %>%
  filter(str_detect(key, 'SOC')) %>%
  select(-SoilTexture) %>%
  unite(var, key, SoilDepth, sep = '_') %>%
  spread(var, value) %>%
  rename(SoilSOCSampleDate = SoilSampleDate) -> soil_SOC
soil_long %>%
  select(SiteID:SoilTexture, -SoilSampleDate) %>%
  filter(!is.na(SoilTexture)) %>% 
  mutate(SoilDepth = paste0('SoilTexture_', SoilDepth)) %>%
  spread(SoilDepth, SoilTexture) -> soil_Texture
# combine SOC and NO3
soil_NO3 %>%
  full_join(soil_SOC, by = c('SiteID', 'PlotID', 'WaterYear')) %>%
  full_join(soil_Texture, by = c('SiteID', 'PlotID')) %>%
  select(SiteID, PlotID, WaterYear, starts_with('SoilTexture'), starts_with('SoilNO3'), everything()) %>%
  ungroup() %>%
  mutate(PlotID = str_remove(PlotID, 'Field A_')) -> soil_wide


# not included
weather

# read metadata
DownloadGoogleSheet('3. Yearly Treatment Data', TYPE = 'Metadata')
ReadExcelSheets('Original_Data/Metadata/3. Yearly Treatment Data_2019-05-29.xlsx') %>%
  pluck(1) %>%
  filter(ExperimentYear == 2018) %>%
  mutate(PlotID = str_remove(PlotID, 'Field A_')) %>%
  mutate(PlotID = str_remove(PlotID, '\\.0$')) %>%
  mutate(TotalNRate = as.numeric(TotalNRate)) %>%
  ungroup() -> trt


# combine data
ghg_interpolated_annual_loss %>%
# ghg_annual_loss %>%
  full_join(water_annual_loss, by = c('SiteID', 'PlotID', 'WaterYear')) %>%
  filter(WaterYear == 2018) %>%
  full_join(crop_annual, by = c('SiteID', 'PlotID', 'WaterYear')) -> data

trt %>%
  right_join(data, by = c('SiteID', 'PlotID', 'ExperimentYear' = 'WaterYear')) %>%
  full_join(soil_wide, by = c('SiteID', 'PlotID', 'ExperimentYear' = 'WaterYear')) -> df

# explore the data
# Crop Yield
df %>%
  ggplot(aes(x=TotalNRate, y=CropYield, col=SiteID)) +
  geom_point(data = . %>% filter(SiteID == 'MUDS2'), size = 5, col = 'grey70') +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  facet_grid(~ CashCrop, scales = 'free') +
  theme_light()
# Corn Grain Yield
df %>%
  ggplot(aes(x=TotalNRate, y=CornGrainYield_Subsample, col=SiteID)) +
  geom_point(data = . %>% filter(SiteID == 'MUDS2'), size = 5, col = 'grey70') +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(0, 200, 50)) +
  theme_light()

# save data
df %>%
  select(-Crop, - sheet) %>%
  write_csv(paste0('Output_Data/', Sys.Date(), '_NN_data_for_Alison.csv'), na = 'n/a')

ghg_interpolated %>%
  spread(key, value) %>%
  select(SiteID, PlotID, Subsample, MeasurementDate, starts_with('N'), comments) %>%
  write_csv(paste0('Output_Data/', Sys.Date(), '_GHG_data_Interpolation_for_Alison.csv'), na = ' ')

