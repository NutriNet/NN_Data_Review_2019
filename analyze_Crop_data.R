source('00_project_settings.R')

# Analyze Crop data
ReadCropData() %>% 
  as.tibble() %>% 
  select(-sheet) %>% 
  filter(ExperimentYear == 2018) -> crop

# quick look at the summary of data
crop %>%
  summary()


# Check Dates -------------------------------------------------------------

# get crop planting dates
gs_title("Management DataStore (NN backend for mandata)") %>%
  gs_read(ws = "Field Operations") -> mngt

# get plot IDs
ReadExcelSheets('Original_Data/Metadata/2. One-Time Plot Data.xlsx') %>%
  purrr::pluck(1) %>%
  select(1:4) %>%
  mutate(PlotID = str_remove(PlotID, '.0$')) %>%
  mutate(PlotIDList = 'ALL PLOTS') -> plotids

# combine mngt and plot ids
mngt %>%
  filter(ActivityType == 'plant',
         ExperimentYear == 2018,
         Crop %in% c('corn', 'soybean')) %>%
  select(SiteID, PlotIDList, ActivityDate, Crop, SeedingRate) %>%
  mutate(PlotIDList = str_remove_all(PlotIDList, 'PLOTS: ')) %>%
  # remove 'Plot ' from TRO's plot names
  mutate(PlotIDList = str_remove_all(PlotIDList, 'Plot ')) %>%
  mutate(PlotIDList = str_split(PlotIDList, ', ')) %>%
  unnest()  %>%
  left_join(plotids, by = c('SiteID', 'PlotIDList')) %>%
  mutate(PlotID = ifelse(is.na(PlotID), PlotIDList, PlotID)) %>%
  select(SiteID, PlotID, Crop, ActivityDate) -> df

# combine crop and management (planting) data
crop %>%
  select(SiteID:Crop, contains('Date')) %>%
  left_join(df, by = c("SiteID", "PlotID", "Crop")) %>%
  mutate(PlantingDate = ymd(ActivityDate)) %>%
  select(-ActivityDate) %>%
  # compare planting and emergence dates
  mutate(DIFF = EmergenceDate - PlantingDate,
         CHECK = PlantingDate >= EmergenceDate) %>%
  filter(CHECK == 1) %>%
  count(SiteID)


# Check Completeness of the Data ------------------------------------------

crop %>%
  # drop dates
  select(-contains('Date')) %>%
  gather(key, value, 6:43) %>%
  filter(!is.na(value)) %>%
  count(SiteID, key, year = ExperimentYear) -> df


# check years with data
df %>%
  select(-year) %>%
  spread(SiteID, n)

# check vars collected at each site
df %>%
  filter(SiteID == 'SUBSURF') 


# Check Validity of Measurements ------------------------------------------

# prepare dataframe for analysis
crop %>%
  # drop dates
  select(-contains('Date')) %>%
  gather(key, value, 6:43) %>%
  filter(!is.na(value)) %>%
  rename(year = ExperimentYear) -> df

# see if there are multiple measurements within site-plot-subsample
df %>%
  filter(value != -9999) %>%
  add_count(SiteID, PlotID, Subsample, key) %>%
  filter(n != 1)
# there was NONE, hence I can ignore time of sample when plotting 



# functions creates labels for plotting
crop_labels <- function(df) {
  df %>%
    mutate(key = factor(key, levels = c("CoverCropBiomass",
                                        "CoverCropStubbleHeight",
                                        "CoverCropBiomassRemoved",
                                        "CoverCropN",
                                        "CoverCropP",
                                        "CoverCropK",
                                        "CornLSNTBiomass",
                                        "CornLSNT_N",
                                        "SilkingEarLeafN",
                                        "SilkingEarLeafP",
                                        "SilkingEarLeafK",
                                        "SPAD",
                                        "SoybeanR6Biomass",
                                        "SoybeanR6N",
                                        "SoybeanR6P",
                                        "SoybeanR6K",
                                        "CornR6Biomass_NonGrain",
                                        "CornPlantN_NonGrain",
                                        "CornPlantP_NonGrain",
                                        "CornPlantK_NonGrain",
                                        "CornGrainYield_Subsample",
                                        "StalkNitrate",
                                        "SeasonEndPopulation",
                                        "CropYield",
                                        "MoistureAtHarvest",
                                        "CornGrainN_Subsample",
                                        "CornGrainP_Subsample",
                                        "CornGrainK_Subsample",
                                        "CornGrainN_Combine",
                                        "CornGrainP_Combine",
                                        "CornGrainK_Combine",
                                        "SoybeanGrainN_Subsample",
                                        "SoybeanGrainP_Subsample",
                                        "SoybeanGrainK_Subsample",
                                        "SoybeanGrainN_Combine",
                                        "SoybeanGrainP_Combine",
                                        "SoybeanGrainK_Combine",
                                        "StoverRemoved"),
                        labels = c("Cover Crop Biomass\n(kg ha-1)",
                                   "Cover Crop StubbleHeight\n(cm)",
                                   "Cover Crop BiomassRemoved\n(%)",
                                   "Cover Crop N\n(g N kg-1 dry biomass)",
                                   "Cover Crop P\n(g P kg-1 dry biomass)",
                                   "Cover Crop K\n(g K kg-1 dry biomass)",
                                   "Corn LSNT Biomass\n(kg ha-1)",
                                   "Corn LSNT N\n(g N kg-1 dry biomass)",
                                   "Silking Ear Leaf N\n(g N kg-1)",
                                   "Silking Ear Leaf P\n(g P kg-1)",
                                   "Silking Ear Leaf K\n(g K kg-1)",
                                   "SPAD\n",
                                   "Soybean R6 Biomass\n(kg ha-1)",
                                   "Soybean R6 N\n(g N kg-1 dry biomass)",
                                   "Soybean R6 P\n(g P kg-1 dry biomass)",
                                   "Soybean R6 K\n(g K kg-1 dry biomass)",
                                   "Corn R6 Biomass non-grain\n(kg ha-1)",
                                   "Corn Plant N non-grain\n(g N kg-1 dry biomass)",
                                   "Corn Plant P non-grain\n(g P kg-1 dry biomass)",
                                   "Corn Plant K non-grain\n(g K kg-1 dry biomass)",
                                   "Corn Grain Yield Subsample\n(kg ha-1)",
                                   "Stalk Nitrate\n(g N kg-1 dry biomass)",
                                   "Season End Population\n(plants ha-1)",
                                   "Crop Yield\n(Mg ha-1)",
                                   "Moisture At Harvest\n(g kg-1)",
                                   "Corn Grain N Subsample\n(g N kg-1)",
                                   "Corn Grain P Subsample\n(g P kg-1)",
                                   "Corn Grain K Subsample\n(g K kg-1)",
                                   "Corn Grain N Combine\n(g N kg-1)",
                                   "Corn Grain P Combine\n(g P kg-1)",
                                   "Corn Grain K Combine\n(g K kg-1)",
                                   "Soybean Grain N Subsample\n(g N kg-1)",
                                   "Soybean Grain P Subsample\n(g P kg-1)",
                                   "Soybean Grain K Subsample\n(g K kg-1)",
                                   "Soybean Grain N Combine\n(g N kg-1)",
                                   "Soybean Grain P Combine\n(g P kg-1)",
                                   "Soybean Grain K Combine\n(g K kg-1)",
                                   "Stover Removed\n(%)")))
  }  



# plot Plant Population data

# get seeding rate
mngt %>%
  # remove 'Plot ' from TRO's plot names
  mutate(PlotIDList = str_remove_all(PlotIDList, 'PLOTS: '),
         PlotIDList = str_remove_all(PlotIDList, 'Plot ')) %>%
  filter(ActivityType == 'plant',
         ExperimentYear == 2018,
         Crop %in% c('corn', 'soybean')) %>%
  select(SiteID, PlotIDList, Crop, SeedingRate) %>%
  # convert SeedingRate to numeric
  mutate(SeedingRate = as.numeric(str_remove_all(SeedingRate, ','))) %>%
  mutate(PlotIDList = str_split(PlotIDList, ', ')) %>%
  unnest()  %>%
  left_join(plotids, by = c('SiteID', 'PlotIDList')) %>%
  mutate(PlotID = ifelse(is.na(PlotID), PlotIDList, PlotID)) %>%
  select(SiteID, PlotID, Crop, SeedingRate) -> seeding_rate
  
  
df %>%
  filter(value != -9999) %>%
  filter(key == 'SeasonEndPopulation') %>% 
  left_join(seeding_rate, by = c('SiteID', 'PlotID', 'Crop')) %>%
  # mutate(CHECK = (value - SeedingRate)/SeedingRate) %>% 
  # filter(CHECK > 0.1) 
  crop_labels() %>%
  ggplot(aes(x=SiteID, col=SiteID)) +
  geom_boxplot(aes(y=SeedingRate), size = 1, alpha = 0.5, col = 'black') +
  geom_point(aes(y=value, group=PlotID)) +
  facet_grid(Crop ~ ., scales = 'free') +
  labs(x = NULL, y = 'Season End Population\n(plants ha-1)', 
       title = 'Crop Population 2018') +
  scale_y_continuous(labels = scales::comma) +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Plant_Population_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)



# plot Yield vs Yield data
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CropYield', 'CornGrainYield_Subsample')) %>%
  crop_labels() %>%
  spread(key, value) %>% 
  filter(complete.cases(.[6:7])) %>%
  ggplot(aes(x= `Corn Grain Yield Subsample\n(kg ha-1)`,
             y= `Crop Yield\n(Mg ha-1)`, 
             col=SiteID)) +
  geom_abline(slope = 1, size = 1) +
  geom_point(size = 3) +
  labs(title = 'Combine vs Subsample Yield',
       subtitle = 'Year 2018',
       x= "Corn Grain Yield Subsample\n(kg ha-1)",
       y= "Crop Yield\n(Mg ha-1)") +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  coord_cartesian(xlim = c(2, 18), ylim = c(2, 18))
ggsave(filename = 'Figs/Crop/All_sites_Yield_vs_Yield_2018.png', width = 12, height = 8)



# plot Yield 
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CropYield')) %>%
  crop_labels() %>%
  mutate(key = paste(str_to_title(Crop), str_remove(key, 'Crop '))) %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Crop Yield 2018') +
  scale_y_continuous(labels = scales::comma) +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Yield_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)


# plot Moisture  
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('MoistureAtHarvest')) %>%
  crop_labels() %>%
  mutate(key = paste(str_to_title(Crop), str_remove(key, 'Crop '))) %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Crop Grain Moisture at Harvest 2018') +
  scale_y_continuous(labels = scales::comma) +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Grain_Moisture_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)


# plot Corn Stalk Nitrate 
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('StalkNitrate')) %>%
  # mutate(value = ifelse(SiteID == 'SUBSURF', value * 1000, value),
  #        value = ifelse(SiteID == 'DOUGLAS', value * 10, value)) %>%
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Stalk Nitrate 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Stalk_Nitrate_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)



# plot Corn Stalk Nitrate vs Yield
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CropYield','StalkNitrate')) %>%
  # mutate(value = ifelse(SiteID == 'SUBSURF' & key == 'StalkNitrate', value * 1000, value)) %>%
  spread(key, value) %>%
  filter(complete.cases(.[6:7])) %>%
  # calculate relative yield by site
  group_by(SiteID) %>%
  mutate(RelativeYield = CropYield/max(CropYield)*100,
         StalkNitrate = StalkNitrate * 1000) %>%
  ungroup() %>%
  ggplot(aes(x=StalkNitrate, y=RelativeYield)) +
  geom_vline(xintercept = c(250, 700, 2000), linetype = 'longdash', col = 'grey50') +
  geom_text(data = tibble(labels = c('Low', 'Marginal', 'Optimal', 'Excessive'),
                          StalkNitrate = c(0, 500, 1400, 3800)), 
            aes(label = labels, x = StalkNitrate, y = 0), 
            size = 7, col = 'grey70',
            angle = 90, hjust = 0, vjust = 0.5) +
  geom_point(aes(col=SiteID)) +
  geom_text(data = . %>% filter(SiteID == "MUDS2", StalkNitrate > 1000), 
            aes(label = PlotID), 
            nudge_x = 100, nudge_y = -1.5) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = 'Stalk Nitrate (ppm)', 
       y = 'Relative Yield of Corn (%)', 
       title = 'Relationship Between \nStalk Nitrate Concentration and Relative Yield \n2018') +
  guides(col = guide_legend(override.aes = list(size = 7, shape = 15), 
                            ncol = 1)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_gio2
ggsave(filename = 'Figs/Crop/All_sites_Stalk_Nitrate_vs_Yield_2018.png', width = 12, height = 8)


# plot Corn LSNT Biomass and N 
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CornLSNTBiomass','CornLSNT_N')) %>%
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Corn LSNT 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Corn_LSNT_2018_UPDATED_', Sys.Date(),'.png'),
       width = 12, height = 8)


# plot Corn Biomass at R6 and LSNT
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CornLSNTBiomass','CornR6Biomass_NonGrain')) %>%
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Corn Biomass 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Corn_Biomass_2018_UPDATED_', Sys.Date(),'.png'),
       width = 12, height = 8)


# plot Corn Biomass at R6 vs Yield
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CropYield','CornR6Biomass_NonGrain')) %>%
  spread(key, value) %>%
  filter(complete.cases(.[6:7])) %>%
  mutate(CropYield = CropYield * 1000) %>%
  ggplot(aes(x=CropYield, y=CornR6Biomass_NonGrain, col=SiteID)) +
  geom_point() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = 'Corn R6 Biomass non-grain (kg ha-1)', 
       x = 'Corn Yield (kg ha-1)', 
       title = 'Corn Biomass vs Yield 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2
ggsave(filename = 'Figs/Crop/All_sites_Corn_Biomass_vs_Yield_2018.png', width = 12, height = 8)


# plot Corn Harvest Index  (CALCULATED)
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CropYield','CornR6Biomass_NonGrain')) %>%
  spread(key, value) %>%
  filter(complete.cases(.[6:7])) %>%
  mutate(CropYield = CropYield * 1000,
         HI = CropYield/(CropYield + CornR6Biomass_NonGrain)) %>%
  ggplot(aes(x=SiteID, y=HI, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  labs(y = 'Harvest Index', 
       x = NULL, 
       title = 'Corn Harvest Index 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2
ggsave(filename = 'Figs/Crop/All_sites_Corn_HI_2018.png', width = 12, height = 8)


# plot Corn Plant N-P-K
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CornR6Biomass_NonGrain', 'CornPlantN_NonGrain', 
                    'CornPlantP_NonGrain', 'CornPlantK_NonGrain')) %>% 
  spread(key, value) %>%
  filter(complete.cases(.[6:8])) %>%
  gather(key, value, 6:9) %>%
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Corn Plant N-P-K 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Corn_Plant_NPK_2018_UPDATED_', Sys.Date(),'.png'),
       width = 12, height = 8)


# plot Corn Silking Ear Leaf N-P-K
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('SilkingEarLeafN', 'SilkingEarLeafP', 'SilkingEarLeafK')) %>% 
  spread(key, value) %>%
  filter(complete.cases(.[6:8])) %>%
  gather(key, value, 6:8) %>%
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Corn Ear Leaf N-P-K 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Corn_Ear_Leaf_NPK_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)


# plot Corn Grain N-P-K
df %>%
  filter(value != -9999) %>%
  filter(str_detect(key, 'CornGrain')) %>%
  filter(!str_detect(key, 'Yield')) %>%
  separate(key, into = c('var', 'type'), sep = "_") %>%
  mutate(var = factor(var, 
                      levels = c('CornGrainN', 'CornGrainP', 'CornGrainK'),
                      labels = c("Corn Grain N\n(g N kg-1)",
                                 "Corn Grain P\n(g P kg-1)",
                                 "Corn Grain K\n(g K kg-1)"))) %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(var ~ type, scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Corn Grain N-P-K 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Corn_Grain_NPK_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)


# plot Corn Grain vs Plant N-P-K
df %>%
  filter(value != -9999) %>%
  filter(str_detect(key, 'CornGrain|CornPlant')) %>%
  filter(!str_detect(key, 'Yield|Subsample')) %>%
  separate(key, into = c('key'), sep = "_", extra = 'drop') %>%
  mutate(part = str_sub(key, 5, 9),
         key = str_replace(key, part, ' ')) %>%
  spread(part, value) %>%
  mutate(key = factor(key, 
                      levels = c('Corn N', 'Corn P', 'Corn K'),
                      labels = c("Corn N\n(g N kg-1)",
                                 "Corn P\n(g P kg-1)",
                                 "Corn K\n(g K kg-1)"))) %>%
  filter(complete.cases(.[7:8])) %>%
  ggplot(aes(x=Grain, y=Plant, col=SiteID)) +
  geom_point() +
  facet_wrap(~ key, scales = 'free') +
  labs(title = 'Corn Grain vs Plant N-P-K\n2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_Corn_Grain_vs_Plant_NPK_2018_UPDATED_', Sys.Date(),'.png'),
       width = 12, height = 8)


# plot SPAD 
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('SPAD')) %>% 
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'SPAD 2018') +
  scale_y_continuous(labels = scales::comma) +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Crop/All_sites_SPAD_2018.png', width = 12, height = 8)



# plot Soy Biomasss 
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('SoybeanR6Biomass')) %>% 
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soybean Biomass 2018') +
  scale_y_continuous(labels = scales::comma) +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Crop/All_sites_Soy_Biomass_2018.png', width = 12, height = 8)



# plot Soy Grain vs Plant N-P-K
df %>%
  filter(value != -9999) %>%
  filter(str_detect(key, 'SoybeanGrain|SoybeanPlant')) %>%
  filter(!str_detect(key, 'Yield')) %>%
  separate(key, into = c('key', 'type'), sep = "_", extra = 'drop') %>%
  mutate(part = str_sub(key, 8, 12),
         key = str_replace(key, part, ' ')) %>%
  spread(part, value) %>%
  mutate(key = factor(key, 
                      levels = c('Soybean N', 'Soybean P', 'Soybean K'),
                      labels = c("Soybean N\n(g N kg-1)",
                                 "Soybean P\n(g P kg-1)",
                                 "Soybean K\n(g K kg-1)"))) %>%
  ggplot(aes(x=SiteID, y=Grain, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ type, scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soybean Grain N-P-K 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Crop/All_sites_Soybean_Grain_NPK_2018.png', width = 12, height = 8)


# Cover Crops -----------------

# plot Cover Crop Biomass 
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CoverCropBiomass', 'CoverCropBiomassRemoved')) %>% 
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Cover Crop Biomass 2018') +
  scale_y_continuous(labels = scales::comma) +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_CC_Biomass_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)


# plot Cover Crop Biomass N-P-K
df %>%
  filter(value != -9999) %>%
  filter(key %in% c('CoverCropBiomass', 'CoverCropN', 'CoverCropP', 'CoverCropK')) %>% 
  spread(key, value) %>%
  filter(complete.cases(.[6:9])) %>%
  gather(key, value, 6:9) %>%
  crop_labels() %>%
  ggplot(aes(x=SiteID, y=value, col=SiteID)) +
  geom_boxplot(col = 'grey30', alpha = 0.25) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Cover Crop N-P-K 2018') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/Crop/All_sites_CC_NPK_2018_UPDATED_', Sys.Date(),'.png'),
       width = 12, height = 8)

