source('00_project_settings.R')

# Analyze GHG data

# read the latest weather data for all sites
ReadGHGData() %>% 
  as.tibble() %>% 
  select(-sheet) -> ghg


# Check Completeness of the Data ------------------------------------------

# Check comments
ghg %>%
  count(SiteID, CommentsGHGSampling) %>% filter(!is.na(CommentsGHGSampling))
ghg %>%
  count(SiteID, CommentsEmissions) %>% filter(!is.na(CommentsEmissions))


# see which sites-years are in the database
ghg %>%
  # drop variables with no data
  select(-SamplingBulkDensity, #- SamplingSoilNH3, -SamplingSoilNO3
         -Subsample) %>%
  # drop comments
  select(-CommentsEmissions, -CommentsGHGSampling) %>%
  gather(key, value, 5:10) %>%
  filter(!is.na(value)) %>%
  mutate(year = year(MeasurementDate)) %>%
  count(SiteID, key, year) -> df

# count records in each year 
df %>%
  spread(year, n)


# Check Validity of Measurements ------------------------------------------

# prepare dataframe for analysis
ghg %>%
  # drop variables with no data
  select(-SamplingBulkDensity, #- SamplingSoilNH3, -SamplingSoilNO3
         -Subsample) %>%
  # drop comments
  select(-CommentsEmissions, -CommentsGHGSampling) %>%
  gather(key, value, 5:10) %>%
  filter(!is.na(value)) %>%
  mutate(year = year(MeasurementDate)) -> df

# see if there are multiple measurements within a day at each plot
df %>%
  filter(value != -9999) %>%
  add_count(SiteID, PlotID, MeasurementDate, key) %>%
  filter(n != 1)
  # there was NONE, hence I can ignore time of sample when plotting 

# make sure that time are reasonable range
df %>%
  mutate(Hour = hour(TimeOfSample)) %>%
  count(Hour)


# functions creates labels for plotting
ghg_labels <- function(df) {
  df %>%
    mutate(key = factor(key, levels = c('N2OEmissions',
                                        'NH3Emissions',
                                        'SamplingSoilNO3',
                                        'SamplingSoilNH4',
                                        'SamplingSoilMoisture',
                                        'SamplingTemperature',
                                        'SamplingBulkDensity'),
                        labels = c('Nitrous Oxide\nEmissions\n(g N2O-N ha-1 day-1)',
                                   'Ammonia\nEmissions\n(g NH3-N ha-1 day-1)',
                                   'Soil Nitrate\nConcentration\n(mg NO3-N kg-1)',
                                   'Soil Ammonium\nConcentration\n(mg NH4-N kg-1)',
                                   'Volumetric\nSoil Moisture\n(m3 m-3)',
                                   'Soil\nTemperature\n(°C)',
                                   'Soil\nBulk Density\n(g cm-3)')))
}  

# plot all data
df %>%
  # remove -9999
  filter(value != -9999) %>%
  ghg_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ SiteID, scales = 'free_y', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'GHG Data',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15),
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/GHG/All_sites_2017_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 16, height = 10)



# plot DUDLEY -------------------------
df %>%
  filter(SiteID == "DUDLEY") %>%
  # remove -9999
  filter(value != -9999) %>%
  ghg_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free_y', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'GHG Data',
       subtitle = 'DUDLEY',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15),
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/GHG/DUDLEY_2017_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)



# plot MUDS2 -------------------------
df %>%
  filter(SiteID == "MUDS2") %>%
  # remove -9999
  filter(value != -9999) %>%
  ghg_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free_y', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'GHG Data',
       subtitle = 'MUDS2',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15),
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/GHG/MUDS2_2017_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)



# plot ONT_4R -------------------------
df %>%
  filter(SiteID == "ONT_4R") %>%
  # remove -9999
  filter(value != -9999) %>%
  ghg_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free_y', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'GHG Data',
       subtitle = 'ONT_4R',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15),
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/GHG/ONT_4R_2017_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)



# plot SUBSURF -------------------------
df %>%
  filter(SiteID == "SUBSURF") %>%
  # remove -9999
  filter(value != -9999) %>%
  ghg_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free_y', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'GHG Data',
       subtitle = 'SUBSURF',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15),
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = paste0('Figs/GHG/SUBSURF_2017_2018_UPDATED_', Sys.Date(),'.png'), 
       width = 12, height = 8)

  
