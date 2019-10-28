source('00_project_settings.R')

# Analyze Weather data

# read the latest weather data for all sites
ReadWeatherData() %>% 
  as.tibble() %>%
  select(-sheet) -> weather


# Check Completeness of the Data ------------------------------------------

# see which sites-years are in the database
weather %>% 
  # there are no comments, so we can drop it
  select(-CommentsWeather) %>%
  gather(key, value, 3:12) %>%
  filter(!is.na(value)) %>%
  filter(value != -9999) %>%
  mutate(year = year(MeasurementDate)) %>%
  count(SiteID, key, year) -> df

# count records in each year 
# should be 92 days in 2017 & 365 in 2018
df %>%
  filter(SiteID != 'NWRF') %>%
  spread(year, n) %>%
  filter(SiteID == 'TRO') %>% #filter(`2018` != 365)
  count(`2017`, `2018`)


# Check Validity of Measurements ------------------------------------------

# prepare dataframe for analysis
weather %>%
  # there are no comments, so we can drop it
  select(-CommentsWeather) %>%
  mutate(year = year(MeasurementDate)) %>%
  gather(key, value, DailyPrecipitation:WindDirection) %>%
  filter(value != -9999) %>%
  filter(!is.na(value)) -> df

# functions creates labels for plotting
weather_labels <- function(df) {
  df %>%
    mutate(key = factor(key, levels = c('DailyPrecipitation',
                                        'DailySnow',
                                        'RelativeHumidity',
                                        'SolarRadiation',
                                        'AveAirTemperature',
                                        'MinAirTemperature',
                                        'MaxAirTemperature',
                                        'SoilTemperature',
                                        'WindSpeed',
                                        'WindDirection'),
                        labels = c(' \nPrecipitation\n(mm)',
                                   ' \nSnowfall\n(mm)',
                                   'Relative\nHumidity\n(%)',
                                   'Solar\nRadiation\n(MJ/m2)',
                                   'Ave Air\nTemp\n(°C)',
                                   'Min Air\nTemp\n(°C)',
                                   'Max Air\nTemp\n(°C)',
                                   'Soil Temp\nat 4\'\n(°C)',
                                   'Wind\nSpeed\n(m/s)',
                                   'Wind\nDirection\n(degree)')))
}  

# plot all data
df %>%
  weather_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=SiteID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Weather Data',
       subtitle = '(daily)',
       col = 'Site ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Weather/All_sites_2017_2018.png', width = 16, height = 10)


# plot all data after removing some outliers
df %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  # exclude variables that have no problems
  filter(key != 'DailySnow') %>%
  # remove erronoues readings
    # Min Air Temp of 382.7°C @ DOUGLAS
  filter(!(key == 'MinAirTemperature' & value > 100)) %>%
    # Wind Direction >360° @ MUDS2 
  filter(!(key == 'WindDirection' & value > 360)) %>% 
    # Wind Speed UNITS @ MUDS2
  filter(!(key == 'WindSpeed' & value > 20)) %>%
    # 3 questionable readings @ MUDS2
  filter(!(key == 'RelativeHumidity' & value < 25)) %>%
  filter(!(key == 'WindSpeed' & month(MeasurementDate) > 10 & value > 12)) %>%
  filter(!(key == 'SoilTemperature' & month(MeasurementDate) > 10 & value > 20)) %>%
  #count(SiteID, month(MeasurementDate, label = T))
  weather_labels() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=SiteID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Weather Data',
       subtitle = '(daily)',
       col = 'Site ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Weather/All_sites_2017_2018_no_outliers.png', width = 16, height = 10)


# plot problematic readings at MUDS2
df %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  # exclude variables that have no problems
  filter(key != 'DailySnow') %>%
  # remove erronoues readings
  # Min Air Temp of 382.7°C @ DOUGLAS
  filter(!(key == 'MinAirTemperature' & value > 100)) %>%
        # Wind Direction >360° @ MUDS2
        filter(!(key == 'WindDirection' & value > 360)) %>%
        # Wind Speed UNITS @ MUDS2
        filter(!(key == 'WindSpeed' & value > 20)) %>%
  weather_labels() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=SiteID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Weather Data',
       subtitle = '(daily)',
       col = 'Site ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
    # ggsave(filename = 'Figs/Weather/All_sites_2017_2018_MUDS2_wind.png', width = 16, height = 10)
ggsave(filename = 'Figs/Weather/All_sites_2017_2018_MUDS2_3_points.png', width = 16, height = 10)


# check wind direction at DOUGLAS
# plot all data after removing some outliers
df %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  # select only wind direction
  filter(key == 'WindDirection') %>%
  # remove erronoues readings
  # Wind Direction >360° @ MUDS2 
  filter(!(key == 'WindDirection' & value > 360)) %>% 
  weather_labels() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=SiteID)) +
  geom_point() +
  facet_grid(SiteID ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Weather Data',
       subtitle = '(daily)',
       col = 'Site ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Weather/All_sites_2017_2018_wind_direction.png', width = 16, height = 10)


# check soil temp at SUBSURF
# plot all data after removing some outliers
df %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  # select only wind direction
  filter(key == 'SoilTemperature') %>%
  weather_labels() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=SiteID)) +
  geom_point() +
  facet_grid(SiteID ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Weather Data',
       subtitle = '(daily)',
       col = 'Site ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Weather/All_sites_2017_2018_soil_temp.png', width = 16, height = 10)




# check wind directions
# plot all data after removing some outliers
df %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  # select only wind direction
  filter(key == 'WindDirection') %>%
  # # remove MUDS2 since its wind direction has some errors
  # filter(SiteID != "MUDS2") %>%
  weather_labels() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=SiteID)) +
  geom_point() +
  facet_grid(SiteID ~ .) +
  labs(x = NULL, y = NULL, 
       title = 'Wind Direction',
       subtitle = '(daily)',
       col = 'Site ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  scale_y_continuous(limits = c(0, 360), breaks = seq(0, 360, 60)) +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Weather/All_sites_2017_2018_wind_direction.png', width = 16, height = 10)
