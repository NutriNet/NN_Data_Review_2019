source('00_project_settings.R')

# Analyze Water data

# read the latest weather data for all sites
ReadWaterData() %>% 
  as.tibble() %>% 
  select(-sheet) -> water


# Check Completeness of the Data ------------------------------------------

# Check comments
water %>% 
  count(SiteID, CommentsDailyDrainage) %>%
  filter(!is.na(CommentsDailyDrainage)) 


# see which sites-years are in the database
water %>%
  # drop comments
  select(-contains('Comments')) %>%
  # get rid of runoff data
  select(-contains('Runoff')) %>%
  gather(key, value, 4:11) %>%
  filter(!is.na(value)) %>%
  mutate(year = year(MeasurementDate)) %>%
  count(SiteID, key, year) -> df

# check years with data
df %>%
  count(SiteID, year) %>%
  spread(year, nn)

# check vars collected at each site
df %>%
  filter(SiteID == 'DUDLEY') %>%
  spread(year, n)


# Check Validity of Measurements ------------------------------------------

# prepare dataframe for analysis ------------------------------------------
water %>%
  # drop comments
  select(-contains('Comments')) %>%
  # get rid of runoff data
  select(-contains('Runoff')) %>%
  gather(key, value, 4:11) %>%
  filter(!is.na(value)) %>%
  mutate(year = year(MeasurementDate)) -> df

# see if there are multiple measurements within a day at each plot
df %>%
  filter(value != -9999) %>%
  add_count(SiteID, PlotID, MeasurementDate, key) %>%
  filter(n != 1)
# there was NONE, hence I can ignore time of sample when plotting 



# functions creates labels for plotting
water_labels <- function(df) {
  df %>%
    mutate(key = factor(key, levels = c('DailyDrainage',
                                        'DailyRunoff',
                                        'DrainageTotalNConc',
                                        'DrainageNO3Conc',
                                        'DrainageNH3Conc',
                                        'DrainageTotalPConc',
                                        'DrainageReactivePConc',
                                        'DrainageTotalKConc',
                                        'DrainageDissolvedKConc',
                                        'RunoffTotalNConc',
                                        'RunoffNO3Conc',
                                        'RunoffTotalPConc',
                                        'RunoffReactivePConc'),
                        labels = c('Daily Drainage\n(mm)',
                                   'Daily Runoff\n(mm)',
                                   'Drainage Total N Conc\n(mg N liter-1)',
                                   'Drainage NO3 Conc\n(mg NO3-N liter-1)',
                                   'Drainage NH3 Conc\n(mg NH3-N liter-1)',
                                   'Drainage Total P Conc\n(mg P liter-1)',
                                   'Drainage Reactive P Conc\n(mg P liter-1)',
                                   'Drainage Total K Conc\n(mg K liter-1)',
                                   'Drainage Dissolved K Conc\n(mg K liter-1)',
                                   'Runoff Total N Conc\n(mg N liter-1)',
                                   'Runoff NO3 Conc\n(mg NO3-N liter-1)',
                                   'Runoff Total P Conc\n(mg P liter-1)',
                                   'Runoff Reactive P Conc\n(mg P liter-1)')))
}  


# plot all draily drainge data
df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  water_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  # geom_point() +
  geom_line() +
  facet_grid(SiteID ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Data',
       subtitle = '(daily)',
       col = 'Site ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/All_sites_Drainage_2017_2018.png', width = 16, height = 10)


# plot NWRF draily drainge data
df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  filter(SiteID == "NWRF") %>%
  water_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  # geom_point() +
  geom_line() +
  facet_grid(SiteID ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Data at NWRF',
       subtitle = '(daily)',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/NWRF_Drainage_2018.png', width = 16, height = 10)


# plot all Annual drainge data for DUDLEY
df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  filter(SiteID == "DUDLEY") %>% 
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001),
         MeasurementDate < ymd(20181001)) %>%
  group_by(SiteID, PlotID, key) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  full_join(weather %>% 
              filter(SiteID == "DUDLEY") %>% 
              filter(MeasurementDate >= ymd(20171001),
                     MeasurementDate < ymd(20181001)) %>%
              group_by(SiteID) %>%
              summarise(Precipitation = sum(DailyPrecipitation, na.rm = TRUE)), by = c("SiteID")) %>%
  water_labels() %>%
  mutate(Order = as.numeric(PlotID)) %>%
  mutate(text = round(value/Precipitation*100, 1),
         text = paste0(text, '%')) %>%
  ggplot(aes(x=reorder(PlotID, Order), y=value)) +
  geom_col() +
  geom_hline(aes(yintercept = Precipitation), col = 'skyblue', size = 3) +
  geom_text(aes(label = text), vjust = -1.1, size = 5) +
  labs(x = "Plot ID", y = "Drainage/Precipitation (mm)", 
       title = 'Annual Drainage at DUDLEY',
       subtitle = '(water year 2018)') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/DUDLEY_Drainage_Annual_2018.png', width = 12, height = 8)


# plot all Annual drainge data for DOUGLAS
df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  filter(SiteID == "DOUGLAS") %>% 
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001),
         MeasurementDate < ymd(20181001)) %>%
  group_by(SiteID, PlotID, key) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  full_join(weather %>% 
              filter(SiteID == "DOUGLAS") %>% 
              filter(MeasurementDate >= ymd(20171001),
                     MeasurementDate < ymd(20181001)) %>%
              group_by(SiteID) %>%
              summarise(Precipitation = sum(DailyPrecipitation, na.rm = TRUE)), by = c("SiteID")) %>%
  water_labels() %>%
  mutate(Order = as.numeric(PlotID)) %>%
  mutate(text = round(value/Precipitation*100, 0),
         text = paste0(text, '%')) %>%
  ggplot(aes(x=reorder(PlotID, Order), y=value)) +
  geom_col() +
  geom_hline(aes(yintercept = Precipitation), col = 'skyblue', size = 3) +
  geom_text(aes(label = text), vjust = -1.1, size = 3) +
  labs(x = "Plot ID", y = "Drainage/Precipitation (mm)", 
       title = 'Annual Drainage at DOUGLAS',
       subtitle = '(water year 2018)') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/DOUGLAS_Drainage_Annual_2018.png', width = 12, height = 8)


# plot MUDS2 draily drainge data
# set a fake df to force scale of intrest on Y 
fdf <- tibble(PlotID = c( "103", "104", "105", "106", "201", "202", "205", "206"),
              MeasurementDate = rep(ymd('2018-05-05'), 8),
              value = c(1050, rep(3, 7)))

df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  filter(SiteID == "MUDS2") %>%
  water_labels() %>%
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001)) %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  geom_point(data = fdf, color = 'white') +
  geom_line() +
  facet_grid(PlotID ~ ., scales = 'free', 
             # labeller = label_wrap_gen(width = 12), 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Data at NWRF',
       subtitle = '(daily)',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/MUDS2_Drainage_2018.png', width = 16, height = 10)


# plot all Annual drainge data for NWRF
df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  filter(SiteID == "NWRF") %>% 
  # include only years relevant to the project
  filter(MeasurementDate >= ymd(20171001),
         MeasurementDate < ymd(20181001)) %>%
  group_by(SiteID, PlotID, key) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  full_join(weather %>% 
              filter(SiteID == "NWRF") %>% 
              filter(MeasurementDate >= ymd(20171001),
                     MeasurementDate < ymd(20181001)) %>%
              group_by(SiteID) %>%
              summarise(Precipitation = sum(DailyPrecipitation, na.rm = TRUE)), by = c("SiteID")) %>%
  water_labels() %>%
  mutate(Order = as.numeric(PlotID)) %>%
  mutate(text = round(value/Precipitation*100, 0),
         text = paste0(text, '%')) %>%
  ggplot(aes(x=reorder(PlotID, Order), y=value)) +
  geom_col() +
  geom_hline(aes(yintercept = Precipitation), col = 'skyblue', size = 3) +
  geom_text(aes(label = text), vjust = -1.1, size = 4) +
  labs(x = "Plot ID", y = "Drainage/Precipitation (mm)", 
       title = 'Annual Drainage at NWRF',
       subtitle = '(water year 2018)') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 0))
ggsave(filename = 'Figs/Water/NWRF_Drainage_Annual_2018.png', width = 12, height = 8)



# plot KELLEY draily drainge data
df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  filter(SiteID == "KELLEY") %>%
  water_labels() %>%
  mutate(date = update(MeasurementDate, year = 2012)) %>%
  ggplot(aes(x=date, y=value, colour=PlotID)) +
  # geom_point() +
  geom_line() +
  facet_grid(year ~ ., scales = 'free') +
  labs(x = NULL, y = NULL, 
       title = 'Water Data at KELLEY',
       subtitle = '(Daily Drainage in mm)',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '1 months', date_labels = '%b') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2
ggsave(filename = 'Figs/Water/KELLEY_Drainage_2016_2018.png', width = 16, height = 10)


# plot TRO draily drainge data
df %>%
  filter(value != -9999) %>%
  filter(key == 'DailyDrainage') %>%
  filter(SiteID == "TRO") %>%
  water_labels() %>%
  mutate(date = update(MeasurementDate, year = 2012)) %>%
  ggplot(aes(x=date, y=value, colour=PlotID)) +
  # geom_point() +
  geom_line() +
  facet_grid(year ~ .) +
  labs(x = NULL, y = NULL, 
       title = 'Water Data at TRO',
       subtitle = '(Daily Drainage in mm)',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '1 months', date_labels = '%b') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 1)) +
  theme_gio2
ggsave(filename = 'Figs/Water/TRO_Drainage_2017_2018.png', width = 16, height = 10)



# prepare water quality data ----------------------------------------------
water %>%
  # get rid of runoff data
  select(-contains('Runoff')) %>%
  gather(key, value, 4:19) %>%
  # change keys
  mutate(key2 = ifelse(str_detect(key, 'Comments'), 'Comments', 'Measurement')) %>%
  mutate(key = str_remove(key, 'Comments')) %>%
  mutate(key = str_remove(key, '^Drainage')) %>% 
  mutate(key = str_remove(key, 'Conc$')) %>%
  spread(key2, value) %>%
  mutate(Measurement = as.numeric(Measurement)) %>%
  filter(!is.na(Measurement)) %>%
  mutate(year = year(MeasurementDate)) %>%
  arrange(SiteID, PlotID, key, MeasurementDate) %>%
  mutate(value = Measurement) -> df


# functions creates labels for plotting
water_labels2 <- function(df) {
  df %>%
    mutate(key = factor(key, levels = c('DailyDrainage',
                                        'TotalN',
                                        'NO3',
                                        'NH3',
                                        'TotalP',
                                        'ReactiveP',
                                        'TotalK',
                                        'DissolvedK'),
                        labels = c('Daily Drainage\n(mm)',
                                   'Drainage Total N Conc\n(mg N liter-1)',
                                   'Drainage NO3 Conc\n(mg NO3-N liter-1)',
                                   'Drainage NH3 Conc\n(mg NH3-N liter-1)',
                                   'Drainage Total P Conc\n(mg P liter-1)',
                                   'Drainage Reactive P Conc\n(mg P liter-1)',
                                   'Drainage Total K Conc\n(mg K liter-1)',
                                   'Drainage Dissolved K Conc\n(mg K liter-1)')))
}  


# plot water quality data at NWRF
df %>%
  filter(value != -9999) %>%
  filter(key != 'DailyDrainage') %>%
  filter(!Comments %in%  c('Imputed', 'Composite', 'BDL')) %>% 
  filter(SiteID == 'NWRF') %>%
  # mutate(value = ifelse(str_detect(key, 'P'), log10(value), value)) %>%
  water_labels2() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Quality Data at NWRF',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/NWRF_Water_Quality_2017_2018.png', width = 16, height = 10)



# plot water quality data at DUDLEY
df %>%
  filter(value != -9999) %>%
  filter(key != 'DailyDrainage') %>%
  filter(!Comments %in%  c('Imputed', 'Composite', 'BDL')) %>% 
  filter(SiteID == 'DUDLEY') %>%
  # mutate(value = ifelse(str_detect(key, 'P'), log10(value), value)) %>%
  water_labels2() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Quality Data at DUDLEY',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/DUDLEY_Water_Quality_2017_2018.png', width = 16, height = 10)


# plot water quality data at DOUGLAS
df %>%
  filter(value != -9999) %>%
  filter(key != 'DailyDrainage') %>%
  filter(!Comments %in%  c('Imputed', 'Composite', 'BDL')) %>% 
  filter(SiteID == 'DOUGLAS') %>%
  # mutate(value = ifelse(str_detect(key, 'P'), log10(value), value)) %>%
  water_labels2() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Quality Data at DOUGLAS',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/DOUGLAS_Water_Quality_2017_2018.png', width = 16, height = 10)


# plot water quality data at KELLEY
df %>%
  filter(value != -9999) %>%
  filter(key != 'DailyDrainage') %>%
  filter(!Comments %in%  c('Imputed', 'Composite', 'BDL')) %>% 
  filter(SiteID == 'KELLEY') %>%
  # mutate(value = ifelse(str_detect(key, 'P'), log10(value), value)) %>%
  water_labels2() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Quality Data at KELLEY',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/KELLEY_Water_Quality_2016_2018.png', width = 16, height = 10)


# plot water quality data at MUDS2
df %>%
  filter(value != -9999) %>%
  filter(key != 'DailyDrainage') %>%
  filter(!Comments %in%  c('Imputed', 'Composite', 'BDL')) %>% 
  filter(SiteID == 'MUDS2') %>%
  # mutate(value = ifelse(str_detect(key, 'P'), log10(value), value)) %>%
  water_labels2() %>%
  ggplot(aes(x=MeasurementDate, y=value, colour=PlotID)) +
  geom_point() +
  facet_grid(key ~ ., scales = 'free', switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Water Quality Data at MUDS2',
       col = 'Plot ID') +
  scale_x_date(date_breaks = '2 months', date_labels = '%b-%Y') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15), 
                            ncol = 2)) +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank())
ggsave(filename = 'Figs/Water/MUDS2_Water_Quality_2017_2018.png', width = 16, height = 10)


# CHECK variables collected and compare with what they should report (from NutriNet Data Collected)
df %>%
  filter(SiteID == "DOUGLAS") %>%
  count(year, key) %>%
  spread(key, n)

