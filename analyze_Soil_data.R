source('00_project_settings.R')


# Analyze Soil data

# read the latest soil data for all sites
ReadSoilData() %>% 
  as.tibble() %>% 
  select(-sheet) -> soil


# Check Completeness of the Data ------------------------------------------

# Check comments
soil %>%
  count(SiteID, CommentsSoilSamples) %>% filter(!is.na(CommentsSoilSamples))


# see which sites-years are in the database
soil %>%
  # drop comments
  select(-CommentsSoilSamples) %>%
  gather(key, value, 7:25) %>%
  filter(!is.na(value)) %>%
  mutate(year = year(SoilSampleDate)) %>%
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

# prepare dataframe for analysis
soil %>%
  # drop comments
  select(-CommentsSoilSamples, -SoilTexture) %>%
  gather(key, value, 7:25) %>% # this waas from 7:24, but than SpringSoilNH4 was added at DOUGLAS
  filter(!is.na(value)) %>%
  mutate(year = year(SoilSampleDate)) -> df

# check dates when soil samples were taken
df %>%
  count(SiteID, SoilSampleDate)

# check if SOC, TN, NO3 and NH4 are calculated correctly
df %>%
  filter(str_detect(key, 'FallSOC|FallSoilTN|FallSoilNO3|FallSoilNH4|Bulk')) %>% 
  filter(value != -9999) %>%
  spread(key, value) %>%
  gather(key, value, 9:16) %>%
  separate(key, into = c('var', 'type'), sep = '_') %>%
  mutate(type = ifelse(is.na(type), 'conc', type)) %>%
  spread(type, value) %>%
  filter(!(is.na(calc) | is.na(conc))) %>% 
  separate(SoilDepth, into = c('top', 'bot'), sep = "-", convert = TRUE, remove = FALSE) %>%
  mutate(bot = parse_number(bot)) %>%
  mutate(depth = bot - top) %>%
  mutate(check = BulkDensity/1000*depth*10^8*conc,
         check = ifelse(var %in% c('FallSoilTN'), check/10^9, check/10^6)) -> df_calc

# see sites and depths with incorect calculations 
df_calc %>%
  mutate(CHECK = near(calc, check, tol = 0.1)) %>%
  filter(CHECK == 0) %>%
  count(SiteID, SoilDepth, var) %>% 
  spread(var, n)

# visualize problematic calculatios
df_calc %>%
  mutate(depth = as.factor(depth)) %>%
  ggplot(aes(x=conc, shape = depth)) +
  geom_point(aes(y=check), col = 'grey30', size = 2.5) +
  geom_point(aes(y=calc, col=SiteID), size = 1.5) +
  facet_wrap(var ~ ., scales = 'free') + 
  theme_gio2

# plot depth-weighted values calculatios
df_calc %>%
  mutate(calc = calc/depth,
         depth = as.factor(depth)) %>%
  ggplot(aes(x=conc, y=calc, col=SiteID)) +
  geom_point( size = 1.5) +
  facet_wrap(var ~ ., scales = 'free') + 
  labs(title = 'Relationship between\nConcentration and Depth Weighted Amount of Nutrients/Carbon',
       col = 'Reseach Site',
       x = 'Concentration, mg N kg-1 / g C kg-1',
       y = 'Amount of Nitrate/Carbon in 1 cm profile, t ha-1') +
  theme_gio2
ggsave('Figs/Soil/All_sites_Soil_Fall_Nutrients_2017_2018.png', 
       width = 12, height = 8)

# plot problematic data for NWRF
df_calc %>%
  filter(SiteID == "NWRF") %>%
  filter(var == 'FallSoilNH4') %>%
  gather(key, value, calc, check) %>%
  mutate(key = ifelse(depth == 15, 'both', key)) %>%
  mutate(key = factor(key, levels = c('calc', 'check', 'both'), labels = c('PI', "Data Team", 'Both'))) %>%
  arrange(key) %>%
  ggplot(aes(x=conc, y=value, col=key)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#444444")) +
  facet_grid(var ~ SoilDepth, scales = 'free', switch = 'y', shrink = F)  +
  labs(y = 'Fall Soil Total NH4 (kg NH4-N ha-1)', 
       x = 'Fall Soil NH4 Concentration (mg NH4-N kg-1 soil)',
       col = 'Calculated by') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.text.y = element_blank(),
        strip.background = element_blank()) -> p1
df_calc %>%
  filter(SiteID == "NWRF") %>%
  filter(var == 'FallSoilNO3') %>%
  gather(key, value, calc, check) %>%
  mutate(key = 'both') %>%
  arrange(key) %>%
  ggplot(aes(x=conc, y=value, col=key)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("#444444")) +
  facet_grid(var ~ SoilDepth, scales = 'free', switch = 'y', shrink = F)  +
  labs(y = 'Fall Soil Total NO3 (kg NO3-N ha-1)', 
       x = 'Fall Soil NO3 Concentration (mg NO3-N kg-1 soil)',
       col = 'Calculated by') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.text.y = element_blank(),
        strip.background = element_blank()) -> p2
title <- 
  cowplot::ggdraw() + 
  cowplot::draw_label("NWRF", fontface='bold', size = 20)
cowplot::plot_grid(title,
                   p1 + theme(legend.position="none"), 
                   p2 + theme(legend.position="none"), 
                   ncol = 1,
                   rel_heights = c(0.1, 1, 1)) -> p3
cowplot::plot_grid(p3, cowplot::get_legend(p1), rel_widths = c(3, 0.3)) 
ggsave(filename = 'Figs/Soil/NWRF_2018_Soil_NO3_and_NH4.png', width = 16, height = 10)



# plot problematic data for MUDS2
df_calc %>%
  filter(SiteID == "MUDS2") %>%
  gather(key, value, calc, check) %>%
  mutate(key = factor(key, levels = c('calc', 'check'), labels = c('PI', "Data Team"))) %>%
  filter(var != 'FallSOC') %>%
  arrange(key) %>%
  ggplot(aes(x=conc, y=value, col=key)) +
  geom_point(aes(size = key)) +
  facet_grid(SoilDepth ~ var, scales = 'free', switch = 'x') + 
  scale_size_manual(values = c(3.5, 2), guide = 'none') +
  scale_color_manual(values = c('#444444', '#D95F02')) +
  labs(x = NULL,
       y = 'Total Ammount (t N/ha)',
       title = 'MUDS2',
       col = 'Calculated by') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.background.x = element_blank(),
        strip.placement.x = 'outside')
ggsave(filename = 'Figs/Soil/MUDS2_2018_Soil_NO3_and_TN.png', width = 16, height = 10)


# plot problematic data for SUBSURF
df_calc %>%
  filter(SiteID == "SUBSURF") %>%
  # filter(year(SoilSampleDate) == 2017) %>%
  gather(key, value, calc, check) %>%
  mutate(key = factor(key, levels = c('calc', 'check'), labels = c('PI', "Data Team"))) %>%
  filter(var != 'FallSOC') %>%
  arrange(key) %>%
  ggplot(aes(x=conc, y=value, col=key)) +
  geom_point(aes(size = key)) +
  facet_grid(SoilDepth ~ var, scales = 'free', switch = 'x') + 
  scale_size_manual(values = c(3.5, 2), guide = 'none') +
  scale_color_manual(values = c('#444444', '#D95F02')) +
  labs(x = NULL,
       y = 'Total Ammount (t N/ha)',
       title = 'SUBSURF',
       col = 'Calculated by') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.background.x = element_blank(),
        strip.placement.x = 'outside')
ggsave(filename = 'Figs/Soil/SUBSURF_2018_Soil_NO3_and_TN.png', width = 16, height = 10)

# Fall soil NH4 is very different form year to year
df_calc %>%
  filter(SiteID == "SUBSURF") %>%
  mutate(year = as.factor(year(SoilSampleDate))) %>%
  filter(str_detect(var, 'FallSoilN')) %>%
  ggplot(aes(x=year, y=calc, fill = var)) +
  geom_boxplot() +
  facet_grid(SoilDepth ~ var, scales = 'free') + 
  labs(x = NULL,
       y = 'N Concentration (mg N/kg soil)',
       title = 'SUBSURF') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.background.x = element_blank(),
        strip.placement.x = 'outside')
# ggsave(filename = 'Figs/Soil/SUBSURF_2018_Soil_NO3_and_TN.png', width = 16, height = 10)


# plot problematic data for ONT_4R
df_calc %>%
  filter(SiteID == "ONT_4R") %>%
  gather(key, value, calc, check) %>%
  mutate(key = factor(key, levels = c('calc', 'check'), labels = c('PI', "Data Team"))) %>%
  filter(var != 'FallSOC') %>%
  filter(var != 'FallSoilNO3') %>%
  arrange(key) %>%
  ggplot(aes(x=conc, y=value, col=key)) +
  geom_point(aes(size = key)) +
  facet_grid(SoilDepth ~ var, scales = 'free', switch = 'x') + 
  scale_size_manual(values = c(3.5, 2), guide = 'none') +
  scale_color_manual(values = c('#444444', '#D95F02')) +
  labs(x = NULL,
       y = 'Total Ammount (t N/ha)',
       title = 'ONT_4R',
       col = 'Calculated by') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.background.x = element_blank(),
        strip.placement.x = 'outside')
ggsave(filename = 'Figs/Soil/ONT_4R_2018_Soil_NH4_and_TN.png', width = 16, height = 10)


# plot problematic data for DOUGLAS
df_calc %>%
  filter(SiteID == "DOUGLAS") %>%
  gather(key, value, calc, check) %>%
  mutate(key = factor(key, levels = c('calc', 'check'), labels = c('PI', "Data Team"))) %>%
  filter(var != 'FallSOC') %>%
  arrange(key) %>%
  ggplot(aes(x=conc, y=value, col=key)) +
  geom_point(aes(size = key)) +
  facet_grid(SoilDepth ~ var, scales = 'free', switch = 'x') + 
  scale_size_manual(values = c(3.5, 2), guide = 'none') +
  scale_color_manual(values = c('#444444', '#D95F02')) +
  labs(x = NULL,
       y = 'Total Ammount (t N/ha)',
       title = 'DOUGLAS',
       col = 'Calculated by') +
  guides(col = guide_legend(override.aes = list(size = 5, shape = 15))) +
  theme_gio2 +
  theme(strip.background.x = element_blank(),
        strip.placement.x = 'outside')
ggsave(filename = 'Figs/Soil/DOUGLAS_2018_Soil_NO3_and_TN.png', width = 16, height = 10)



# functions creates labels for plotting
soil_labels <- function(df) {
  df %>%
    mutate(key = factor(key, levels = c("BulkDensity",
                                        "PercentSand",
                                        "PercentSilt",
                                        "PercentClay",
                                        "SoilpH",
                                        "FallSOC",
                                        "FallSoilTN",
                                        "FallSoilNO3",
                                        "FallSoilNH4",
                                        "FallSOC_calc",
                                        "FallSoilTN_calc",
                                        "FallSoilNO3_calc",
                                        "FallSoilNH4_calc",
                                        "SpringSoilNO3",
                                        "SoilCa",
                                        "SoilMg",
                                        "SoilP",
                                        "SoilK"),
                        labels = c("Bulk Density\n(g cm-3)",
                                   "Percent Sand\n(%)",
                                   "Percent Silt\n(%)",
                                   "Percent Clay\n(%)",
                                   "Soil pH\n(unitless)",
                                   "Fall SOC\n(g C kg-1 dry soil)",
                                   "Fall Soil TN\n(mg N kg-1 soil)",
                                   "Fall Soil NO3\n(mg NO3-N kg-1 soil)",
                                   "Fall Soil NH4\n(mg NH4-N kg-1 soil)",
                                   "Fall SOC calc\n(t C ha-1)",
                                   "Fall Soil TN calc\n(t N ha-1)",
                                   "Fall Soil NO3 calc\n(kg NO3-N ha-1)",
                                   "Fall Soil NH4 calc\n(kg NH4-N ha-1)",
                                   "Spring Soil NO3\n(mg NO3-N kg-1 soil)",
                                   "Soil Ca\n(ug Ca g-1)",
                                   "Soil Mg\n(ug Mg g-1)",
                                   "Soil P\n(ug P g-1)",
                                   "Soil K\n(ug K g-)1"
                                   )))
}  


# plot all data
df %>%
  # remove -9999
  filter(value != -9999) %>%
  soil_labels() %>%
  ggplot(aes(x=SoilDepth, y=value, group=PlotID)) +
  geom_point() +
  facet_grid(key ~ SiteID, scales = 'free', 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soil Data') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text.y = element_text(size = 10, angle = 180))
ggsave(filename = 'Figs/Soil/All_sites_2017_2018.png', width = 16, height = 10)



# plot Fall NO3 data
df %>%
  # remove -9999
  filter(value != -9999) %>%
  # get rid of calculated values
  filter(key == 'FallSoilNO3') %>%
  soil_labels() %>%
  ggplot(aes(x=SoilDepth, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.25, col = 'grey30', alpha = 0.75, size = 1.25) +
  facet_grid(key ~ SiteID, scales = 'free', 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soil Data') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text.y = element_text(size = 10))
ggsave(filename = 'Figs/Soil/All_sites_Soil_Fall_NO3_2017_2018.png', width = 12, height = 8)


# plot Bulk Density data
df %>%
  # remove -9999
  filter(value != -9999) %>%
  # get rid of calculated values
  filter(key == 'BulkDensity') %>%
  soil_labels() %>%
  ggplot(aes(x=SoilDepth, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.25, col = 'grey30', alpha = 0.75, size = 1.25) +
  facet_grid(key ~ SiteID, scales = 'free', 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soil Data') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text.y = element_text(size = 10))
ggsave(filename = 'Figs/Soil/All_sites_Soil_BD_2017_2018.png', width = 12, height = 8)


# plot some data
df %>%
  # remove -9999
  filter(value != -9999) %>%
  # get rid of calculated values
  filter(!str_detect(key, '_calc')) %>%
  # there is no problems with NH4 and spring and fall NO3 at NWRF
  filter(!str_detect(key, 'NH4|NO3')) %>%
  # get rid of bulk density
  filter(!str_detect(key, 'Bulk')) %>%
  soil_labels() %>%
  ggplot(aes(x=SoilDepth, y=value, group=PlotID)) +
  geom_point() +
  facet_grid(key ~ SiteID, scales = 'free', 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soil Data') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text.y = element_text(size = 10, angle = 180))
# ggsave(filename = 'Figs/Soil/All_sites_2017_2018.png', width = 16, height = 10)


# check SOC to TN relationship
df %>%
  filter(str_detect(key, 'SOC|TN'),
         !str_detect(key, '_calc')) %>%
  spread(key, value) %>%
  mutate(CN_ratio = FallSOC/FallSoilTN*1000) %>% #filter(CN_ratio < 3)
  # calculate C:N ratio
  gather(key, value, FallSOC:CN_ratio) %>%
  soil_labels() %>%
  mutate(key = as.character(key)) %>%
  mutate(key = ifelse(is.na(key), 'C:N ratio', key)) %>%
  ggplot(aes(x=SoilDepth, y=value, group=PlotID)) +
  geom_point() +
  facet_grid(key ~ SiteID, scales = 'free', 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soil Data') +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text.y = element_text(size = 10, angle = 180))
ggsave(filename = 'Figs/Soil/All_sites_CN_ratio_2017_2018.png', width = 16, height = 10)



# plot soil P data
df %>%
  # remove -9999
  filter(value != -9999) %>%
  # get rid of calculated values
  filter(key == 'SoilP') %>% #filter(value > 300)
  soil_labels() %>%
  ggplot(aes(x=SoilDepth, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.25, col = 'grey30', alpha = 0.75, size = 1.25) +
  facet_grid(key ~ SiteID, scales = 'free', 
             switch = 'y') +
  labs(x = NULL, y = NULL, 
       title = 'Soil Data') +
  # scale_y_log10() +
  theme_gio2 +
  theme(strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text.y = element_text(size = 10))
ggsave(filename = 'Figs/Soil/All_sites_Soil_P_2017_2018.png', width = 12, height = 8)
