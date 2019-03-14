library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(scales)
library(units)
library(viridis)
library(RCurl)
library(curl)
library(zoo)
library(lubridate)
#----------------------------------------------------------------------------

########################################
## FUNCTIONS
########################################

monitor_dat_to_disk <- function(YEAR) {
  # create url for zip files of annual monitoring data
  url_path <- paste("https://aqs.epa.gov/aqsweb/airdata/annual_conc_by_monitor_", 
                    YEAR, 
                    ".zip", 
                    sep = "")
  
  # create name of destfile
  destfile_name <- paste("raw_data/annual_monitors_zip/annual_conc_by_monitor_",
                         YEAR,
                         ".zip",
                         sep = "")
                         
                         
  # download statement
  curl_download(url_path, destfile = destfile_name)
  
}



unzip_mon_dat <- function(YEAR) {
  
  # name of zip file
  zip_file <- paste("raw_data/annual_monitors_zip/annual_conc_by_monitor_",
                    YEAR,
                    ".zip",
                    sep = "")
  
  
  # unzip csv files of annual monitoring data
  csv_name <- paste("annual_conc_by_monitor_",
                    YEAR,
                    ".csv",
                    sep = "")
  
  # extract directory path
  ex_path <- "raw_data/annual_monitors_csv"
                   
  
  # print(zip_file)
  # print(tif_name)
  # print(ex_path)
  
  unzip(zipfile = zip_file,
        files = csv_name,
        exdir = ex_path)
  
}  


########################################
## functions + font misc
########################################


theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "black"),
      plot.title = element_text(face = "bold"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}


#----------------------------------------------------------------------------

#----------------------------------------------------------------------------

########################################
## load data
########################################

planning <- readOGR("gis/planning_area")
planning <- spTransform(planning, CRS("+init=epsg:4269 +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
planning_df <- broom::tidy(planning)

wayne <- readOGR("gis/wayne_nf_simple")
wayne_50km <- gBuffer(wayne, width = 50000)
wayne_50km <- spTransform(wayne_50km, CRS("+init=epsg:4269 +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
wayne <- spTransform(wayne, CRS("+init=epsg:4269 +proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
wayne_df <- broom::tidy(wayne)



#----------------------------------------------------------------------------


########################################
## download annual data from site
########################################

years <- seq(1980, 2017, 1)

lapply(years, function(x) {monitor_dat_to_disk(x)})



#----------------------------------------------------------------------------


########################################
## unzip monitoring data 
########################################

lapply(years, function(x) {unzip_mon_dat(x)})


#----------------------------------------------------------------------------


########################################
## convert csv to spatial points and extract monitors in planning area
########################################

csv_files <- list.files("raw_data/annual_monitors_csv")

get_planning_mons <- function(CSVFILE) {
 
  csv_filepath <- paste("raw_data/annual_monitors_csv/",
                        CSVFILE,
                        sep = "")
  
  # load data and convert to spatialpointsdataframe
  csv_dat <- read.csv(csv_filepath, stringsAsFactors = FALSE) %>% 
    filter(!(is.na(Latitude)))
  
  coordinates(csv_dat) <- c("Longitude", "Latitude")
  proj4string(csv_dat) <- CRS("+init=epsg:4269")
  
  # intersect with planning area 
  planning_mons <- raster::intersect(csv_dat, planning) %>% 
    as.data.frame() %>% 
    mutate_if(is.factor, as.character)
}


planning_mons_df <- plyr::ldply(csv_files, get_planning_mons )
write_csv(planning_mons_df, "data/planning_area_air_monitors_annual.csv")



#----------------------------------------------------------------------------


########################################
## ozone/pm trends
########################################



# after reviewing the freq of use, selected the following parameters
o3_pm_dat <- read_csv("data/planning_area_air_monitors_annual.csv") %>% 
  filter(Parameter.Name %in% c("Ozone", 
                               # "PM10-2.5 - Local Conditions", 
                               # "PM10-2.5 STP",
                               # "PM10 - LC",                           
                               "PM10 Total 0-10um STP",
                               "PM2.5 - Local Conditions"             
                               # "PM2.5 Raw Data", 
                               # "PM2.5 Total Atmospheric"
                               )) %>% 
  mutate(State.Code = as.character(str_pad(State.Code, 2, pad = "0")),
         County.Code = as.character(str_pad(County.Code, 3, pad = "0")),
         Site.Num = as.character(str_pad(Site.Num, 4, pad = "0"))) %>% 
  mutate(monitor_id = paste(State.Code,
                            County.Code,
                            Site.Num,
                            Parameter.Code,
                            POC,
                            sep = "_"))
    

# par_nums <- o3_pm_dat %>% 
#   group_by(Year, Parameter.Name) %>% 
#   summarise(n_vars = n()) %>% 
#   ungroup()
  
par_units <- o3_pm_dat %>% 
  dplyr::select(Year, Parameter.Name, Metric.Used) %>% 
  distinct()


# ozone
o3_dat <- o3_pm_dat %>% 
  filter(Parameter.Name == "Ozone") %>% 
  filter(Pollutant.Standard == "Ozone 8-hour 2015") %>% 
  filter(Year > 1998) %>% 
  # mutate(Year = lubridate::ymd(Year, truncated = 2L)) %>% 
  dplyr::select(monitor_id, Year, X4th.Max.Value) %>% 
  distinct() %>% 
  group_by(monitor_id) %>% 
  arrange(Year) %>% 
  mutate(mean_3_year = rollapply(X4th.Max.Value, 
                                 width = 3, 
                                 FUN = mean,
                                 fill = NA,
                                 align = "right")) %>% 
  ungroup()




ggplot(o3_dat, aes(Year, mean_3_year, group = monitor_id)) +
  geom_point(color = "darkred",
             alpha = 0.8, 
             size = 4) +
  geom_hline(yintercept = 0.070, 
             linetype = "dashed",
             size = 1.4) +
  theme_minimal() +
  labs(x = "Year (end of 3-year average)",
       y = "Ozone (ppm)",
       title = "Ozone") +
  scale_x_continuous(limits = c(2000, 2018),
    breaks = seq(2001, 2017, 4),
    minor_breaks = seq(2001, 2017, 1),
    labels = seq(2001, 2017, 4)) +
  scale_y_continuous(limits = c(0, 0.09),
                     breaks = seq(0,0.09,0.01)) +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 18)) +

  annotate("text", 
           x = 2016,
           y = 0.078,
           label = "2015 Ozone \nNAAQS",
           size = 4)


ggsave("figures/ozone_3_year_mean.jpg",
       height = 4,
       width = 6,
       unit = "in")
  



# pm 

pm_2_5_dat <- o3_pm_dat %>% 
  filter(Parameter.Name != "Ozone") %>% 
  filter(Metric.Used == "Daily Mean") %>% 
  filter(Year > 1990) %>% 
  # mutate(Year = lubridate::ymd(Year, truncated = 2L)) %>% 
  dplyr::select(monitor_id, Year, Parameter.Name, Arithmetic.Mean, X98th.Percentile) %>% 
  group_by(monitor_id, Parameter.Name) %>% 
  arrange(Year) %>% 
  mutate(mean_3_year = rollapply(Arithmetic.Mean, 
                                 width = 3, 
                                 FUN = mean,
                                 fill = NA,
                                 align = "right"),
         mean_98th_3_year = rollapply(X98th.Percentile, 
                                      width = 3, 
                                      FUN = mean,
                                      fill = NA,
                                      align = "right")) %>% 
  ungroup() %>% 
  dplyr::select(-Arithmetic.Mean, -X98th.Percentile) %>% 
  filter(Parameter.Name == "PM2.5 - Local Conditions") 



ggplot(pm_2_5_dat, aes(Year, mean_3_year)) +
  geom_point(color = "darkred",
             alpha = 0.8, 
             size = 4) +
  geom_hline(aes(yintercept = 12),
             linetype = "dashed",
             size = 1.4) +
  theme_minimal() +
  labs(x = "Year (end of 3-year average)",
       y = expression(paste("Particulate ", matter[2.5], " (", mu, g, "/", m^3, ")", sep = "")),
       title = expression(paste("Mean Annual Particulate ", Matter[2.5], sep = ""))) +
  scale_x_continuous(limits = c(2000, 2018),
                     breaks = seq(2001, 2017, 4),
                     minor_breaks = seq(2001, 2017, 1),
                     labels = seq(2001, 2017, 4)) +
  scale_y_continuous(limits = c(0, 24),
                     breaks = seq(0,24,4)) +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  annotate("text", 
           x = 2016,
           y = 13.2,
           label = "PM[2.5]~NAAQS",
           size = 4,
           fontface = "italic",
           parse = TRUE)
  
ggsave("figures/pm_3_year_mean.jpg",
       height = 4,
       width = 6,
       unit = "in")



ggplot(pm_2_5_dat, aes(Year, mean_98th_3_year)) +
  geom_point(color = "darkred",
             alpha = 0.8, 
             size = 4) +
  geom_hline(aes(yintercept = 35),
             linetype = "dashed",
             size = 1.4) +
  theme_minimal() +
  labs(x = "Year (end of 3-year average)",
       y = expression(paste("Particulate ", matter[2.5], " (", mu, g, "/", m^3, ")", sep = "")),
       title = expression(paste("98th Percentile Particulate ", Matter[2.5], sep = ""))) +
  scale_x_continuous(limits = c(2000, 2018),
                     breaks = seq(2001, 2017, 4),
                     minor_breaks = seq(2001, 2017, 1),
                     labels = seq(2001, 2017, 4)) +
  scale_y_continuous(limits = c(0, 50),
                     breaks = seq(0,50,10)) +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 18)) +
  annotate("text", 
           x = 2016,
           y = 37.3,
           label = "PM[2.5]~NAAQS",
           size = 4,
           fontface = "italic",
           parse = TRUE)

ggsave("figures/pm_3_year_98th.jpg",
       height = 4,
       width = 6,
       unit = "in")


##-------------
## map of monitors
##-------------


o3_mon_dat <- o3_pm_dat %>% 
  filter(Parameter.Name == "Ozone") %>% 
  filter(Pollutant.Standard == "Ozone 8-hour 2015") %>% 
  filter(Year > 1998) %>% 
  dplyr::select(Longitude, Latitude, Parameter.Name) %>% 
  distinct()

pm_mon_dat <-  o3_pm_dat %>% 
  filter(Parameter.Name == "PM2.5 - Local Conditions") %>% 
  filter(Metric.Used == "Daily Mean") %>% 
  filter(Year > 1990) %>% 
  dplyr::select(Longitude, Latitude, Parameter.Name) %>% 
  distinct() %>% 
  mutate(Parameter.Name = rep("PM", n()))

mon_dat <- bind_rows(o3_mon_dat, pm_mon_dat)


# 
# planning_sf <- sf::st_as_sf(planning)
# wayne_sf <- sf::st_as_sf(wayne)


ggplot() +
  geom_polygon(aes(long, lat, group = group, alpha = "Planning Area"),
               color = "black", 
               size = 1,
               fill = NA,
               data = planning_df) +
  geom_polygon(aes(long, lat, group = group, alpha = "Wayne NF"),
               color = "darkgreen", 
               size = 1,
               fill = "darkgreen",
               data = wayne_df) +
  scale_alpha_manual(name = NULL, values = c(0.4, 0.4)) +
  geom_jitter(aes(x = Longitude, 
                  y = Latitude, 
                  shape = Parameter.Name), 
              size = 3.5, 
              # height = 0.1,
              # width = 0.1, 
              color = "black",
              fill = "darkgoldenrod3",
              data = mon_dat) +
  theme_map() +
  labs(title = expression(paste("Locations of Ozone and ", PM[2.5], " Monitors", sep = ""))) +
  scale_shape_manual(name = NULL,
                     values = c(21,23),
                     labels = c("Ozone", expression(PM[2.5]))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18)) +
  guides(alpha = guide_legend(override.aes = list(color = c("black", "darkgreen"),
                                                  fill = c(NA, "darkgreen"))),
         shape = guide_legend(override.aes = list(size = 5)))



ggsave("figures/oz_pm_monitors.jpg",
       height = 4,
       width = 7,
       units = "in")





#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
########################################
## archive
########################################

pm_2_5$pm_var_2 <- factor(pm_2_5$pm_var, labels = c("Annual mean", "98th percentile"))


pm_naaqs <- data.frame(pm_var_2 = c("Annual mean", "98th percentile"), 
                       x = c(2014.3, 2014.3),
                       y = c(13.3, 36.3),
                       lab = c(paste("P", "M[2.5]*", " NAAQS", sep = ""),
                               paste("P", "M[2.5]*", " NAAQS", sep = "")))




ggplot(pm_2_5, aes(Year, pm_val)) +
  geom_point(color = "darkred",
             alpha = 0.8, 
             size = 3) +
  geom_hline(aes(yintercept = naaqs),
             linetype = "dashed",
             size = 1.4) +
  theme_minimal() +
  facet_grid(~pm_var_2, scales = "free") + 
  labs(x = "Year (end of 3-year average)",
       y = expression(paste("Particulate ", matter[2.5], " (", mu, g, "/", m^3, ")", sep = ""))) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 13)) +
  theme(strip.text = element_text(size = 14)) +
  
  # #text labels for naaqs 
  # geom_text(data = data.frame(pm_var_2 = "Annual Mean",
  #                             y = 12.5,
  #                             x = 2013),
  #           aes(x,
  #               y,
  #               label = paste("P", "M[2.5]*", " NAAQS", sep = "")),
  #               # group = pm_var_2,
  #           parse = TRUE)
  geom_text(data = pm_naaqs, aes(x, y, label = lab, fontface = "italic"), 
            parse = TRUE)




#----------------------------------------------------------------------------




















