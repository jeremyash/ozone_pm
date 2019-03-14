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
library(extrafont)
library(grid)
library(zoo)
#----------------------------------------------------------------------------

#############################################################################
## functions
#############################################################################

########################################
## functions + font misc
########################################


theme_map <- function(...) {
  theme_minimal() +
    theme(
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

#############################################################################
## load data: ozone from : http://views.cira.colostate.edu/fed/SvcUi/AqSummaryData.aspx
#############################################################################

nat_oz_dat <- read_csv("raw_data/ozone_126.csv")

nat_oz_dat$W126_1YR[nat_oz_dat$W126_1YR == -999] <- NA
oz_dat <- nat_oz_dat %>% 
  filter(!(is.na(W126_1YR))) %>% 
  filter(!(is.na(Latitude)))

planning <- readOGR("GIS/planning_area")
planning <- spTransform(planning, CRS("+init=epsg:4269"))



# intersect planning area and ozone stations
coordinates(oz_dat) <- c("Longitude", "Latitude")
proj4string(oz_dat) <- CRS("+init=epsg:4269")

# intersect with planning area 
planning_mons <- raster::intersect(oz_dat, planning) %>% 
  as.data.frame() %>% 
  mutate_if(is.factor, as.character) 
  
# summary
planning_mons_summ <- planning_mons %>% 
  group_by(YearNum) %>% 
  summarise(mean_oz = mean(W126_1YR, na.rm = TRUE),
            sd_oz = sd(W126_1YR, na.rm = TRUE)) %>% 
  ungroup()


#############################################################################
## plotting
#############################################################################


##-------------
## trends
##-------------

oz_lims <- range(oz_dat$W126_1YR, na.rm = TRUE)


# 
#   site_order <- oz_dat%>% 
#     filter(domain == DOMAIN) %>% 
#     filter(YearNum == 2015) %>% 
#     arrange(desc(W126_1YR)) %>% 
#     pull(improve_site)
#   
#   plot_dat <-  oz_dat %>% 
#     filter(domain == DOMAIN) %>% 
#     mutate(improve_site = factor(improve_site, levels = PLOTTING_ORDER))
#   
#   x_breaks <- ifelse((max(plot_dat$YearNum ) - min(plot_dat$YearNum ))%% 2 == 0,
#                      2,
#                      3)



oz_cols <- c("chartreuse4",
             # "lightgreen",
             "gold",
             # "darkorange3",
             "firebrick4")


 ggplot() +
   # high risk
   geom_rect(aes(xmin = 1990, xmax = 2021, ymin = 15, ymax = 34), fill = oz_cols[3], color = oz_cols[3], alpha = 0.6) +
   geom_rect(aes(xmin = 1990, xmax = 2021, ymin = 7, ymax = 14.999999), fill = oz_cols[2], color = oz_cols[2], alpha = 0.6)+    geom_rect(aes(xmin = 1990, xmax = 2021, ymin = 0, ymax = 6.9999999), fill = oz_cols[1], color = oz_cols[1], alpha = 0.6)+
    geom_point(aes(YearNum , W126_1YR, group = SiteCode), 
               size = 5, 
               color = "black",
               data = planning_mons) +
    geom_line(aes(YearNum , W126_1YR, group = SiteCode), 
              size = 2, 
              show.legend = FALSE,
              data = planning_mons) +
    theme_minimal() +
    labs(x = "Year", 
         y = "W126 Ozone, ppm-hours  ", 
         title = "Ozone Concentration in Planning Area") +
  scale_x_continuous(limits = c(1990, 2021),
                     breaks = seq(1991, 2015, 3),
                     labels = seq(1991, 2015, 3)) +
   geom_text(aes(x = 2018.5, y = 32),label = "High risk", size = 7) +
   geom_text(aes(x = 2018.5, y = 14),label = "Moderate", size = 7) +
   geom_text(aes(x = 2018.5, y = 12),label = "risk", size = 7) +
   geom_text(aes(x = 2018.5, y = 6),label = "Low risk", size = 7) +
   theme(axis.title = element_text(size = 20),
         axis.text.y = element_text(size = 14),
         axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
         plot.title = element_text(size = 30),
         plot.subtitle = element_text(size = 30),
         legend.text = element_text(size = 18))
 
 
 
  ggsave(filename = file_path,
         plot = dat_plot,
         height = 8.5,
         width = 11,
         units = "in")
  


oz_cols <-brewer_pal(palette = "Reds", direction = -1)(3)
show_col(oz_cols)  


oz_cols <- c("grey60", 
             # "gold",
             "darkorange3",
             "darkred")

  
  ggplot() +
    geom_rect(aes(xmin = 2015.02, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "white") +
    geom_rect(aes(xmin = 1990, xmax = Inf, ymin = 15, ymax = Inf), fill = oz_cols[3], alpha = 0.7) +
    geom_rect(aes(xmin = 1990, xmax = Inf, ymin = 7, ymax = 14.999999), fill = oz_cols[2], alpha = 0.6)+    geom_rect(aes(xmin = 1990, xmax = Inf, ymin = 0, ymax = 6.9999999), fill = oz_cols[1], alpha = 0.6)+
    geom_errorbar(aes(YearNum , ymin = mean_oz + sd_oz, ymax = mean_oz - sd_oz),
                  data = planning_mons_summ) +
    geom_point(aes(YearNum , mean_oz), 
               size = 4, 
               color = "black",
               data = planning_mons_summ) +
    geom_line(aes(YearNum ,  mean_oz), 
              size = 1.2, 
              show.legend = FALSE,
              data = planning_mons_summ) +
    theme_minimal() +
    labs(x = "Year", 
         y = "W126 Ozone, ppm-hours  ", 
         title = "Ozone Concentration in Planning Area") +
    scale_x_continuous(limits = c(1990, 2023),
                       breaks = seq(1991, 2015, 3),
                       labels = seq(1991, 2015, 3),
                       minor_breaks = seq(1991,2015,1)) +
    scale_y_continuous(limits = c(0,36),
                       breaks = seq(0,35,5),
                       labels = seq(0,35,5)) +
    geom_text(aes(x = 2020, y = 36),label = "High risk", size = 5) +
    geom_text(aes(x = 2020, y = 13.5),label = "Moderate", size = 5) +
    geom_text(aes(x = 2020, y = 11.2),label = "risk", size = 5) +
    geom_text(aes(x = 2020, y = 5.5),label = "Low risk", size = 5) +
    theme(axis.title = element_text(size = 16),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 30),
          legend.text = element_text(size = 18))
  

ggsave("figures/oz_w126_planning.jpg",
        width = 6,
        height = 4,
        units = "in")






