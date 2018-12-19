pacman::p_load(ggplot2,
               dplyr,
               lubridate,
               rworldmap,
               ggmap,
               sf,
               ggpomological,
               maps,
               viridis,
               ggsn, 
               grid)

# https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
historical_EQ <- read.delim("~/Downloads/results.tsv")

selected_EQ <- historical_EQ %>%
  select(I_D, YEAR,
         EQ_PRIMARY,
         LOCATION_NAME,
         LATITUDE,
         LONGITUDE) %>%
  filter(!is.na(LONGITUDE) | !is.na(LATITUDE)) %>%
  filter(EQ_PRIMARY > 8) %>%
  filter(YEAR > 1499)

selected_EQ$LONGITUDE2 <- ifelse(selected_EQ$LONGITUDE < -25,
                                 selected_EQ$LONGITUDE + 360,
                                 selected_EQ$LONGITUDE)
mapWorld <- map_data('world', wrap = c(-17, 335), ylim = c(-55, 75))

p <- ggplot() +
  geom_polygon(
    data = mapWorld,
    aes(x = long, y = lat, group = group),
    alpha = .75,
    fill = "#919c4c",
    col = NA) + 
  geom_polygon(
    data = mapWorld,
    aes(x = long, y = lat, group = group),
    alpha = 1,
    col = "#919c4c",
    fill = NA) +
  geom_density2d(data = selected_EQ,
                 aes(x = LONGITUDE2, y = LATITUDE,
                     alpha = ..level..),
                 col = "#c03728") +
  scale_alpha(range = c(0.35, 0.85)) +
  geom_point(data = selected_EQ,
             aes(x = LONGITUDE2, y = LATITUDE), 
             fill ="#c03728",
             col = "#4f5157", 
             pch = 21, 
             size = 2,
             alpha = .75) +
  labs(title = "Locations and Spatial Density of Earthquakes 
       with Magnitude >8 since 1500 C.E.") +
  scale_x_continuous(breaks=c(0,100,200, 300),
                     labels=expression(0, 90, 180, -90)) + 
  theme_pomological_fancy(base_family = "Homemade Apple", 12) + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20),
    panel.grid.major.x = element_line(colour = "#efe1c6",size=0.5),
    panel.grid.minor.x = element_line(colour = "#efe1c6",size=0.25),
    panel.grid.major.y = element_line(colour = "#efe1c6",size=0.5),
    panel.grid.minor.y = element_line(colour = "#efe1c6",size=0.5)) + 
  north(mapWorld, symbol = 16, scale = .15, location = "bottomleft")


paint_pomological(p, res = 75)
