library(flexdashboard)
library(tidyverse)
library(tidyjson)
library(magrittr)
library(data.table)
library(cowplot)
library(vegan)
library(hrbrthemes)
library(maps)
library(wordcloud)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
#library(rnaturalearthdata)

options(scipen = 9999)


simpson <- load("Simpson_GISRUK.RData")

bind_rows(country_list) %>% 
  group_by(Country_full) %>% 
  summarise(Changeset = sum(Changeset)) -> country_df


world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  left_join(country_df, by = c("geounit" = "Country_full")) 

world_sp <- world %>% as_Spatial()

m <- 
  leaflet(world_sp) %>% 
  setView(lng = 0, lat = 52, zoom = 1) %>% 
  addTiles()



bins <- c(0, 500, 1000, 2000, 5000, 10000, 50000, Inf)
pal <- colorBin("YlOrRd", domain = world_sp$Changeset, bins = bins)

m %>% 
  addPolygons(
  fillColor = ~pal(Changeset),
  weight = 1,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)




