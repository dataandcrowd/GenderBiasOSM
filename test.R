library(flexdashboard)
library(tidyverse)
library(purrr)
library(magrittr)
library(cowplot)
library(vegan)
library(hrbrthemes)
library(maps)
library(wordcloud)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)

options(scipen = 9999)


simpson <- load("Simpson_GISRUK.RData")

bind_rows(country_list) %>% 
  group_by(Country_full) %>% 
  summarise(Changeset = sum(Changeset)) %>% 
  mutate(Country_full = case_when(Country_full == "United States" ~ "United States of America",
                                  TRUE ~ as.character(Country_full))) -> country_df


world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  left_join(country_df, by = c("geounit" = "Country_full")) 

world_sp <- world %>% as_Spatial()

m <- 
  leaflet(world_sp) %>% 
  setView(lng = 0, lat = 52, zoom = 5) %>% 
  addTiles()


bins <- c(0, 1000, 5000, 10000, 50000, Inf)
pal <- colorBin("YlOrRd", domain = world_sp$Changeset, bins = bins)

m %>% 
  addPolygons(
  fillColor = ~pal(Changeset),
  weight = 1,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)


#########################################
#-Filter participants over 5 countries--#
#########################################

#sapply(country_list, "[", 1) #<- you can unhash this and test them 
#sapply(country_list, "[", 2)

sapply(country_list, nrow) -> country_rows
sum(country_rows >= 5, na.rm=TRUE)

country_list %>% 
  purrr::map(pluck, 2) %>% 
  sapply(length) -> number_of_contributed_contries

country_list %>% 
  purrr::map(pluck, 2) %>% 
  sapply(sum) -> number_of_changesets


########################
#--Simpson's D index--##
########################
#--Using vegan package-##
# Simpson D
country_list %>% 
  purrr::map(pluck, 2) %>% 
  sapply(diversity, index = "simpson") -> simpson

simpson[simpson == 0] <- NA

1 - simpson

#########################
#--Simpson D by group -##
#########################

simpson_df <- tibble(
  usernames_df %>% select(gender, age)) %>%
  mutate(#id = 1:length(username),
    age_combined = case_when(age == "18-24" ~ "20s",
                             age == "25-29" ~ "20s",
                             age == "30-34" ~ "30s",
                             age == "35-39" ~ "30s",
                             age == "40-44" ~ "40s",
                             age == "45-49" ~ "40s",
                             age == "50-54" ~ "ov50",
                             age == "55-59" ~ "ov50",
                             age == "60-64" ~ "ov50",
                             age == "65-69" ~ "ov50",
                             age == ">70" ~ "ov50"),
    age_combined = factor(age_combined, levels = c("20s", "30s", "40s", "ov50")))



## Add Simpson D
simpson_df %>% 
  bind_cols(simpsonD = 1 - simpson,
            no_of_countries = number_of_contributed_contries,
            total_changesets = number_of_changesets) %>% 
  filter(no_of_countries < 5) -> simpson_df1





