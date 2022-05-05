library(tidyverse)
library(tidyjson)
library(magrittr)
library(rstatix)

# 1. File import
# this should give you a character vector, with each file name represented by an entry
filenames <- list.files("Contributors/", pattern="*.json", full.names=TRUE) 
#filenames <- list.files("Contributors_raw/", pattern="*.json", full.names=TRUE) 


# Import data
osm_json <-filenames %>% 
  map(read_json) %>% 
  reduce(bind_rows) %>% 
  mutate(document.id = row_number())

# Check data type
class(osm_json)  


# Take a look at the data
osm_json %>%  
  spread_all() %>% 
  glimpse()

# browse Types 
osm_json %>% 
  gather_object %>% 
  json_types %>%
  count(name, type) %>% 
  print(n = Inf)


# Glance at the first index
osm_json %>% 
  enter_object('contributor') %>% 
  gather_object('index.1') %>% 
  append_values_string() 


osm_json %>% 
  enter_object('contributor') %>% 
  spread_values(name = jstring(name),
                 uid = jstring(uid),
                traces = jstring(traces),
                blocks = jstring(blocks)
                )


osm_json %>%
  as_tbl_json(drop.nulljson = T) %>% 
  enter_object('changesets') %>%
  spread_values(days = jstring(days)) 


# Now convert them to tibble
osm_json %>% 
  spread_all() %>% 
  as_data_frame.tbl_json() %>% 
  mutate(contributor.since = as.Date(contributor.since)) -> osm_tibble

class(osm_tibble)



##
duration <- as.numeric(osm_tibble$changesets.changes)
summary(duration)
quantile(duration, c(.01, .05, .32, .57, .98)) 


# Drop users who contributed less than 2000 changesets
osm_tibble %<>% 
  filter(changesets.changes >= 1500)


## year
lubridate::year(osm_tibble$contributor.since) %>% hist(main = "User's First Year of Contribution")
lubridate::year(osm_tibble$contributor.since) %>% table()




# import Excel
survey_original <- readxl::read_xlsx("OSM survey data.xlsx") %>% select(-c(`(Found) Username`, `6.a. If you selected Other, please specify:`))
  
survey_original %>% 
  rename(username = `1. What is your OpenStreetMap Username?`,
         gender = `2. What gender do you identify as?`,
         age = `3. What is your age?`,
         country_residence = `4. What is your country of residence?`,
         nationality = `5. What is your nationality?`,
         education = `6. What is your highest level of education?`
         ) -> survey

# Create a dataframe that contains all users names
data.frame(filenames) -> usernames_df

osm_tibble %>% 
  select(document.id, contributor.name) %>% 
  rename(username = contributor.name) -> osm_filtered

usernames_df %<>% 
  rename(username = filenames) %>% 
  mutate(username = gsub("Contributors//","", .$username)) %>% 
  mutate(username = gsub(".json","", .$username)) %>% 
  left_join(survey, by = "username") %>% 
  left_join(osm_filtered, by = "username") %>% 
  drop_na(document.id, gender) %>%
  filter(gender != "Prefer not to say") %>% 
  as_tibble()


usernames_df %>% 
  group_by(gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
  

usernames_df %>% 
  group_by(gender, age) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



###################################
#############--hours--#############
###################################

usernames_df %>% 
  bind_cols(hours_df) %>% 
  mutate(age = fct_relevel(age, "18-24", "25-29", "30-34", "35-39", "40-44", 
                           "45-49", "50-54", "55-59", "60-64", "65-69", ">70")) -> hours_df_combined

hours_df_combined %<>% 
  select(document.id, `timezone(UTC)`, h00:h23) %>% 
  pivot_longer(!c(document.id, `timezone(UTC)`), names_to = "UTC", values_to = "Count") %>% 
  mutate(UTC = as.numeric(str_sub(UTC, -2)),
         local = `timezone(UTC)` + UTC,
         local = case_when(local < 24 & local >= 0 ~ local, 
                           local >= 24 ~ local - 24,
                           local < 0 ~ local + 24),
         local = str_c("h0", local),
         local = ifelse(local == "h00"|
                          local == "h01"|
                          local == "h02"|
                          local == "h03"|
                          local == "h04"|
                          local == "h05"|
                          local == "h06"|
                          local == "h07"|
                          local == "h08"|
                          local == "h09", local, paste0("h", str_sub(local,-2)) 
         )) %>% 
  select(document.id, Count, local, -`timezone(UTC)`) %>% 
  arrange(document.id, local, Count) %>% 
  pivot_wider(names_from = local, values_from = Count) %>% 
  left_join(hours_df_combined %>% select(username:document.id), by = "document.id") 



#hours_df_combined %<>% 
#  mutate(age_combined = case_when(age == "18-24" ~ "20s",
#                                  age == "25-29" ~ "20s",
#                                  age == "30-34" ~ "30s",
#                                  age == "35-39" ~ "30s",
#                                  age == "40-44" ~ "40s",
#                                  age == "45-49" ~ "40s",
#                                  age == "50-54" ~ "ov50",
#                                  age == "55-59" ~ "ov50",
#                                  age == "60-64" ~ "ov50",
#                                  age == "65-69" ~ "ov50",
#                                  age == ">70" ~ "ov50"
#  ))

hours_df_combined %<>% 
  mutate(age_combined = case_when(age == "18-24" ~ "Others",
                                  age == "25-29" ~ "Economically Active",
                                  age == "30-34" ~ "Economically Active",
                                  age == "35-39" ~ "Economically Active",
                                  age == "40-44" ~ "Economically Active",
                                  age == "45-49" ~ "Others",
                                  age == "50-54" ~ "Others",
                                  age == "55-59" ~ "Others",
                                  age == "60-64" ~ "Others",
                                  age == "65-69" ~ "Others",
                                  age == ">70" ~ "Others"
  ))




hours_df_combined %>% 
  group_by(gender) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-document.id)


hours_df_combined %>%
  select(gender, age, h00:h23) %>%
  group_by(gender) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%  
  reshape2::melt(id = "gender", variable.name = "Hours", value.name = "Value") %>% 
  ggplot(aes(x = Hours, y = Value, group = gender, colour = gender)) +
  geom_point() + 
  geom_line() +
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) 


hours_df_combined %>%
  select(gender, age_combined, h00:h23) %>%
  group_by(age_combined, gender) %>% 
  get_summary_stats() %>% 
  select(gender, age_combined, variable, mean, se) %>% 
  #reshape2::melt(id = c("gender", "age_combined", ), variable.name = "Hours", value.name = "Value") %>% 
  ggplot(aes(x = variable, y = mean, group = gender)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),colour = "grey40", alpha = .6) +
  geom_line(aes(colour = gender)) +
  geom_point(alpha = .5) + 
  facet_wrap(~age_combined) + 
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_hours_gender_age

plot_hours_gender_age

#ggsave("plot_hours_gender_age.png", plot_hours_gender_age, width = 9, height = 4)


# Germany
hours_df_combined %>% 
  filter(country_residence == "Germany", gender == "Female") %>% 
  select(document.id, gender, age, h00:h23) %>%
  group_by(gender) %>% 
  #summarise_if(is.numeric, mean, na.rm = TRUE) %>%  
  reshape2::melt(id = c("gender", "age", "document.id"), variable.name = "Hours", value.name = "Value") %>% 
  ggplot(aes(x = Hours, y = Value, group = gender, colour = gender)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~document.id, scales = "free_y") + 
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) 


hours_df_combined %>%
  filter(gender != "Prefer not to say") %>% 
  select(gender, age, h00:h23) %>% 
  group_by(gender) %>% 
  rstatix::get_summary_stats(type = "mean_se") %>% 
  ungroup() %>% 
  mutate(Hours = variable,
         Hours = factor(Hours, levels = c("h00", "h01", "h02", "h03", "h04", "h05",
                                          "h06", "h07", "h08", "h09", "h10", "h11",
                                          "h12", "h13", "h14", "h15", "h16", "h17",
                                          "h18", "h19", "h20", "h21", "h22", "h23"))) -> hour_stats


hour_stats %>% 
  ggplot(aes(x = Hours, y = mean, group = gender, colour = gender)) +
  geom_line() +
  geom_pointrange(aes(ymin=mean-se, ymax=mean+se), width=.2,
                  position=position_dodge(0.05)) +
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_line_hours


#ggsave("line_hour.png", plot_line_hours, width = 7, height = 4)


hours_df_combined %>% 
  select(gender, age, h00:h23) %>% 
  group_by(gender, age) %>% 
  rowwise() %>% 
  mutate(dailysum = sum(c_across(h00:h23))) %>% 
  ungroup() %>% 
  mutate(across(h00:h23, ~ . / dailysum)) %>% 
  mutate_if(is.numeric, round, digits=3) %>% 
  select(-dailysum) %>% 
  pivot_longer(!c(gender, age), names_to = "Hours", values_to = "Count") -> hours_per_df




hours_per_df %>%
  select(gender, Hours, Count) %>%
  group_by(gender, Hours) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  ggplot(aes(x = Hours, y = Count, group = gender, colour = gender)) +
  geom_point() + 
  geom_line() +
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_line_hours_per


#plot_line_hours_fin <- cowplot::plot_grid(plot_line_hours, plot_line_hours_per, labels = c('A', 'B'), label_size = 20, ncol = 2)
#cowplot::save_plot("line_hours.png", plot_line_hours_fin, base_height = 4, base_width = 10)



##-------------------------------------------------

hours_df_combined %>%
  select(gender, age, h00:h23) %>%
  group_by(age, gender) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  reshape2::melt(id = c("gender","age"), variable.name = "Hours", value.name = "Value") %>% 
  ggplot(aes(x = Hours, y = Value, group = gender, colour = gender)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~age, scales = "free_y", ncol = 3) +
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_line_hours_age

#ggsave("age.png", plot_line_hours_age, width = 12, height = 7.5)



hours_per_df %>% 
  group_by(age, gender, Hours) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  ggplot(aes(x = Hours, y = Count, group = gender, colour = gender)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~age, scales = "free_y", ncol = 3) +
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0))-> plot_line_hours_age_per

#ggsave("age_per.png", plot_line_hours_age_per, width = 12, height = 7.5)




##---------Weekday/Weekend-------------------

hours_df_combined %>% 
  select(gender, age_combined, contains("h")) %>% 
  pivot_longer(!c(gender, age_combined), names_to = "Hours", values_to = "Count") %>% 
  mutate(hours_cat = case_when(Hours == "h00" ~ "offpeak",
                               Hours == "h01" ~ "night",
                               Hours == "h02" ~ "night",
                               Hours == "h03" ~ "night",
                               Hours == "h04" ~ "night",
                               Hours == "h05" ~ "night",
                               Hours == "h06" ~ "night",
                               Hours == "h07" ~ "night",
                               Hours == "h08" ~ "night",
                               Hours == "h09" ~ "core",
                               Hours == "h10" ~ "core",
                               Hours == "h11" ~ "core",
                               Hours == "h12" ~ "core",
                               Hours == "h13" ~ "core",
                               Hours == "h14" ~ "core",
                               Hours == "h15" ~ "core",
                               Hours == "h16" ~ "core",
                               Hours == "h17" ~ "offpeak",
                               Hours == "h18" ~ "offpeak",
                               Hours == "h19" ~ "offpeak",
                               Hours == "h20" ~ "offpeak",
                               Hours == "h21" ~ "offpeak",
                               Hours == "h22" ~ "offpeak",
                               Hours == "h23" ~ "offpeak"
  )) %>% 
  select(-Hours) -> hours_tidy_df


hours_tidy_df %>% 
  #select(gender, age_combined, hours_cat) %>% 
  mutate(gender = factor(gender, levels = c("Male", "Female")),
         age_combined = factor(age_combined, levels = c("20s", "30s", "40s", "ov50")),
         hours_cat = factor(hours_cat, levels = c("core", "offpeak", "night")))  -> hours_gaw_df


hours_gaw_df %>%
  group_by(hours_cat) %>% 
  rstatix::get_summary_stats(type = "common") -> weekstat_df

weekstat_df

hours_gaw_df %>% 
  group_by(hours_cat) %>%  
  ggplot(aes(x = hours_cat, y = Count, fill = hours_cat)) +
  geom_boxplot() +
  geom_point(data = weekstat_df, aes(x = hours_cat, y = mean), size = 3, colour = "red") +
  ylim(0,800) +
  labs(x = "",
       y = "Average Contributions") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=15),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_box_weekday

#ggsave("box_weekly_hr.png", plot_box_weekday, width = 5, height = 6)





