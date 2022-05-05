library(tidyverse)
library(tidyjson)
library(magrittr)
library(rstatix)

# 1. File import
load('OSM_WinOS.RData')

usernames_df %>% 
  group_by(gender) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
  

usernames_df %>% 
  group_by(gender, age) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))



###################
### Days of Week ##
###################
#osm_json %>%
#  as_tbl_json(drop.nulljson = T) %>% 
#  enter_object('changesets') %>%
#  spread_values(days = jstring(days)) %>% 
#  filter(document.id %in% usernames_df$document.id) %>%  
#  as.data.frame() %>% 
#  pull(2)-> days

data.frame(days) %>% 
  mutate(days = gsub("\\|$","", days)) %>%
  separate_rows(days, sep = "[|]") %>%
  separate(days, c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), ",") %>% 
  mutate(across(where(is.character), as.numeric)) -> days_df


days_df %>% colMeans() %>% round(.,0)


usernames_df %>%  
  bind_cols(days_df) %>% 
  mutate(age = fct_relevel(age, "18-24", "25-29", "30-34", "35-39", "40-44", 
                           "45-49", "50-54", "55-59", "60-64", "65-69", ">70")) %>% 
  as_tibble() -> days_df_combined

days_df_combined %<>% 
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


days_df_combined %>%
  select(gender, Sun:Sat) %>% 
  pivot_longer(!gender, names_to = "days", values_to = "count") %>% 
  group_by(gender) %>% 
  summarise(mean_cont_per_week = mean(count))


days_df_combined %>% 
  group_by(gender) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select(-c(tz, document.id))

days_df_combined %>% 
  group_by(gender) %>% 
  summarise_if(is.numeric, sd, na.rm = TRUE) %>% 
  select(-c(tz, document.id))


days_df_combined %>%
  filter(gender != "Prefer not to say") %>% 
  select(gender, age, Sun:Sat) %>% 
  rowwise() %>% 
  mutate(mean_cont_per_week = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
  select(gender, age, mean_cont_per_week) %>%
  ggplot(aes(x = gender, y = mean_cont_per_week, fill = gender)) +
  geom_boxplot() +
  ylim(0,35000) +
  labs(x = "",
       y = "Average Contributions Per Week") +
  theme_bw() +
  theme(legend.position="bottom")


days_df_combined %>%
  filter(gender != "Prefer not to say") %>% 
  select(gender, age, Sun:Sat) %>% 
  rowwise() %>% 
  mutate(mean_cont_per_week = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
  select(gender, age, mean_cont_per_week) %>%
  ggplot(aes(x = gender, y = mean_cont_per_week, fill = age)) +
  geom_boxplot() +
  ylim(0,35000) +
  labs(x = "",
       y = "Average Contributions Per Week") +
  theme_bw() +
  theme(legend.position="bottom")



days_df_combined %>%
  filter(gender != "Prefer not to say") %>% 
  select(gender, age, Sun:Sat) %>% 
  rowwise() %>% 
  mutate(mean_cont_per_week = sum(c_across(where(is.numeric)), na.rm = T)) %>% 
  select(gender, age, mean_cont_per_week) %>%
  ggplot(aes(x = age, y = mean_cont_per_week, fill = gender)) +
  geom_boxplot() +
  ylim(0,35000) +
  labs(x = "",
       y = "Average Contributions Per Week") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) 

##---------------------------------------
# Line plot

days_df_combined %>%
  filter(gender != "Prefer not to say") %>% 
  select(gender, age, Sun:Sat) %>% 
  group_by(gender) %>% 
  rstatix::get_summary_stats(type = "mean_se") %>% 
  ungroup() %>% 
  mutate(Days = variable,
         Days = factor(Days, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) -> days_df_tidy



days_df_tidy %>% 
  ggplot(aes(x = Days, y = mean, group = gender, colour = gender)) +
  geom_line() +
  geom_pointrange(aes(ymin=mean-se, ymax=mean+se), width=.2,
                  position=position_dodge(0.5), size = 1) +
  labs(x = "",
       y = "Average Contributions During the Week") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=20),
        axis.title.y = element_text(size = 13),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_line_mean_se

plot_line_mean_se

#ggsave("Data-Exploration-during-the-week.png", plot_line_mean_se, width = 8, height = 5)



# Decomposed by age group

days_df_combined %>%
  select(gender, age_combined, Sun:Sat) %>% 
  group_by(gender, age_combined) %>% 
  get_summary_stats() %>% 
  rename(Days = variable) %>% 
  mutate(Days = factor(Days, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(x = Days, y = mean, group = gender, colour = gender)) +
  #geom_point() + 
  geom_line() +
  geom_pointrange(aes(ymin=mean-se, ymax=mean+se), width=.2,
                  position=position_dodge(0.5), size = 1) +
  labs(x = "",
       y = "Average Contributions During the Week") +
  #facet_wrap(~age_combined, scales = "free_y") +
  facet_wrap(~age_combined) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 13),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_week_age

plot_week_age

ggsave("Data-Exploration-by-week-and-age.png", plot_week_age, width = 10, height = 5)



