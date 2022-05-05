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



###################
### Days of Week ##
###################
osm_json %>%
  as_tbl_json(drop.nulljson = T) %>% 
  enter_object('changesets') %>%
  spread_values(days = jstring(days)) %>% 
  filter(document.id %in% usernames_df$document.id) %>%  
  as.data.frame() %>% 
  pull(2)-> days

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


#########################
##---kruskal wallis---##
#########################

days_df_combined %>% 
  select(gender, age_combined, Sun:Sat) %>% 
  group_by(gender, age_combined) %>% 
  rowwise() %>% 
  mutate(weekday = sum(c_across(Mon:Fri)),
         weekend = sum(Sun, Sat)) %>% 
  select(-c(Sun:Sat)) %>% 
  pivot_longer(!c(gender, age_combined), names_to = "Week", values_to = "Count") %>%
  mutate(gender = factor(gender, levels = c("Male", "Female")),
         age_combined = factor(age_combined, levels = c("Economically Active", "Others")),
         Week = factor(Week, levels = c("weekday", "weekend"))) %>%  
  ungroup() -> days_df_tidy

##------------------------
days_df_tidy %>% 
  group_by(gender, age_combined, Week) %>% 
  summarise(across(everything(), list(mean = mean)))



#--------------------------
# Split-Violin-Plot
source("geom_split_violin.R")
library(cowplot)

days_df_tidy %>% 
  ggplot(aes(x = Week, y = Count, fill = gender)) +
  geom_split_violin() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  ylim(0,12000) +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "Contributions (Days)") +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_week_gender


days_df_tidy %>% 
  ggplot(aes(x = age_combined, y = Count, fill = gender)) +
  geom_split_violin() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  ylim(0,12000) +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "") +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_age_gender


days_df_tidy %>% 
  ggplot(aes(x = age_combined, y = Count, fill = Week)) +
  geom_split_violin() +
  scale_fill_manual(values = c("seagreen3", "#F8766D")) +
  ylim(0,12000) +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "") +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_age_week

homo <- plot_grid(plot_week_gender, plot_age_gender, plot_age_week, labels = c('A', 'B', 'C'), label_size = 20, ncol = 3)
#save_plot("homo_days.png", homo, base_height = 4, base_width = 10)


#------------------------
#days_df_tidy %>%
#  group_by(gender, age_combined, Week) %>% 
#  get_summary_stats() -> days_df_tidy_summary
  
days_df_tidy %>% 
  mutate(rank_gender = factor(rank(gender, ties.method = "average")),
         rank_age = factor(rank(age_combined, ties.method = "average")),
         rank_week = factor(rank(Week, ties.method = "average")),
         rank_count = rank(Count, ties.method = "average")) %>% 
  select(-c(gender, age_combined, Week, Count)) -> days_df_rank


days_df_rank %<>% 
  group_by(rank_gender, rank_age, rank_week) %>% 
  get_summary_stats() %>% 
  mutate(rank_gender = case_when(rank_gender == 188.5 ~ "Male", T ~ "Female"),
         rank_age = case_when(rank_age == 131.5 ~ "Economically Active", T ~ "Others"),
         rank_week = case_when(rank_week == 109.5 ~ "weekday", T ~ "weekend")
         )

  
days_df_rank %>% 
  ggplot(aes(x = rank_age, y = median, colour = rank_gender)) +
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd, group = rank_gender), 
                colour="grey40", width=.3, position=position_dodge(0.4)) +
  #geom_line(position=position_dodge(0.2)) +
  geom_point(position=position_dodge(0.4), size=3, shape=21, fill="white") +
  facet_wrap(~rank_week)+#, scales = "free_y") +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "") +
  theme(legend.position = "none",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> days_df_rank_plot

#ggsave("kruskal_weekday-weekend.png", days_df_rank_plot, width = 6, height = 4)

#------------------------
days_df_tidy %>% 
#  filter(age_combined == "Economically Active") %>% 
  group_by(age_combined, Week) %>% 
  kruskal_test(Count ~ gender)



#######################################
# Post-hoc test
library(FSA)
options(scipen = 2)

dunnTest(Count ~ Week,
         data=days_gaw_df,
         two.sided = F,
         method="bonferroni")


dunnTest(Count ~ gender,
         data=days_gaw_df,
         two.sided = F,
         method="bonferroni")


dunnTest(Count ~ age_combined,
         data=days_gaw_df,
         two.sided = F,
         method="bonferroni")


#https://rcompanion.org/handbook/F_14.html


##----------------------------------------
library(ggpubr) 



days_gaw_df_rank %>%
  mutate(Gender = rank_gender,
         Age = rank_age,
         Week = rank_week,
         Count = rank_count,
         Gender = case_when(Gender == 228.5 ~ "Male",
                   Gender == 491.5 ~ "Female"),
         Age = case_when(Age == 69.5  ~ "20s",
                   Age == 225.5 ~ "30s",
                   Age == 371.5 ~ "40s",
                   Age == 478.5 ~ "ov50"),
         Week = case_when(Week == 132 ~ "Weekdays",
                          Week == 395 ~ "Weekends")) %>% 
  select(Gender, Age, Week, Count) %>% 
  mutate(Gender = factor(Gender, levels = c("Male", "Female")),
        Age = factor(Age, levels = c("20s", "30s", "40s", "ov50"))) -> days_gaw_df1
  
  
days_gaw_df1 %>% 
  group_by(Gender, Age, Week) %>% 
  #summarise_if(is.numeric, mean, na.rm = TRUE) 
  rstatix::get_summary_stats(type = "common")

bxp <- ggboxplot(
  days_gaw_df1, x = "Age", y = "Count", 
  color = "black", fill = "Week", facet.by = "Gender", ylab = "Average Contributions by Rank", xlab = ""
)
bxp

#https://dzchilds.github.io/stats-for-bio/two-way-anova-in-r.html
