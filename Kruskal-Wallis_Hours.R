options(scipen = 1000)

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

osm_json %>% 
  spread_all() %>% 
  as_data_frame.tbl_json() %>% 
  mutate(contributor.since = as.Date(contributor.since)) -> osm_tibble

# Drop users who contributed less than 2000 changesets
osm_tibble %<>% 
  filter(changesets.changes >= 1500)




survey_original <- readxl::read_xlsx("OSM survey data.xlsx") %>% select(-c(`(Found) Username`, `6.a. If you selected Other, please specify:`))

survey_original %>% 
  rename(username = `1. What is your OpenStreetMap Username?`,
         gender = `2. What gender do you identify as?`,
         age = `3. What is your age?`,
         country_residence = `4. What is your country of residence?`,
         nationality = `5. What is your nationality?`,
         education = `6. What is your highest level of education?`,
         continent = Continent,
         tz = `timezone(UTC)`
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



##########################
osm_json %>%
  as_tbl_json(drop.nulljson = T) %>% 
  enter_object('changesets') %>%
  spread_values(hours = jstring(hours)) %>% 
  filter(document.id %in% usernames_df$document.id) %>%  
  as.data.frame() %>% 
  pull(2)-> hours

data.frame(hours) %>% 
  mutate(hours = gsub("\\|$","", hours)) %>%
  separate_rows(hours, sep = "[|]") %>%
  separate(hours, c("h00", "h01", "h02", "h03", "h04", "h05",
                    "h06", "h07", "h08", "h09", "h10", "h11",
                    "h12", "h13", "h14", "h15", "h16", "h17",
                    "h18", "h19", "h20", "h21", "h22", "h23"
  ), ",") %>%
  mutate(across(where(is.character), as.numeric)) -> hours_df


usernames_df %>% 
  bind_cols(hours_df) %>% 
  mutate(age = fct_relevel(age, "18-24", "25-29", "30-34", "35-39", "40-44", 
                           "45-49", "50-54", "55-59", "60-64", "65-69", ">70")) -> hours_df_combined

hours_df_combined %<>% 
  select(document.id, tz, h00:h23) %>% 
  pivot_longer(!c(document.id, tz), names_to = "UTC", values_to = "Count") %>% 
  mutate(UTC = as.numeric(str_sub(UTC, -2)),
         local = tz + UTC,
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
  select(document.id, Count, local, -tz) %>% 
  arrange(document.id, local, Count) %>% 
  pivot_wider(names_from = local, values_from = Count) %>% 
  left_join(hours_df_combined %>% select(username:document.id), by = "document.id") 


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
  select(-Hours) -> normality_hours_df

# Stats
normality_hours_df %>%
  group_by(gender, age_combined, hours_cat) %>%
  rstatix::get_summary_stats(Count, type = "common")


# Normality test
source("geom_split_violin.R")
library(cowplot)

normality_hours_df %>% 
  ggplot(aes(x = hours_cat, y = Count, fill = gender)) +
  geom_split_violin() +
  ylim(0,200) +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "Contributions (Hours)") +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_hour_gender


normality_hours_df %>% 
  ggplot(aes(x = age_combined, y = Count, fill = gender)) +
  geom_split_violin() +
  ylim(0,200) +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "") +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_age_gender



normality_hours_df %>% 
  ggplot(aes(x = age_combined, y = Count, fill = hours_cat)) +
  geom_split_violin() +
  ylim(0,200) +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "") +
  theme(legend.position = "bottom",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> plot_age_hour



homo_h <- plot_grid(plot_hour_gender, plot_age_gender, plot_age_hour, labels = c('D', 'E', 'F'), label_size = 20, ncol = 3)
#save_plot("homo_hour.png", homo_h, base_height = 4, base_width = 10)
#####################################################

normality_hours_df %>% 
  mutate(rank_gender = factor(rank(gender, ties.method = "average")),
         rank_age = factor(rank(age_combined, ties.method = "average")),
         rank_hour = factor(rank(hours_cat, ties.method = "average")),
         rank_count = rank(Count, ties.method = "average")) %>% 
  select(-c(gender, age_combined, hours_cat, Count)) -> normality_df_rank


normality_df_rank %<>% 
  group_by(rank_gender, rank_age, rank_hour) %>% 
  get_summary_stats() %>% 
  mutate(rank_gender = case_when(rank_gender == 2976.5 ~ "Male", T ~ "Female"),
         rank_age = case_when(rank_age == 4188.5 ~ "Others", T ~ "Economically Active"),
         rank_hour = case_when(rank_hour == 872.5 ~ "Core", 
                               rank_hour == 4360.5 ~ "Offpeak",
                               rank_hour == 2616.5 ~ "Night"))


normality_df_rank$rank_hour <- factor(normality_df_rank$rank_hour, levels=c("Core", "Offpeak", "Night"))


normality_df_rank %>% 
  ggplot(aes(x = rank_age, y = median, colour = rank_gender, group = rank_gender)) +
  geom_errorbar(aes(ymin=median-sd, ymax=median+sd), colour="grey40", width=.3, position=position_dodge(0.4)) +
  #geom_line(position=position_dodge(0.4)) +
  geom_point(position=position_dodge(0.4), size=3, shape=21, fill="white") +
  facet_wrap(~rank_hour, scales = "free_y") +
  theme_bw() +
  labs(fill="", 
       x = "",
       y = "") +
  theme(legend.position = "none",
        text = element_text(size=15),
        strip.text.x = element_text(size = 16),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,0,0,0)) -> normality_df_rank_plot

#ggsave("kruskal_hourly.png", normality_df_rank_plot, width = 9, height = 4)


#------------------------
normality_hours_df %>%
  group_by(age_combined, hours_cat) %>% 
  kruskal_test(Count ~ gender)


#######################################
# Post-hoc test
library(FSA)
options(scipen = 2)

dunnTest(Count ~ hours_cat,
         data=normality_hours_df,
         two.sided = F,
         method="bonferroni")


dunnTest(Count ~ gender,
         data=normality_hours_df,
         two.sided = F,
         method="bonferroni")


dunnTest(Count ~ age_combined,
         data=normality_hours_df,
         two.sided = F,
         method="bonferroni")


#https://rcompanion.org/handbook/F_14.html

##----------------------------------------
library(ggpubr) 



hours_df_rank1 %>%
  mutate(Count = rank_count,
         Gender = gender,
         Age = age,
         Hours = hours) %>% 
  select(Gender, Age, Hours, Count) %>% 
  mutate(Gender = factor(Gender, levels = c("Male", "Female")),
         Age = factor(Age, levels = c("20s", "30s", "40s", "ov50")),
         Hours = factor(Hours, levels = c("core", "offpeak", "night"))
         ) -> hours_df_rank2


hours_df_rank2 %>% 
  group_by(Gender, Age, Hours) %>% 
  rstatix::get_summary_stats(type = "common")

bxp <- ggboxplot(
  hours_df_rank2, x = "Age", y = "Count", 
  color = "black", fill = "Hours", facet.by = "Gender", ylab = "Average Contributions by Rank", xlab = ""
)
bxp

#https://dzchilds.github.io/stats-for-bio/two-way-anova-in-r.html
