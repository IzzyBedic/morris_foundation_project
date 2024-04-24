# Libraries --------------------------------------------------------------------
library(here)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidymodels)
library(janitor)

# Notes ------------------------------------------------------------------------
# kmeans?

# roc auc, confusion matrix good evaluations of model
# Presence of illness
# If energy curve is bimodel (bunch of energetic and un-energetic dogs, then can do energy)
# congruence btwn dift models
# age -- lubridate (enrolled year - birth year + year in study)
# h1 - H5 header size

# Bonus if incorporated into portfoliodown

## So, I am trying to predict presence of illness based on TOPIC in behavior, 
# activity level, and age-- I want to be able to have the person manually adjust 
# those inputs

# Libraries --------------------------------------------------------------------
# Age
profile_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                             "dog_profile.csv"))

# Activity, shade vs sun, etc. 
activity_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                              "activity_lifestyle", 
                              "activity_overview.csv"))

# Has energy score, excitability, trainability, dog rivalry, chasing
behavior_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                              "behavior", 
                              "behavior_summary.csv"))

conditions_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                               "conditions_summary.csv"))



profile_tbl <- as_tibble(profile_data)
activity_tbl <- as_tibble(activity_data)
behavior_tbl <- as_tibble(behavior_data)
conditions_tbl <- as_tibble(conditions_data)


# Cleaning ---------------------------------------------------------------------

profile_w_age <- profile_tbl %>% 
  mutate(birth_date_new = ym(birth_date),
         enrolled_date_new = ym(enrolled_date),
         starting_age = enrolled_date_new - birth_date_new) %>% 
  select(subject_id, sex_status, starting_age, birth_date_new, enrolled_date_new)

activity_reordered <- activity_tbl %>% 
  mutate(activity_level_reordered = fct(activity_level, 
                                  c("",
                                    "None",
                                    "Little",
                                    "Moderate",
                                    "Very active")),
         sun_exposure_duration_reordered = fct(sun_exposure_duration,
                                               c("",
                                                 "Less than 3 hours",
                                                 "Between 3-8 hours",
                                                 "Between 8-16 hours",
                                                 "Between 9-16 hours",
                                                 "More than 16 hours")),
         sun_exposure_shade_access_reordered = fct(sun_exposure_shade_access,
                                                   c("",
                                                     "No access to full shade",
                                                     "Access to full shade"))
         ) %>% 
  select(subject_id,
         year_in_study,
         activity_level_reordered, 
         sun_exposure_duration_reordered,
         sun_exposure_shade_access_reordered)

behavior_pivot <- behavior_tbl %>% 
  mutate(present = 1) %>% 
  pivot_wider(names_from = topic, 
               values_from = present) # %>% 
  # pivot_longer(cols = starts_with("count_"),
  #             names_to = "frequency_reported",
  #             values_to = "times_frequency_reported") %>% 
  # mutate(frequency_reported = fct(frequency_reported,
  #                                 c("count_na",
  #                                   "count_0",
  #                                   "count_1",
  #                                   "count_2",
  #                                   "count_3",
  #                                   "count_4")))
  # 

profile_y0 <- profile_w_age
  
activity_y0 <- activity_reordered %>% 
  filter(year_in_study == 0)

behavior_separation_y0 <- behavior_pivot %>% 
  filter(year_in_study == 0, 
         to_date == 0, 
         score_separation_related_problems == 1) %>% 
  select(subject_id, 
         year_in_study, 
         count_0, count_1, count_2, count_3, count_4, count_na,
         score_separation_related_problems)

behavior_energy_y0 <- behavior_pivot %>% 
  filter(year_in_study == 0, 
         to_date == 0, 
         score_energy == 1) %>% 
  select(subject_id, 
         year_in_study, 
         count_0, count_1, count_2, count_3, count_4, count_na,
         score_energy)

behavior_touch_y0 <- behavior_pivot %>% 
  filter(year_in_study == 0, 
         to_date == 0, 
         score_touch_sensitivity == 1) %>% 
  select(subject_id, 
         year_in_study, 
         count_0, count_1, count_2, count_3, count_4, count_na,
         score_touch_sensitivity)

behavior_attachment_y0 <- behavior_pivot %>% 
  filter(year_in_study == 0, 
         to_date == 0, 
         score_attachment_attention_seeking == 1) %>% 
  select(subject_id, 
         year_in_study, 
         count_0, count_1, count_2, count_3, count_4, count_na,
         score_attachment_attention_seeking)

behavior_trainability_y0 <- behavior_pivot %>% 
  filter(year_in_study == 0, 
         to_date == 0, 
         score_trainability == 1) %>% 
  select(subject_id, 
         year_in_study, 
         count_0, count_1, count_2, count_3, count_4, count_na,
         score_trainability)
  
behavior_nonsocial_fear_y0 <- behavior_pivot %>% 
  filter(year_in_study == 0, 
         to_date == 0, 
         score_nonsocial_fear == 1) %>% 
  select(subject_id, 
         year_in_study, 
         count_0, count_1, count_2, count_3, count_4, count_na,
         score_nonsocial_fear)

behavior_excitability_y0 <- behavior_pivot %>% 
  filter(year_in_study == 0, 
         to_date == 0, 
         score_excitability == 1) %>% 
  select(subject_id, 
         year_in_study, 
         count_0, count_1, count_2, count_3, count_4, count_na,
         score_excitability)

conditions_y0 <- conditions_tbl %>% 
  filter(year_in_study == 0, 
         to_date == 0) %>% 
  select(subject_id,
         year_in_study,
         any)


y0_tbl <- left_join(profile_y0, 
                    activity_y0, 
                    by = "subject_id") %>% 
  left_join(conditions_y0,
            by = c("subject_id", 
                   "year_in_study"))
  

# Visualizing ------------------------------------------------------------------

# tables
y0_tbl %>%
  tabyl(any, activity_level_reordered)

y0_tbl %>%
  tabyl(any, sun_exposure_duration_reordered)

y0_tbl %>%
  tabyl(any, sun_exposure_shade_access_reordered)

# Most dogs are pets, might want to remove others or see if they affect model negatively
lifestyle_tbl %>% 
  filter(year_in_study == 0, is_primary == 1) %>% 
  ggplot(aes(lifestyle)) +
  geom_bar()

# Distribution of ages within the study, mostly young dogs
profile_w_age %>% 
  ggplot(aes(starting_age)) +
  geom_density()


# sun_exposure_duration, sun_exposure_shade_access, activity_level

## skewed more active
y0_tbl %>% 
  ggplot(aes(activity_level_reordered)) +
  geom_bar()

## skewed less sun exposure
y0_tbl %>% 
  ggplot(aes(sun_exposure_duration_reordered)) +
  geom_bar()

## skewed much more access to shade
y0_tbl %>% 
  ggplot(aes(sun_exposure_shade_access_reordered)) +
  geom_bar()
  
#
y0_tbl %>% 
  ggplot(aes(as_factor(any), starting_age, fill = any)) +
  geom_boxplot()
  

# Combining



### LINES 188
# DATA SPLITTING AND RESAMPLING -------------------------------------------

set.seed(123)
splits <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test  <- testing(splits)

# training set proportions by children
tabyl(hotel_other$children)

# test set proportions by children
tabyl(hotel_test$children)







```{r create.workflow, eval = FALSE}
lr_workflow_golden <- reactive({
  workflow() %>% 
    add_model(lr_mod_golden) %>% 
    add_recipe(lr_recipe_golden())
})

```




