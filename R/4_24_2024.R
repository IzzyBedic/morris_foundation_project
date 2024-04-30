#### NEED 4 MORE PREDICTORS A LA GUIDELINES, WILL BE BEHAVIOR ONCE CLEANED
library(here) # helps importing data on different devices
library(dplyr) # pipe commands, data cleaning
library(ggplot2) # plots
library(lubridate) # converting to date type variable
library(tidymodels) # modelling
library(janitor)
library(flexdashboard)
library(shiny) # shiny dashboard
library(vip) # variable importance plots
library(forcats) # factors
library(DT)

# Import Data 
# My data can be obtained by making an account on this website:
# https://datacommons.morrisanimalfoundation.org/
# The file names have not been changed, but I put all of the data into a 
# folder called "DO_NOT_UPLOAD_DATA", so delete that if you don't have your 
# data in a folder also called that

# Age
profile_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                              "dog_profile.csv"))

# Activity, shade vs sun, etc., in a subfolder called "activity_lifestyle"
# get rid of "activity_lifestyle" if you don't have your data in a folder
# called that
activity_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                               "activity_lifestyle", 
                               "activity_overview.csv"))

# Has energy score, excitability, trainability, dog rivalry, chasing
# in a subfolder called "behavior" get rid of "behavior" if you don't have your 
# data in a folder called that
behavior_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                               "behavior", 
                               "behavior_summary.csv"))

conditions_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                                 "conditions_summary.csv"))



profile_tbl <- as_tibble(profile_data)
activity_tbl <- as_tibble(activity_data)
behavior_tbl <- as_tibble(behavior_data)
conditions_tbl <- as_tibble(conditions_data)


--------------------------------------------------------------------------------
  
  # Making a variable called "starting age" using birth date and enrolled date
  # lubridating all dates
profile_w_age <- profile_tbl %>% 
  mutate(birth_date_new = ym(birth_date),
         enrolled_date_new = ym(enrolled_date),
         starting_age = as.integer(enrolled_date_new - birth_date_new),
         sex_status = as_factor(sex_status)) %>% 
  select(subject_id, sex_status, starting_age, birth_date_new, enrolled_date_new)

# Making characters into factors that are ordered specifically 
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


# Just want presence of illness of any given year
conditions <- conditions_tbl %>% 
  filter(to_date == 0) %>%  # to date == 0 means that we are not including presence of sickness in previous years
  select(subject_id,
         year_in_study,
         any) %>% 
  drop_na(any) %>%
  mutate(any = as_factor(any))

# separation
behavior_separation <- behavior_tbl %>%
  filter(to_date == 0,
         topic == "score_separation_related_problems") %>%
  mutate(
    count_0_sep = count_0 / 8,
    count_1_sep = count_1 / 8,
    count_2_sep = count_2 / 8,
    count_3_sep = count_3 / 8,
    count_4_sep = count_4 / 8,
    count_na_sep = count_na / 8
  ) %>%
  select(
    subject_id,
    year_in_study,
    count_0_sep,
    count_1_sep,
    count_2_sep,
    count_3_sep,
    count_4_sep,
    count_na_sep
  )

# Joining the data
data_y_all <- full_join(profile_w_age,
                        activity_reordered,
                        by = "subject_id") %>%
  full_join(conditions,
            by = c("subject_id",
                   "year_in_study")) %>%
  full_join(behavior_separation,
            by = c("subject_id",
                   "year_in_study")) %>%
  na.omit()

-------------------------------------------------------------------------------
  
listy <- as.vector(c("age at start of study" = "starting_age",
           "activity level" = "activity_level_reordered",
           "sun exposure" = "sun_exposure_duration_reordered",
           "does the dog have access to shade?" = "sun_exposure_shade_access_reordered",
           "does the dog have separation anxiety?" = "count_0_sep"))
topic <- "sep"
render <- function (input, topic) {
  if (str_detect(str_flatten(input, collapse = ", "), str_c("count_0", topic, sep = "_"))) {
    input <- c(
      input,
      str_c("count_1", topic, sep = "_"),
      str_c("count_2", topic, sep = "_"),
      str_c("count_3", topic, sep = "_"),
      str_c("count_4", topic, sep = "_")
    )
    return(input)
  }
  else{
    return(input)
  }
}


library(stringr)
data_y_all %>% 
  select(any,
         year_in_study,
         render(listy, "sep")) %>%
  filter(year_in_study == 2)
  
  
data_y2 <- data_y_all %>%
  select(any,
         year_in_study,
         listy) %>%
  filter(year_in_study == 2)
  
  
data_y2 <- data_y_all %>% 
  filter(year_in_study == 2) %>% 
  select(any, 
        # year_in_study, 
         sex_status, 
         starting_age,
         activity_level_reordered,
         sun_exposure_duration_reordered)
        # sun_exposure_shade_access_reordered)

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
set.seed(123)

splits <- initial_split(data_y2, strata = any)


golden_other <- training(splits)

golden_test  <- testing(splits)

# Create a validation set
set.seed(234)
prop.validation <- .20

val_set <- initial_validation_split(golden_other, 
                   prop   = c(1 - prop.validation, prop.validation))

lr_model_golden <- logistic_reg(mixture = 1, 
                                penalty = tune()) %>% 
  set_engine("glmnet")

lr_recipe_golden <- recipe(any ~ ., data = golden_other) %>%  
    step_dummy(all_nominal(), -all_outcomes()) %>% 
    step_zv(all_predictors()) %>% 
    step_normalize(all_predictors())

lr_workflow_golden <- workflow() %>% 
    add_model(lr_model_golden) %>% 
    add_recipe(lr_recipe_golden)

lr_reg_grid_golden <- tibble(penalty = 10^seq(-4, -1, length.out = 10)) 


glm(any ~ ., 
    data = golden_other,
    family = "binomial")
-------------------------------------------------------------------------------
  
# Train and tune the model
lr_tune_golden <- lr_workflow_golden %>% 
    tune_grid(resamples = val_set, # should be fine
              grid      = lr_reg_grid_golden, # should be fine
              control   = control_grid(save_pred = TRUE), # needed to get data for ROC curve
              metrics   = metric_set(roc_auc))


lr_best_golden <- lr_tune_golden %>% 
    select_best(metric = "roc_auc")

lr_tune_golden %>% 
  collect_metrics() 

lr_plot_golden <- 
  lr_tune_golden %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())
lr_plot_golden 


lr_best_golden <- # works for prof but not me
  lr_tune_golden %>% 
  select_best(metric = "roc_auc")


# Sometimes we may want to choose a penalty value further along the x-axis, 
# closer to where we start to see the decline in model performance
# since it has effectively the same performance as the numerically 
# best model, but might eliminate more predictors. But for this example
# we will go with the `penalty` from best model.
lr_plot_golden + 
  geom_vline(xintercept     = lr_best_golden$penalty,
             color      = 'red',
             linetype   = 'dotted')


lr_auc <- 
  lr_tune_golden %>% 
  collect_predictions(parameters = lr_best_golden) %>% 
  roc_curve(any, .pred_1) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

lr_auc_plot <- lr_tune_golden %>%
    collect_predictions(parameters = lr_best_golden) %>%
    roc_curve(any, .pred_0) %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(color = 'green') +
    geom_abline(lty = 3, color = 'darkgreen') +
    coord_equal() +
    theme_classic()

plotOutput(outputId = "lr_auc_golden")


renderPrint(lr_tune_golden()
            %>% collect_predictions(parameters = lr_best_golden()) )

data <- data_y_all %>% 
  select(where(is.factor)) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "factor")


data %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(facets = vars(variable), scales = "free_y")

