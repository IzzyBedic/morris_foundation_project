# Libraries --------------------------------------------------------------------
library(here)

# Notes ------------------------------------------------------------------------
# kmeans?

# roc auc, confusion matrix good evaluations of model
# Presence of illness
# If energy curve is bimodel (bunch of energetic and un-energetic dogs, then can do energy)
# congruence btwn dift models
# age -- lubridate (enrolled year - birth year + year in study)
# h1 - H5 header size

# Bonus if incorporated into portfoliodown

# Libraries --------------------------------------------------------------------
# Age
profile_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                             "dog_profile.csv"))

# Whether pet, whether employed (Primary lifestyle is pet or not)
lifestyle_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                               "activity_lifestyle", 
                               "lifestyle.csv"))

# Activity, shade vs sun, etc. 
activity_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                              "activity_lifestyle", 
                              "activity_overview.csv"))

# Has energy score, excitability, trainability, dog rivalry, chasing
behavior_data <- read.csv(here("DO_NOT_UPLOAD_DATA", 
                              "behavior", 
                              "behavior_summary.csv"))


# Functions --------------------------------------------------------------------





