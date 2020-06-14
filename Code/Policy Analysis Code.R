###########################################################################
###########################################################################

#######               U.S. Policy Fatalities Analysis               #######

###########################################################################
###########################################################################
# In this script, I will analyze the U.S. Policy Fatalities dataset included
# in the data folder of this repository. I will use a variety of exploratory
# and modeling techniques to do so, including, but not limited to:

#   --

###########################################################################
# Set Up ------------------------------------------------------------------
###########################################################################
# Bring in packages
suppressMessages(library("tidyverse")) # Used for data wrangling
suppressMessages(library("tidyr")) # Used for data cleaning
suppressMessages(library("ggplot2")) # Used for visualizations
suppressMessages(library("readxl")) # Used for loading excel files
suppressMessages(library("readr")) # Used for working with files
suppressMessages(library("pander")) # Used for pretty tables
suppressMessages(library("lubridate")) # Used for fixing dates


# Bring in the data, taking advantage of the project structure
police_data_first <- readr::read_csv(here::here("Data/Police Fatalities.csv"))
police_data_second <- readr::read_csv(here::here("Data/kaggle_PoliceKillingsUS.csv"))

# Convert to a tibble, my preferred data structure
(police_data_first <- as_tibble(police_data_first))
(police_data_second <- as_tibble(police_data_second))


########################################################################
## Reconcile/Join Data -------------------------------------------------
########################################################################
# Notice that I've brought in a few datasets. Different datasets include
# different ranges of time for police fatalities, with some overlap. On
# top of that, I have some census, incident, and subject data

# Start by cleaning our date columns
police_data_first_date <- police_data_first %>%
  # Use lubridate to fix up the dates
  mutate(Date = mdy(Date),
         # Change the flee column to be a character
         Flee = as.character(if_else(T, "Flee", "Not fleeing	"))) %>%
  # Order by Date
  arrange(Date) %>%
  # Make all the column names lower case
  janitor::clean_names()

# Repeat for our other dataset
police_data_second_date <- police_data_second %>%
  # Use lubridate to fix up the dates
  mutate(date = dmy(date)) %>%
  # Rename our mental illness column
  rename(mental_illness = signs_of_mental_illness) %>%
  # Order by Date
  arrange(date)

# What's our range of dates?
range(police_data_first_date$Date)
range(police_data_second_date$date)

# How much overlap is there between the two datasets?
table(police_data_first_date$name %in% police_data_second_date$name)

# Split out our names that are not in the first dataset
second_no_overlap <- police_data_second_date[!(police_data_second_date$name %in% police_data_first_date$name), ]

# Join both datasets together
police_all_dates <- police_data_first_date %>%
  # There are a lot of duplicates, so let's try to remove these
  # We'll look for duplicative names + dates, since there are naturally
  # some repeat common names
  distinct(name, age, .keep_all = T) %>%
  bind_rows(second_no_overlap) %>%
  # Get rid of previous ID column and make our ID row unique
  select(-id) %>%
  mutate(uid = row_number()) %>%
  rename(id = uid) %>%
  print()

police_compare <- police_data_first_date %>%
  # There are a lot of duplicates, so let's try to remove these
  distinct(name, .keep_all = T) %>%
  bind_rows(second_no_overlap) %>%
  # Get rid of previous ID column and make our ID row unique
  select(-id) %>%
  mutate(uid = row_number()) %>%
  rename(id = uid) %>%
  print()

# Where's the overlap?
compare <- police_all_dates[duplicated(police_all_dates$name), ]


# We have our full dataset now! Let's bring in a few more columns
# that will eventually help us immensely.
