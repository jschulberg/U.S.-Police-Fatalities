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
suppressMessages(library("praise")) # Used for positive reinforcement


# Bring in the data, taking advantage of the project structure
police_data_first <- readr::read_csv(here::here("Data/Police Fatalities.csv"))
police_data_second <- readr::read_csv(here::here("Data/kaggle_PoliceKillingsUS.csv"))
census_data <- readr::read_csv(here::here("Data/censusStatePopulations2014.csv"))
county_data <- readr::read_csv(here::here("Data/HighSchoolCompletionPovertyRate.csv"))
police_deaths <- readr::read_csv(here::here("Data/clean_data.csv"))

# Convert to a tibble, my preferred data structure
(police_data_first <- as_tibble(police_data_first))
(police_data_second <- as_tibble(police_data_second))
(census_data <- as_tibble(census_data))
(county_data <- as_tibble(county_data))
(police_deaths <- as_tibble(police_deaths))

########################################################################
## Reconcile/Join Data -------------------------------------------------
########################################################################
# Notice that I've brought in a few datasets. Different datasets include
# different ranges of time for police fatalities, with some overlap. On
# top of that, I have some census, incident, and subject data

# Start by cleaning our 2000-2015 data
police_data_first_date <- police_data_first %>%
  # Use lubridate to fix up the dates
  mutate(Date = mdy(Date),
         # Change the flee column to be a character
         Flee = as.character(if_else(T, "Flee", "Not fleeing")),
         # Make the weapon with which the victim was armed all lowercase
         Armed = tolower(Armed)) %>%
  # Order by Date
  arrange(Date) %>%
  # Make all the column names lower case
  janitor::clean_names()

# Repeat for our other dataset
police_data_second_date <- police_data_second %>%
  # Use lubridate to fix up the dates
  mutate(date = dmy(date),
         # Clean up uor gender column
         gender = case_when(
           gender == "M" ~ "Male",
           gender == "F" ~ "Female",
           gender == "NA" ~ "Unknown"
         ),
         race = case_when(
           race == "W" ~ "White",
           race == "B" ~ "Black",
           race == "A" ~ "Asian",
           race == "N" ~ "Native",
           race == "H" ~ "Hispanic",
           race == "O" ~ "Other"
         ),
         manner_of_death = case_when(
           manner_of_death == "shot" ~ "Shot",
           manner_of_death == "shot and Tasered" ~ "Shot and Tasered"
         ),
         # Make the weapon with which the victim was armed all lowercase
         armed = tolower(armed)
         ) %>%
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

### Clean county/census data
# Before joining in our county/census data, let's clean it up a bit
county_cleaned <- county_data %>%
  # Clean up our column names
  janitor::clean_names() %>%
  # Super annoying, but the dataset I imported from Kaggle has an extra word
  # after every single city name, so we need to split on the final space
  mutate(city = gsub(" [^ ]*$", "", city)) %>%
  print()

# We have our full dataset now! Let's bring in a few more columns
# that will eventually help us immensely.
police_joined <- police_all_dates %>%
  rename(stateCode = state) %>%
  # Join in our census data
  left_join(census_data) %>%
  # Join in our county data
  left_join(county_cleaned, by = c("city" = "city", "stateCode" = "geographic_area")) %>%
  # Make our numeric columns not characters
  mutate(percent_completed_hs = as.numeric(percent_completed_hs),
         poverty_rate = as.numeric(poverty_rate)) %>%
  print()



########################################################################
## Analysis Time  ------------------------------------------------------
########################################################################
# Let's start by checking out the number of fatalities over time.

# Visualization time
police_joined %>%
  # Create our month and year variables
  mutate(month = month(date),
         year = year(date)) %>%
  # Start by grouping by month AND year of dates
  group_by(month, year) %>%
  # Count up our sums
  summarise(fatalities = n()) %>%
  # Bring our dates back together
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  # run our visualization
  ggplot(aes(x = date, y = fatalities)) +
    # Let's make it a column graph and change the color
    geom_line(color = "slateblue") +
    # Change the theme to classic
    theme_classic() +
    # Let's change the names of the axes and title
    xlab("") +
    ylab("Number of Fatalities") +
    labs(title = "Number of Police-caused Fatalities over Time",
         subtitle = "Data runs from 2000-2017",
         caption = "Data is gathered from Kaggle and Data.World") +
    # format our title and subtitle
    theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
          plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
          plot.caption = element_text(color = "dark gray", face = "italic", size = 10))


# What's the breakout by race?
police_joined %>%
  # Create our month and year variables
  mutate(month = month(date),
         year = year(date)) %>%
  # Start by grouping by month AND year of dates
  group_by(month, year, race) %>%
  # Count up our sums
  summarise(fatalities = n()) %>%
  # Bring our dates back together
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  # run our visualization
  ggplot(aes(x = date, y = fatalities)) +
  # Let's make it a column graph and change the color/transparency
  # geom_line(aes(color = race), lwd = .5, alpha = .7) +
  geom_line(color = "slateblue") +
  # Separate vizzes for each race
  facet_wrap(~ race) +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("") +
  ylab("Number of Fatalities") +
  labs(title = "Number of Police-caused Fatalities over Time",
       subtitle = "Data runs from 2000-2017",
       caption = "Data is gathered from Kaggle and Data.World") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"))


# Which states had the most fatalities?
police_joined %>%
  # Start by grouping by state
  group_by(state) %>%
  # Count up our sums
  summarise(fatalities = n()) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(state, fatalities), y = fatalities), label = fatalities) +
  # Let's make it a column graph and change the color/transparency
  # geom_line(aes(color = race), lwd = .5, alpha = .7) +
  geom_col(fill = "slateblue") +
  # Add a label by recreating our data build from earlier
  geom_label(data = police_joined %>%
               # Start by grouping by state
               group_by(state) %>%
               # Count up our sums
               summarise(fatalities = n()) %>%
               top_n(10),
             aes(label = fatalities),
             size = 2.5) +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("") +
  ylab("Number of Fatalities") +
  labs(title = "Number of Police-caused Fatalities by State",
       subtitle = "Data runs from 2000-2017",
       caption = "Data is gathered from Kaggle and Washington Post") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic")) +
  # flip the axes
  coord_flip()

# This isn't helpful since the populations are so different. For example,
# some of our biggest states by population, like California and Texas, are
# showing up here. I have a hunch that if we normalize by the state population
# using some of our census data from 2014, we'll get a better sense of fatalities.


# Let's normalize by state population
police_joined %>%
  # Start by grouping by state
  group_by(state) %>%
  # Count up our sums
  summarise(fatalities = n()) %>%
  # To normalize by state population, let's rejoin this data in
  left_join(census_data) %>%
  # Create our normalized data and multiply by 1,000,000 so we get the number
  # of fatalities per 1,000,000 people
  mutate(fatalities_normalized = 1000000*fatalities/popEst2014) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(state, fatalities_normalized), y = fatalities_normalized), label = fatalities_normalized) +
  geom_col(fill = "slateblue") +
  # Add a label by recreating our data build from earlier
  geom_label(data = police_joined %>%
               # Start by grouping by state
               group_by(state) %>%
               # Count up our sums
               summarise(fatalities = n()) %>%
               # To normalize by state population, let's rejoin this data in
               left_join(census_data) %>%
               # Create our normalized data and multiply by 1,000,000 so we get the number
               # of fatalities per 1,000,000 people
               mutate(fatalities_normalized = 1000000*fatalities/popEst2014) %>%
               top_n(10),
             aes(label = round(fatalities_normalized, 0)),
             size = 2.5) +  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("") +
  ylab("Number of Fatalities*") +
  labs(title = "Number of Police-caused Fatalities by State",
       subtitle = "*Fatalities per 1,000,000 population",
       caption = "Data is gathered from Kaggle and Washington Post") +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic")) +
  # flip the axes
  coord_flip()

# Super interesting! We now have a lot of states in the new visualization,
# showing that just because a state, like Texas, has a lot of police-caused
# fatalities, it doesn't mean that it's as high as other states proportionately
# to state population


########################################################################
## High School Completion Rate Analysis  -------------------------------
########################################################################
# I'd like to analyze the number of shootings against the average city
# population which completed high school. My hypothesis here is that there
# will be more shootings in cities with low high school completion rates.

# The first thing to do is to fill out our data a bit more. There is a lot
# of data that didn't match. What I plan to do for imputation is find the
# average percent_completed_highschool for each state and impute that.

# Start by computing the average high school completion rates by state
hs_averages <- police_joined %>%
  group_by(state) %>%
  summarise(percent_completed_hsavg = mean(percent_completed_hs, na.rm = T))

# Split our dataset on those with NA values and those without
police_hs_na <- police_joined %>%
  # Only keep our nas for the first one
  filter(is.na(percent_completed_hs)) %>%
  left_join(hs_averages) %>%
  # now get rid of the percent_completed_hs field and rename the other one to match
  select(-percent_completed_hs) %>%
  rename(percent_completed_hs = percent_completed_hsavg) %>%
  print()

police_hs_nona <- police_joined %>%
  # Only keep our non-nas for the second one
  filter(!(is.na(percent_completed_hs)))

# Now bring both datasets back together
police_hs <- bind_rows(police_hs_na, police_hs_nona)

# If the number of rows we ended with doesn't match the number of rows
# we started with, or if there are still any nulls in the percent_completed_hs
# field, throw an error message.
if (nrow(police_hs) != nrow(police_joined) | any(is.na(police_hs$percent_completed_hs))) {
  "ERROR! SOMETHING'S UP WITH THE police_hs dataframe."
} else {
  cat(praise(), "Let's keep going.")
}

# Let's look at a boxplot of the data
ggplot(police_hs, aes(y = percent_completed_hs)) +
  geom_boxplot(outlier.colour="slateblue4",
               outlier.size=2,
               color = "slateblue3") +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = "High School Completion Rate by Geographic Area",
       subtitle = "Data is collected from 2014 Census data.",
       caption = "") +
  ylab("Percentage (%)") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"),
        # remove the x axis labels because they don't mean much for us
        axis.text.x = element_blank()) +
  # I thought the boxplot was too thick, so let's make it a little skinnier
  scale_x_discrete(limits=c("-.1", ".1"))

# It looks like most of the areas have pretty high high school completion
# rates, with a median of
median(police_hs$percent_completed_hs)
# and an average of
mean(police_hs$percent_completed_hs)

### Correlation
# Let's now look to see if there's any correlation between high school
# completion rate and number of police-related fatalities.
police_hs %>%
  # Start by grouping by state
  group_by(state) %>%
  # Count up our sums
  summarise(fatalities = n(),
            hs_completionavg = mean(percent_completed_hs)) %>%
  ggplot(aes(x = hs_completionavg, y = fatalities)) +
  # Make it a scatter plot
  geom_point(color = "slateblue", alpha = .8) +
  geom_text(aes(label = state), # label by state
            color = "slateblue", # Make our color match
            size = 3, # shrink the size
            alpha = .8, # add some transparency
            check_overlap = T, # avoid overlabelling
            nudge_y = 150) + # nudge the text a bit off center
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = "Police-caused Fatalities by High School Completion Rate",
       subtitle = "Data is broken out by state and uses 2014 Census data.",
       caption = "") +
  xlab("High School Completion Rate (%)") +
  ylab("Police-caused Fatalities") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"))


# Again, this doesn't account for normalizing our data by population. Let's
# see how that changes things
police_hs %>%
  # Start by grouping by state
  group_by(state) %>%
  # Count up our sums
  summarise(fatalities = n(),
            hs_completionavg = mean(percent_completed_hs)) %>%
  # To normalize by state population, let's rejoin this data in
  left_join(census_data) %>%
  # Create our normalized data and multiply by 1,000,000 so we get the number
  # of fatalities per 1,000,000 people
  mutate(fatalities_normalized = 1000000*fatalities/popEst2014) %>%
  ggplot(aes(x = hs_completionavg, y = fatalities_normalized)) +
  # Make it a scatter plot
  geom_point(color = "slateblue", alpha = 1) +
  theme_classic() +
  # Let's change the names of the axes and title
  labs(title = "Normalized Police-caused Fatalities\nby High School Completion Rate",
       subtitle = "Police-caused fatalities are per 1,000,000 population\nusing 2014 Census data.",
       caption = "*per 1,000,000 population") +
  xlab("High School Completion Rate (%)") +
  ylab("Police-caused Fatalities*") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"))

# Between both of these graphs, I can't see any correlation in the data. It's
# reassuring to know that police-caused fatalities would not drastically increase
# in areas with lower high school completion rates.

#What's the actual correlation?
police_hs %>%
  # Start by grouping by state
  group_by(state) %>%
  # Count up our sums
  summarise(fatalities = n(),
            hs_completionavg = mean(percent_completed_hs)) %>%
  # To normalize by state population, let's rejoin this data in
  left_join(census_data) %>%
  # Create our normalized data and multiply by 1,000,000 so we get the number
  # of fatalities per 1,000,000 people
  mutate(fatalities_normalized = 1000000*fatalities/popEst2014) %>%
  select(-state, -stateCode) %>%
  cor() %>%
  pander()

# From this we can see that the correlation is highest with population,
# which makes sense. Once normalizing for population, the correlation
# drops to -.025, which is highly uncorrelated. Overall, I would say
# that there is no correlation between fatalities and high school
# completion rate.

########################################################################
## Poverty Rate Analysis  ----------------------------------------------
########################################################################
# I'd like to analyze the number of shootings against the average city
# poverty rate. My hypothesis here is that there will be more shootings
# in cities with high poverty rates, although my hypothesis earlier
# was debunked, so we will see!
