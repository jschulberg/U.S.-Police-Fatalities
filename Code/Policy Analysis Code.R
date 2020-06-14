#######################################################################
#######################################################################

#####               U.S. Policy Fatalities Analysis               #####

#######################################################################
#######################################################################
# In this script, I will analyze the U.S. Policy Fatalities dataset included
# in the data folder of this repository. I will use a variety of exploratory
# and modeling techniques to do so, including, but not limited to:

#   --

#######################################################################
# Set Up --------------------------------------------------------------
#######################################################################
# Bring in packages
suppressMessages(library("tidyverse")) # Used for data wrangling
suppressMessages(library("tidyr")) # Used for data cleaning
suppressMessages(library("ggplot2")) # Used for visualizations
suppressMessages(library("readxl")) # Used for loading excel files
suppressMessages(library("readr")) # Used for working with files
suppressMessages(library("pander")) # Used for pretty tables


# Bring in the data, delimited by a tab ("\t")
data_crime <- read.delim(here::here("Data/uscrime.txt"), header = T)

# Convert to a tibble, my preferred data structure
data_crime <- as_tibble(data_crime)

# Let's take a peek under the hood
head(data_crime)
summary(data_crime)

# Below is a list of the variables in data along with their associated descriptions. We want to predict the last
# column, Crime, based on the other predictor variables.
# Variable	 	Description
# M		percentage of males aged 14-24 in total state population
# So		indicator variable for a southern state
# Ed		mean years of schooling of the population aged 25 years or over
# Po1		per capita expenditure on police protection in 1960
# Po2		per capita expenditure on police protection in 1959
# LF		labour force participation rate of civilian urban males in the age-group 14-24
# M.F		number of males per 100 females
# Pop		state population in 1960 in hundred thousands
# NW		percentage of nonwhites in the population
# U1		unemployment rate of urban males 14-24
# U2		unemployment rate of urban males 35-39
# Wealth		wealth: median value of transferable assets or family income
# Ineq		income inequality: percentage of families earning below half the median income
# Prob		probability of imprisonment: ratio of number of commitments to number of offenses
# Time		average time in months served by offenders in state prisons before their first release
# Crime		crime rate: number of offenses per 100,000 population in 1960


########################################################################
# Linear Regression ----------------------------------------------------
########################################################################
# In the first section, we'll use a simple linear regression model to
# predict on our crime data.


