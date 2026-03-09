install.packages('tidyverse')
library(tidyverse)

# Read the cleaned_covid_data.csv file into an R data frame.
read.csv('cleaned_covid_data.csv')
cleaned_covid_data <- read.csv('cleaned_covid_data.csv')

# Subset the data set to just show states that begin with "A"
# and save this as an object called A_states.
A_states <- cleaned_covid_data %>%
  filter(str_detect(Province_State, "A"))

# Create a plot of that subset showing Deaths over time, with a
# separate facet for each state.

A_states %>%
  ggplot(aes(x = Last_Update,
             y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Province_State, scales = 'free')


# (Back to the full dataset) Find the "peak" of Case_Fatality_Ratio
# for each state and save this as a new data frame object called
# state_max_fatality_rate.
State1 <- cleaned_covid_data %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE))

State_max_fatality_rate <- arrange(State1, desc(Maximum_Fatality_Ratio))

# Use that new data frame from task IV to create another plot.
State_max_fatality_rate %>%
  ggplot(aes( x = reorder(Province_State, -Maximum_Fatality_Ratio),
              y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90))

# Using the FULL data set, plot cumulative deaths for the entire US
# over time
cleaned_covid_data %>%
  ggplot(aes(x = Last_Update,
             y = Deaths)) +
  geom_bar(stat = 'identity')
