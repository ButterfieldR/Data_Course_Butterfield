# Load Libraries
library(tidyverse)
library(easystats)

# import the data set
dat = read.csv("Utah_Religions_by_County.csv")

# Clean it into tidy shape
colnames(dat)

dat2 = dat %>%
  pivot_longer(cols = starts_with(c('Assemblies.of.God', 'Episcopal.Church',
                                    'Pentecostal.Church.of.God', 'Greek.Orthodox',
                                    'LDS', 'Southern.Baptist.Convention', 
                                    'United.Methodist.Church', 'Buddhism.Mahayana',
                                    'Catholic', 'Evangelical', 'Muslim', 'Non.Denominational',
                                    'Orthodox')),
               names_to = 'Religion',
               values_to = 'Percent_Pop')


# Explore
dat2 %>%
  ggplot(aes(x = Pop_2010, y = Religious, color = County)) +
  geom_point()

dat2 %>%
  ggplot(aes(x = Pop_2010, y = Religious, color = County)) +
  geom_point() +
  geom_label(aes(label = County))
     #to see if there was any pattern with the percent religious as the population increased

dat2 %>%
  ggplot(aes(x = Religion, y = Percent_Pop, color = Religion)) +
  geom_point() +
  facet_wrap(~County)
    #to see what religion was the most popular in each county, compare how popular
    #the most popular religion was to the other religions, and compare counties

cor.test(dat2$Pop_2010, dat2$Percent_Pop)

cor.test(dat2$Pop_2010, dat2$Religious)

cor.test(dat2$Percent_Pop, dat2$Non.Religious)
#use cor.test() to see if there is any significant correlation value

# Address the questions
  #1. Does population of a county correlate with the proportion of any specific religious
  # group in that county? No, there is not a correlation (the values are so small
  # they can be considered negligible).
  #2. Does proportion of any specific religion in a given county correlate with the 
  # proportion of non-religious people? No. The correlation value is very small 
  # making it negligible