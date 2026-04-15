library(tidyverse)
library(easystats)
library(stringr)

# Read the unicef data
dat = read.csv('unicef-u5mr.csv')

# Get it into tidy format
colnames(dat) <- gsub('^U5MR.', '', colnames(dat))
  
dat2 = dat %>%
  pivot_longer(col = '1950':'2015',
               names_to = 'Year',
               values_to = 'U5MR')

# Plot each country's U5MR over time
dat2 %>%
  ggplot(aes(x = Year, y = U5MR)) +
  geom_line(aes(group = CountryName)) +
  facet_wrap(~Continent) +
  scale_x_discrete(breaks = c(1960, 1980, 2000), 
                   label = c('1960', '1980', '2000')) +
  theme_bw()

# Save the plot
ggsave("BUTTERFIELD_Plot_1.png")

# Create another plot that shows the mean U5MR for all the countries
# within a given continent at each year
dat3 = dat2 %>%
  filter(!is.na(U5MR)) %>%
  group_by(Continent, Year) %>%
  mutate(Mean_U5MR = mean(U5MR, na.rm = TRUE))

dat3 %>%
  ggplot(aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(aes(group = Continent), linewidth = 1) +
  theme_bw() +
  scale_x_discrete(breaks = c(1960, 1980, 2000),
                   label = c('1960', '1980', '2000'))

# Save the plot
ggsave('BUTTERFIELD_Plot_2.png')

# Create three models of U5MR
mod1 = glm(U5MR ~ Year, data = dat3)
mod2 = glm(U5MR ~ Year + Continent, data = dat3)
mod3 = glm(U5MR ~ Year + Continent + Year:Continent, data = dat3)

# Compare the three models with respect to their performance
compare_performance(mod1, mod2, mod3) %>% plot()
        # I think mod3 is best because it has the highest R2 value
        # and is the fullest plot.

# Plot the 3 models' predictions
dat3$mod1 = predict(mod1, dat3)
dat3$mod2 = predict(mod2, dat3)
dat3$mod3 = predict(mod3, dat3)

dat3 %>%
  pivot_longer(cols = c('mod1', 'mod2', 'mod3'),
               names_to = 'Model',
               values_to = 'Predicted_U5MR') %>%
  ggplot(aes(x = Year, y = Predicted_U5MR, color = factor(Continent))) +
  geom_line(aes(group= Continent)) +
  geom_smooth(method = 'lm') +
  facet_wrap(~Model) +
  theme_bw() +
  scale_x_discrete(breaks = c(1960, 1980, 2000),
                   labels = c('1960', '1980', '2000'))

# BONUS - Predict what the U5MR would be for Ecuador in the year 2020.

modE = glm(U5MR ~ Year + CountryName, data = dat3)

