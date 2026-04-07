library(tidyverse)
library(gganimate)
library(gifski)

dat <- read.csv("BioLog_Plate_Data.csv")

# Clean the Data into tidy form
dat %>%
  distinct(Sample.ID)

dat2 = dat %>%
  pivot_longer(cols = c('Hr_24', 'Hr_48', 'Hr_144'),
               names_to = "Time",
               values_to = 'Absorbance') %>%
  mutate(Time = case_when(Time == 'Hr_24'~24,
                          Time == 'Hr_48'~48,
                          Time == 'Hr_144'~144))

# Create a new column specifying whether a sample is from soil or water
dat3 = dat2 %>%
  mutate(Type = case_when(Sample.ID == 'Clear_Creek' ~ 'Water',
                          Sample.ID == 'Waste_Water' ~ 'Water',
                          Sample.ID == 'Soil_1' ~ 'Soil',
                          Sample.ID == 'Soil_2' ~ 'Soil'))

# Generate Plot
dat3 %>%
  filter(Dilution == '0.001') %>%
  ggplot(aes(x = Time, y = Absorbance, color = Type)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Substrate) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, 2, by = 0.5))


# Generate animated plot
dat4 = dat3 %>%
  filter(Substrate == 'Itaconic Acid')

dat5 = dat4 %>%
  pivot_wider(names_from = Rep, values_from = Absorbance)

dat5$Mean_absorbance <- rowMeans(dat5[, c('1', '2', '3')])

dat5 %>%
  ggplot(aes(x = Time, y = Mean_absorbance, color = Sample.ID)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Dilution)

dat6 = dat5 %>%
  mutate(hour = Time)

dat6 %>%
  ggplot(aes(x = Time, y = Mean_absorbance, color = Sample.ID)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~Dilution) +
  transition_time(hour)
