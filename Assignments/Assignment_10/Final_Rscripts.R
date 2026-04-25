library(tidyverse)
library(stringr)
library(kableExtra)
library(easystats)
library(readr)

list.files()

dat = read.csv("GallusGallusDomesticus.csv")

summary(dat$GallusBreed)
table(dat$GallusBreed)
unique(dat$GallusBreed)

range(dat$Age)
unique(dat$Age)
mean(dat$Age)

unique(dat$GallusEggColor)

dat %>%
  ggplot(aes(x = GallusBreed, y = Age, color = GallusBreed)) +
  geom_boxplot()

dat %>%
  ggplot(aes(x=AmountOfFeed, y= EggsPerDay, color = GallusBreed)) +
  geom_point()

cor.test('AmountOfFeed', 'EggsPerDay', method = 'pearson')

is.numeric(dat$AmountOfFeed)
is.numeric(dat$EggsPerDay)
dat$AmountOfFeed <- as.numeric(dat$AmountOfFeed)
dat$EggsPerDay <- as.numeric(dat$EggsPerDay)
cor('AmountOfFeed', 'EggsPerDay', use = 'complete.obs', method ='pearson')
str(dat)

mod1 <- glm(data = dat, EggsPerDay ~ AmountOfFeed)
mod2<- glm(data= dat, EggsPerDay ~ Amouglm(data = dat, EggsPerDay ~ AmountOfFeed + GallusEggWeight + SunLightExposure)ntOfFeed + GallusBreed)
mod3 <- glm(data = dat, EggsPerDay ~ GallusEggWeight + AmountOfFeed)
mod4 <- glm(data = dat, EggsPerDay ~ AmountOfFeed + GallusEggWeight + SunLightExposure)
# do not use: mod5 <- glm(data = dat, EggsPerDay ~ AmountOfFeed + GallusEggWeight + Age)
# mod6 <- glm(data = dat, EggsPerDay ~ AmountOfFeed + Age)
compare_performance(mod1,mod2,mod3,mod4) %>% plot()
#Best: high R2, Low AIC and BIC and low Sigma and RMSE
# Low RMSE/Sigma is essentail for forcasting and predictive accuracy
# we chose a model based on that because we are trying to predict how many eggs we get per day
# which meansmod 3


dat %>%
  ggplot(aes(x=AmountOfFeed, y=EggsPerDay)) +
  geom_point() +
  geom_smooth(method = 'glm')
ggsave('EggvsFeed.png')

dat %>%
  ggplot(aes(x=AmountOfFeed, y=EggsPerDay, color = GallusBreed)) +
  geom_point() +
  geom_smooth(method = 'glm')
dat %>%
  ggplot(aes(x=AmountOfFeed, y=EggsPerDay, color = GallusEggWeight)) +
  geom_point() +
  geom_smooth(method = 'glm')

shapiro.test(dat$AmountOfFeed)
shapiro.test(dat$EggsPerDay)
cor(dat$AmountOfFeed, dat$EggsPerDay, method = 'kendall')
cor.test(dat$AmountOfFeed, dat$EggsPerDay, method = 'kendall')
#first do shapiro test to see if the data is normal (it is not)
#use kendall test because of tied ranks with non-normal data
# small negative kendall value means that as the amount of feed increases,
# the number of eggs per day decreases
#then you run cor.test to see if the correlation is significant
# P-value: 0.001094 < 0.05 means it is statistically significant (unlikely to occur by random chance)
# report Kendall's correlation coefficient (-0.08616118)
# the coefficient means very weak negative association (as one increases, the other decreases)
# Overall: there is a relationship but it is very weak


-3.2653 > 1.96
0.001094 < 0.05
2.2e-16 < 0.05

shapiro.test(dat$GallusEggWeight)
2.476e-15<0.05 #meaning non-normal distribution

#not : cor(dat[,c('EggsPerDay','AmountOfFeed','GallusEggWeight')], method = 'kendall')

dat %>%
  ggplot(aes(x= GallusBreed, y = GallusEggWeight, color = Age)) +
  geom_point() +
  geom_smooth(method = 'glm')

dat %>%
  ggplot(aes(x=AmountOfFeed, y=GallusWeight, color = GallusBreed)) +
  geom_point() +
  geom_smooth(method = 'glm')

shapiro.test(dat$GallusWeight)
2.2e-16<0.05
#meaning non-normal distribution

cor(dat$AmountOfFeed, dat$GallusWeight, method = 'kendall')
cor.test(dat$AmountOfFeed, dat$GallusWeight, method = 'kendall')
#p-value mean highly statistically significant
# z-score means statistically significant
#tau valuee shows very weak positive correlation (negligable)

install.packages('broom')