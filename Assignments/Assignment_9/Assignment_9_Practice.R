library(tidyverse)
library(easystats)
library(GGally)
library(modelr)

dat= read.csv("GradSchool_admissions.csv")
summary(dat)

dat %>%
  ggpairs()

moda= glm(admit ~ gpa, data = dat, family = 'binomial')
summary(moda)
modb = glm(admit ~ gre, data= dat, family = 'binomial')
summary(modb) 
modc = glm(admit ~ rank, data = dat, family = 'binomial')
summary(modc)
modd = glm(admit ~ gpa + rank + gre, data = dat, family = 'binomial')
summary(modd)
modf = glm(admit ~ gpa:gre, data = dat, family = 'binomial')
summary(modf)
modg = glm(admit ~ gpa:rank, data = dat, family = 'binomial')
summary(modg)
modh = glm(admit ~ gpa + gre + gpa:gre, data = dat, family = 'binomial')
summary(modh)

compare_performance(moda, modb, modc, modd, modf, modg, modh) %>% plot()
compare_performance(moda, modb, modc, modf, modg, modh) %>% plot()
compare_performance(moda, modb, modf, modg, modh) %>% plot()

#compare AIC -> we want to find the lowest value
AIC(moda, modb, modc, modd, modf, modg, modh)

#compare BIC -> we want to find the lowest value
BIC(moda, modb, modc, modd, modf, modg, modh)

compare_performance(moda, modb, modc, modd, modf, modg, modh, rank = TRUE)



grad %>%
  gather_predictions(modd, modc) %>%
  ggplot(aes(x = admit, y =pred, color=model)) +
  facet_wrap(~rank) +
  geom_smooth()

modd %>% model_parameters(exponentiate = TRUE)
modd %>% model_parameters()

odds_rations <- exp(coef(modd))
dat3 = dat2
dat3$odds = exp(coef(modd))

dat %>%
  ggplot(aes(x = admit, y = gpa, color = gre)) +
  geom_point() +
  facet_wrap(~rank) +
  geom_smooth(method = 'glm')

dat2 = dat

dat2$pred = predict(modd, dat2, type = 'response')

dat2 %>%
  ggplot(aes(x = admit, y = pred, color = rank)) +
  geom_point()

dat %>%
  ggplot(aes(x = gpa, y = gre, color = admit)) +
  geom_point() +
  geom_smooth(method = 'glm') +
  facet_wrap(~rank)
