library(modelr)
library(easystats)
library(broom)
library(tidyverse)
library(fitdistrplus)

#load the data set
dat <- read.csv('../../Data/mushroom_growth.csv')

#create several plots exploring relationships between the response and predictors
#define at least 4 models that explain the dependent variable "GrowthRate"

mod1 = lm(formula = GrowthRate ~ Light, data = dat)
summary(mod1)
dat %>%
  ggplot(aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()

mod2 = lm(formula = GrowthRate ~ Nitrogen, data = dat)
summary(mod2)
dat %>%
  ggplot(aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_minimal()

mod3 = lm(formula = GrowthRate ~ Nitrogen + Light, data = dat)
summary(mod3)
dat %>%
  ggplot(aes( x = Nitrogen, y = GrowthRate, color = Light)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

mod4 = lm(formula = GrowthRate ~ Light + Nitrogen + Humidity, data = dat)
summary(mod4)

mod5 = lm(formula = GrowthRate ~ Species, data = dat)

#calculate the mean sq. error of each model
#select the best model you tried
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2) #best model
mean(mod5$residuals^2)

#add predictions based on new hypothetical values
newdf = data.frame(Light = c(25, 30, 40, 45),
                   Nitrogen = c(15, 50, 55, 23),
                   Humidity = c('High', 'Low', 'High', 'Low'))

pred = predict(mod4, newdata = newdf)

hyp_preds <- data.frame(Light = newdf$Light,
                        Nitrogen = newdf$Nitrogen,
                        Humidity = newdf$Humidity,
                        pred = pred)

dat$PredictionType <- 'Real'
hyp_preds$PredictionType <- 'Hypothetical'

fulldat <- full_join(dat,hyp_preds)

# plot the predictions alongside the real data
fulldat %>%
  ggplot(aes( x = Nitrogen, y = pred, color = PredictionType)) +
  geom_point() +
  geom_smooth(aes( y = GrowthRate), color = 'black') +
  theme_minimal()


##Canvas Upload

#Are any of your predicted response values for your best model scientifically meaningless? Explain
  #No, I do not think any of the values are meaningless because all of the values are positive
  #and the predicted values are reasonable.

#In your plots, did you find any non-linear relationships?
  #Yes, my 3rd model (mod3) had a non-linear relationship.
  #Modeling non-linear relationships:
    #https://www.econometrics-with-r.org/8.1-a-general-strategy-for-modelling-nonlinear-regression-functions.html
    #https://www.datacamp.com/tutorial/introduction-to-non-linear-model-and-insights-using-r

#Write the code you would use to model the data found in "/Data/non_linear_relationship.csv"
#with a linear model
nondat <- read.csv('../../Data/non_linear_relationship.csv')
modnl1 = lm(response ~ predictor, data = nondat)

