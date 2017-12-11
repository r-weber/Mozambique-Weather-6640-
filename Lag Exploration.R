################################
# Lag Variable Exploration
# December 5, 2017
# Rachel Weber
################################
setwd("C:/Users/rache/Documents/6640/Final Project")

temp7 <- read.table(file="complete_data.txt", sep="")
library(dplyr)
library(Hmisc)
library(lme4)
############################################################################
# Create Lag in Rainfall
temp6 <- temp3 %>%
  group_by(DISTCODE) %>%
  mutate(rain2w = Lag(raintot, +2),
         rain4w = Lag(raintot, +4),
         rain8w = Lag(raintot, +8))

write.table(temp6, file="rainlag.txt")

# correlation of rain lag variables
rain_cor <- rcorr(as.matrix(rainvars)) # need Hmisc package for rcorr
round(rain_cor$r, 3)
  # since there is no correlation, all lag vars belong in the model

# find mutual information
rain <- as.matrix(rainvars)
rain <- discretize(rain)
# mutual information is the measure of dependence between variables
# The reduction in uncertainty in X due to the knowledge of Y
# if 0, the variables are independent
mutinformation(rain)

# maybe log transforming improves the trends?
temp9$log_rain2w <- log(temp9$rain2w)
temp9$log_rain4w <- log(temp9$rain4w)
temp9$log_rain8w <- log(temp9$rain8w)

ggplot(temp9, aes(x=log_rain2w, y=cases, color = '2 weeks')) +
  geom_smooth(se=F, size=1.5) +
  geom_smooth(aes(x=log_rain4w, y=cases, color = '4 weeks'), se=F, size=1.5) +
  geom_smooth(aes(x=log_rain8w, y=cases, color = '8 weeks'), se=F, size=1.5) +
  labs(x="Rainfall (mm)", y = "Cases", title = "Rain Lag Predicting Reported Cases") +
  scale_color_manual(name = "Lag", values = c('blue3', 'darkorange', 'darkorchid1'))
  #relationship doesn't achieve added clarity when I log transform the lag variables

############################################################
# Create Lag in Humidity
temp8 <- temp7 %>%
  group_by(DISTCODE) %>%
  mutate(rh2w = Lag(rh, +2),
         rh4w = Lag(rh, +4),
         rh8w = Lag(rh, +8))


###############################################################
# Create Lag in Temperature
temp9 <- temp8 %>%
  group_by(DISTCODE) %>%
  mutate(tavg2w = Lag(tavg, +2),
         tavg4w = Lag(tavg, +4),
         tavg8w = Lag(tavg, +8))

write.table(temp9, file="actually_complete_data.txt")

temp9 <- read.table(file="actually_complete_data.txt", sep="")
#########################################################################
# Model Temperature

# omit NAs so Partial F Test can be run on full and reduced models
temp10 <- temp9[!is.na(temp9$tavg8w),]

# full model
plz <- lm(cases ~ tavg2w + tavg4w + tavg8w + (1|DISTCODE), offset=log(u5total), data=temp10)

plot(plz)
  # heteroscedastic
  # unequal variance because relevent covariates aren't being included in model?

# Full/reduced models for Partial F tests
plz3 <- lm(cases ~ tavg2w + tavg4w + (1|DISTCODE), offset=log(u5total), data=temp10)

anova(plz, plz3)
  # significant
  # the addition of 8 weeks has a stat'ly sig impact on the model

plz4 <- lm(cases ~ tavg2w + tavg4w + tavg8w + (1|DISTCODE), offset=log(u5total), data=temp10)
plz5 <- lm(cases ~ tavg2w + tavg8w + (1|DISTCODE), offset=log(u5total), data=temp10)

anova(plz4, plz5)
# significant
# the addition of 4 weeks has a stat'ly sig impact on the model

plz6 <- lm(cases ~ tavg2w + tavg4w + tavg8w + (1|DISTCODE), offset=log(u5total), data=temp10)
plz7 <- lm(cases ~ tavg4w + tavg8w + (1|DISTCODE), offset=log(u5total), data=temp10)

anova(plz6, plz7)
# significant
# the addition of 2 weeks has a stat'ly sig impact on the model

# test for correlation of temperature lag variables
tempvars <- temp9[,26:28]
rain_cor <- rcorr(as.matrix(tempvars)) # need Hmisc package for rcorr
round(rain_cor$r, 3)
  #moderate correlation

# model all 2 week lag variables together
model <- glm(cases ~ rain2w + tavg2w + rh2w + (1|DISTCODE), offset=log(u5total),
             family='poisson', data=temp9)
  # all variables are significant

plot(model)
  # bad model fit, multiple assumptions are likely violated
  # not surprising since I don't know how to model these

# test for correlation between temp, rain, humidity 2 weeks from clinic visit
temp11 <- temp9[!is.na(temp9$rain2w),]
cor(temp10$rain2w, temp10$rh2w) #low
cor(temp10$rain2w, temp10$tavg2w) #low
cor(temp10$tavg2w, temp10$rh2w) #low
  #since the variables don't have very strong correlation, they could be used together in a model

###########################################################################
# model all lags together

m1 <- glmer(cases ~ 1 + tavg + tavg4w + raintot + rain4w + ITNprot + 
              IRSprot + (1|DISTCODE), offset = log(u5total), family = "poisson", data = temp9)
  # model doesn't converge

m2 <- glmer(cases ~ 1 + tavg4w + rain4w + ITNprot + 
              IRSprot + (1|DISTCODE), offset = log(u5total), family = "poisson", data = temp9)
  # model doesn't converge

m3 <- glmer(cases ~ 1 + tavg4w + rain4w + rh4w + (1|DISTCODE), offset = log(u5total), family = "poisson", data = temp9)
  # model doesn't converge