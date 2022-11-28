dat = read.csv("pilot data analysis.csv", header=TRUE, na.strings=c("", "NA", " ", "N/A"))  

require(tidyverse)
require(lme4)
require(lmerTest)

# Remove First Row (junk)
dat <- dat[-c(1),]

# Variables necessary for viewing and analyzing the data
dat <- dat %>%
  select(participant, hitPercent, pointsGiven, horOrTilt, hitOrMiss, trialType, trialsNum, tasksNum, wallOrient, Sound)

# Outlier analysis - Exclude participants who missed the ball 50%+ of the time
dat = dat %>% group_by(participant) %>%
  mutate(hitPercent2 = mean(hitOrMiss == "hit",na.rm = TRUE))  %>%
  filter(hitPercent2 > 0.5)

# Repeated Measures ANOVA: x1 = Wall Orientation, x2 = Sound, y = Points Given 
dat$Sound <- as.factor(dat$Sound)
dat$wallOrient <- as.factor(dat$wallOrient)
dat$pointsGiven <- as.numeric(dat$pointsGiven)

Model3 = lmer(pointsGiven ~ wallOrient*Sound + (1 | participant), 
              data = dat)

summary(Model3)
