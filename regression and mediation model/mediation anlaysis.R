rm(list = ls())

library(Matrix)
library(plyr)
library(car)
library(stringr)
library(foreign)
library(ggplot2)
library(ggpmisc)
library(psych)
library(lme4)
library(tidyr)
library(gridExtra)
library("MASS")
library("Matrix")
library("mvtnorm")
library("mediation")
library("sandwich")

setwd('D:/behavioral study/EADanalysis/results/data seperated into two parts')

b <- read.csv("twogroup2500-6000.csv",sep = ";",header = TRUE)   # read into the file

#state anxiety

lm1 <- lm(taversivesound ~ stateanxiety * group,b)  
summary(lm1)
anova(lm1)
Anova(lm1)

lm2<- lm(taversivesound ~ stateanxiety,b[b$group=="social",])  
summary(lm2)
anova(lm2)
Anova(lm2)

lm3 <- lm(taversivesound ~ stateanxiety,b[b$group=="alone",])  
summary(lm3)
anova(lm3)
Anova(lm3)

#plot
p.postanxiety <- ggplot(data = b, mapping = aes(x = stateanxiety, y = taversivesound))+aes(colour = factor(b$group))+stat_smooth(method = lm)

p.postanxiety+ theme_bw()

#traitanxiety

lm4 <- lm(taversivesound ~ traitanxiety * group,b)  
summary(lm4)
anova(lm4)
Anova(lm4)

#anxiety sensitivity

lm5 <- lm(taversivesound ~ ASI * group,b)  
summary(lm5)
anova(lm5)
Anova(lm5)



# mediation effect-state anxiety
model0 <- lm(taversivesound ~ stateanxiety+significant.other+friends,data = b[b$group=="social",])
summary(model0)
anova(model0)
Anova(model0)

model1 <- lm( similarity ~ stateanxiety+significant.other+friends, data = b[b$group=="social",])
summary(model1)

model2 <- lm(taversivesound ~ similarity+stateanxiety+significant.other+friends, data = b[b$group=="social",])
summary(model2)

model3 <- mediate(model1, model2,  treat = "stateanxiety", mediator = "similarity",
                  boot = TRUE,sims = 5000,data = b[b$group=="social",] )
summary(model3, digits=5)


