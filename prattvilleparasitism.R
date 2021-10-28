library(gridExtra)
library(grid)
library(lattice)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(viridis)
library(hrbrthemes)
library(tibble)
library(caret)
library(ggridges)
library(binaryLogic)

"C:/Users/kstacey/OneDrive - University of Florida/R Stuff/R/parasitism data/prattvilleparasitism.csv"
prattvilleparasitism<-read.csv("Raw_data/prattvilleparasitism.csv")
############################################# data types  ##########################################
prattvilleparasitism$larvalEmergence<-factor(prattvilleparasitism$larvalEmergence, levels=c("1","0"))
prattvilleparasitism$eggs<-factor(prattvilleparasitism$eggs, levels=c("y","n"))
prattvilleparasitism$Sex<-factor(prattvilleparasitism$Sex, levels=c("m","f","nymph"))
prattvilleparasitism$matingpair<-factor(prattvilleparasitism$matingpair, levels=c("y","n"))
prattvilleparasitism$Crop<-factor(prattvilleparasitism$Crop, levels=c("Corn BF","Corn 1","Corn 2","Corn 3","Corn 4","Corn 5"))

####################################### emergence by sex ###################################################
emsex<-glm(larvalEmergence~Sex*matingpair, data=prattvilleparasitism %>% filter(Sex!="nymph"), family=binomial(link="cloglog"))
Anova(emsex)
emmeansex<-emmeans(emsex,~Sex*matingpair)
emmeansex
plot(emmeansex)

ggplot(prattvilleparasitism%>% filter(Sex!="nymph"), aes(x=larvalEmergence, fill=Sex))+
  geom_histogram(stat="count", position="dodge")

ggplot(prattvilleparasitism%>% filter(Sex!="nymph" & larvalEmergence=="1"), aes(x=Sex, fill=matingpair))+
  geom_histogram(stat="count", position="dodge") #shows count of male/fem and matingpair y/n for larval emergence=yes
###################################### emergence by egg # and eggs ######################################
emegg<-glm(larvalEmergence~eggNumber + eggs, data=prattvilleparasitism %>% filter(Sex!="nymph"), family=binomial(link="cloglog"))
Anova(emegg)
emmeanegg<-emmeans(emegg,~eggNumber)
emmeanegg
plot(emmeanegg)

ggplot(prattvilleparasitism%>% filter(Sex!="nymph" & eggs=="y"), aes(x=larvalEmergence, y=eggNumber, color=larvalEmergence))+
  geom_point(position="jitter")
################################## crop and row information #########################################
crop<-glm(larvalEmergence~Crop, data=prattvilleparasitism, family=binomial(link="cloglog"))
Anova(crop)
emmeancrop<-emmeans(crop,~Crop)
emmeancrop
plot(emmeancrop)





