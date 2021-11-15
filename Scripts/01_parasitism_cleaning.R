library(imputeTS)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(forcats)
library(car)
library(plyr)

prattvilleparasitism<-read_csv("Raw_data/prattvilleparasitism.csv")

################################# boring stuff #############################################
prattvilleparasitism$larvalEmergence<-factor(prattvilleparasitism$larvalEmergence, levels=c("1","0"))
prattvilleparasitism$eggs<-factor(prattvilleparasitism$eggs, levels=c("y","n"))
prattvilleparasitism$Sex<-factor(prattvilleparasitism$Sex, levels=c("m","f","nymph"))
prattvilleparasitism$matingpair<-factor(prattvilleparasitism$matingpair, levels=c("y","n"))
prattvilleparasitism$Crop<-factor(prattvilleparasitism$Crop, levels=c("Corn BF","Corn 1","Corn 2","Corn 3","Corn 4","Corn 5"))
prattvilleparasitism$scutellum<-as.double(prattvilleparasitism$scutellum)
########################### sex data #################################################
#### eggs laid ~ sex ####

eggnum_sex_em<-as.data.frame(eggnum_sex_em)
head(eggnum_sex_em)

#### larval emergence ~ sex ####
emergence_noNA<- prattvilleparasitism %>% 
  filter(!is.na(larvalEmergence)) 


########################### biology info#############################################
#### pupation length ####
clean_pupationlength<-prattvilleparasitism %>% 
  filter(!is.na(daysPupationToAdult) & daysPupationToAdult!=0)

#### days to death ~ parasitism ####
clean_death<-prattvilleparasitism %>% 
  filter(!is.na(daystoDeath) & !is.na(larvalEmergence))


para_death<-prattvilleparasitism %>% filter(!is.na(prattvilleparasitism$daysPupaFoundtoDeath)) %>% 
  select(Insectnumber, daysPupaFoundtoDeath)
summary(para_death$daysPupaFoundtoDeath)


####fecundity ~ laravalemergence ####
fecundity<- prattvilleparasitism %>% 
  filter(Sex=="f" & !is.na(larvalEmergence)) %>% 
  mutate(clutches=coalesce(clutches, 0)) %>% 
  select(larvalEmergence, clutches, eggs)
fecundity


################################# egg placement ##########################################


egg_place<-prattvilleparasitism %>% 
  pivot_longer(cols=scutellum:wing,
               names_to="body_part",
               values_to="egg_num") %>% 
  select(body_part, egg_num, larvalEmergence) %>% 
  filter(!is.na(egg_num) & !is.na(larvalEmergence))

####################### parasitism rates ###################################################

perc_eggem<-prattvilleparasitism %>% 
  filter(!is.na(eggs) & !is.na(larvalEmergence)) %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(count_f=n())
perc_eggem<-as.data.frame(perc_eggem)

