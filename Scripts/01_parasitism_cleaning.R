#kendall stacey, kstacey@ufl.edu
#graduate assistant, UF entomology department
#this script is mostly removing NA's from my dataset and subsetting the dataset
#for more specific analyses


################################## intro ###############################################
library(imputeTS)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(forcats)
library(car)
library(patchwork)
library(jpeg)


prattvilleparasitism<-read_csv("Raw_data/prattvilleparasitism.csv")

################################# boring stuff #############################################
prattvilleparasitism$larvalEmergence<-factor(prattvilleparasitism$larvalEmergence, levels=c("1","0"))
prattvilleparasitism$eggs<-factor(prattvilleparasitism$eggs, levels=c("y","n"))
prattvilleparasitism$Sex<-factor(prattvilleparasitism$Sex, levels=c("m","f","nymph"))
prattvilleparasitism$matingpair<-factor(prattvilleparasitism$matingpair, levels=c("y","n"))
prattvilleparasitism$Crop<-factor(prattvilleparasitism$Crop, levels=c("Corn BF","Corn 1","Corn 2","Corn 3","Corn 4","Corn 5"))
prattvilleparasitism$scutellum<-as.double(prattvilleparasitism$scutellum)

#### larval emergence ~ sex ####
emergence_noNA<- prattvilleparasitism %>% 
  filter(!is.na(larvalEmergence)) 
#filterd Na's out of the emergence column


########################### biology info#############################################
#### pupation length ####
clean_pupationlength<-prattvilleparasitism %>% 
  filter(!is.na(daysPupationToAdult) & daysPupationToAdult!=0)
#selected only the specimens that emerged as adults by filtering out Na's and 
#zeros

#### days to death ~ parasitism ####
clean_death<-prattvilleparasitism %>% 
  filter(!is.na(daystoDeath) & !is.na(larvalEmergence))


para_death<-prattvilleparasitism %>% 
  filter(!is.na(prattvilleparasitism$daysPupaFoundtoDeath)) %>% 
  select(Insectnumber, daysPupaFoundtoDeath)
#subsetted dataset to determine how many days after pupation the hosts died for each
#specimen


####fecundity ~ laravalemergence ####
fecundity<- prattvilleparasitism %>% 
  filter(Sex=="f" & !is.na(larvalEmergence)) %>% 
  mutate(clutches=coalesce(clutches, 0)) %>% 
  select(larvalEmergence, clutches, eggs)
fecundity
#determined how many clutches were laid by each host that produced a parasitoid

################################# egg placement ##########################################


egg_place<-prattvilleparasitism %>% 
  pivot_longer(cols=scutellum:wing,
               names_to="body_part",
               values_to="egg_num") %>% 
  select(body_part, egg_num, eggNumber, Insectnumber, larvalEmergence) %>% 
  filter(!is.na(egg_num) & !is.na(larvalEmergence)) %>% 
  rename(total_egg=eggNumber)
egg_place
#the original dataset is wide so I used this pivot longer code to make it longer
#I can still see which bugs had multiple eggs laid on them because of the insect
#number column

####################### parasitism rates ###################################################

perc_eggem<-prattvilleparasitism %>% 
  filter(!is.na(eggs) & !is.na(larvalEmergence)) %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(count_f=n())
perc_eggem<-as.data.frame(perc_eggem)
#determined how many bugs had parasitoid eggs and produced a pupa, how many did
#not have parasitoid eggs and prodecued a pupa, how many had parasitoid eggs and did
#not produce a pupa, and how many did not have eggs and did not produce a pupa
