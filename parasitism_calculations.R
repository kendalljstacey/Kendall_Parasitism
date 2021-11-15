library(imputeTS)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(forcats)
library(car)

prattvilleparasitism<-read_csv("Raw_data/prattvilleparasitism.csv")

########################### sex data #################################################
#### eggs laid ~ sex ####


eggnum_sex<-lm(eggNumber~ Sex*matingpair, prattvilleparasitism %>% filter(Sex!="nymph" & eggs=="y"))
Anova(eggnum_sex)
#how is egg number affected by sex of host and whether or not they were in mating pair? 
#Sex and mating pair are good predictors of egg number, but there is no interaction b/w the two
#visualization line 5 and 23

eggnum_sex_em<-emmeans(eggnum_sex,~ Sex*matingpair)
pairs(eggnum_sex_em)
#biggest contrasts between males and females not in a mating pair, males not in a mating pair
#and females in a mating pair, and males in a mating pair and males not in a mating pair
#visualization line 5 and 23
eggnum_sex_em<-as.data.frame(eggnum_sex_em)
#made this dataframe so I can plot it

mean_eggnum_sex<- prattvilleparasitism %>% 
  filter(eggs=="y" & Sex!="nymph") %>% 
  filter(!is.na(eggNumber)) %>% 
  group_by(Sex) %>% 
  summarise(mean_eggnum=mean(eggNumber))
head(mean_eggnum_sex) #avg eggs laid on males is 3.69, avg eggs laid on females is 1.94, 
#males-nomatingpair may be throwing this off

num_parasitized<-prattvilleparasitism %>% 
  filter(Sex!="nymph") %>% 
  group_by(Sex, eggs) %>% 
  summarise(count=n())
num_parasitized
ggtexttable(num_parasitized, rows = NULL, theme = ttheme("classic")) #table showing this
#shows the difference in parasitism for males and females, gives counts of males and females
#that had eggs, and males and females that did not have eggs laid on them 

#finding percentage of each sex that is parasitized
121/(121+80) # percentage of males that had eggs was 60.2% 
105/(105+128) # percentage of females taht had eggs was 45.1%



#### larval emergence ~ sex ####

emergence_noNA<- prattvilleparasitism %>% 
  filter(!is.na(larvalEmergence)) 

emsex<-glm(larvalEmergence~Sex, data=emergence_noNA %>% filter(Sex!="nymph"), family=binomial(link="cloglog"))
Anova(emsex)
#while there are more males with parasitoid eggs, there's no difference inthe number of 
#parasitoids that emerge from each sex, WEIRD!
#visulaization line 44

emsex_em<-emmeans(emsex,~Sex)
plot(emsex_em)
# no sig difference in sex and larval emergence 


female<- emergence_noNA %>% 
  filter(Sex=="f") %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_f=n())
head(female)
#shows the count of female hosts taht produced a pupa and the count that did not
proportion_fem<- 73/(73+154)
proportion_fem #proportion of females taht produced larvae is 32.2% 


male<- emergence_noNA %>% 
  filter(Sex=="m") %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_m=n())
head(male)
#shows the count of male hosts that produced a pupa and the cuotn that did not
proportion_mal<- 78/(78+119)
proportion_mal #proportion of males that produced larvae is 39.6% 



########################### biology info#############################################
#### pupation length ####


clean_pupationlength<-prattvilleparasitism %>% 
  filter(!is.na(daysPupationToAdult) & daysPupationToAdult!=0)
#on average, how long do the parasitoids pupate? 
  mean(clean_pupationlength$daysPupationToAdult) #mean pupation length is 12.795
#visualization line 61
  
  
  
#### days to death ~ parasitism ####
  
clean_death<-prattvilleparasitism %>% 
  filter(!is.na(daystoDeath) & !is.na(larvalEmergence)) #cleaning NA's out of death and emergence


death<-lm(daystoDeath~larvalEmergence*eggs, clean_death)
Anova(death)
#how is lifespan (daystodeath) of the host affected by larval emergence and the presence of parasitoid eggs?
#both are great predictors of lifespan


death_avg<- clean_death %>% 
  group_by(larvalEmergence) %>% 
  summarise(days_dead=mean(daystoDeath))
death_avg 
# how does the average lifespan differ for bugs that produced a parastioid and ones that didn't
#avg lifespan for larval emergence is 11.3, 
#avg lifespan for bugs with no larval emergence is 39.8


avg_death_para<-clean_death %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(days_dead=mean(daystoDeath))
ggtexttable(avg_death_para, rows = NULL, theme = ttheme("classic"),
            cols = c("Eggs","Parasitoid Emergence","Days to Death"))
#calculated mean days to death for bugs that had eggs and production of a parasitoid


para_death<-prattvilleparasitism %>% filter(!is.na(prattvilleparasitism$daysPupaFoundtoDeath)) %>% 
  select(Insectnumber, daysPupaFoundtoDeath)
summary(para_death$daysPupaFoundtoDeath)
#subsets dataset by insect ID and lifespan (days to death) and removes NA's


####fecundity ~ laravalemergence ####
fecundity<- prattvilleparasitism %>% 
  filter(Sex=="f" & !is.na(larvalEmergence) & !is.na(eggs)) %>% 
  mutate(larvalEmergence=case_when(
    larvalEmergence== "0"~ "No parasitoid",
    larvalEmergence== "1"~ "Parasitoid")) %>% 
  mutate(eggs=case_when(
    eggs== "y" ~ "Had eggs",
    eggs== "n" ~ "Had no eggs")) %>% 
  mutate(clutches=coalesce(clutches, 0)) %>% 
  select(larvalEmergence, clutches, eggs)
fecundity
#subsets dataset by selecting females, fecudnity of females (clutches), and parasitoid emergence


fecundity_avg <- fecundity %>% 
  group_by(larvalEmergence, eggs) %>% 
  summarise(mean=mean(clutches)) 
  
fecundity_avg 
#table of mean clutch number for female hosts that produced a parasitoid or had parasitoid eggs 


fec_lm<-lm(clutches~ larvalEmergence+eggs, fecundity)
Anova(fec_lm) #is statistically signficant 
#how are number of clutches laid by female hosts affected by the emergence of a parasitoid,
#or the presence of parasitoid eggs? 
#both are good predictors of numbers of clutches laid
fec_em<-emmeans(fec_lm,~larvalEmergence+eggs)
pairs(fec_em)
plot(fec_em)

ggtexttable(fecundity_avg, rows = NULL, theme = ttheme("classic"))



#### eggs laid on host ~ successful emergence##### 

eggnum_glm<-glm(larvalEmergence~eggNumber, prattvilleparasitism %>% filter(!is.na(larvalEmergence) & eggNumber!=0), family=binomial(link="cloglog"))
Anova(eggnum_glm)
#is parasitoid emergence influenced by egg number laid on host? 
#no! 


mean_eggnum_em<-prattvilleparasitism %>% 
  filter(!is.na(eggNumber) & !is.na(larvalEmergence) & eggs=="y") %>% 
  group_by(larvalEmergence) %>% 
  summarise(mean_eggnum=mean(eggNumber))
mean_eggnum_em 
#mean egg number laid on bugs that had emergence was 2.56, mean for other is 3.26


################################# egg placement ##########################################
#set it up
egg_place<-prattvilleparasitism %>% 
  pivot_longer(cols=scutellum:wing,
               names_to="body_part",
               values_to="egg_num") %>% 
  select(body_part, egg_num, larvalEmergence) %>% 
  filter(!is.na(egg_num) & !is.na(larvalEmergence))
#pivot longer code to analyze eggs laid on body area

eggplace<-glm(larvalEmergence~body_part, egg_place, family=binomial(link="cloglog"))
Anova(eggplace)
#how is larval emergence influenced by which body part the eggs is laid on? 
# apparently it doesn't... 

em_place<-emmeans(eggplace,~body_part)
em_place
plot(em_place)
head(em_place)
em_place<-as.data.frame(em_place)
head(em_place)

####################### parasitism rates ###################################################
perc_em<-prattvilleparasitism %>% 
  filter(!is.na(larvalEmergence)) %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_f=n())
#creates dataset of count of bugs that produced a parasitoid and those that didn't
perc_em
153/(153+275)
#percent of all bugs that produced a parasitoid = 36%


perc_egg<-prattvilleparasitism %>% 
  filter(!is.na(eggs) & !is.na(larvalEmergence)) %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(count_f=n())
perc_egg
#takes count of bugs with eggs or not and bugs taht produced a parasitoid or not

(134+92+19)/(134+92+19+183) #proprotion of bugs that showed evidence of parasitism= 57%


perc_eggem<-prattvilleparasitism %>% 
  filter(!is.na(eggs) & !is.na(larvalEmergence)) %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(count_f=n())
perc_eggem<-as.data.frame(perc_eggem)
