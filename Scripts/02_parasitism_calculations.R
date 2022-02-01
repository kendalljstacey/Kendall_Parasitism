#kendall stacey, kstacey@ufl.edu
#graduate assistant, UF entomology department
#this script contains all of my linear models for different comparisons and emmeans


################################## intro ###############################################
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

eggnum_sex_em<-emmeans(eggnum_sex,~ Sex*matingpair)
pairs(eggnum_sex_em)
eggnum_sex_em<-as.data.frame(eggnum_sex_em)
head(eggnum_sex_em)
#biggest contrasts between males and females not in a mating pair, males not in a mating pair
#and females in a mating pair, and males in a mating pair and males not in a mating pair
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

mp_para<-prattvilleparasitism %>% 
  filter(!is.na(eggNumber) & eggNumber>0) %>% 
  group_by(matingpair, Sex) %>% 
  summarise(avg_egg=mean(eggNumber))
mp_para

#### larval emergence ~ sex ####

emergence_noNA<- prattvilleparasitism %>% 
  filter(!is.na(larvalEmergence)) 

emsex<-glm(larvalEmergence~Sex, data=emergence_noNA %>% filter(Sex!="nymph"), family=binomial(link="cloglog"))
Anova(emsex)
#while there are more males with parasitoid eggs, there's no difference inthe number of 
#parasitoids that emerge from each sex, WEIRD!

emsex_em<-emmeans(emsex,~Sex)
plot(emsex_em)
# no sig difference in sex and larval emergence 


female<- emergence_noNA %>% 
  filter(Sex=="f" & eggs=="y") %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_f=n())
head(female)
#shows the count of female hosts taht produced a pupa and the count that did not
proportion_fem<- 63/(63+41)
proportion_fem #proportion of females taht produced larvae is 32.2% 


male<- emergence_noNA %>% 
  filter(Sex=="m" & eggs=="y") %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_m=n())
head(male)
#shows the count of male hosts that produced a pupa and the cuotn that did not
proportion_mal<- 69/(69+51)
proportion_mal #proportion of males that produced larvae is 39.6% 


pupa_sex<-prattvilleparasitism %>% 
  filter(!is.na(successfullPupation) & Sex!="nymph") %>% 
  group_by(Sex, successfullPupation) %>% 
  summarise(count=n())
41/(41+36) #males
41/(41+31) #females 
########################### biology info#############################################
#### pupation length ####


clean_pupationlength<-prattvilleparasitism %>% 
  filter(!is.na(daysPupationToAdult) & daysPupationToAdult!=0)
#on average, how long do the parasitoids pupate? 
  mean(clean_pupationlength$daysPupationToAdult) #mean pupation length is 12.795
  standard_error <- function(x) sd(x) / sqrt(length(x)) 
  x<-clean_pupationlength$daysPupationToAdult
  standard_error(x)
  
  clean_pupationlength$daysPupationToAdult<-as.numeric(clean_pupationlength$daysPupationToAdult)
  puplength<-lm(daysPupationToAdult~Sex, data=clean_pupationlength)
  Anova(puplength)
#visualization line 61
  
  
  
#### days to death ~ parasitism ####
  
clean_death<-prattvilleparasitism %>% 
  filter(!is.na(daystoDeath) & !is.na(larvalEmergence)) #cleaning NA's out of death and emergence


death<-lm(daystoDeath~larvalEmergence*eggs, clean_death)
Anova(death)
#how is lifespan (daystodeath) of the host affected by larval emergence and the presence of parasitoid eggs?
#both are great predictors of lifespan


death_avg<- clean_death %>% 
  group_by(larvalEmergence, eggs) %>% 
  summarise(days_dead_mean=mean(daystoDeath))
death_avg 
ggtexttable(death_avg, rows = NULL, theme = ttheme("classic"))
# how does the average lifespan differ for bugs that produced a parastioid and ones that didn't
#avg lifespan for larval emergence is 11.3, 
#avg lifespan for bugs with no larval emergence is 39.8


avg_death_para<-clean_death %>% 
  group_by(larvalEmergence, eggs) %>% 
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
    larvalEmergence== "0"~ "No",
    larvalEmergence== "1"~ "Yes")) %>% 
  mutate(eggs=case_when(
    eggs== "y" ~ "Had eggs",
    eggs== "n" ~ "Had no eggs")) %>% 
  mutate(clutches=coalesce(clutches, 0)) %>% 
  select(larvalEmergence, clutches, eggs)
fecundity
#subsets dataset by selecting females, fecudnity of females (clutches), and parasitoid emergence


fecundity_avg <- fecundity %>% 
  group_by(larvalEmergence, eggs) %>% 
  summarise(mean_fecundity=mean(clutches)) 
  
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
  filter(!is.na(eggNumber) & !is.na(larvalEmergence) & eggNumber!=0) %>% 
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
  select(body_part, egg_num, larvalEmergence, Insectnumber, Replicate) %>% 
  filter(!is.na(egg_num) & !is.na(larvalEmergence))
str(egg_place)
egg_place$body_part<-as.factor(egg_place$body_part)
#pivot longer code to analyze eggs laid on body area
simple_eggplace<-egg_place %>% 
  mutate(body_p=fct_collapse(egg_place$body_part, abdomen=c("dorsalabdomen", "ventralabdomen"), 
                      thorax=c("dorsalthorax","ventralthorax"))) %>% 
  select(body_p, egg_num, larvalEmergence, Insectnumber, Replicate)

egg_means<-simple_eggplace %>% 
  group_by(body_p, larvalEmergence) %>% 
  summarise(mean_egg=mean(egg_num))

eggplace<-glm(larvalEmergence~body_p, family=binomial(link="cloglog"), data=simple_eggplace)
Anova(eggplace)
#how is larval emergence influenced by which body part the eggs is laid on? 
# apparently it doesn't... 

em_place<-emmeans(eggplace,~body_p, type="response")
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

evidencepara<-prattvilleparasitism %>% 
  filter(!is.na(eggs) & !is.na(larvalEmergence)) %>% 
  mutate(evidence_parasitism=fct_collapse(larvalEmergence, yes=c("1"), 
                                          no=c("0"))) %>% 
  group_by(eggs, larvalEmergence, evidence_parasitism) %>% 
  summarise(count_f=n()) 
evidencepara$evidence_parasitism[2]="yes" #yes is 245, no is 183


perc_eggem<-prattvilleparasitism %>% 
  filter(!is.na(eggs) & !is.na(larvalEmergence)) %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(count_f=n())
perc_eggem<-as.data.frame(perc_eggem)
mosaicplot(~larvalEmergence+eggs, data=prattvilleparasitism)

colony_init<- prattvilleparasitism %>% 
  filter(!is.na(daysPupationToAdult))
summary(colony_init)

colony_ideal<- prattvilleparasitism %>% 
  filter(larvalEmergence=="1")
summary(colony_ideal)

######################## number collected each date ################################
ndate<- prattvilleparasitism %>% 
  filter(Date!= "6/29/2021") %>% 
  group_by(Date) %>% 
  summarise('Number Collected' =n())
ndate

ggtexttable(ndate, rows = NULL, theme = ttheme("classic"))
