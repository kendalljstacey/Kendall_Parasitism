library(imputeTS)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(forcats)
prattvilleparasitism<-read_csv("Raw_data/prattvilleparasitism.csv")
################################# boring stuff #############################################
prattvilleparasitism$larvalEmergence<-factor(prattvilleparasitism$larvalEmergence, levels=c("1","0"))
prattvilleparasitism$eggs<-factor(prattvilleparasitism$eggs, levels=c("y","n"))
prattvilleparasitism$Sex<-factor(prattvilleparasitism$Sex, levels=c("m","f","nymph"))
prattvilleparasitism$matingpair<-factor(prattvilleparasitism$matingpair, levels=c("y","n"))
prattvilleparasitism$Crop<-factor(prattvilleparasitism$Crop, levels=c("Corn BF","Corn 1","Corn 2","Corn 3","Corn 4","Corn 5"))
########################### sex data #################################################
#### eggs laid ~ sex ####
eggs_sex<-ggplot(prattvilleparasitism %>% filter(Sex!="nymph" & eggs=="y" & eggNumber<20), aes(x=Sex, color=Sex, y=eggNumber))+
  geom_point(position = "jitter")+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Proportion of Eggs Laid on Host by Host Sex")+
  xlab('Sex of Host')+
  ylab('Egg Number Laid on Host')+
  scale_x_discrete(labels=c("Male", "Female"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75))+
  facet_wrap(~matingpair)
eggs_sex
ggsave("eggs_sex.tiff", eggs_sex)
#of all the bugs with eggs, males had higher numbers of eggs laid on them

eggnum_sex<-lm(eggNumber~ Sex*matingpair, prattvilleparasitism %>% filter(Sex!="nymph" & eggs=="y"))
Anova(eggnum_sex) #significant
eggnum_sex_em<-emmeans(eggnum_sex,~ Sex*matingpair)
pairs(eggnum_sex_em)

eggnum_sex_em<-as.data.frame(eggnum_sex_em)
ggplot() + 
  geom_pointrange(data=eggnum_sex_em, aes(y=emmean, x=Sex, ymin=lower.CL, ymax=upper.CL, color=matingpair), size=2, position="jitter")+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Eggs Laid on Hosts",
       subtitle = "by Sex and Mating Pair")+
  xlab('Sex of Host')+
  ylab('Mean Egg Number Laid on Host')+
  scale_x_discrete(labels=c("Male", "Female"))+
  scale_fill_manual(name="Mating Pair",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",size = 0.75), 
        legend.position = c(.8,.8))


head(eggnum_sex_em)
mean_eggnum_sex<- prattvilleparasitism %>% 
  filter(eggs=="y" & Sex!="nymph") %>% 
  filter(!is.na(eggNumber)) %>% 
  group_by(Sex) %>% 
  summarise(mean_eggnum=mean(eggNumber))
head(mean_eggnum_sex) #avg eggs laid on males is 3.69, avg eggs laid on females is 1.94

num_parasitized<-prattvilleparasitism %>% 
  filter(Sex!="nymph") %>% 
  group_by(Sex, eggs) %>% 
  summarise(count=n())
num_parasitized

121/(121+80)
105/(105+128)
#### larval emergence ~ sex ####
emergence_noNA<- prattvilleparasitism %>% 
  filter(!is.na(larvalEmergence)) 
  ggplot(emergence_noNA%>% filter(Sex!="nymph"), aes(fill=Sex, x=larvalEmergence))+
  geom_histogram(stat="count", position="dodge") +
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Emergence of Parasitoid by Sex of Host")+
  xlab('Parasitoid Emergence')+
  ylab('Count')+
  scale_x_discrete(labels=c("Yes", "No"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.1,.8))
#this doesn't tell us much 

emsex<-glm(larvalEmergence~Sex, data=emergence_noNA %>% filter(Sex!="nymph"), family=binomial(link="cloglog"))
Anova(emsex)
emsex_em<-emmeans(emsex,~Sex)
plot(emsex_em)
# no difference in sex and larval emergence 

female<- emergence_noNA %>% 
  filter(Sex=="f") %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_f=n())
head(female)
proportion_fem<- 73/(73+154)
proportion_fem #proportion of females taht produced larvae is 32.2% 

male<- emergence_noNA %>% 
  filter(Sex=="m") %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_m=n())
head(male)
proportion_mal<- 78/(78+119)
proportion_mal #proportion of males that produced larvae is 39.6% 

########################### biology info#############################################
#### pupation length ####
clean_pupationlength<-prattvilleparasitism %>% 
  filter(!is.na(daysPupationToAdult) & daysPupationToAdult!=0)
  mean(clean_pupationlength$daysPupationToAdult) #mean pupation length is 12.795
ggplot(clean_pupationlength, aes(x=daysPupationToAdult, fill=daysPupationToAdult))+
    geom_histogram(stat="count", position="dodge")+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")#this doesn't tell us much 

#### days to death ~ parasitism ####
clean_death<-prattvilleparasitism %>% 
  filter(!is.na(daystoDeath) & !is.na(larvalEmergence))
ggplot(clean_death, aes(x=larvalEmergence, y=daystoDeath, fill=larvalEmergence))+
  geom_boxplot()+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")
ggplot(clean_death %>% filter(eggNumber<20 & eggNumber>0), aes(x=eggNumber, y=daystoDeath, color=larvalEmergence))+
  geom_point(size=2, position = "jitter")+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")

death<-lm(daystoDeath~larvalEmergence*eggs, clean_death)
Anova(death)

death_avg<- clean_death %>% 
  group_by(larvalEmergence) %>% 
  summarise(days_dead=mean(daystoDeath))
death_avg #avg lifespan for larval emergence is 11.3, avg lifespan for bugs with no larval emergence is 39.8

avg_death_para<-clean_death %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(days_dead=mean(daystoDeath))
ggtexttable(avg_death_para, rows = NULL, theme = ttheme("classic"))

library("ggpmisc")
ggplot(avg_death_para, aes(x=larvalEmergence, y=days_dead, fill=eggs))+
  geom_col(position = "dodge")+
  theme(text=element_text(size=15))+
  #scale_fill_brewer(palette = "Set2")+
  labs(title = "Average Days to Death",
       subtitle = "for Stink Bugs With or Without Evidence of Parasitism")+
  xlab(' ')+
  ylab('Days to Death')+
  scale_x_discrete(labels=c("Produced a Parasitoid", "Did not Produce a Parasitoid"))+
  scale_fill_manual(name="Bearing Parasitoid Eggs",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.25,.8))

para_death<-prattvilleparasitism %>% filter(!is.na(prattvilleparasitism$daysPupaFoundtoDeath)) %>% 
  select(Insectnumber, daysPupaFoundtoDeath)
summary(para_death$daysPupaFoundtoDeath)

ggplot(para_death, aes(x=daysPupaFoundtoDeath, stat="count"))+
  geom_histogram(fill="coral")+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Density Plot of Days to Death after Parasitoid Emergence")+
  xlab('Days to Death after Parasitoid Emergence')+
  ylab('Count')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75))

####fecundity ~ laravalemergence ####
fecundity<- prattvilleparasitism %>% 
  filter(Sex=="f" & !is.na(larvalEmergence)) %>% 
  mutate(clutches=coalesce(clutches, 0)) %>% 
  select(larvalEmergence, clutches, eggs)
fecundity

ggplot(fecundity, aes(x=larvalEmergence, y=clutches, fill=eggs))+
  geom_boxplot()+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Distribution of Egg Masses Laid",
       subtitle = "for Stink Bugs With or Without Evidence of Parasitism")+
  xlab('')+
  ylab('Number of Egg Masses Laid by Stink Bug')+
  scale_x_discrete(labels=c("Produced a Parasitoid", "Did not Produce a Parasitoid"))+
  scale_fill_manual(name="Bearing Parasitoid Eggs",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.2,.8))

fecundity_avg<- fecundity %>% 
  group_by(larvalEmergence, eggs) %>% 
  summarise(mean_clutch=mean(clutches))
fecundity_avg #parasitized bugs laid an avg of .260 clutches, unparasitized laid an average of 2.48

fec_lm<-lm(clutches~ larvalEmergence+eggs, fecundity)
Anova(fec_lm) #is statistically signficant 
fec_em<-emmeans(fec_lm,~larvalEmergence)
plot(fec_em)

ggtexttable(fecundity_avg, rows = NULL, theme = ttheme("classic"))
#### eggs laid on host ~ successful emergence##### 
ggplot(prattvilleparasitism %>% filter(!is.na(larvalEmergence) & eggNumber!=0), aes(x=larvalEmergence, color=larvalEmergence, y=eggNumber))+
  geom_point(position = "jitter")+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")# at what did egg number affect parasitism? 

ggplot(prattvilleparasitism %>% filter(!is.na(larvalEmergence) & eggNumber!=0), aes(x=eggNumber, fill=larvalEmergence))+
  geom_histogram(position = "dodge")+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")

ggplot(prattvilleparasitism %>% filter(!is.na(larvalEmergence) & eggNumber!=0), aes(x=larvalEmergence, fill=larvalEmergence, y=eggNumber))+
  geom_boxplot()+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")

eggnum_glm<-glm(larvalEmergence~eggNumber, prattvilleparasitism %>% filter(!is.na(larvalEmergence) & eggNumber!=0), family=binomial(link="cloglog"))
Anova(eggnum_glm)

mean_eggnum_em<-prattvilleparasitism %>% 
  filter(!is.na(eggNumber) & !is.na(larvalEmergence) & eggs=="y") %>% 
  group_by(larvalEmergence) %>% 
  summarise(mean_eggnum=mean(eggNumber))
mean_eggnum_em #mean egg number laid on bugs that had emergence was 2.56, mean for other is 3.26

################################# egg placement ##########################################
#set it up

egg_place<-prattvilleparasitism %>% 
  pivot_longer(cols=scutellum:wing,
               names_to="body_part",
               values_to="egg_num") %>% 
  select(body_part, egg_num, larvalEmergence) %>% 
  filter(!is.na(egg_num) & !is.na(larvalEmergence))

ggplot(egg_place, aes(x=body_part, y=egg_num))+
  geom_col(position = "dodge", fill="aquamarine3")+
  theme(text=element_text(size=15))+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))+
  labs(title = "Frequency of Eggs Laid on Each Body Part",
       subtitle = "by Parasitoid Emergence")+
  xlab('Body Part')+
  ylab('Count')+
  scale_x_discrete(labels=c("Dorsal Abd", "Dorsal Thor", "Head", "Leg", "Pronotum", "Scutellum", "Ventral Abd", "Ventral Thor", "Wing"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75))

ggplot(egg_place, aes(x=body_part, fill=larvalEmergence))+
  geom_histogram(stat="count", position = "dodge")+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))+
  labs(title = "Frequency of Eggs Laid on Each Body Part",
       subtitle = "by Parasitoid Emergence")+
  xlab('Body Part')+
  ylab('Count')+
  scale_x_discrete(labels=c("Dorsal Abd", "Dorsal Thor", "Head", "Leg", "Pronotum", "Scutellum", "Ventral Abd", "Ventral Thor", "Wing"))+
  scale_fill_manual(name="Parasitoid Emergence",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.2,.8))


eggplace<-glm(larvalEmergence~body_part, egg_place, family=binomial(link="cloglog"))
Anova(eggplace)
em_place<-emmeans(eggplace,~body_part)
em_place
plot(em_place)
head(em_place)
em_place<-as.data.frame(em_place)
head(em_place)

ggplot() + 
  geom_pointrange(data=em_place, aes(x=body_part, y=emmean, ymin=asymp.LCL, ymax=asymp.UCL), size=2, color="coral")+
  theme(text=element_text(size=15))+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))+
  labs(title = "Mean Eggs Laid on Each Body Part")+
  xlab('Body Part')+
  ylab('Mean Egg Number Laid on Body Parts')+
  scale_x_discrete(labels=c("Dorsal Abd", "Dorsal Thor", "Head", "Leg", "Pronotum", "Scutellum", "Ventral Abd", "Ventral Thor", "Wing"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75))
####################### parasitism rates ###################################################
perc_em<-prattvilleparasitism %>% 
  filter(!is.na(larvalEmergence)) %>% 
  group_by(larvalEmergence) %>% 
  summarise(count_f=n())
perc_em
153/(153+275)

perc_egg<-prattvilleparasitism %>% 
  filter(!is.na(eggs)) %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(count_f=n())
perc_egg
228/(228+210)
(134+92+19)/(134+92+19+183
             )
134+92+19+183
perc_eggem<-prattvilleparasitism %>% 
  filter(!is.na(eggs) & !is.na(larvalEmergence)) %>% 
  group_by(eggs, larvalEmergence) %>% 
  summarise(count_f=n())
perc_eggem<-as.data.frame(perc_eggem)
ggplot(perc_eggem)+
  geom_col(aes(x=larvalEmergence, y=count_f, fill=eggs), position = "dodge")+
  theme(text=element_text(size=15))+
  labs(title = "Proportion of Stink Bugs that Produced a Parasitoid", 
       subtitle = "With or Without Intact Parasitoid Eggs")+
  xlab(' ')+
  ylab('Number of Stink Bugs')+
  scale_fill_manual(name="Bearing Parasitoid Eggs",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  scale_x_discrete(labels=c("Produced a Parasitoid","Did not Produce a Parasitoid"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.5,.8))
