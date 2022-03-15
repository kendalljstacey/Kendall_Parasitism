#kendall stacey, kstacey@ufl.edu
#graduate assistant, UF entomology department
#this script contains my code for creating figures

################################## intro ###############################################


########################### sex data #################################################
#### eggs laid ~ sex ####
ggplot(prattvilleparasitism %>% filter(Sex!="nymph" & eggs=="y" & eggNumber<20), aes(x=Sex, color=Sex, y=eggNumber))+
  geom_point(position = "jitter")+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Proportion of Eggs Laid on Host by Host Sex and Mating Pair")+
  xlab('Sex of Host')+
  ylab('Egg Number Laid on Host')+
  scale_x_discrete(labels=c("Male", "Female"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75))+
  facet_wrap(~matingpair, labeller="label_both")+
  geom_smooth(method="lm")+
  theme_bw()
ggsave(filename = file.path("Outputs","mpegglaying.png"))
#how many eggs are laid on each sex of host? does mating pair affect it? 
#of all the bugs with eggs, males had higher numbers of eggs laid on them


ggplot() + 
  geom_pointrange(data=eggnum_sex_em, aes(y=emmean, x=Sex, ymin=lower.CL, ymax=upper.CL, color=matingpair), size=2, position="jitter")+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Eggs Laid on Hosts",
       subtitle = "by Sex and Mating Pair")+
  xlab('Sex of Host')+
  ylab('Mean Egg Number Laid on Host')+
  scale_x_discrete(labels=c("Male", "Female"))+
  scale_color_manual(name="Mating Pair",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",size = 0.75), 
        legend.position = c(.8,.8),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","mpegglayingemmeans.png"))

#mean egg number laid on each sex and mating pair combination
#males not in a mating pair had much higher average egg # than any other combo

ggtexttable(num_parasitized, rows = NULL, theme = ttheme("classic"),
            cols = c("Sex","Parasitoid Eggs","Count")) #table showing this
ggsave(filename = file.path("Outputs","table_sex_parasitism.png"))

#### larval emergence ~ sex ####

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
ggsave(filename = file.path("Outputs","emergencebar.png"))

#count of males and females that produced parasitoids
#males and femlaes had similar levels of parasitiod production


########################### biology info#############################################
#### pupation length ####
clean_pupationlength$daysPupationToAdult<-as.factor(clean_pupationlength$daysPupationToAdult)
ggplot(clean_pupationlength, aes(x=daysPupationToAdult, fill=daysPupationToAdult))+
  geom_histogram(stat="count", position="dodge", fill="aquamarine3")+
  theme(text=element_text(size=15))+
  #scale_fill_brewer(palette = "Set2")+
  labs(x="Pupation Length (Days)",
       y="Frequency",
       title = "Pupation Length Frequency")+
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_blank())
ggsave(filename = file.path("Outputs","pupationhisto.png"))

  #histogram of pupation length

#### days to death ~ parasitism ####

ggplot(clean_death, aes(x=larvalEmergence, y=daystoDeath, fill=larvalEmergence))+
  geom_boxplot()+
  theme(text=element_text(size=15),
        legend.position = "none")+
  scale_fill_brewer(palette = "Set2")+
  scale_x_discrete(labels=c("Produced a Parasitoid", "Did not Produce a Parasitoid"))+
  labs(x=" ", 
        y="Days until Death", 
       title = "Days to death and Parasitoid Emergence")
ggsave(filename = file.path("Outputs","survivalboxplot.png"))

#how is liefspan affected by parasitoid emergence? 
#the group that produced a parasitoid ahd much lower lifespans


ggplot(clean_death %>% filter(eggNumber<20 & eggNumber>0 & daystoDeath<100), 
           aes(y=eggNumber, x=daystoDeath, color=larvalEmergence))+
  geom_point(size=3, alpha=.7, position = "jitter")+
  theme(text=element_text(size=15))+
  geom_smooth(method="lm", se=FALSE)+
  labs(y="Number of Eggs on Host",
       x="Days until Death", 
       title = "Days until Death in Parasitized Bugs",
       subtitle = "by Number of Eggs laid on Host")+
  scale_color_manual(name="Produced a Parasitoid", labels=c("Yes","No"), 
                     values = c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.5,.8),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.background = element_blank(),
        strip.text = element_blank())+
  scale_y_continuous(breaks=seq(0, 12, 2))+
  coord_cartesian(ylim=c(0,12))+
  facet_wrap(~larvalEmergence)
graph
ggsave(filename = file.path("Outputs","survivalemdotplotfacet.png"))
#path<-"/Users/kstacey/Desktop/Kendall_Parasitism/Figures/20210805_131454667_iOS.JPEG"
#img<-readJPEG(path, native = TRUE)
#img_graph<-graph +
 # inset_element(p=img, 
  #              left = .05,
   #             bottom=.65,
    #            right=.95,
     #           top=.95)
#img_graph

ggplot(clean_death %>% filter(eggNumber<20 & eggNumber>0 & daystoDeath<100), 
       aes(y=eggNumber, x=daystoDeath, color=larvalEmergence))+
  geom_point(size=3, alpha=.7, position = "jitter")+
  theme(text=element_text(size=15))+
  geom_smooth(method="lm", se=FALSE)+
  labs(y="Number of Eggs on Host",
       x="Days until Death", 
       title = "Days until Death in Parasitized Bugs",
       subtitle = "by Number of Eggs laid on Host")+
  scale_color_manual(name="Produced a Parasitoid", labels=c("Yes","No"), 
                     values = c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.8,.8),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        strip.background = element_blank(),
        strip.text = element_blank())+
  scale_y_continuous(breaks=seq(0, 12, 2))+
  coord_cartesian(ylim=c(0,12))
ggsave(filename = file.path("Outputs","survivalemdotplot.png"))


ggplot(clean_death_avg, aes(y=eggNumber, x=mean_daysdeath, color=larvalEmergence))+
  geom_point(position="jitter")
#how is lifespan affected by the number of eggs laid on each host? 
#filtered out any bugs with no eggs and any with more than 20
#included whether or not host prdouced a parasitoid
#as egg number increases, lifespan decreases 
#the group that did not produce a parasitoid had much more drastic decreases when 
#egg number was higher


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
        legend.position = c(.25,.8),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","avgdaystodeath.png"))
#using dataset that calculated mean days to death for bugs that had eggs 
#and production of a parasitoid



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
#density plot showing days to death of all bugs by insect number


fix_pratt<- prattvilleparasitism %>% 
  mutate(larvalEmergence=case_when(
    larvalEmergence== "0"~ "No parasitoid emerged",
    larvalEmergence== "1"~ "Parasitoid emerged")) %>% 
  mutate(eggs=case_when(
    eggs== "y" ~ "Had eggs",
    eggs== "n" ~ "Had no eggs")) %>% 
  filter(!is.na(larvalEmergence))


ggplot(fix_pratt,
       aes(x=daystoDeath, fill=larvalEmergence))+
  geom_histogram()+
  facet_wrap(larvalEmergence~eggs)+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Days until Death",
       y="Frequency",
       title = "Days to Death for Parasitoid Emergence and Eggs")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = "none",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        strip.text = element_text(size=14),
        plot.title = element_text(size = 18))
ggsave(filename = file.path("Outputs","survivalembarfacet.png"))



ggplot(prattvilleparasitism %>% filter( eggs=="y" & !is.na(larvalEmergence) & !is.na(eggs)),
       aes(x=daystoDeath, fill=larvalEmergence))+
  geom_density(alpha=0.7, position = 'identity')+
  geom_vline(xintercept = 11.1)+
  geom_vline(xintercept = 20.3)+
  xlim(0,60)+
  scale_fill_brewer(palette = "Set2")+
  geom_text(aes(11.1, label="Avg days Emergence", y=.02), angle=0, size=4)+
  geom_text(aes(20.3, label="Avg days no Emergence", y=.01), angle=0, size=4)
ggsave(filename = file.path("Outputs","survivalfreqency.png"))


ggplot(evidenceparadeath, aes(x=evidence_parasitism, y=daystoDeath, fill=evidence_parasitism))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Average Survival")+
  ylab('Days to Death')+
  xlab("Parasitized")+ 
  theme(legend.position = "none")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        plot.title = element_text(size = 18))
ggsave(filename = file.path("Outputs","survival_evidencepara.png"))

####fecundity ~ laravalemergence ####


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
        legend.position = c(.3,.8),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","fecundityboxplot.png"))

#this plot shows egg masses (clutches) laid by female hosts and wehter or not
#they produced a parasitoid or bore parasitoid eggs 


ggtexttable(fecundity_avg, rows = NULL, theme = ttheme("classic"),
cols = c("Larval Emergence","Parasitoid Eggs","Mean Egg Masses"))
ggsave(filename = file.path("Outputs","table_fecundity.png"))

fec_em<-as.data.frame(fec_em)
ggplot() + 
  geom_pointrange(data=fec_em, aes(y=emmean, x=larvalEmergence, ymin=lower.CL, ymax=upper.CL, color=eggs), size=2, position="jitter")+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Mean Fecundity",
       subtitle = "by Parasitoid Eggs and Parasitoid Emergence")+
  xlab(' ')+
  ylab('Mean Fecundity')+
  scale_x_discrete(labels=c("Produced a Parasitoid", "Did not Produce a Parasitoid"))+
  scale_fill_manual(name="Eggs on Host",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",size = 0.75), 
        legend.position = c(.9,.8),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","meanfecundityem.png"))

#plot of emmeans calculations on fecundity and parasitoid emergence/ parasitoid eggs 

ggplot(egg_fert_simp %>% filter(totalfertileeggs>0), aes(x=clutches, y=totalfertileeggs, color=eggs))+
  geom_point(position="jitter", size=2)+
  geom_smooth(method = "lm")+
  theme((text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Mean Fecundity",
       subtitle = "by Clutch Number and Parasitism")+
  xlab('Number of Egg masses Laid')+
  ylab('Mean Fecundity')+
  scale_color_manual(name="Eggs on Host",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",size = 0.75), 
        legend.position = c(.85,.85),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","meanfecundityclutch.png"))

ggplot(evidenceparafec, aes(x=evidence_parasitism, y=clutches, fill=evidence_parasitism))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Average Lifetime Fecundity")+
  ylab('Egg Masses')+
  xlab("Parasitized")+ 
  theme(legend.position = "none")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        plot.title = element_text(size = 18))
ggsave(filename = file.path("Outputs","fecundity_evidencepara.png"))

 #### eggs laid on host ~ successful emergence##### 
ggplot(prattvilleparasitism%>% filter(!is.na(larvalEmergence) & eggNumber!=0 & Sex!="nymph" & eggNumber<20), 
       aes(x=daystoDeath, color=larvalEmergence, y=eggNumber))+
geom_point()+
  facet_grid(Sex~matingpair)

ggplot(prattvilleparasitism %>% filter(!is.na(larvalEmergence) & eggNumber!=0), 
       aes(x=larvalEmergence, color=larvalEmergence, y=eggNumber))+
  geom_point(position = "jitter", size=2)+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set2")+# at what did egg number affect parasitism? 
  ylim(0,10)

ggplot(prattvilleparasitism %>% filter(!is.na(larvalEmergence) & eggNumber!=0), aes(x=eggNumber, fill=larvalEmergence))+
  geom_histogram(position = "dodge")+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set2")+
  xlim(0,15)

ggplot(prattvilleparasitism %>% filter(!is.na(eggNumber) & !is.na(larvalEmergence) & eggNumber!=0), aes(x=larvalEmergence, fill=larvalEmergence, y=eggNumber))+
  geom_violin(alpha=.8)+
  geom_point(position="jitter")+
  theme(text=element_text(size=15),
        legend.position = "none")+
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_blank())+
  scale_fill_brewer(palette = "Set2")+
  scale_y_continuous(breaks = seq(1, 10, by=1), limits=c(1,10))+
  xlab(" ")+
  ylab("Egg Number")+
  labs(title = "Successful Parasitism by Egg Number")+
  scale_x_discrete(labels=c("Produced a Parasitoid", "Did not"))
ggsave(filename = file.path("Outputs","meaneggnumparasitism.png"))

ggplot(em_eggs, aes(y=emmean, x=larvalEmergence, ymin=lower.CL, ymax=upper.CL))+
  geom_pointrange(size=2, color="coral")+
  scale_y_continuous(breaks = c(1,2,3,4,5), labels = c(1,2,3,4,5))+
  theme(text=element_text(size=15))+
  labs(title = "Mean Egg Number laid on Hosts that Produced a Parasitoid or did not")+
  xlab('Larval Emergence')+
  ylab('Egg Number')
  
################################# egg placement ##########################################
#set it up

ggplot(simple_eggplace, aes(x=body_p, y=egg_num))+
  geom_col(position = "dodge", fill="aquamarine3")+
  theme(text=element_text(size=15))+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))+
  labs(title = "Frequency of Eggs Laid on Each Body Part",
       subtitle = "by Parasitoid Emergence")+
  xlab('Body Part')+
  ylab('Egg Number')+
  scale_x_discrete(labels=c("Abdomen","Thorax", "Head", "Leg", "Pronotum", "Scutellum","Wing"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75))+
  facet_wrap(~larvalEmergence)

#frequency of eggs laid on each body part
#more eggs laid on ventral abdomen and thorax


ggplot(simple_eggplace %>% filter(egg_num<8, egg_num!=0), aes(y=egg_num, x=Replicate, color=larvalEmergence))+
  geom_point(position = "jitter")

ggplot(simple_eggplace, aes(x=body_p, y=egg_num, color=larvalEmergence))+
  geom_point(size=2, position="jitter")

ggplot(simple_eggplace %>% filter(egg_num<10 & egg_num>0), aes(x=body_p, y=egg_num, fill=larvalEmergence, color=larvalEmergence))+
  geom_col(position = "dodge")

ggplot() + 
  geom_pointrange(data=em_place, aes(x=body_part, y=prob, ymin=asymp.LCL, ymax=asymp.UCL), size=2, color="coral")+
  theme(text=element_text(size=15))+
  theme (axis.text.x = element_text (angle = 45, vjust = 1, hjust=1))+
  labs(title = "Likelihood of Successful Parasitism",
       subtitle = "by Egg Location")+
  xlab('Body Part')+
  ylab('Likelihood of Producing Parasitoid')+
  scale_x_discrete(labels=c("Dorsal Abdomen", "Dorsal Thorax", "Head", "Leg", 
                            "Pronotum", "Scutellum", "Ventral Abdomen", "Dorsal Abdomen", "Wing"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75))
ggsave(filename = file.path("Outputs","eggsbodypartem.png"))

#body part
ggplot(top_bot, aes(x=body_p, y=egg_num, color=larvalEmergence))+
  geom_point(position="jitter", size=2)+
  scale_color_manual(name="Produced a Parasitoid",labels=c("Yes", "No"), values=c("aquamarine3","coral"))
  



####################### parasitism rates ###################################################
ggplot(perc_eggem, aes(x=larvalEmergence, y=count_f, fill=eggs))+
  geom_col()+
  theme(text=element_text(size=15))+
  labs(title = "Count of bugs that produced parasitoid",
       subtitle = "with or without parasitoid eggs")+
  ylab('Number of Stink Bugs')+
  xlab("")+
  scale_fill_manual(name="Bearing Parasitoid Eggs",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  scale_x_discrete(labels=c("Produced a Parasitoid","Did not"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.25,.85), 
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","proportionparaeggstacked.png"))

ggplot(perc_eggem)+
  geom_col(aes(x=larvalEmergence, y=count_f, fill=eggs), position = "dodge")+
  theme(text=element_text(size=15))+
  labs(title = "Proportion of Stink Bugs that Produced a Parasitoid")+
  ylab('Number of Stink Bugs')+
  xlab("")+
  scale_fill_manual(name="Bearing Parasitoid Eggs",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  scale_x_discrete(labels=c("Produced a Parasitoid","Did not"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        legend.position = c(.5,.85), 
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","proportionparaegg.png"))

ggplot(perc_eggem)+
  geom_col(aes(x=larvalEmergence, y=count_f, fill=larvalEmergence), position = "dodge")+
  theme(text=element_text(size=15))+
  labs(title = "Proportion of Stink Bugs that Produced a Parasitoid")+
  ylab('Number of Stink Bugs')+
  xlab("")+
  scale_fill_manual(name="Bearing Parasitoid Eggs",labels=c("Yes", "No"), values=c("aquamarine3","coral"))+
  scale_x_discrete(labels=c("Produced a Parasitoid","Did not"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","proportionpara.png"))



ggplot(evidencepara, aes(x="", y=count_f, fill=evidence_parasitism))+
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  scale_fill_brewer(palette="Set2")+
  theme_void()
  
ggplot(evidencepara, aes(x=evidence_parasitism, y=count_f, fill=evidence_parasitism))+
  geom_col()+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Proportion of Parasitized Bugs")+
  ylab('Number of Stink Bugs')+
  xlab("Parasitized")+ 
  theme(legend.position = "none")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        plot.title = element_text(size = 18))
######################## date parasitized #########################################


ggplot()+
  geom_pointrange(data=em_date, size=2, color="coral", aes(x=Date, y=emmean, ymin=asymp.LCL, ymax=asymp.UCL, color=Date))+
  theme(text=element_text(size=15))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        legend.position = "none")
  
ggplot(prattvilleparasitism %>% filter(Date!="6/29/2021" & eggNumber<10), aes(x=Date, y=eggNumber))+
  geom_boxplot()


ggplot(bugs_date, aes(x=Date, y=count, fill=eggs))+
  geom_col()+
  annotate("text", x=1, y=57, label="32%", color="white", size=6)+
  annotate("text", x=2, y=47, label="55%", color="white", size=6)+
  annotate("text", x=3, y=118, label="40%", color="white", size=6)+
  annotate("text", x=4, y=36, label="62%", color="white", size=6)+
  annotate("text", x=5, y=36, label="80%", color="white", size=6)+
  annotate("text", x=6, y=22, label="71%", color="white", size=6)+
  theme(text=element_text(size=15))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size=16),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = c(.7,.8),
        legend.box.background = element_rect(colour = "black"))+
  theme (axis.text.x = element_text (angle = 30, vjust = 1, hjust=1))+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Proportion of Parasitized Bugs By Date")+
  ylab('Number of Stink Bugs')+
  xlab("Date")+
  scale_fill_manual(name="Bearing Parasitoid Eggs",labels=c("Yes", "No"), values=c("aquamarine3","coral"))
ggsave(filename = file.path("Outputs","date_prop_para.png"))  
  