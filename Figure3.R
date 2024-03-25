library(tidyverse)
library(cli)
library(ggplot2)
library(survival)
library(ggpubr)
library(dplyr)
library(survminer)
library(gridExtra)


####LHOPP figure 3 panel 3. panel c

inftotal$clone<-recode_factor(inftotal$clone, "BD 0846"="BD08")
inftotal$clone<-recode_factor(inftotal$clone, "DW 2258" = "DW22")
inftotal$clone<-recode_factor(inftotal$clone, "DW 2975" = "DW29")
inftotal$clone<-recode_factor(inftotal$clone, Standard="S")



library(dplyr)
inftotal$paras<-recode_factor(inftotal$paras, C="Control")
#create "treatment" column for use in graphing
inftotal$Treatment<-inftotal$paras
#add h to treatment column for labels for reviewer
inftotal$Treatment<-recode_factor(inftotal$Treatment, "MicG48" = "MicG 48h")
inftotal$Treatment<-recode_factor(inftotal$Treatment, "MicG120"="MicG 120h")
inftotal$Treatment<-factor(inftotal$Treatment, levels=c('Control','MicG 48h','MicG 120h'), ordered=TRUE)
Panel3<-  inftotal %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", 
                                 "MicG 48h", "MicG 120h")) %>%
  ggplot(data=inftotal, mapping=aes(x = Treatment, y=x, fill=Treatment)) +theme(text = element_text(family = "Times New Roman"))+
  geom_boxplot(outlier.shape=NA)+theme_classic() + geom_boxplot(outlier.shape = NA)+
  labs(x = "Treatment", y = "Total Reproduction", ) + 
  ylim(c(0, 350))+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+ 
  scale_fill_manual(values = colorRampPalette(c("WHITE","BLACK","#A9A9A9"))(3)) +
  theme(legend.position="right")+ 
  theme(text = element_text(size = 15, colour="BLACK"))+theme(axis.text.y= element_text(color="BLACK"),  axis.text.x = element_blank() ) +
  #facet_grid(.~ clone)+
  facet_grid(.~factor(clone, levels=c('BD08', 'DW22', 'DW29', 'S')))+
  #facet_wrap(.~ clone, labeller = labeller(clone = label_wrap_gen(width = 4)))+
  theme(#legend.key.size = unit(1, 'cm'), #change legend key size
        #legend.key.height = unit(1, 'cm'), #change legend key height
        #legend.key.width = unit(1, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10))+ #change legend text font size
  geom_point()+ #adds points to plot
  geom_point(color='red')+ #adds point color
  labs(tag="C")
Panel3

#####LHOPP figure 3 panel 1
allclone$clone<-recode_factor(allclone$clone, "A4-3" = "A 43")
allclone$clone<-recode_factor(allclone$clone, "BD08-46"="BD 0846")
allclone$clone<-recode_factor(allclone$clone, "DW22-58" = "DW 2258")
allclone$clone<-recode_factor(allclone$clone, "DW29-75" = "DW 2975")
allclone$clone<-recode_factor(allclone$clone, MID37="MID 37")
allclone$clone<-recode_factor(allclone$clone, "ML32-84"="ML 3284")
allclone$clone<-recode_factor(allclone$clone, STD="Standard")

allclone$clone<-recode_factor(allclone$clone, "A 43"="A43")
allclone$clone<-recode_factor(allclone$clone, "BD 0846"="BD08")
allclone$clone<-recode_factor(allclone$clone, "DW 2258"="DW22")
allclone$clone<-recode_factor(allclone$clone, "DW 2975"="DW29")
allclone$clone<-recode_factor(allclone$clone, "MID 37"= "M37")
allclone$clone<-recode_factor(allclone$clone, "ML 3284"="ML32")
allclone$clone<-recode_factor(allclone$clone, "Standard"="S")



allclone$Treatment<-allclone$paratx

library(dplyr)
##removing animals that died prior to assessment of infection on day 14.
allcloneold <- allclone[allclone$end >13,]
###get summary for the prevalence
library(Rmisc)
allclonese <- summarySE(allcloneold, measurevar="INFo", groupvars=c("paratx","clone"))
##drop control from dataset
allclonese1<-allclonese[allclonese$paratx=='MicG48'|allclonese$paratx=='MicG120',]
##remove weird na rows
allclonese1<-allclonese1[!is.na(allclonese1$clone), ] 
allclonese1$Treatment<-allclonese1$paratx
allclonese1$Treatment<-recode_factor(allclonese1$Treatment, "MicG120"="MicG 120h")
allclonese1$Treatment<-recode_factor(allclonese1$Treatment, "MicG48"="MicG 48h")
library(dplyr)
allclonese1$clone<-recode_factor(allclonese1$clone, STD="Standard")

library(forcats)
pd <- position_dodge(0.1) # move them .05 to the left and right
Panel1<-allclonese1 %>%
  mutate(Treatment = fct_relevel(Treatment, 
                                 "MicG48", "MicG120")) %>%
  mutate(clone = fct_relevel(clone, 
                                 "BD08", "DW22","DW29", "S","M37", "A43","ML32" )) %>%
  ggplot(allclonese1, mapping=aes(x=Treatment, y=INFo, colour=Treatment)) + 
  #Added to add h to labels
  #scale_fill_manual(values=c("#999999", "BLACK"), 
   #                 name="Treatment",
   #                 breaks=c("MicG48", "MicG120"),
    #                labels=c("MicG 48h", "MicG 120h"))+
  geom_point(position=pd, size=7, shape=20) + # 20 is filled circle
  geom_errorbar(aes(ymin=INFo-se, ymax=INFo+se), width=0, position=pd) +
  #C/O to add h to labels
  scale_color_manual(values = colorRampPalette(c("BLACK","#A9A9A9"))(2))+
  geom_point(position=pd)+
  labs(x = "Treatment", y = "Prevalence", ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),strip.background=element_rect(fill="WHITE", color="BLACK"), axis.line = element_line(colour = "black"))+ #removes grey grid/background
  theme(text = element_text(size = 15, colour="BLACK"))+#(axis.text.x = element_blank())+ theme(axis.text.y= element_text(color="BLACK"))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
    #theme(legend.box=element.blank())+
  theme(legend.key = element_rect(fill = "WHITE", size = 0.25))+
  facet_grid(.~clone)+ #separates into facets by clone
  #facet_wrap(.~ clone, labeller = labeller(clone = label_wrap_gen(width = 4)))+
  theme(#legend.key.size = unit(1, 'cm'), #change legend key size
    #legend.key.height = unit(1, 'cm'), #change legend key height
    #legend.key.width = unit(1, 'cm'), #change legend key width
    legend.title = element_text(size=12), #change legend title font size
    legend.text = element_text(size=10))+ #change legend text font size
  labs(tag="A")#label for multipanel figure

Panel1 



####PANEL 4
STDlife$parastx<-factor(STDlife$parastx, levels=c('C','MicG48','MicG120'), ordered=TRUE)
Panel4<-ggsurvplot(fit=survfit(Surv(end, censor) ~  parastx, data = STDlife),
                        xlab = "Days", 
                        ylab = "Survival Probability",
                        conf.int = FALSE, 
                        risk.table = FALSE,
                        censor=FALSE,
                        linetype = c("dotted", 
                                     "solid", 
                                     "solid"),
                        size = 1, 
                        ggtheme = theme_classic2(base_size=15, base_family = "Arial"),
                        palette = c("BLACK","BLACK", "#A9A9A9"),
                        legend = c(.25,.25), 
                        legend.title = "Parasite Exposure",
                        font.tickslab = c( "BLACK"), #Makes axis black
                        legend.labs = c( "Control",
                                         "MicG 48h",
                                         "MicG 120h"))+
                        labs(tag="D")#label for multipanel figure
Panel4

####UPLOAD AND PREP PICTURE FOR PANEL 2
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")
##ok to click n on a/s/n option
library(EBImage)
clones3 = readImage("panel2.png")
display(clones3, method = "raster")
ggclones3<- ggplot() +
  background_image(clones3) +
  # This ensures that the image leaves some space at the edges
  theme(plot.margin = margin(t=0, l=0, r=0, b=0, unit = "cm"))+
  labs(tag="B")# adds the corner label for multipanel figure

STDlifespan<-lifespan[lifespan$clone=="STD",]
BDlifespan<-lifespan[lifespan$clone=="BD 0846",]
DW29lifespan<-lifespan[lifespan$clone=="DW 2975",]
DW22lifespan<-lifespan[lifespan$clone=="DW 2258",]
####Survival STD
STDlifespan$paratx<-factor(STDlifespan$paratx, levels=c('C','MicG48','MicG120'), ordered=TRUE)
S3life<-ggsurvplot(fit=survfit(Surv(lifespan, censor) ~  paratx, data = STDlifespan),
                  xlab = "Days", 
                  ylab = "Survival Probability",
                  conf.int = FALSE, 
                  risk.table = FALSE,
                  censor=FALSE,
                  linetype = c("dotted", 
                               "solid", 
                               "solid"),
                  size = 1, 
                  ggtheme = theme_classic2(base_size=15, base_family = "Arial"),
                  palette = c("BLACK","BLACK", "#A9A9A9"),
                  legend = c(0.25,0.25), 
                  legend.title = "Parasite Exposure",
                  font.tickslab = c( "BLACK"), #Makes axis black
                  legend.labs = c( "Control",
                                   "MicG 48h",
                                   "MicG 120h"))+
  labs(tag="D")#label for multipanel figure
S3life




library(grid)
library(ggplotify)
panel4<-S3life$plot
f3<-grid.arrange (Panel1,ggclones3, Panel3,panel4, ncol=2, nrow=2)
f3
ggsave("lhoppfigure3.png", f3)
ggsave("aprillhoppfigure3.png", f3, width = 8, height = 5.33, units = "in")
