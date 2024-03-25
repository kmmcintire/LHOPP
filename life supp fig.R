library(tidyverse)
library(cli)
library(ggplot2)
library(survival)
library(ggpubr)
library(dplyr)
library(survminer)
library(gridExtra)

STDlifespan<-lifespan[lifespan$clone=="STD",]
BDlifespan<-lifespan[lifespan$clone=="BD 0846",]
DW29lifespan<-lifespan[lifespan$clone=="DW 2975",]
DW22lifespan<-lifespan[lifespan$clone=="DW 2258",]
####Survival STD
STDlifespan$paratx<-factor(STDlifespan$paratx, levels=c('C','MicG48','MicG120'), ordered=TRUE)
Slife<-ggsurvplot(fit=survfit(Surv(lifespan, censor) ~  paratx, data = STDlifespan),
                   xlab = "Days", 
                   ylab = "Survival Probability",
                  title = "S genotype",
                   conf.int = FALSE, 
                   risk.table = FALSE,
                   censor=FALSE,
                   linetype = c("dotted", 
                                "solid", 
                                "solid"),
                   size = 1, 
                   ggtheme = theme_classic2(base_size=15, base_family = "Arial"),
                   palette = c("BLACK", "#A9A9A9","BLACK"),
                   legend = "none", 
                   #legend.title = "Parasite Exposure",
                   font.tickslab = c( "BLACK"))+ #Makes axis black
                   #legend.labs = c( "Control",
                        #            "MicG 48h",
                         #           "MicG 120h"))+
  labs(tag="D")#label for multipanel figure
Slife
####Survival BD
BDlifespan$paratx<-factor(BDlifespan$paratx, levels=c('C','MicG48','MicG120'), ordered=TRUE)
Blife<-ggsurvplot(fit=survfit(Surv(lifespan, censor) ~  paratx, data = BDlifespan),
                   xlab = "Days", 
                   ylab = "Survival Probability",
                  title ="BD08 genotype",
                   conf.int = FALSE, 
                   risk.table = FALSE,
                   censor=FALSE,
                   linetype = c("dotted", 
                                "solid", 
                                "solid"),
                   size = 1, 
                   ggtheme = theme_classic2(base_size=15, base_family = "Arial"),
                   palette = c("BLACK", "#A9A9A9","BLACK"),
                   legend = "none", 
                   #legend.title = "Parasite Exposure",
                   font.tickslab = c( "BLACK"))+ #Makes axis black
                   #legend.labs = c( "Control",
                    #                "MicG 48h",
                     #               "MicG 120h"))+
  labs(tag="A")#label for multipanel figure
Blife
####Survival STD
DW22lifespan$parastx<-factor(DW22lifespan$paratx, levels=c('C','MicG48','MicG120'), ordered=TRUE)
D2life<-ggsurvplot(fit=survfit(Surv(lifespan, censor) ~  paratx, data = DW22lifespan),
                   xlab = "Days", 
                   ylab = "Survival Probability",
                   title = "DW22 genotype",
                   conf.int = FALSE, 
                   risk.table = FALSE,
                   censor=FALSE,
                   linetype = c("dotted", 
                                "solid", 
                                "solid"),
                   size = 1, 
                   ggtheme = theme_classic2(base_size=15, base_family = "Arial"),
                   palette = c("BLACK", "#A9A9A9","BLACK"),
                   legend = c(.85,.75), 
                   legend.title = "Parasite Exposure",
                   font.tickslab = c( "BLACK"), #Makes axis black
                   legend.labs = c( "Control",
                                    "MicG 120h",
                                    "MicG 48h"))+
  labs(tag="B")#label for multipanel figure
D2life
####Survival STD
DW29lifespan$paratx<-factor(DW29lifespan$paratx, levels=c('C','MicG48','MicG120'), ordered=TRUE)
D9life<-ggsurvplot(fit=survfit(Surv(lifespan, censor) ~  paratx, data = DW29lifespan),
                   xlab = "Days", 
                   ylab = "Survival Probability",
                   title = "DW29 genotype",
                   conf.int = FALSE, 
                   risk.table = FALSE,
                   censor=FALSE,
                   linetype = c("dotted", 
                                "solid", 
                                "solid"),
                   size = 1, 
                   ggtheme = theme_classic2(base_size=15, base_family = "Arial"),
                   palette = c("BLACK","BLACK", "#A9A9A9"),
                   legend = "none",
                   font.tickslab = c( "BLACK"))+ #Makes axis black
                   #legend.labs = c( ""))+
  labs(tag="C") #label for multipanel figure
  
D9life

###Body size figure infected clones. for supplement. Feb.2.2024
##data sourced from "size lhopp" excel file
#uses size dataset to create "sizefig" set for modification for figure


sizefig<-size[,]

library(dplyr)

#create "treatment" column for use in graphing
sizefig$Treatment<-sizefig$paratx
#recode names for consistency across manuscript
sizefig$clone<-recode_factor(sizefig$clone, STD="S")
sizefig$clone<-recode_factor(sizefig$clone, BD="BD08")


sizefig$Treatment<-recode_factor(sizefig$Treatment, C="Control")
sizefig$Treatment<-recode_factor(sizefig$Treatment, "MG48" = "MicG 48h")
sizefig$Treatment<-recode_factor(sizefig$Treatment, "MG120"="MicG 120h")
sizefig$Treatment<-factor(sizefig$Treatment, levels=c('Control','MicG 48h','MicG 120h'), ordered=TRUE)
sizefigure<-  sizefig %>%
  mutate(Treatment = fct_relevel(Treatment, "Control", 
                                 "MicG 48h", "MicG 120h")) %>%
  ggplot(data=sizefig, mapping=aes(x = Treatment, y=size, fill=Treatment)) +theme(text = element_text(family = "Times New Roman"))+
  geom_boxplot(outlier.shape=NA)+theme_classic() + geom_boxplot(outlier.shape = NA)+
  labs(x = "Treatment", y = "Body Length (µm)", ) + 
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
  geom_point(color='red') #adds point color
  #labs(tag="D")
sizefigure


library(grid)
library(ggplotify)
panela<-Blife$plot
panelb<-D2life$plot
panelc<-D9life$plot
pandeld<-Slife$plot
supplfig<-grid.arrange (panela,panelb, panelc,pandeld, ncol=2, nrow=2)
supplefig
supplfigvert<-grid.arrange (panela,panelb, panelc, ncol=1, nrow=3)
supplefigvert

ggsave("lhoppfigure3.png", f3)
ggsave("aprillhoppfigure3.png", f3, width = 8, height = 5.33, units = "in")