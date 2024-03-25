install.packages("survival")
install.packages("ggplot2")
install.packages("dplyer")
install.packages("ggfortify")
install.packages("ranger")
install.packages("phia")
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)

library(ARTool)

BDinf<-allcloneold[allcloneold$clone=="BD08",]
BDinf<-BDinf[BDinf$para=="yes",]
BDinf<-na.omit(BDinf)
kruskal.test(INFo ~ paratx, data = BDinf)

####total reproduction

####4 clones displaying significant infection. only infected animals in exposure groups
### yes/no parasite
##totalreproinf sheet from LHOPP final data set
infrepro$para<-as.factor(infrepro$para)
infrepro$clone<-as.factor(infrepro$clone)
m<-art(babies ~para*clone, data = infrepro)
mcon<-art(babies ~para*clone, data = infrepro)
summary(m)
anova(m)
art.con(m,~para*clone)
art.con(m, "para:clone", adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
##significant differences between standard clone yes/no


####total repro
###4 clones with sig infection. all animals in clones. yes/no parasite
####totalrepro sheet from LHOPP final data set
repro$para<-as.factor(repro$para)
repro$clone<-as.factor(repro$clone)
m<-art(x ~para*clone, data = repro)
mcon<-art(x ~para*clone, data = repro)
summary(m)
anova(m)
art.con(m,~para*clone)
art.con(m, "para:clone", adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
# sig difference between standard clone parasite yes/no


####total repro
###4 clones with sig infection. all animals in clones. parasite treatments.
##totalrepro sheet from LHOPP final data set
repro$paratx<-as.factor(repro$paratx)
repro$clone<-as.factor(repro$clone)
m<-art(x ~paratx*clone, data = repro)
mcon<-art(x ~paratx*clone, data = repro)
summary(m)
anova(m)
art.con(m,~paratx*clone)
art.con(m, "paratx:clone", adjust="holm") %>%  # run ART-C for X1 × X2
  summary() %>%  # add significance stars to the output
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
####sig diff in STD clone between control and micg48

##############################
#######life span analysis

#analysis comparing control with only infected animals of dw22, dw29, bd, and std clones
#uses lifespaninf sheet from final lhopp data excel
onlyinflife<-coxph(Surv(lifespan,censor)~ para*strata(clone), data=lifespaninf)
summary(onlyinflife)
test.ph<-cox.zph(onlyinflife)
test.ph

##LIFESPAN for 4 INFECTED CLONES. parasite yes/no
##uses lifespan sheet from final lhopp data excel
lifes<-coxph(Surv(lifespan, censor) ~  para*strata(clone), data = lifespan)
summary(lifes)
##test for assumption of prop hazards. p<0.05, failed assumption test
test.ph <- cox.zph(lifes)
test.ph

##LIFESPAN for INFECTED CLONES. effect of paarasite treatment. all animals in 4 clones
#uses lifespan sheet from final lhopp data excel
lifesp<-coxph(Surv(lifespan, censor) ~  paratx*strata(clone), data = lifespan)
summary(lifesp)
##test for assumption of prop hazards. p<0.05, failed assumption test
test.ph <- cox.zph(lifesp)
test.ph


###LIFESPAN for STD
STDlifespan<-lifespan[lifespan$clone=="STD",]
stdlife<-coxph(Surv(lifespan, censor) ~  paratx, data = STDlifespan)
anova(stdlife)
summary(stdlife)
##passes z test. sig difference 48 vs control. control vs 120 p=0.099 
test.ph <- cox.zph(stdlife)
test.ph


bdlife<-allclone[allclone$clone=="BD08",]
Bdlife<-coxph(Surv(end,censor)~ paratx, data=bdlife)
anova(Bdlife)
summary(Bdlife)
# passes z  test. no significant differencces between treatments
test.ph<-cox.zph(Bdlife)
test.ph

dw22life<-allclone[allclone$clone=="DW22",]
Dw22life<-coxph(Surv(end,censor)~ paratx, data=dw22life)
anova(Dw22life)
summary(Dw22life)
#passes z test. no sig differences between treatments
test.ph<-cox.zph(Dw22life)
test.ph

dw29life<-allclone[allclone$clone=="DW29",]
dw29lifesp<-coxph(Surv(end,censor)~ paratx, data=dw29life)
anova(dw29lifesp)
summary(dw29lifesp)
#passes z test. mg48 vs control p=0.061
test.ph<-cox.zph(dw29lifesp)
test.ph









#######################################################################
#SIZE
sizemm <- glm(log(size) ~ para * clone, data = sizeinf)
summary(sizemm)
anova(sizemm, test="F")
plot (sizemm)
qqnorm(resid(sizemm))
qqline(resid(sizemm))


sizemm <- glm(log(size) ~ para * clone, data = size)
summary(sizemm)
anova(sizemm, test="F")
plot (sizemm)
qqnorm(resid(sizemm))
qqline(resid(sizemm))

sizetx <- glm(log(size) ~ paratx * clone, data = size)
summary(sizetx)
anova(sizetx, test="F")
plot (sizetx)
qqnorm(resid(sizetx))
qqline(resid(sizetx))