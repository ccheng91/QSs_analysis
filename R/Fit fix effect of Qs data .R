
########### run it everytime ###################### run it everytime ###################### run it everytime ###########

setwd("/Users/chencheng/Desktop/data/QS_anlysis")
rm(list=ls())
data <- read.csv("QS_101.csv")
library(ggplot2)
library(Matrix)
library(car)
library(lme4)
library(visreg)
library(dplyr)

########### run it everytime ###################### run it everytime ###################### run it everytime ###################### run it everytime ###########
data$Nationality <- factor(data$Nationality)
data$Village <- factor(data$Village)
data$Gender <- factor(data$Gender)
data$Education <- factor(data$Education)
data$Time.to.PAs <- as.numeric(as.character(data$Time.to.PAs))
data.l <- reshape(data,
                  varying = c("A_A_1", "A_A_2","A_A_3","A_A_4","A_A_5",
                              "A_P_1", "A_P_2","A_P_3","A_P_4","A_P_5",
                              "A_PA_1", "A_PA_2","A_PA_3","A_PA_4","A_PA_5",
                              "S_A_1", "S_A_2","S_A_3","S_A_4","S_A_5",
                              "S_P_1", "S_P_2","S_P_3","S_P_4","S_P_5",
                              "S_PA_1", "S_PA_2","S_PA_3","S_PA_4","S_PA_5"), 
                  v.names = "Score",
                  timevar = "Items", 
                  times = c("A_A_1", "A_A_2","A_A_3","A_A_4","A_A_5",
                            "A_P_1", "A_P_2","A_P_3","A_P_4","A_P_5",
                            "A_PA_1", "A_PA_2","A_PA_3","A_PA_4","A_PA_5",
                            "S_A_1", "S_A_2","S_A_3","S_A_4","S_A_5",
                            "S_P_1", "S_P_2","S_P_3","S_P_4","S_P_5",
                            "S_PA_1", "S_PA_2","S_PA_3","S_PA_4","S_PA_5"), 
                  new.row.names = 1:100000,
                  direction = "long")  

data.long <- select(data.l, Num, id, Items, Score, Nationality, Gender, Age, Education, NO.of.fam, Income, Village, PAs, 
                    Time.to.PAs)
head(data.long)

z <- glm(Score ~ Age+ Nationality+ Gender+ 
           Education+ NO.of.fam+ Income +Village +PAs +Time.to.PAs, data=data.long)
zspt <- lmer(Score ~ Age+ Nationality+ Gender+ Education+ NO.of.fam+ Income +Time.to.PAs 
             + (1|PAs/Village), data=data.long)
fit <- Anova(zspt, type="3")
fit
summary(zspt)
summary(fit)
visreg(z)
visreg(z.t)

hist(data.l$No_of_Punishment)

table(data.long$Time.to.PAs)



########### fit the Generalized mixed effect  model #############

########### Social norm of plant collection inside PA ####################

zspt <- lmer(S_P_T ~ Score_of_Punishment_Of_P +  Relationship + Age + 
               Gender + Education + Income + Nationality + Time_of_outreach + 
               No_of_Punishment + (1|PAs/Village), data=data)
 
zspt <- lmer(S_P_T ~  Age + Gender + Education + Income + Nationality + (1|PAs/Village), data=data)
summary(zspt)

fitspt <- Anova(zspt, type= "3")
fitspt
visreg(zspt)

########### Social norm of establish a PA  ####################

zsest <- lmer(S_EST_PA ~ Relationship + Age + 
                Gender + Education + Income + Nationality + Time_of_outreach + 
                No_of_Punishment + (1|PAs/Village)  , data=data)

fsest <- Anova(zsest, type="3")
fsest
visreg(zsest)

########### Social norm of live next to PA  ###################

zdis <- lmer(S_PA_Dis ~ Relationship + Age + 
               Gender + Education + Income + Nationality + Time_of_outreach + 
               No_of_Punishment + (1|PAs/Village)  , data=data)

fdis <- Anova(zdis, type="3")
fdis
visreg(zdis)

############## Attitude towards animal ##########################

zaa4 <- lmer(A_A_4 ~ Score_of_Punishment_Of_H + Relationship + Age + Gender + Education + Income + Nationality + 
               Time_of_outreach + No_of_Punishment + (1|PAs/Village), data=data)

faa4 <- Anova(zaa4, type= "3")
faa4
visreg(zaa4)

########### Attitude towards plant collection ####################

zap1 <- lm(A_P_1 ~ PAs, data=data)
zap2 <- lm(A_P_2 ~ PAs, data=data) ## Not significant in Anova 
zap3 <- lm(A_P_3 ~ PAs, data=data) ### ??????????
zap4 <- lm(A_P_4 ~ PAs, data=data)
zap5 <- lm(A_P_5 ~ PAs, data=data) ## Not significant in Anova

visreg(zap1)
visreg(zap2)
visreg(zap3)
visreg(zap4)
visreg(zap5)

########### Attitude towards PA####################

zpark1 <- lm(A_PA_1 ~ PAs, data=data)
zpark2 <- lm(A_PA_2 ~ PAs, data=data)
zpark3 <- lm(A_PA_3 ~ PAs, data=data)
zpark4 <- lm(A_PA_4 ~ PAs, data=data)
zpark5 <- lm(A_PA_5 ~ PAs, data=data)

visreg(zap1)
visreg(zap2)
visreg(zap3)
visreg(zap4)
visreg(zap5)

########### Score of illege hunting punishment #########

zsh <- lmer(Score_of_Punishment_Of_H ~ Relationship + Age + Gender + Education + Income + Nationality + 
              Time_of_outreach + No_of_Punishment + (1|PAs/Village), data=data)
fzsh <- Anova(zsh, type= "3")
fzsh
visreg(zsh)

########### Score of illege collection punishment #########

zsp <- lmer(Score_of_Punishment_Of_P ~ Relationship + Age + Gender + Education + Income + Nationality + 
              Time_of_outreach + No_of_Punishment + (1|PAs/Village), data=data)
fzsp <- Anova(zsp, type= "3")
fzsp
visreg(zsp)


###########################################################  trial trial trial ##################################################################################
zaa4 <- lm(A_A_4 ~  Score_of_Punishment_Of_H + PAs + Village + Relationship + Age + Gender + Education +
             Income + Nationality + Time_of_outreach + No_of_Punishment , data=data)
summary(zaa4)
fitaa4 <- aov(zaa4)
summary(fitaa4)
visreg(zaa4)

zaa5 <- lm(A_A_5 ~  Score_of_Punishment_Of_H + PAs + Village + Relationship + Age + Gender + Education +
             Income + Nationality + Time_of_outreach + No_of_Punishment , data=data)
summary(zaa5)
fitaa5 <- aov(zaa5)
summary(fitaa5)
visreg(zaa5)

zspause <- lmer(S_PA_useful ~ Score_of_Punishment_Of_P +  Score_of_Punishment_Of_P +PAs +  Relationship + 
                  Age + Gender + Education + Income + Nationality + Time_of_outreach + No_of_Punishment + (1|Village), data=data)
summary(zspause)
fitspause <- Anova(zspause , type= "3")
summary(fitspause)
visreg(zspause)
fitspause

zspadis <- lmer(S_PA_Dis ~ Score_of_Punishment_Of_P +  Score_of_Punishment_Of_H +PAs +  Relationship + Age + 
                  Gender + Education + Income + Nationality + Time_of_outreach + No_of_Punishment + (1|Village), data=data)
fitspadis <- Anova(zspadis , type= "3")
summary(fitspadis)
visreg(zspadis)
fitspadis

zt <- lm(S_P_T ~ Score_of_Punishment_Of_P, data=data)
summary(zt)
fit1 <- aov(zt)
summary(zt)
fit <- aov(z)
summary(fit)
plot(z)
AIC(z)
AIC(zt)

summary(powerTransform(data$S_P_T))

library(car)
fit <- Anova(z, type= 3)
summary(fit)
library(visreg)
visreg(za)

table(data$Village)

library(MASS)
stepAIC(z, direction="backward")
#################################################  trial trial trial ###########################################################################################################



########################### ANOVA   ################################################
aovaa1 <- aov(A_A_1 ~ PAs, data=data)
aovaa2 <- aov(A_A_2 ~ PAs, data=data)
aovaa3 <- aov(A_A_3 ~ PAs, data=data)
aovaa4 <- aov(A_A_4 ~ PAs, data=data)
aovaa5 <- aov(A_A_5 ~ PAs, data=data)

aovap1 <- aov(A_P_1 ~ PAs, data=data)
aovap2 <- aov(A_P_2 ~ PAs, data=data)
aovap3 <- aov(A_P_3 ~ PAs, data=data)
aovap4 <- aov(A_P_4 ~ PAs, data=data)
aovap5 <- aov(A_P_5 ~ PAs, data=data)

aovapa1 <- aov(A_PA_1 ~ PAs, data=data)
aovapa2 <- aov(A_PA_2 ~ PAs, data=data)
aovapa3 <- aov(A_PA_3 ~ PAs, data=data)
aovapa4 <- aov(A_PA_4 ~ PAs, data=data)
aovapa5 <- aov(A_PA_5 ~ PAs, data=data)

aovsa1 <- aov(S_A_1 ~ PAs, data=data)
aovsa2 <- aov(S_A_2 ~ PAs, data=data)
aovsa3 <- aov(S_A_3 ~ PAs, data=data)
aovsa4 <- aov(S_A_4 ~ PAs, data=data)
aovsa5 <- aov(S_A_5 ~ PAs, data=data)

aovsp1 <- aov(S_P_1 ~ PAs, data=data)
aovsp2 <- aov(S_P_2 ~ PAs, data=data)
aovsp3 <- aov(S_P_3 ~ PAs, data=data)
aovsp4 <- aov(S_P_4 ~ PAs, data=data)
aovsp5 <- aov(S_P_5 ~ PAs, data=data)

aovspa1 <- aov(S_PA_1 ~ PAs, data=data)
aovspa2 <- aov(S_PA_2 ~ PAs, data=data)
aovspa3 <- aov(S_PA_3 ~ PAs, data=data)
aovspa4 <- aov(S_PA_4 ~ PAs, data=data)
aovspa5 <- aov(S_PA_5 ~ PAs, data=data)

asd1 <- lm(S_PA_1 ~ PAs, data=data)
asd2 <- lm(S_PA_2 ~ PAs, data=data)
visreg(asd1)
visreg(asd2)

summary(aovaa1)
summary(aovaa2)
summary(aovaa3)
summary(aovaa4)
summary(aovaa5)

summary(aovap1)
summary(aovap2)
summary(aovap3)
summary(aovap4)
summary(aovap5)

summary(aovapa1)
summary(aovapa2)
summary(aovapa3)
summary(aovapa4)
summary(aovapa5)

summary(aovsa1)
summary(aovsa2)
summary(aovsa3)
summary(aovsa4)
summary(aovsa5)

summary(aovsp1)
summary(aovsp2)
summary(aovsp3)
summary(aovsp4)
summary(aovsp5)

summary(aovspa1)
summary(aovspa2)
summary(aovspa3)
summary(aovspa4)
summary(aovspa5)

#################################### Chi-squre ################################################

mytable <- table(data$Time_of_outreach,data$PAs) # A will be rows, B will be columns 
mytable # print table 
margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)

table(data$Education)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages
chisq.test(mytable)
barplot(mytable,col = c("lightblue", "mistyrose","lightcyan", "lavender","blue","green"))
legend("topleft", 
       legend = c("Elementary", "High", "Middle", "None Secondary school", "Temple" ), 
       fill= c("lightblue", "mistyrose","lightcyan", "lavender","blue","green"),
       cex = 0.75)


#################################### Chi-squre ################################################

### PCA and EFA ####

library(psych)
fa.parallel(data[2:6], fa="pc", n.iter=100, show.legend=FALSE, main="WTF")
pc <- principal(data[2:6], nfactors=2, rotate="varimax")
pc

fa.parallel(data[2:6],fa='0', n.obs=374, n.iter=100 )
cov <- cov2cor(data[2:6])
fa <- fa(data[2:6], nfactors=3, n.obs=374, rotate="none", fm="ml", missing=TRUE)
fa


fa.parallel(data[7:11], fa="pc", n.iter=100, show.legend=FALSE, main="WTF")
pc <- principal(data[7:11], nfactors=2, rotate="varimax")
pc

fa.parallel(data[7:11],fa="both", n.iter=100 )
fa <- fa(data[7:11], nfactors=3, n.obs=374, rotate="none", fm="ml", missing=TRUE)
fa



fa.parallel(data[12:16], fa="pc", n.iter=100, show.legend=FALSE, main="WTF")
pc <- principal(data[12:16], nfactors=2, rotate="varimax")
pc

fa.parallel(data[12:16],fa="both", n.iter=100 )
fa <- fa(data[12:16], nfactors=2, n.obs=374, rotate="none", fm="ml", missing=TRUE)
fa

fa.parallel(data[17:21], fa="pc", n.iter=100, show.legend=FALSE, main="WTF")
pc <- principal(data[17:21], nfactors=2, rotate="varimax")
pc

fa.parallel(data[17:21],fa="both", n.iter=100 )
fa <- fa(data[17:21], nfactors=3, n.obs=374, rotate="none", fm="ml", missing=TRUE)



