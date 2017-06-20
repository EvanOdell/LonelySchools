#dataset: isolatedSchools

library(ggplot2)
library(stargazer)
library(CBPS)
library(scales)
library(cobalt)
library(Hmisc)

##Correlations
fsm.tt5.cor <- cor(isolatedSchools$AC.GCSE.FSM,isolatedSchools$IsoIndex)
all.tt5.cor <- cor(isolatedSchools$AC.GCSE.ALL,isolatedSchools$IsoIndex)
print(fsm.tt5.cor)
print(all.tt5.cor)

###Plots
fsm.tt5.plot <- ggplot(isolatedSchools, aes(y=AC.GCSE.FSM, x=IsoIndex)) +
  geom_point(shape=20, size=3) + 
  stat_smooth(se=FALSE, fullrange=TRUE, colour="Red", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Percentage of Ever6FSM students at 5+ A*-C GCSE(EM)")

all.tt5.plot <- ggplot(isolatedSchools, aes(y=AC.GCSE.ALL, x=IsoIndex)) +
  geom_point(shape=20, size=3) + 
  geom_smooth(se=FALSE, fullrange=TRUE, colour="Blue", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Percentage of all students at 5+ A*-C GCSE(EM)")

#Making plots look pretty
all.tt5.plot + scale_y_continuous(labels=percent,breaks=pretty_breaks(n=11)) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(panel.background= element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=1),
        panel.grid.major=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) +
  coord_cartesian(ylim=c(0, 1.0)) + 
  coord_cartesian(xlim=c(0, 60), expand=FALSE)

fsm.tt5.plot + scale_y_continuous(labels=percent,breaks=pretty_breaks(n=11)) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(panel.background= element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=1),
        panel.grid.major=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) +
  coord_cartesian(ylim=c(0, 1.0)) + 
  coord_cartesian(xlim=c(0, 60), expand=FALSE)

###Weighting using cohort sizes
model1.all.wght <- lm(AC.GCSE.ALL ~ IsoIndex + EVER6FSM + EAL + 
                        SEN + GIRLS + TOTPUPS + KS2APS + IDACI +
                        London, data = isolatedSchools, weights = TPUPS)
summary(model1.all.wght)

model2.fsm.wght <- lm(AC.GCSE.FSM ~ IsoIndex + EVER6FSM + EAL + 
                        SEN + GIRLS + TOTPUPS + KS2APS + IDACI +
                        London, data = isolatedSchools, weights = TEVER6PUPS)
summary(model2.fsm.wght)

#making standardised versions of all the variables
isolatedSchools$AC.GCSE.ALL.z <- scale(isolatedSchools$AC.GCSE.ALL)
isolatedSchools$AC.GCSE.FSM.z <- scale(isolatedSchools$AC.GCSE.FSM)
isolatedSchools$IsoIndex.z <- scale(isolatedSchools$IsoIndex)
isolatedSchools$EVER6FSM.z <- scale(isolatedSchools$EVER6FSM)
isolatedSchools$EAL.z <- scale(isolatedSchools$EAL)
isolatedSchools$SEN.z <- scale(isolatedSchools$SEN)
isolatedSchools$GIRLS.z <- scale(isolatedSchools$GIRLS)
isolatedSchools$TOTPUPS.z <- scale(isolatedSchools$TOTPUPS)
isolatedSchools$KS2APS.z <- scale(isolatedSchools$KS2APS)
isolatedSchools$IDACI.z <- scale(isolatedSchools$IDACI)
isolatedSchools$TPUPS.z <- scale(isolatedSchools$TPUPS)
isolatedSchools$TEVER6PUPS.z <- scale(isolatedSchools$TEVER6PUPS) 


#CBPS stuffs
fit.all <- npCBPS(IsoIndex ~ EVER6FSM + EAL + 
                    SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, data = isolatedSchools, 
                  weights=TPUPS)

model3.all.wght.cbps <- lm(AC.GCSE.ALL ~ IsoIndex + EVER6FSM + EAL + 
                             SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, 
                           data = isolatedSchools, weights = fit.all$weights)

fit.fsm <- npCBPS(IsoIndex ~ EVER6FSM + EAL + 
                    SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, data = isolatedSchools, 
                  weights=TEVER6PUPS)

model4.fsm.wght.cbps <- lm(AC.GCSE.FSM ~ IsoIndex + EVER6FSM + EAL + 
                             SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, 
                           data = isolatedSchools, weights = fit.fsm$weights)

fit.all.z <- npCBPS(IsoIndex.z ~ EVER6FSM.z + EAL.z + 
                      SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                      IDACI.z + London, data = isolatedSchools,  weights=TPUPS.z)

model5.all.wght.cbps.z <- lm(AC.GCSE.ALL.z ~ IsoIndex.z + EVER6FSM.z + EAL.z + 
                               SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                               IDACI.z + London, data = isolatedSchools, 
                             weights = fit.all$weights)

fit.fsm.z <- npCBPS(IsoIndex.z ~ EVER6FSM.z + EAL.z + 
                      SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                      IDACI.z + London, data = isolatedSchools, weights=TEVER6PUPS.z)

model6.fsm.wght.cbps.z <- lm(AC.GCSE.FSM.z ~ IsoIndex.z + EVER6FSM.z + EAL.z + 
                               SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                               IDACI.z + London, data = isolatedSchools, 
                             weights = fit.fsm.z$weights)

###Tables
stargazer(model1.all.wght, model2.fsm.wght, 
          model3.all.wght.cbps, model4.fsm.wght.cbps,
          model5.all.wght.cbps.z, model6.fsm.wght.cbps.z,
          star.cutoffs = c(0.05, 0.01, 0.001))

stargazer(model5.all.wght.cbps.z, model6.fsm.wght.cbps.z,
          star.cutoffs = c(0.05, 0.01, 0.001))


stargazer(model6.all.wght, model6.all.wght.cbps, model6.all.wght.cbps.z, star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE)

stargazer(model6.fsm.wght, model6.fsm.wght.cbps, model6.fsm.wght.cbps.z, star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE)


sd(iso4all$avg5.tt.nid)

t <- sd(iso4all$ac.gcse.all.3y)

t * -0.086

u <- sd(iso4all$ac.gcse.fsm.3y)

u* -0.12 

var <- wtd.var(iso4all$avg5.tt.nid, iso4all$tpup.3y)



