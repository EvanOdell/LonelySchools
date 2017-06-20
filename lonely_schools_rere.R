


# Calculations for Paper --------------------------------------------------

pacman::p_load("ggplot2")
pacman::p_load("stargazer")
pacman::p_load("CBPS")
pacman::p_load("scales")
pacman::p_load("cobalt")
pacman::p_load("readr")


iso4all <- read_csv("iso4all.csv")

summary(iso4all)

##Correlations
fsm.tt5.cor <- cor(iso4all$ac.gcse.fsm.3y,iso4all$avg5.tt.nid)
all.tt5.cor <- cor(iso4all$ac.gcse.all.3y,iso4all$avg5.tt.nid)
fsm.tt5.cor
all.tt5.cor

#simple regressions
fsm.tt5.lm <- lm(ac.gcse.fsm.3y ~ avg5.tt.nid, data = iso4all)
summary(fsm.tt5.lm)
all.tt5.lm <- lm(ac.gcse.all.3y ~ avg5.tt.nid, data = iso4all)
summary(all.tt5.lm)

###Plots
fsm.tt5.plot <- ggplot(iso4all, aes(y=ac.gcse.fsm.3y, x=avg5.tt.nid)) +
  geom_point(shape=20, size=3) + 
  stat_smooth(se=FALSE, fullrange=TRUE, colour="Red", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Percentage of Ever6FSM students at 5+ A*-C GCSE(EM)")

all.tt5.plot <- ggplot(iso4all, aes(y=ac.gcse.all.3y, x=avg5.tt.nid)) +
  geom_point(shape=20, size=3) + 
  geom_smooth(se=FALSE, fullrange=TRUE, colour="Blue", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Percentage of all students at 5+ A*-C GCSE(EM)")

print(fsm.tt5.plot)
print(all.tt5.plot)

all.tt5.plot <- all.tt5.plot + scale_y_continuous(labels=percent,breaks=pretty_breaks(n=11)) +
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

fsm.tt5.plot <- fsm.tt5.plot + scale_y_continuous(labels=percent, breaks=pretty_breaks(n=11)) +
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

#unstandardised versions
model6.fsm <- lm(ac.gcse.fsm.3y ~ avg5.tt.nid + ever6fsm.3y + eal.3y + 
                   sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI +
                   London, data = iso4all)
summary(model6.fsm)

model6.all <- lm(ac.gcse.all.3y ~ avg5.tt.nid + ever6fsm.3y + eal.3y + 
                   sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI +
                   London, data = iso4all)
summary(model6.all)
plot(model6.all)


###Weighting experiment
model6.all.wght <- lm(ac.gcse.all.3y ~ avg5.tt.nid + ever6fsm.3y + eal.3y + 
                        sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI +
                        London, data = iso4all, weights = tpup.3y)
summary(model6.all.wght)

model6.fsm.wght <- lm(ac.gcse.fsm.3y ~ avg5.tt.nid + ever6fsm.3y + eal.3y + 
                   sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI +
                   London, data = iso4all, weights = tever6pup.3y)
summary(model6.fsm.wght)
plot(model6.fsm.wght)




#making standardised versions of all the variables
iso4all$ac.gcse.all.3y.z <- scale(iso4all$ac.gcse.all.3y)
iso4all$ac.gcse.fsm.3y.z <- scale(iso4all$ac.gcse.fsm.3y)
iso4all$avg5.tt.nid.z <- scale(iso4all$avg5.tt.nid)
iso4all$ever6fsm.3y.z <- scale(iso4all$ever6fsm.3y)
iso4all$eal.3y.z <- scale(iso4all$eal.3y)
iso4all$sen.3y.z <- scale(iso4all$sen.3y)
iso4all$girls.3y.z <- scale(iso4all$girls.3y)
iso4all$totpups.3y.z <- scale(iso4all$totpups.3y)
iso4all$ks2aps.3y.z <- scale(iso4all$ks2aps.3y)
iso4all$IDACI.z <- scale(iso4all$IDACI)
iso4all$tpup.3y.z <- scale(iso4all$tpup.3y)
iso4all$tever6pup.3y.z <- scale(iso4all$tever6pup.3y) 

#Standardised versions
model6.fsm.scaled <- lm(ac.gcse.fsm.3y.z ~ avg5.tt.nid.z + ever6fsm.3y.z + eal.3y.z + 
                   sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z + IDACI.z +
                   London, data = iso4all)
summary(model6.fsm.scaled)

model6.all.scaled <- lm(ac.gcse.all.3y.z ~ avg5.tt.nid.z + ever6fsm.3y.z + eal.3y.z + 
                   sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z + IDACI.z +
                   London, data = iso4all)
summary(model6.all.scaled)

model6.fsm.scaled.wght <- lm(ac.gcse.fsm.3y.z ~ avg5.tt.nid.z + ever6fsm.3y.z + eal.3y.z + 
                          sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z + IDACI.z +
                          London, data = iso4all, weights = tever6pup.3y)
summary(model6.fsm.scaled.wght)

model6.all.scaled.wght <- lm(ac.gcse.all.3y.z ~ avg5.tt.nid.z + ever6fsm.3y.z + eal.3y.z + 
                          sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z + IDACI.z +
                          London, data = iso4all, weights = tpup.3y)
summary(model6.all.scaled.wght)

#CBPS stuffs
cbps.m6.all <- CBPS(model6.all)
cbps.m6.allvcov <- vcov(cbps.m6.all)
summary(cbps.m6.all)
summary(cbps.m6.allvcov)
plot(cbps.m6.all, covars=1)
plot(cbps.m6.allvcov)
plot(cbps.m6.all)
balance(cbps.m6.all)
summary(model6.all)

fit.all <- npCBPS(avg5.tt.nid ~ ever6fsm.3y + eal.3y + 
  sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI + London, data = iso4all, 
  weights=tpup.3y)

model6.all.wght.cbps <- lm(ac.gcse.all.3y ~ avg5.tt.nid + ever6fsm.3y + eal.3y + 
          sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI + London, 
        data = iso4all, weights = fit.all$weights)

fit.fsm <- npCBPS(avg5.tt.nid ~ ever6fsm.3y + eal.3y + 
                    sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI + London, data = iso4all, 
                  weights=tever6pup.3y)

model6.fsm.wght.cbps <- lm(ac.gcse.fsm.3y ~ avg5.tt.nid + ever6fsm.3y + eal.3y + 
          sen.3y + girls.3y + totpups.3y + ks2aps.3y + IDACI + London, 
        data = iso4all, weights = fit.fsm$weights)
summary(model6.fsm.wght.cbps)

fit.all.z <- npCBPS(avg5.tt.nid.z ~ ever6fsm.3y.z + eal.3y.z + 
                    sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z +
                      IDACI.z + London, data = iso4all,  weights=tpup.3y.z)
summary(fit.all.z)

model6.all.wght.cbps.z <- lm(ac.gcse.all.3y.z ~ avg5.tt.nid.z + ever6fsm.3y.z + eal.3y.z + 
                               sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z +
                               IDACI.z + London, data = iso4all, 
                             weights = fit.all$weights)
summary(model6.all.wght.cbps.z)

fit.fsm.z <- npCBPS(avg5.tt.nid.z ~ ever6fsm.3y.z + eal.3y.z + 
                      sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z +
                      IDACI.z + London, data = iso4all, weights=tever6pup.3y.z)

model6.fsm.wght.cbps.z <- lm(ac.gcse.fsm.3y.z ~ avg5.tt.nid.z + ever6fsm.3y.z + eal.3y.z + 
                               sen.3y.z + girls.3y.z + totpups.3y.z + ks2aps.3y.z +
                               IDACI.z + London, data = iso4all, 
                             weights = fit.fsm.z$weights)

summary(model6.fsm.wght.cbps.z)

###Tables

stargazer(model6.all.wght, model6.fsm.wght, 
          model6.all.wght.cbps, model6.fsm.wght.cbps,
          model6.all.wght.cbps.z, model6.fsm.wght.cbps.z,
          star.cutoffs = c(0.05, 0.01, 0.001))




