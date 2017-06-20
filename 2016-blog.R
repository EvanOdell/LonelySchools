
pacman::p_load("osrm")
pacman::p_load("readxl")
pacman::p_load("dplyr")
pacman::p_load("ggplot2")
pacman::p_load("stargazer")
pacman::p_load("CBPS")
pacman::p_load("scales")
pacman::p_load("cobalt")
pacman::p_load("memisc")
pacman::p_load("pander")

ks4_2016 <- read_excel("~/Documents/GitHub/lonely-schools/england_ks4final2016.xlsx")

#options(osrm.server = "http://localhost:5000/")
#options(osrm.profile = "car")

#x <- ks4_2016[,c("urn", "Longitude","Latitude")]

#names(x)[names(x)=="urn"] <- "id"
#names(x)[names(x)=="Longitude"] <- "lon"
#names(x)[names(x)=="Latitude"] <- "lat"

#x$id <- as.character(x$id)
#x <- as.data.frame(x)

#trip_time <- osrmTable(x)

#trip_time$durations[1:5,1:5]

#trip_time$durations <- as.data.frame(trip_time$durations)

#write_csv(trip_time$durations, "trip_time.csv", row.names = TRUE)

trip_time <- read_excel("~/Documents/GitHub/lonely-schools/trip_time.xlsx")

ks4_2016 <- left_join(ks4_2016, trip_time)

##Correlations -------------------
fsm.tt5.cor <- cor(ks4_2016$attainment8_ever6fsm, ks4_2016$avg5.tt.nid)
all.tt5.cor <- cor(ks4_2016$attainment8_all, ks4_2016$avg5.tt.nid)
fsm.tt5.cor
all.tt5.cor

#simple regressions -------------------
fsm.tt5.lm <- lm(attainment8_ever6fsm ~ avg5.tt.nid, data = ks4_2016)
summary(fsm.tt5.lm)
all.tt5.lm <- lm(attainment8_all ~ avg5.tt.nid, data = ks4_2016)
summary(all.tt5.lm)

### Plots -------------------
fsm.tt5.plot <- ggplot(ks4_2016, aes(y=attainment8_ever6fsm, x=avg5.tt.nid)) +
  geom_point(shape=20, size=3) + 
  stat_smooth(se=FALSE, fullrange=TRUE, colour="Red", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Average Attainment 8 Point Score, Disadvantaged Students")

all.tt5.plot <- ggplot(ks4_2016, aes(y=attainment8_all, x=avg5.tt.nid)) +
  geom_point(shape=20, size=3) + 
  geom_smooth(se=FALSE, fullrange=TRUE, colour="Blue", size=1) +
  labs(x= "Average travel time (in minutes) to five nearest secondary schools") + 
  labs(y= "Average Attainment 8 Point Score, All Students")

print(fsm.tt5.plot)
print(all.tt5.plot)

all.tt5.plot <- all.tt5.plot + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) 

fsm.tt5.plot <- fsm.tt5.plot + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  scale_x_continuous(breaks=pretty_breaks(n=12)) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) 

ggsave(plot=fsm.tt5.plot, filename="fsm.tt5.plot2016.png", type = "cairo-png", dpi= 2000, width = 20, height = 20, units = "cm")
ggsave(plot=all.tt5.plot, filename="all.tt5.plot2016.png", type = "cairo-png", dpi= 2000, width = 20, height = 20, units = "cm")


## plots comparing 2013-2015 with 2016 -------------------

cor_test <- subset(ks4_2016, is.na(ac.gcse.all.3y)==FALSE)

att8_gcse_fsm <- cor(cor_test$attainment8_ever6fsm, cor_test$ac.gcse.fsm.3y)
att8_gcse_all <- cor(cor_test$attainment8_all, cor_test$ac.gcse.all.3y)

cor_test$ac.gcse.fsm.3y <-cor_test$ac.gcse.fsm.3y * 100
cor_test$ac.gcse.all.3y <- cor_test$ac.gcse.all.3y * 100

att8_gcse_fsm <- lm(attainment8_ever6fsm ~ac.gcse.fsm.3y, data=cor_test)
summary(att8_gcse_fsm)

att8_gcse_fsm_plot <- ggplot(cor_test, aes(y=attainment8_ever6fsm, x=ac.gcse.fsm.3y)) +
  geom_point(shape=20, size=3) + 
  geom_smooth(method='lm', colour="Red", size=1) +
  labs(x= "5+ A*-C GCSE(EM)") + 
  labs(y= "Average Attainment 8 Point Score, Disadvantaged Students") + 
  scale_y_continuous(breaks=pretty_breaks(n=11)) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black")) + 
  annotate("text", label = as.character(expression(paste(R^{2}," = 0.4675"))), x = 60, y = 30, size = 4, colour = "black", parse=TRUE, hjust=0) + 
  annotate("text", label = as.character(expression(paste("N = 2460"))), x = 60, y = 27, size = 4, colour = "black", parse=TRUE, hjust=0)

att8_gcse_fsm_plot


att8_gcse_all <- lm(attainment8_all ~ac.gcse.all.3y, data=cor_test)
summary(att8_gcse_all)

att8_gcse_all_plot <- ggplot(cor_test, aes(y=attainment8_all, x=ac.gcse.all.3y)) +
  geom_point(shape=20, size=3) + 
  geom_smooth(method='lm', colour="Blue", size=1) +
  labs(x= "5+ A*-C GCSE(EM)", y= "Average Attainment 8 Point Score, All Students") +
  scale_y_continuous(breaks=pretty_breaks(n=12)) +
  #scale_x_continuous(labels=percent) +
  theme(axis.text.y = element_text(size=14, colour = "black"),
        axis.text.x = element_text(size=14, colour = "black"))  + 
  annotate("text", label = as.character(expression(paste(R^{2}," = 0.7437"))), x = 60, y = 30, size = 4, colour = "black", parse=TRUE, hjust=0) + 
  annotate("text", label = as.character(expression(paste("N = 2460"))), x = 60, y = 27, size = 4, colour = "black", parse=TRUE, hjust=0)

att8_gcse_all_plot



ggsave(plot=att8_gcse_fsm_plot, filename="att8_gcse_fsm_plot.png", type = "cairo-png", dpi= 2000, width = 20, height = 20, units = "cm")
ggsave(plot=att8_gcse_all_plot, filename="att8_gcse_all_plot.png", type = "cairo-png", dpi= 2000, width = 20, height = 20, units = "cm")

# unstandardised versions -------------------
model6.fsm <- lm(attainment8_ever6fsm ~ avg5.tt.nid + EVER6FSM + EAL + 
                   SEN + GIRLS + TOTPUPS + KS2APS + IDACI +
                   London, data = ks4_2016)
summary(model6.fsm)

model6.all <- lm(attainment8_all ~ avg5.tt.nid + EVER6FSM + EAL + 
                   SEN + GIRLS + TOTPUPS + KS2APS + IDACI +
                   London, data = ks4_2016)
summary(model6.all)


###Weighting experiment -------------------
model6.all.wght <- lm(attainment8_all ~ avg5.tt.nid + EVER6FSM + EAL + 
                        SEN + GIRLS + TOTPUPS + KS2APS + IDACI +
                        London, data = ks4_2016, weights = TPUP)
summary(model6.all.wght)

model6.fsm.wght <- lm(attainment8_ever6fsm ~ avg5.tt.nid + EVER6FSM + EAL + 
                        SEN + GIRLS + TOTPUPS + KS2APS + IDACI +
                        London, data = ks4_2016, weights = TEVER6FSM)
summary(model6.fsm.wght)





#making standardised versions of all the variables
ks4_2016$attainment8_all.z <- scale(ks4_2016$attainment8_all)
ks4_2016$attainment8_ever6fsm.z <- scale(ks4_2016$attainment8_ever6fsm)
ks4_2016$avg5.tt.nid.z <- scale(ks4_2016$avg5.tt.nid)
ks4_2016$EVER6FSM.z <- scale(ks4_2016$EVER6FSM)
ks4_2016$EAL.z <- scale(ks4_2016$EAL)
ks4_2016$SEN.z <- scale(ks4_2016$SEN)
ks4_2016$GIRLS.z <- scale(ks4_2016$GIRLS)
ks4_2016$TOTPUPS.z <- scale(ks4_2016$TOTPUPS)
ks4_2016$KS2APS.z <- scale(ks4_2016$KS2APS)
ks4_2016$IDACI.z <- scale(ks4_2016$IDACI)
ks4_2016$TPUP.z <- scale(ks4_2016$TPUP)
ks4_2016$TEVER6FSM.z <- scale(ks4_2016$TEVER6FSM) 

#Standardised versions -------------------
model6.fsm.scaled <- lm(attainment8_ever6fsm.z ~ avg5.tt.nid.z + EVER6FSM.z + EAL.z + 
                          SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z + IDACI.z +
                          London, data = ks4_2016)
summary(model6.fsm.scaled)

model6.all.scaled <- lm(attainment8_all.z ~ avg5.tt.nid.z + EVER6FSM.z + EAL.z + 
                          SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z + IDACI.z +
                          London, data = ks4_2016)
summary(model6.all.scaled)

model6.fsm.scaled.wght <- lm(attainment8_ever6fsm.z ~ avg5.tt.nid.z + EVER6FSM.z + EAL.z + 
                               SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z + IDACI.z +
                               London, data = ks4_2016, weights = TEVER6FSM)
summary(model6.fsm.scaled.wght)

model6.all.scaled.wght <- lm(attainment8_all.z ~ avg5.tt.nid.z + EVER6FSM.z + EAL.z + 
                               SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z + IDACI.z +
                               London, data = ks4_2016, weights = TPUP)
summary(model6.all.scaled.wght)

#CBPS stuff -------------------
cbps.m6.all <- CBPS(model6.all)
cbps.m6.allvcov <- vcov(cbps.m6.all)
summary(cbps.m6.all)
summary(cbps.m6.allvcov)

fit.all <- npCBPS(avg5.tt.nid ~ EVER6FSM + EAL + 
                    SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, data = ks4_2016, 
                  weights=TPUP)

model6.all.wght.cbps <- lm(attainment8_all ~ avg5.tt.nid + EVER6FSM + EAL + 
                             SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, 
                           data = ks4_2016, weights = fit.all$weights)

fit.fsm <- npCBPS(avg5.tt.nid ~ EVER6FSM + EAL + 
                    SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, data = ks4_2016, 
                  weights=TEVER6FSM)

model6.fsm.wght.cbps <- lm(attainment8_ever6fsm ~ avg5.tt.nid + EVER6FSM + EAL + 
                             SEN + GIRLS + TOTPUPS + KS2APS + IDACI + London, 
                           data = ks4_2016, weights = fit.fsm$weights)
summary(model6.fsm.wght.cbps)

fit.all.z <- npCBPS(avg5.tt.nid.z ~ EVER6FSM.z + EAL.z + 
                      SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                      IDACI.z + London, data = ks4_2016,  weights=TPUP.z)
summary(fit.all.z)

model6.all.wght.cbps.z <- lm(attainment8_all.z ~ avg5.tt.nid.z + EVER6FSM.z + EAL.z + 
                               SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                               IDACI.z + London, data = ks4_2016, 
                             weights = fit.all$weights)
summary(model6.all.wght.cbps.z)

fit.fsm.z <- npCBPS(avg5.tt.nid.z ~ EVER6FSM.z + EAL.z + 
                      SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                      IDACI.z + London, data = ks4_2016, weights=TEVER6FSM.z)

model6.fsm.wght.cbps.z <- lm(attainment8_ever6fsm.z ~ avg5.tt.nid.z + EVER6FSM.z + EAL.z + 
                               SEN.z + GIRLS.z + TOTPUPS.z + KS2APS.z +
                               IDACI.z + London, data = ks4_2016, 
                             weights = fit.fsm.z$weights)

summary(model6.fsm.wght.cbps.z)

###Tables

star_table <- stargazer(model6.all.wght, model6.fsm.wght, 
          model6.all.wght.cbps, model6.fsm.wght.cbps,
          model6.all.wght.cbps.z, model6.fsm.wght.cbps.z,
          star.cutoffs = c(0.075, 0.05, 0.01, 0.001), no.space = TRUE)


table <- mtable('Model 1' = model6.all.wght,
                'Model 2' = model6.fsm.wght,
                'Model 3' = model6.all.wght.cbps,
                'Model 4' = model6.fsm.wght.cbps,
                'Model 5' = model6.all.wght.cbps.z,
                'Model 6' = model6.fsm.wght.cbps.z, 
                    summary.stats = TRUE, sdigits=3)

write.mtable(table, "table.md")
