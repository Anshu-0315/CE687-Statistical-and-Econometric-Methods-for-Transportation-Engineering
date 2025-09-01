###set working directory (change it to where you have downloaded the file and code)
setwd("C:/Users/Aditya/OneDrive - IIT Kanpur/Teaching/2024-25/2024-25-II/Lab/Lab_Ordered_Response")
library(tidyverse)
library(MASS)



######crash data#########
dat<-read.csv("Michigan_Motorcycle_Non_Intersection_Data_Subset.csv",stringsAsFactors = T)

colnames(dat)
summary(dat)


####Some of the variables may be described in Michigan's coding manual for crash reports
# https://www.michigan.gov/msp/-/media/Project/Websites/msp/cjic/Traffic-Crash-Reporting-Unit-Files/2022-Manual-FINAL.pdf?rev=cb7f9a6b4c6a4a9b9c572508154e84eb







###Create an ordered response variable, injury severity
###0 - No Injury (O)
###1 - Possible Injury (C)
###2 - Suspected Minor Injury (B)
###3 - Suspected Serious Injury (A)
###4 - Fatal Injury (K)
dat$Injury_Severity<-0
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Possible Injury (C)"]<-1
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Suspected Minor Injury (B)"]<-2
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Suspected Serious Injury (A)"]<-3
dat$Injury_Severity[dat$Worst.Injury.in.Crash == "Fatal Injury (K)"]<-4

dat$Injury_Severity<-as.factor(dat$Injury_Severity)
summary(dat$Injury_Severity)


###Creating explanatory variables
dat$Urban<-as.numeric(dat$Rural.Urban.Area=="Urban")
dat$Pedestrian<-as.numeric(dat$Crash..Pedestrian=="Pedestrian Involved")
dat$Late_Night<-as.numeric(dat$Time.of.Day=="12:00 Midnight - 12:59 AM" |
                             dat$Time.of.Day=="1:00 AM - 1:59 AM" |
                             dat$Time.of.Day=="2:00 AM - 2:59 AM" |
                             dat$Time.of.Day=="3:00 AM - 3:59 AM" |
                             dat$Time.of.Day=="4:00 AM - 4:59 AM")

dat$Parked_Vehicle<-as.numeric(dat$Crash..Lane.Departure=="Parked Vehicle")

###Probit model
m1_probit <- polr(Injury_Severity~Speed.Limit.at.Crash.Site+Urban+
                    Pedestrian + Parked_Vehicle + Late_Night
                    ,
                  data=dat,method="probit")
summary(m1_probit)
logLik(m1_probit)


###Probit model
m1_logit <- polr(Injury_Severity~Speed.Limit.at.Crash.Site+Urban+
                    Pedestrian + Parked_Vehicle + Late_Night
                  ,
                  data=dat,method="logistic")
summary(m1_logit)
logLik(m1_logit)

###Compare= coefficients
###m1_probit$coefficients, m1_logit$coefficients

m1_probit$coefficients/m1_logit$coefficients

m1_logit$coefficients/m1_probit$coefficients




