##### 
#Extract data from IPDfromKM package and some basic KM graph

library(IPDfromKM)
SOLVD_placebo<-getpoints("C:/Users/Tri Van/Desktop/1Year_analysis/Capture.JPG",x1=0,x2=12,y1=0,y2=100)
SOLVD_placebo
SOLVD_trisk<-c(0,2,4,6,8,10,12) # unit =1 year
SOLVD_nrisk<-c(1284,940,719,562,425,328,151)

#Process data coordinates
SOLVD_placebo_preprocess<-preprocess(dat=SOLVD_placebo,trisk=SOLVD_trisk,nrisk=SOLVD_nrisk,maxy=100)
#Reconstruct IPD
est_SOLVD_placebo<-getIPD(prep=SOLVD_placebo_preprocess, armID=0, tot.events = NULL)
#Accuracy assessment
summary(est_SOLVD_placebo)
plot(est_SOLVD_placebo)

#Secondary analysis
report<-survreport(ipd1=est_SOLVD_placebo$IPD,arms=1,interval = 1)
print(report)

#A <- structure(list(Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30), 
#Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9, 46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6, 22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), 
#.Names = c("Time", "Counts"), row.names = c(1L, 2L, 3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L,31L), class = "data.frame")

A <- structure(list(Time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                    Surv = c(0.887,0.7688,0.6607,0.5931,0.5303,0.4695,0.3968,0.3467,0.3163,0.2891,0.2588,0.2231)), 
               .Names = c("Time", "Surv"), class = "data.frame")
exponential.model <- lm(log(A$Surv)~ A$Time)
summary(exponential.model)
#plot(exponential.model)
#
df<-est_SOLVD_placebo$IPD
write.csv(df,"C:/Users/Tri Van/Desktop/1Year_analysis/SOLVD_placebo_df.csv")

df<-read.csv("C:/Users/Tri Van/Desktop/1Year_analysis/SOLVD_placebo_df.csv")
library(survival)
km.model<-survfit(Surv(df$time,df$status)~1,type="kaplan-meier")
km.model
plot(km.model, main ="placbo of SOLVD repoduced KM curve",xlab="Time (years)",ylab="Survival Prob", col=c(1,5,10),lty=c(1,2,3),lwd=2)
# fit the weibull model 
Weib.model <- survreg( Surv(df$time,df$status) ~ 1 , dist="weibull" )
summary(Weib.model)
#
exp.model <- survreg( Surv(df$time,df$status) ~ 1 , dist="exponential" )
summary(exp.model)
# here, we see the coef of 2.0669 - minor change since I need to resample
# recall, to interpet this as an efffect on the rate/hazard, we should
# take the negative of this...
#   coef= -2.0669
exp(-2.0669)
#or
exp(-coef(exp.model)) # this is the constant rate/hazard... over the period of 12 year?
#Try my TreeAge Model
dftreeage<-read.csv("C:/Users/Tri Van/Desktop/1Year_analysis/treeage.csv")
library(survival)
km.treeage<-survfit(Surv(dftreeage$t_lifeyear,dftreeage$t_numberofdeath)~1,type="kaplan-meier")
km.treeage
plot(km.treeage, main ="TreeAge KM curve",xlab="Time (years)",ylab="Survival Prob", col=c(1,5,10),lty=c(1,2,3),lwd=2)

#Try my TreeAge Model with multiple Profiles
dftreeage1<-read.csv("C:/Users/Tri Van/Desktop/1Year_analysis/treeage1.csv")
library(survival)
km.treeage1<-survfit(Surv(dftreeage1$t_lifeyear,dftreeage1$t_numberofdeath)~dftreeage1$Profile,type="kaplan-meier")
km.treeage1
plot(km.treeage1, main ="TreeAge KM curve",xlab="Time (years)",ylab="Survival Prob", col=c(1,5,10),lty=c(1,2,3),lwd=2)

#Try my TreeAge Model with multiple Profiles (1 year)#
dftreeage2<-read.csv("C:/Users/Tri Van/Desktop/1Year_analysis/treeage2.csv")
library(survival)
km.treeage2<-survfit(Surv(dftreeage2$t_lifeweek,dftreeage2$t_numberofdeath)~dftreeage2$Profile,type="kaplan-meier")
km.treeage2
plot(km.treeage2, main ="TreeAge KM curve 1 year",xlab="Time (Weeks)",ylab="Survival Prob", col=c(1,2,3,4,5,6,7,8),lty=c(1,2,3,4,5,6,7,8),lwd=2)
plot(km.treeage2, main ="TreeAge KM curve 1 year",xlab="Time (Weeks)",ylab="Survival Prob", col=c(1,2,3,4,5,6,7,8),lty=c(1,2,3,4,5,6,7,8),lwd=2, ylim=c(0.85,1.0),)
