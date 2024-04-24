##### 
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
 #Extract data from IPDfromFM package and some basic KM graph
##### 
#V7_1Yr_Death#
library(readxl)
dfV7_1Yr_Death<-read_excel("C:/Users/Tri Van/Desktop/1Year_analysis/V7_1Yr_Death.xlsx")
library(survival)
km.V7_1Yr_Death<-survfit(Surv(dfV7_1Yr_Death$t_week_till_death,dfV7_1Yr_Death$t_numberofdeath)~dfV7_1Yr_Death$Profile,type="kaplan-meier")
km.V7_1Yr_Death
plot(km.V7_1Yr_Death, main ="TreeAge KM curve 1 year",xlab="Time (Weeks)",ylab="Survival Prob", col=c(1,2,3,4,5,6,7,8),lty=c(1,2,3,4,5,6,7,8),lwd=2)
plot(km.V7_1Yr_Death, main ="TreeAge KM curve 1 year",xlab="Time (Weeks)",ylab="Survival Prob", col=c(1,2,3,4,5,6,7,8),lty=c(1,2,3,4,5,6,7,8),lwd=2, ylim=c(0.90,1.0),)

# Plotting Kaplan-Meier survival curve
plot(
  km.V7_1Yr_Death,
  main ="TreeAge KM curve 1 year",
  xlab="Time (Weeks)",
  ylab="Survival Prob",
  col=c(1,2,3,4,5,6,7,8,9,10,11,12),
  lty=c(1,2,3,4,5,6,7,8,9,10,11,12),
  lwd=2,
  ylim=c(0.90, 1.0),
  grid=TRUE  # Adding grid lines
)

# Creating a data frame for the Kaplan-Meier plot

# Calculating survival curves for each profile
surv_curves <- survfit(Surv(t_week_till_death, t_numberofdeath) ~ Profile, data = dfV7_1Yr_Death,type="kaplan-meier")
summary(surv_curves)
plot(surv_curves)

# Calculate the total number of profiles and repetitions
num_profiles <- 6
repetitions <- 1

# Create the Profile column
surv_df <- data.frame(
  time = surv_curves$time,
  surv = surv_curves$surv,
  upper = surv_curves$upper,
  lower = surv_curves$lower
)
# Determine the total number of profiles (6 profiles repeated 2 times each)
total_profiles <- 6
repetitions <- 2
# Create an empty vector for the profile sequence
profile_sequence <- character()
# Generate the profile sequence as required
for (i in 1:total_profiles) {
  profile_sequence <- c(profile_sequence, rep(paste0("Profile_", i, letters[1:repetitions]), each = 53))
}
# Truncate the sequence to match the desired length
profile_sequence <- profile_sequence[1:636]
# Append the 'Profile' column to the surv_df dataframe
surv_df$Profile <- profile_sequence
# Creating the Kaplan-Meier plot using ggplot2
library(ggplot2)
ggplot(data = surv_df, aes(x = time, y = surv, color = Profile, shape = Profile)) +
  geom_step() +
  labs(
    title = "Kaplan-Meier Survival Curve - 1 Year",
    x = "Time (Weeks)",
    y = "Survival Prob"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_discrete(name = "Profiles")



##### 
library(cmprsk)
# Compute cumulative incidence curves - Death
cum_inc_curve <- cuminc(dfV7_1Yr_Death$t_week_till_death, dfV7_1Yr_Death$t_numberofdeath, dfV7_1Yr_Death$Profile)

par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
#           main ="Cumulative incidence of death",  
            ylim=c(0, 0.2),
#           wh=c(45,0.04), 
            wh=c(0,10),  #Move legends out of the way
            xlab ="Weeks",  
            col=c(1,2,3,4,5,6,7,8,9,10,11,12),
            lty=c(1,2,3,4,5,6,7,8,9,10,11,12),
            lwd=2)
legend("topleft",
       legend = c("Profile 1a", "Profile 1b", "Profile 2a", "Profile 2b", "Profile 3a", "Profile 3b", 
                  "Profile 4a", "Profile 4b", "Profile 5a", "Profile 5b", "Profile 6a", "Profile 6b"),
       col = 1:12,
       lty = 1:12,
       lwd = 2,
       title = "Profiles",
       cex = 0.8,  # Adjust this value to change the font size within the legend
       bg = "white",  # Background color of the legend box
       box.lty = 0,  # Set box line type to 0 for no box
       bty = "n")   # Set legend box type to "n" for no box

# Compute cumulative incidence curves - 1st HFH
dfV7_1Yr_HFH<-read_excel("C:/Users/Tri Van/Desktop/1Year_analysis/V7_1Yr_HFH.xlsx")
cum_inc_curve <- cuminc(ftime=dfV7_1Yr_HFH$Weeks, fstatus=dfV7_1Yr_HFH$HFH_outcomes, group=dfV7_1Yr_HFH$Profile)

print(cum_inc_curve)

par(cex = 0.7)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
#           main ="Cumulative incidence of HFH",  
            ylim=c(0, 0.2),
#           wh=c(45,0.06), 
            wh=c(0,10),  #Move legends out of the way
            xlab ="Weeks",  
            col=c(1,2,3,4,5,6,7,8,9,10,11,12),
            lty=c(1,2,3,4,5,6,7,8,9,10,11,12),
            lwd=2)
# Now, add a manual legend using the legend() function
legend("bottomright",
       legend = c("Profile 1a", "Profile 1b", "Profile 2a", "Profile 2b", "Profile 3a", "Profile 3b", 
                  "Profile 4a", "Profile 4b", "Profile 5a", "Profile 5b", "Profile 6a", "Profile 6b"),
       col = 1:12,
       lty = 1:12,
       lwd = 2,
       title = "Profiles",
       cex = 0.8,  # Adjust this value to change the font size within the legend
       bg = "white",  # Background color of the legend box
       box.lty = 0,  # Set box line type to 0 for no box
       bty = "n")   # Set legend box type to "n" for no box

##### 
# split into 2 figures – biweekly (Va) and weekly (Vb) profiles (both with untreated for comparison – for each outcome? 
# Use same colors for profiles 1a and 1b, and so forth? Use full lines for biweekly and dotted lines for weekly?
library(cmprsk)
library(readxl)
#Import data
# Set the directory where the file is located
file_directory <- getwd()
dfV7_1Yr_Death<-read_excel(file.path(file_directory, "V7_1Yr_Death.xlsx"))
dfV7_1Yr_HFH  <-read_excel(file.path(file_directory, "V7_1Yr_HFH.xlsx"))
#Split data in weekly and biweekly
# Subset for death
Death_profile_biweekly <- subset(dfV7_1Yr_Death, Profile %in% c("Untreated", "Profile 1a", "Profile 2a",
                                                                "Profile 3a", "Profile 4a",
                                                                "Profile 5a", "Profile 6a"))
Death_profile_weekly   <- subset(dfV7_1Yr_Death, Profile %in% c("Untreated", "Profile 1b", "Profile 2b",
                                                                "Profile 3b", "Profile 4b",
                                                                "Profile 5b", "Profile 6b"))
# Subset for HFH
HFH_profile_biweekly   <- subset(dfV7_1Yr_HFH, Profile %in% c("Untreated", "Profile 1a", "Profile 2a",
                                                                "Profile 3a", "Profile 4a",
                                                                "Profile 5a", "Profile 6a"))
HFH_profile_weekly     <- subset(dfV7_1Yr_HFH, Profile %in% c("Untreated", "Profile 1b", "Profile 2b",
                                                                "Profile 3b", "Profile 4b",
                                                                "Profile 5b", "Profile 6b"))
# Compute cumulative incidence curves - Death
#biweekly
cum_inc_curve <- cuminc(Death_profile_biweekly$t_week_till_death, Death_profile_biweekly$t_numberofdeath, Death_profile_biweekly$Profile)
par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
#           main ="Cumulative incidence of death",  
            ylim=c(0, 0.2),
#           wh=c(45,0.04), 
            wh=c(0,10),  #Move legends out of the way
            xlab ="Weeks",  
            col = 1:7,
            lty = 1,
            lwd = 2)
legend("topleft",
       legend = c("Profile 1a", "Profile 2a", "Profile 3a", "Profile 4a", "Profile 5a", "Profile 6a", "Untreated"),
       col = 1:7,
       lty = 1,
       lwd = 2,
       title = "Profiles",
       cex = 0.8,  # Adjust this value to change the font size within the legend
       bg = "white",  # Background color of the legend box
       box.lty = 0,  # Set box line type to 0 for no box
       bty = "n")   # Set legend box type to "n" for no box
#Weekly
cum_inc_curve <- cuminc(Death_profile_weekly$t_week_till_death, Death_profile_weekly$t_numberofdeath, Death_profile_weekly$Profile)
par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
#           main ="Cumulative incidence of death",  
            ylim=c(0, 0.2),
#           wh=c(45,0.04), 
            wh=c(0,10),  #Move legends out of the way
            xlab ="Weeks",  
            col = 1:7,
            lty = 3,
            lwd = 2)
legend("topleft",
       legend = c("Profile 1b", "Profile 2b", "Profile 3b", "Profile 4b", "Profile 5b", "Profile 6b", "Untreated"),
       col = 1:7,
       lty = 3,
       lwd = 2,
       title = "Profiles",
       cex = 0.8,  # Adjust this value to change the font size within the legend
       bg = "white",  # Background color of the legend box
       box.lty = 0,  # Set box line type to 0 for no box
       bty = "n")   # Set legend box type to "n" for no box
# Compute cumulative incidence curves - HFH
#biweekly
cum_inc_curve <- cuminc(ftime=HFH_profile_biweekly$Weeks, fstatus=HFH_profile_biweekly$HFH_outcomes, group=HFH_profile_biweekly$Profile)
par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
#           main ="Cumulative incidence of death",  
            ylim=c(0, 0.2),
#           wh=c(45,0.04), 
            wh=c(0,10),  #Move legends out of the way
            xlab ="Weeks",  
            col = 1:7,
            lty = 1,
            lwd = 2)
legend("topleft",
       legend = c("Profile 1a", "Profile 2a", "Profile 3a", "Profile 4a", "Profile 5a", "Profile 6a", "Untreated"),
       col = 1:7,
       lty = 1,
       lwd = 2,
       title = "Profiles",
       cex = 0.8,  # Adjust this value to change the font size within the legend
       bg = "white",  # Background color of the legend box
       box.lty = 0,  # Set box line type to 0 for no box
       bty = "n")   # Set legend box type to "n" for no box
#Weekly
cum_inc_curve <- cuminc(ftime=HFH_profile_weekly$Weeks, fstatus=HFH_profile_weekly$HFH_outcomes, group=HFH_profile_weekly$Profile)
par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
#           main ="Cumulative incidence of death",  
            ylim=c(0, 0.2),
#           wh=c(45,0.04), 
            wh=c(0,10),  #Move legends out of the way
            xlab ="Weeks",  
            col = 1:7,
            lty = 3,
            lwd = 2)
legend("topleft",
       legend = c("Profile 1b", "Profile 2b", "Profile 3b", "Profile 4b", "Profile 5b", "Profile 6b","Untreated"),
       col = 1:7,
       lty = 3,
       lwd = 2,
       title = "Profiles",
       cex = 0.8,  # Adjust this value to change the font size within the legend
       bg = "white",  # Background color of the legend box
       box.lty = 0,  # Set box line type to 0 for no box
       bty = "n")   # Set legend box type to "n" for no box
