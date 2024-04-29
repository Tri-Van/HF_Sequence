rm(list=ls())
library(readxl)
library(survival)
library(ggplot2)
library(cmprsk)

##### 
#V7_1Yr_Death#
dfV7_1Yr_Death<-read_excel("Data/V7_1Yr_Death.xlsx")
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
rm(list=ls())
library(cmprsk)
library(readxl)
library(ggplot2)
#Import data
# Set the directory where the file is located
file_directory <- getwd()
dfV7_1Yr_Death<-read_excel(file.path(file_directory, "Data/V7_1Yr_Death.xlsx"))
dfV7_1Yr_HFH  <-read_excel(file.path(file_directory, "Data/V7_1Yr_HFH.xlsx"))
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
           main ="(A) Cumulative Incidence of Death Biweekly Adjustments",  
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
#Export png as 600-600 Figure3A_Death_a

#Weekly
cum_inc_curve <- cuminc(Death_profile_weekly$t_week_till_death, Death_profile_weekly$t_numberofdeath, Death_profile_weekly$Profile)
par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
           main ="(B) Cumulative Incidence of Death Weekly Adjustments",  
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
# Export png 600-600 Figure3B_Death_b

# Compute cumulative incidence curves - HFH
#biweekly
cum_inc_curve <- cuminc(ftime=HFH_profile_biweekly$Weeks, fstatus=HFH_profile_biweekly$HFH_outcomes, group=HFH_profile_biweekly$Profile)
par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
           main ="(C) Cumulative Incidence of HF Hospitalization Biweekly Adjustments",  
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
#Export 600-600 Figure3C_HFH_a

#Weekly
cum_inc_curve <- cuminc(ftime=HFH_profile_weekly$Weeks, fstatus=HFH_profile_weekly$HFH_outcomes, group=HFH_profile_weekly$Profile)
par(cex = 0.8)  # Adjust this value to change the font size of legends
plot.cuminc(cum_inc_curve,
           main ="(D) Cumulative Incidence of HF Hospitalization Weekly Adjustments",  
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
#Export 600-600 Figure3D_HFH_b


