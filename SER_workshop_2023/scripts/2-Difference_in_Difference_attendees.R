#------------------------------------------------------------------------------#
#-----An overview of Difference-in-Difference and Synthetic Control Methods----- 
#-------------------------Classical and Novel Approaches-----------------------#
#-----------Society for Epidemiologic Research (SER)---------------------------# 
#---------------------------------Workshop: Part 2/4---------------------------#
#-----------------------------Date:07/07/23------------------------------------#
#Roch Nianogo (niaroch@ucla.edu) & Tarik Benmarhnia (tbenmarhnia@health.ucsd.edu)
#------------------------------------------------------------------------------#

#------------------------------------Checking the directory---------------------
getwd()


#-------------------------------------Installing packages-----------------------


if (!require("pacman")){
  install.packages("pacman", repos = 'http://cran.us.r-project.org')
} # a nice package to load several packages simultaneously


p_load("tidyverse","magrittr","broom",        #manipulate data
       "mise",                                #mise en place 
       "here",                                #directory managment
       "Synth", "gsynth",                     #synthetic control
       "panelView", "lme4", "estimatr",       #multi-level modeling
       "gtsummary")                           #for tables

#------------------Mise en place (clear everthing)----------------------------
mise() #same as remove(list=ls())





#---------------------------------Loading the data------------------------------
#Use the updated data


#-----------------------------------------Analysis------------------------------


#--------------------------------------------------------#
#----------------Preliminary notions----------------------
#-----(pre-post designs, controlled pre-post desings and--
#------Interrupted time series designs)-------------------
#This part is to illustrate other related designs
#---------------------------------------------------------#

#-----------------------#
#----Pre-post designs----
#-----------------------#
#Pre-post analysis (before-after) no control group: one state, two time points

#Subset the data to California and the years 1995 and 2005
#Preview the data
#Plot the data
#Fit a linear model to estimate the effect of the policy on the outcome y
#What are potential problems

##Create the data
dt <- mydata %>%
  filter(state=="California",
         year %in% c(1995, 2005)) 


##Preview the data
head(dt)


##Plot the data
dt %>% 
  ggplot(aes(x=year, y=y, group=state, color = state)) + 
  labs(title = paste("Outcome by year"),
       x = "Year", 
       y = "Outcome",
       colour = "Treatment") +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = year_policy, lty=2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

##fit a linear model using the post as the exposure of interest
fit <- lm(y ~ post + xi + xt + xit, data=dt)
tidy(fit)

# Potential problems
#-------------------#
# i.	The years before and after the policy have been implemented are picked arbitrarily
# ii.	It is impossible to get standard errors
# iii.	It is impossible to adjust for covariates
# iv.	This model would be wrong if the parallel trend assumption is violated
# v.	This model would be wrong if the common schock assumption is violated
# Conclusion 
# As expected the estimate is biased and standard errors inestimable


#----------------------------------#
#----Controlled Pre-post designs----
#----------------------------------#
#2 Pre-post analysis (before-after) with a control group: Two states, two time points


#Subset the data to California and Georgia and the years 1995 and 2005
#Preview the data
#Plot the data
#Fit a linear model to estimate the effect of the policy on the outcome y
#What are potential problems

##Create the data
dt1 <- mydata %>% 
  filter(state=="California" | state=="Georgia",
         year %in% c(1995, 2005))


##Preview the data
head(dt1)


##Plot the data
dt1 %>% 
  ggplot(aes(x=year, y=y, group=state, color = state)) + 
  labs(title = paste("Outcome by year"),
       x = "Year", 
       y = "Outcome",
       colour = "Treatment") +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = year_policy, lty=2) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

##fit a linear model using the post as the exposure of interest
fit <- lm(y ~ treated*post, data=dt1) 
fit <- lm(y ~ treated + post + treated:post, data=dt1) 
summary(fit)
tidy(fit)

# Potential problems 
#--------------------#
# i.	The years before and after the policy have been implemented are picked 
# arbitrarily 
# ii.	The control units are picked arbitrarily
# iii.	It is impossible to get standard errors
# iv.	It is impossible to adjust for covariates
# v.	This model would be wrong if the parallel trend assumption is violated
# vi.	This model would be wrong if the common shock assumption is violated

# Conclusion 
# As expected the estimate is biased and standard errors inestimable



#---------------------------------------#
#----Interrupted Time Series Design----
#---------------------------------------#
###Interupted time series: One state, Multiple time points


#Subset the data to California
#Preview the data
#Plot the data
#Fit a linear model to estimate the effect of the policy on the outcome y
#What are potential problems

##Create the data



##Preview the data



##Plot the data



##Fit the model



#-------------------------------------------------#
#-----Controlled Interrupted Time Series (CITS)----
#----or Difference-in-Difference Designs-----------
#-------------------------------------------------#

# Manual DID
#Let's obaint the DID manually
#1)Obtain the average outcome y among the treated in the pre-policy period
#2)Obtain the average outcome y among the treated in the post-policy period
#3)Obtain the average outcome y among the controls in the pre-policy period
#4)Obtain the average outcome y among the controls in the post-policy period

#Estimate the DID by before/after difference in treated - before/after difference in control
#Estimate the DID by control/treated difference in the post period - control/treated difference in the pre period





#---Step 1: Checking the parallel trends assumption-----

#Check the parallel trends assumptions
#To do so
#restrict the data to the period before the policy
#Run a linear regression of the outcome on time-varying
#covariates and on an interaction term between treated indicator and year
#Pay particular attention to the value and p-value value of
# treated:year.

#can also try visualizing the trends
  #hint you can copy some code from the intro set of codes


#---Step 2: Implementing the DID analysis method-----
# Fit a linear model with the treated, post and their interaction
# Don't forget to add any other time-varying covariates
# What is the regression coefficient of the interaction term


