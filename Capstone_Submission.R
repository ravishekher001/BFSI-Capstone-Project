####################################################################################################################
# BUISNESS UNDERSTANDING                                                                                           #
# ================================================================================================================ #
# 1. CredX is a leading credit card provider receiving huge volume of applications every year.                     #
# 2. In recent times unfortunately CredX is experiencing an increase in credit loss due to not reaching the right  #
#    customers during acquisition.                                                                                 #
####################################################################################################################

####################################################################################################################
# PROBLEM STATEMENT                                                                                                #
# ================================================================================================================ #
# 1. For a provider like CredX it becomes very important to acquire right customers in order to increase their     #
#    profitability by keeping their business costs in control.                                                     #
# 2. In this Project, We help CredX in exactly doing the same. We using our various predictive models help CredX   #
#    acquire the right customers there by increasing their profits.                                                #
# 3. In this process we also use our predictive models in determining the factors affecting the credit risk and    #
#    creating the strategies to mitigate the acquisition risks.                                                    #                                                                                   #
####################################################################################################################

####################################################################################################################
# OBJECTIVE                                                                                                        #
# ================================================================================================================ #
# 1. Predict the probability of default if credit card is approved ?                                               #
# 2. Create a scorecard for each applicant.                                                                        #
# 3. Decide a cut-off by balancing trade-off between approval rate and risk of credit loss.                        #
####################################################################################################################

# REMOVING ENVIRONMENTAL VARIABLES
rm(list = ls())

###################
# LIBRARY LOADING #
###################

library(data.table) 
library(dplyr)      
library(ggplot2)    
library(lattice)    
library(caret)      
library(corrplot)   
library(cowplot)    
library(caTools)    
library(MASS)       
library(ROCR)       
library(lift)       
library(car)        
library(woe)    
library(Information)
library(plyr)
library(caret)
library(caTools)
library(MASS)
library(car)
library(ROSE)
library(mlr)
library(ROSE)
library(rpart)
library(rpart.plot)
library(kernlab)
library(readr)
library(randomForest)

################
################
# DATA LOADING #
################
################

demographic <- read.csv("Demographic data.csv", header = TRUE, stringsAsFactors = F, na.strings = c("NA", "", " "))
credit <- read.csv("Credit Bureau data.csv", header = TRUE, stringsAsFactors = F, na.strings = c("NA", "", " "))

##############################################
# DEMOGRAPHIC DATA ANALYSIS & UNDERSTANDINGS #
##############################################

# Application ID:	Unique ID of the customers
# Age:	Age of customer
# Gender:	Gender of customer
# Marital Status:	Marital status of customer (at the time of application)
# No of dependents:	No. of childrens of customers
# Income:	Income of customers
# Education:	Education of customers
# Profession:	Profession of customers
# Type of residence:	Type of residence of customers
# No of months in current residence:	No of months in current residence of customers
# No of months in current company:	No of months in current company of customers
# Performance Tag:	Status of customer performance (1 represents "Default")

########################
########################
# DATA DUPLICACY CHECK #
########################
########################

# Check the duplicates in demogrphic data source.
# -----------------------------------------------
sum(duplicated(demographic))
# OBSERVATION - All the rows are unique
# -------------------------------------

# Check the duplicates in application ids only. since this is a primay key and if it is duplicated, we have to remove it.
# -----------------------------------------------------------------------------------------------------------------------
sum(duplicated(demographic$Application.ID))
sum(duplicated(credit$Application.ID))
# OBSERVATION - Both the datasets have 71295 rows
# -----------------------------------------------

# Now let us see the unique application Ids count in demographic dataset
# ----------------------------------------------------------------------
length(unique(tolower(demographic$Application.ID)))
# OBSERVATION - 71292 Records & 3 Duplicate Records
# -------------------------------------------------

# Now let us see the unique application Ids count in credit dataset
# -----------------------------------------------------------------
length(unique(tolower(credit$Application.ID)))
# OBSERVATION - 71292 Records & 3 Duplicate Records
# -------------------------------------------------

# Let us explore the duplicate application Ids data
# -------------------------------------------------
demographic[duplicated(demographic$Application.ID),] 
credit[duplicated(credit$Application.ID),]

# OBSERVATION - After reviewing both the dataset of duplicate Ids, that is a discrepency in both the datasets for the same users
#               We will remove these duplicate Application Ids from Demographic dataset.
# ------------------------------------------------------------------------------------------------------------------------------
demographic <- demographic[!demographic$Application.ID %in% demographic[duplicated(demographic$Application.ID), "Application.ID"],]
sum(duplicated(demographic$Application.ID))

# We will remove these duplicate Application Ids from credit bureau dataset.
credit <- credit[!credit$Application.ID %in% credit[duplicated(credit$Application.ID), "Application.ID"],]
sum(duplicated(credit$Application.ID))

# OBSERVATION - 765011468, 653287861 & 671989187 are duplicate application id in both the datasets
# ------------------------------------------------------------------------------------------------ 

##########################################
##########################################
# DATA QUALITY CHECK - DEMOGRAPHIC DATA  #
##########################################
##########################################

# Check for NA's in demographic dataset
# -------------------------------------
sum(is.na(demographic))
# OBSERVATION - Many records are NA as count shows 1577.
# ------------------------------------------------------

# Feature which have NA columns in demographic dataset
# ----------------------------------------------------
colSums(is.na(demographic))>0
colSums(is.na(select_if(demographic, colSums(is.na(demographic))>0)))
# OBSERVATION - These feature have NA values
# ------------------------------------------
# Gender, MaritalStatus, No of Dependents, Education, Profession, Type Of Residence & Performance Tag

# NA Values are there in these columns except performance Tag.
sapply(demographic[,c(3,4,5,7,8,9,12)], function(x) unique(x)) 
str(demographic)

##########################################
##########################################
# UNIVARIATE ANALYSIS - DEMOGRAPHIC DATA #
##########################################
##########################################

# Demographic Data - Performance Tag
# ----------------------------------
PerformanceDistribution <- demographic %>% group_by(Performance.Tag) %>% 
  dplyr::summarise(Count = n()) %>% 
                mutate(freq = format((Count * 100)/sum(Count)))

ggplot(PerformanceDistribution) + geom_bar(aes(Performance.Tag, Count), stat = "Identity", fill = "red")+ 
  geom_label(aes(Performance.Tag, Count), label = PerformanceDistribution$freq, vjust = 0.5)

# OBSERVATION - DIstribution of Performance Tag is Imbalanced where 1 represents Defaults and has share only 4.21 percent in the data (We wuld fix this issue later)
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Demographic Data - Age
# ----------------------
AgeDistribution <- demographic %>% group_by(Age) %>% 
                dplyr::summarise(Count = n()) %>% 
            mutate(freq = format((Count * 100)/sum(Count)))

ggplot(AgeDistribution) + geom_bar(aes(Age, Count), stat = "Identity", fill="red")
boxplot(demographic$Age, horizontal = T)
quantile(demographic$Age, seq(0,1,0.001))

# OBSERVATION - Since we will use Weight of evedence replacement value by binning the features, WOE will handle the Missing values and outliers
# But for the analysis sake and with the logic we would replace any age less than 18 to 18, because 18 is the legal age to get the credit card.
# ---------------------------------------------------------------------------------------------------------------------------------------------
demographic$Age[demographic$Age < 18] <- 18

ggplot(demographic, aes(x=Age)) + geom_histogram(aes(y=..density..),  binwidth=.5,
      colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Age, na.rm=T)), color="red", linetype="dashed", size=1)

# OBSERVATION - Most users are in  30 to early 50 age range. Some outliers(invalid age i.e. -3) values are present.
# ----------------------------------------------------------------------------------------------------------------- 

# Demographic Data - Gender
# -------------------------

# Some NA values exists, since male is the mode of this data, we would replace the na values with male.
demographic$Gender[which(is.na(demographic$Gender))] <- "M"

# `GenderDistribution`
# --------------------
GenderDistribution <- demographic %>% group_by(Gender) %>% dplyr::summarise(Count = n()) %>% mutate(Freq = (Count * 100)/sum(Count))

ggplot(GenderDistribution) + geom_bar(aes(Gender, Count), stat = "Identity") + 
              geom_label(aes(Gender, Count), label = GenderDistribution$Freq)

# `GenderPerformanceDistribution`
# -------------------------------
GenderPerformanceDistribution <- demographic %>% group_by(Gender, Performance.Tag) %>% dplyr::summarise(Count = n()) %>% mutate(Freq = (Count * 100)/sum(Count))

ggplot(GenderPerformanceDistribution) + geom_bar(aes(Gender, Count), stat = "identity", fill = "red") +
                       geom_label(aes(Gender, Count, label = GenderPerformanceDistribution$Freq), vjust = 0.5)

str(demographic)

# Demographic Data - Marital Status
# ---------------------------------

# Some NA values exists, since `Married` is the mode of this data, we would replace the na values with `Married`.
demographic$Marital.Status..at.the.time.of.application.[which(is.na(demographic$Marital.Status..at.the.time.of.application.))] <- "Married"

histogram(as.factor(demographic$Marital.Status..at.the.time.of.application.))
ggplot(demographic %>% group_by(Marital.Status..at.the.time.of.application., Performance.Tag) %>% dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Marital.Status..at.the.time.of.application., Count), stat = "identity", fill = "green") +
  geom_label(aes(Marital.Status..at.the.time.of.application., Count, label = freq), vjust = 0.5)

# Demographic Data - Education
# ----------------------------

# Some NA values exists, since `Not Captured` is the mode of this data, we would replace the na values with `Not Captured`.
demographic$Education[which(is.na(demographic$Education))] <- "Not Captured"

histogram(as.factor(demographic$Education))
ggplot(demographic %>% group_by(Education, Performance.Tag) %>% dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Education, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Education, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Demographic Data - Profession
# -----------------------------

# Some NA values exists, since `SAL` is the mode of this data, we would replace the na values with `SAL`.
demographic$Profession[which(is.na(demographic$Profession))] <- "SAL"

histogram(as.factor(demographic$Profession))
ggplot(demographic %>% group_by(Profession, Performance.Tag) %>% dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(Profession, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Profession, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Demographic Data - Type Of Residance
# ------------------------------------

# Some NA values exists, since `Rented` is the mode of this data, we would replace the na values with `Rented`.
demographic$Type.of.residence[which(is.na(demographic$Type.of.residence))] <- "Rented"

histogram(as.factor(demographic$Type.of.residence))
ggplot(demographic %>% group_by(Type.of.residence, Performance.Tag) %>% dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Type.of.residence, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Type.of.residence, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Demographic Data - Dependents
# -----------------------------

# Some NA values exists, we would replace the na values with median value
demographic$No.of.dependents[which(is.na(demographic$No.of.dependents))] <- median(demographic$No.of.dependents, na.rm = T)

histogram(demographic$No.of.dependents)
ggplot(demographic %>% group_by(No.of.dependents, Performance.Tag) %>% dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.dependents, Count), stat = "identity", fill = "blue2", width = .5) +
  geom_label(aes(No.of.dependents, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Demographic Data - Income
# -------------------------

# Some NA values exists, since `0` is the mode of this data, we would replace the na values with `0`.
demographic$Income[demographic$Income < 0.0] <- 0.0

ggplot(demographic, aes(x=Income)) + geom_histogram(aes(y=..density..),  binwidth=.5,
          colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Income, na.rm=T)), color="red", linetype="dashed", size=1)

# Demographic Data - Residance DUration
# -------------------------------------

ggplot(demographic, aes(x=No.of.months.in.current.residence)) + geom_histogram(aes(y=..density..),  binwidth=.5,
      colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(No.of.months.in.current.residence, na.rm=T)), color="red", linetype="dashed", size=1)

# Demographic Data - Current Company
# ----------------------------------

ggplot(demographic, aes(x=No.of.months.in.current.company)) + geom_histogram(aes(y=..density..),  binwidth=.5,
   colour="black", fill="white") +  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(No.of.months.in.current.company, na.rm=T)), color="red", linetype="dashed", size=1)

# Violin and BoxPlot
# ------------------

ggplot(demographic, aes(x=Performance.Tag, y=Age)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + 
  geom_boxplot(width=0.1)

ggplot(demographic, aes(x=Performance.Tag, y=No.of.months.in.current.residence)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + 
  geom_boxplot(width=0.1)

ggplot(demographic, aes(x=Performance.Tag, y=No.of.months.in.current.company)) + geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + 
  geom_boxplot(width=0.1)

# Visualisation to help binning out continuous Variables
# ------------------------------------------------------

ggplot(demographic %>% group_by(Age, Performance.Tag) %>% 
         dplyr::summarise(Count_By_PerformanceTag = n())) + 
  geom_point(aes(x=Age, y=Count_By_PerformanceTag, color = factor(Performance.Tag)))

ggplot(demographic %>% group_by(Income, Performance.Tag) %>% 
         dplyr::summarise(Count_By_PerformanceTag = n())) + 
  geom_point(aes(x=Income, y=Count_By_PerformanceTag, color = factor(Performance.Tag)))

ggplot(demographic %>% group_by(No.of.months.in.current.company, Performance.Tag) %>% 
         dplyr::summarise(Count_By_PerformanceTag = n())) + 
  geom_point(aes(x=No.of.months.in.current.company, y=Count_By_PerformanceTag, color = factor(Performance.Tag)))


ggplot(demographic %>% group_by(No.of.months.in.current.residence, Performance.Tag) %>%
         dplyr::summarise(Count_By_PerformanceTag = n())) +
  geom_point(aes(x=No.of.months.in.current.residence, y=Count_By_PerformanceTag, color = factor(Performance.Tag)))

demographic$Gender <- as.factor(demographic$Gender)
demographic$Marital.Status..at.the.time.of.application. <- as.factor(demographic$Marital.Status..at.the.time.of.application.)
demographic$Education <- as.factor(demographic$Education)
demographic$Profession <- as.factor(demographic$Profession)
demographic$Type.of.residence <- as.factor(demographic$Type.of.residence)

###########################################
###########################################
# DATA QUALITY CHECK - CREDIT BUREAU DATA #
###########################################
###########################################

str(credit)

######################
## Credit Card Data ##
######################

# Application ID:	Customer application ID
# No of times 90 DPD or worse in last 6 months:	Number of times customer has not payed dues since 90days in last 6 months
# No of times 60 DPD or worse in last 6 months:	Number of times customer has not payed dues since 60 days last 6 months
# No of times 30 DPD or worse in last 6 months:	Number of times customer has not payed dues since 30 days days last 6 months
# No of times 90 DPD or worse in last 12 months:	Number of times customer has not payed dues since 90 days days last 12 months
# No of times 60 DPD or worse in last 12 months:	Number of times customer has not payed dues since 60 days days last 12 months
# No of times 30 DPD or worse in last 12 months:	Number of times customer has not payed dues since 30 days days last 12 months
# Avgas CC Utilization in last 12 months:	Average utilization of credit card by customer
# No of trades opened in last 6 months:	Number of times the customer has done the trades in last 6 months
# No of trades opened in last 12 months:	Number of times the customer has done the trades in last 12 months
# No of PL trades opened in last 6 months:	No of PL trades in last 6 month of customer
# No of PL trades opened in last 12 months:	No of PL trades in last 12 month of customer
# No of Inquiries in last 6 months (excluding home & auto loans):	Number of times the customers has inquired in last 6 months
# No of Inquiries in last 12 months (excluding home & auto loans):	Number of times the customers has inquired in last 12 months
# Presence of open home loan:	Is the customer has home loan (1 represents "Yes")
# Outstanding Balance:	Outstanding balance of customer
# Total No of Trades:	Number of times the customer has done total trades
# Presence of open auto loan:	Is the customer has auto loan (1 represents "Yes")
# Performance Tag:	Status of customer performance (1 represents "Default")


# Check for NA's in Credit dataset
# --------------------------------
sum(is.na(credit))
# OBSERVATION - 3028 NA Values Observed
# -------------------------------------

# Feature which have NA columns in demographic dataset
# ----------------------------------------------------
colSums(is.na(credit))>0
colSums(select_if(credit, colSums(is.na(credit))>0))
# OBSERVATION - These Features have NA values
# -------------------------------------------
# AVerage CC Utilization Last 12 months, No Of trades, Home loan, Outstanding balance & Performance Tag

sapply(credit, function(x){sum(is.na(x))})
#credit$Performance.Tag <- as.factor(credit$Performance.Tag)
credit$No.of.trades.opened.in.last.6.months[which(is.na(credit$No.of.trades.opened.in.last.6.months))] <- median(credit$No.of.trades.opened.in.last.6.months, na.rm = T)

############################################
############################################
# UNIVARIATE ANALYSIS - CREDIT BUREAU DATA #
############################################
############################################

# Credit Bureau Data - Performance Tag
# ------------------------------------
ggplot(credit %>% group_by(Performance.Tag) %>% dplyr::summarise(Count = n()) %>% 
         mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(Performance.Tag, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Performance.Tag, Count, label = freq), vjust = 0.5)

# Credit Bureau Data - No of times 90 DPD or worse in last 6 months
# -----------------------------------------------------------------
ggplot(credit %>% group_by(No.of.times.90.DPD.or.worse.in.last.6.months, Performance.Tag) %>% 
         dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.90.DPD.or.worse.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.90.DPD.or.worse.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Credit Bureau Data - No of times 60 DPD or worse in last 6 months
# -----------------------------------------------------------------
ggplot(credit %>% group_by(No.of.times.60.DPD.or.worse.in.last.6.months, Performance.Tag) %>% 
         dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.60.DPD.or.worse.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.60.DPD.or.worse.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Credit Bureau Data - No of times 30 DPD or worse in last 6 months
# -----------------------------------------------------------------
ggplot(credit %>% group_by(No.of.times.30.DPD.or.worse.in.last.6.months, Performance.Tag) %>% 
         dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.30.DPD.or.worse.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.30.DPD.or.worse.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Credit Bureau Data - No of times 90 DPD or worse in last 12 months
# ------------------------------------------------------------------
ggplot(credit %>% group_by(No.of.times.90.DPD.or.worse.in.last.12.months, Performance.Tag) %>% 
          dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(No.of.times.90.DPD.or.worse.in.last.12.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.90.DPD.or.worse.in.last.12.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Credit Bureau Data - No of times 60 DPD or worse in last 12 months
# ------------------------------------------------------------------
ggplot(credit %>% group_by(No.of.times.60.DPD.or.worse.in.last.12.months, Performance.Tag) %>% 
          dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.times.60.DPD.or.worse.in.last.12.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.60.DPD.or.worse.in.last.12.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Credit Bureau Data - No of times 30 DPD or worse in last 12 months
# ------------------------------------------------------------------
ggplot(credit %>% group_by(No.of.times.30.DPD.or.worse.in.last.12.months, Performance.Tag) %>% 
               dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) +  
  geom_bar(aes(No.of.times.30.DPD.or.worse.in.last.12.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.times.30.DPD.or.worse.in.last.12.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Credit Bureau Data - No of trades opened in last 6 months
# ---------------------------------------------------------
ggplot(credit %>% group_by(No.of.trades.opened.in.last.6.months, Performance.Tag) %>% 
               dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(No.of.trades.opened.in.last.6.months, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(No.of.trades.opened.in.last.6.months, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Violin and BoxPlot & Visualisation to help binning out continuous Variables
# ---------------------------------------------------------------------------
ggplot(credit, aes(x=Performance.Tag, y=No.of.trades.opened.in.last.12.months)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

ggplot(credit, aes(x=Performance.Tag, y=No.of.PL.trades.opened.in.last.6.months)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

ggplot(credit, aes(x=Performance.Tag, y=No.of.PL.trades.opened.in.last.12.months)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

ggplot(credit, aes(x=Performance.Tag, y=No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

ggplot(credit, aes(x=Performance.Tag, y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + geom_boxplot(width=0.1)

ggplot(credit %>% group_by(Presence.of.open.home.loan, Performance.Tag) %>% 
         dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Presence.of.open.home.loan, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Presence.of.open.home.loan, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(credit, aes(x=Performance.Tag, y=Total.No.of.Trades)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + 
  geom_boxplot(width=0.1)

credit$Presence.of.open.home.loan[which(is.na(credit$Presence.of.open.home.loan))] <- 99
str(credit$No.of.trades.opened.in.last.12.months)

ggplot(credit %>% group_by(Presence.of.open.home.loan, Performance.Tag) %>% 
         dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Presence.of.open.home.loan, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Presence.of.open.home.loan, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(credit %>% group_by(Presence.of.open.auto.loan, Performance.Tag) %>% 
         dplyr::summarise(Count = n()) %>% mutate(freq = format((Count * 100) / sum(Count)))) + 
  geom_bar(aes(Presence.of.open.auto.loan, Count), stat = "identity", fill = "blue2") +
  geom_label(aes(Presence.of.open.auto.loan, Count, label = freq), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

credit$Avgas.CC.Utilization.in.last.12.months[which(is.na(credit$Avgas.CC.Utilization.in.last.12.months))] <- 199

ggplot(credit, aes(x=Performance.Tag, y=Avgas.CC.Utilization.in.last.12.months)) + 
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + 
  geom_boxplot(width=0.1)

summary(credit$Outstanding.Balance)

credit$Outstanding.Balance[which(is.na(credit$Outstanding.Balance))] <- 0

ggplot(credit, aes(x=Performance.Tag, y=Outstanding.Balance)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred") + 
  geom_boxplot(width=0.1)

ggplot(credit, aes(x=Avgas.CC.Utilization.in.last.12.months)) + 
  geom_histogram(aes(y=..density..),  binwidth=.5, colour="black", fill="white") +  
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Avgas.CC.Utilization.in.last.12.months, na.rm=T)), color="red", linetype="dashed", size=1)

ggplot(credit, aes(x=(Outstanding.Balance/10000))) + 
  geom_histogram(aes(y=..density..),  binwidth=.5,colour="black", fill="white") +  
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(Outstanding.Balance, na.rm=T)/10000), color="red", linetype="dashed", size=1)

# MERGE THE DATA SETS, demographic & credit based on "Application ID" & "Performance Tag"
# ---------------------------------------------------------------------------------------
merged_EDA <- merge(demographic, credit, by = c("Application.ID", "Performance.Tag"))
merged_EDA$Performance.Tag <- as.factor(merged_EDA$Performance.Tag)


#########################################################
#########################################################
# BIVARIATE/ MULTIVARIATE ANALYSIS - CREDIT BUREAU DATA #
#########################################################
#########################################################

# Merged Data - Avgas CC Utilization in last 12 months Vs No of PL trades opened in last 12 months
# ------------------------------------------------------------------------------------------------
ggplot(merged_EDA, aes(x = Avgas.CC.Utilization.in.last.12.months, y = No.of.PL.trades.opened.in.last.12.months, group=Performance.Tag, color=Performance.Tag))+
  geom_line(stat='summary', fun.y='mean') +  
  geom_point(stat='summary', fun.y='mean')
# OBSERVATION - No of PL-trades opened is relatively higher for default users. 
# ----------------------------------------------------------------------------

# Merged Data - No of times 90 DPD or worse in last 6 months Vs Avgas CC Utilization in last 12 months
# ----------------------------------------------------------------------------------------------------
ggplot(data=merged_EDA, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months, y=Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
# OBSERVATION - For default users Avg-CC-utilization is overall higher , Also CC-usage is going high with increasing DPD values. 
# ------------------------------------------------------------------------------------------------------------------------------

# Merged Data - Total No of Trades Vs Outstanding Balance
# -------------------------------------------------------
ggplot(data=merged_EDA, aes(x=Total.No.of.Trades, y=Outstanding.Balance, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean')
# OBSERVATION - Total no of trades is overall in higherno nos for default users. 
#               Also outstanding balance is relatively higher for most of default users.
# --------------------------------------------------------------------------------------


# Merged Data - Income Vs Outstanding Balance
# -------------------------------------------
ggplot(data=merged_EDA, aes(x=Income, y=Outstanding.Balance, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean')
# OBSERVATION - For defaulters Outstanding balance is higher.
# No upward/downward trend for outstanding balance with increasing income.
# If outstanding is more than 12.5lakh its a matter of concern.
# ------------------------------------------------------------------------


# Merged Data - Income Vs Avgas CC Utilization in last 12 months
# --------------------------------------------------------------
ggplot(merged_EDA, aes(x = Income, y = Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag, color=Performance.Tag))+
  geom_line(stat='summary', fun.y='mean') +  
  geom_point(stat='summary', fun.y='mean')
# OBSERVATION - With increasing income avg-cc-usage decreases for whole population.
# If avg cc usage is >40 for a low income, >30 for middle income, >25 for higher income,they should be looked at.
# ---------------------------------------------------------------------------------------------------------------


# Merged Data - Income Vs No of times 90 DPD or worse in last 6 months
# --------------------------------------------------------------------
ggplot(data=merged_EDA, aes(x=Income, y=No.of.times.90.DPD.or.worse.in.last.6.months, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 

# OBSERVATION - With increasing Income, DPD nos are decreasing. 
# Also for defaulting users DPD nos are way higher.
# High no of defaulters are in lower to medium income range. 
# -------------------------------------------------------------

# Merged Data - Income Vs No of Inquiries in last 12 months excluding home and auto loans 
# ---------------------------------------------------------------------------------------
ggplot(data=merged_EDA, aes(x=Income, y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
# OBSERVATION - With increase in income no of inquiries are decreasing for non defaulters.
# With increase in income no of inquiries relatively higher for defaulters.
# --------------------------------------------------------------------------------------

# Merged Data - No of dependents Vs Income
# ----------------------------------------
ggplot(data=merged_EDA, aes(x=No.of.dependents, y=Income, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
# OBSERVATION - Income per no of dependants is very low for defaulters comapared to non-defaulters. 
# -------------------------------------------------------------------------------------------------

# Merged Data - No of Inquiries in last 12 months excluding home and auto loans Vs Total No of Trades
# ---------------------------------------------------------------------------------------------------
ggplot(data=merged_EDA, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., y=Total.No.of.Trades , group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 

# OBSERVATION - With increasing no of inquiries in last 12months, 
# total no of trades increases, then gradually it becomes constant.
# for default users total no of trades is higher.
# -----------------------------------------------------------------


# Creating the Bins to replace it with Weight of Evidence Values
# -----------------------------------------------------------------

# For Demographic data
# --------------------
demographic$Age <- as.character(cut(demographic$Age, breaks = c(0, 18, 30, 35, 38, 41, 44, 47, 50, 53, 57, 65)))
demographic$No.of.dependents <- as.character(cut(demographic$No.of.dependents, breaks = c(-1, 2, 3, 4, 5)))
demographic$Income <- as.character(cut(demographic$Income, breaks = c(-1, 4, 10, 16, 21, 26, 31, 36, 41, 48, 60)))
demographic$No.of.months.in.current.residence <- as.character(cut(demographic$No.of.months.in.current.residence, breaks = c(5, 9, 28, 49, 72, 97, 126)))
demographic$No.of.months.in.current.company <- as.character(cut(demographic$No.of.months.in.current.company, breaks = c(0, 20, 40, 60, 80, 100, 120, 140)))

# For Credit Bureau bedata
# ------------------------
credit$No.of.times.90.DPD.or.worse.in.last.6.months <- as.character(cut(credit$No.of.times.90.DPD.or.worse.in.last.6.months, breaks = c(-1,0,3)))
credit$No.of.times.60.DPD.or.worse.in.last.6.months <- as.character(cut(credit$No.of.times.60.DPD.or.worse.in.last.6.months, breaks = c(-1,0,5)))
credit$No.of.times.30.DPD.or.worse.in.last.6.months <- as.character(cut(credit$No.of.times.30.DPD.or.worse.in.last.6.months, breaks = c(-1,0,1,7)))
credit$No.of.times.90.DPD.or.worse.in.last.12.months <- as.character(cut(credit$No.of.times.90.DPD.or.worse.in.last.12.months, breaks = c(-1,0,1,5)))
credit$No.of.times.60.DPD.or.worse.in.last.12.months <- as.character(cut(credit$No.of.times.60.DPD.or.worse.in.last.12.months, breaks = c(-1,0,1,7)))
credit$No.of.times.30.DPD.or.worse.in.last.12.months <- as.character(cut(credit$No.of.times.30.DPD.or.worse.in.last.12.months, breaks = c(-1,0,2,9)))
credit$No.of.trades.opened.in.last.6.months <- as.character(cut(credit$No.of.trades.opened.in.last.6.months, breaks = c(-1, 0, 1, 2, 3, 4, 12)))
credit$No.of.trades.opened.in.last.12.months <- as.character(cut(credit$No.of.trades.opened.in.last.12.months, breaks = c(-1, 0, 1, 2, 3, 4, 8,12,16,20,24,28)))
credit$No.of.PL.trades.opened.in.last.6.months <- as.character(cut(credit$No.of.PL.trades.opened.in.last.6.months, breaks = c(-1, 0, 1, 2, 6)))
credit$No.of.PL.trades.opened.in.last.12.months <- as.character(cut(credit$No.of.PL.trades.opened.in.last.12.months, breaks = c(-1, 0, 1, 2, 3, 4, 5, 12)))
credit$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.character(cut(credit$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., breaks = c(-1, 0, 1, 2, 4, 10)))
credit$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.character(cut(credit$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = c(-1, 0, 1, 2, 4, 5, 8, 20)))
credit$Presence.of.open.auto.loan <- as.character(cut(credit$Presence.of.open.auto.loan, breaks = c(-1, 0, 1)))
credit$Outstanding.Balance <- as.character(cut(credit$Outstanding.Balance, breaks = c( -1, 6847, 25522, 386837, 585441, 774241, 972458, 1357419, 2961007, 3282409, 5218801)))
credit$Avgas.CC.Utilization.in.last.12.months <- as.character(cut(credit$Avgas.CC.Utilization.in.last.12.months, breaks = c(-1, 5, 7, 9, 12, 15, 22, 38, 52, 72, 113,199)))
credit$Presence.of.open.home.loan <- as.character(cut(credit$Presence.of.open.home.loan, breaks = c(-1, 0, 1,99)))
credit$Total.No.of.Trades <- as.character(cut(credit$Total.No.of.Trades, breaks = c(-1, 0, 5,10,15,20,25,30,35,40,45,50)))

# Check for NA values in Credit Bureau & Demographic Datasets
# -----------------------------------------------------------
sum(is.na(credit))
sum(is.na(demographic))

# Check the structure of Demographic Dataset
# ------------------------------------------
str(demographic)

#############################################
#############################################
# MODELING - To predict the Performance Tag #
#############################################
#############################################

# We would separate the demographic data for rejected population, where performance tag is NA.
# Since performance tag is our target variable we would separatly take care of this later.

DemographicDataRejected <- subset(demographic, is.na(demographic$Performance.Tag))
demographic <- demographic[!is.na(demographic$Performance.Tag),]

CreditDataRejected <- subset(credit, is.na(credit$Performance.Tag))
credit <- credit[!is.na(credit$Performance.Tag),]

bemographicbackup <- demographic
demographic <- bemographicbackup

IV <- create_infotables(demographic[-1], y="Performance.Tag", bins=10, parallel=FALSE)
IV_backup <- IV
IV_Value = data.frame(IV$Summary)
IV_Value

names <- IV$Summary$Variable
plots <- list()
for (i in 1:length(names)){
  print(IV$Summary[i,]$IV > 0.01)
  plots[[i]] <- plot_infotables(IV, names[i])
}

# Showing the top 18 variables
# ----------------------------
length(plots)
plots

plotFrame <- IV$Summary[order(-IV$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable, levels = plotFrame$Variable[order(-plotFrame$IV)])
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

plotFrame

demoBackup <- demographic
demographic <-demoBackup


# Replace Woe values for demographic data
# ---------------------------------------
demographic$Age_Woe <- mapvalues(demographic$Age, from = IV$Tables$Age$Age, to = IV$Tables$Age$WOE)
demographic$Gender_Woe <- mapvalues(as.character(demographic$Gender), from = IV$Tables$Gender$Gender, to = IV$Tables$Gender$WOE)
demographic$Marital.Status..at.the.time.of.application._Woe <- mapvalues(as.character(demographic$Marital.Status..at.the.time.of.application.), from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
demographic$No.of.dependents_Woe <- mapvalues(as.character(demographic$No.of.dependents), from = IV$Tables$No.of.dependents$No.of.dependents, to = IV$Tables$No.of.dependents$WOE)
demographic$Income_Woe <- mapvalues(as.character(demographic$Income), from = IV$Tables$Income$Income, to = IV$Tables$Income$WOE)
demographic$Education_Woe <- mapvalues(as.character(demographic$Education), from = IV$Tables$Education$Education, to = IV$Tables$Education$WOE)
demographic$Profession_Woe <- mapvalues(as.character(demographic$Profession), from = IV$Tables$Profession$Profession, to = IV$Tables$Profession$WOE)
demographic$Type.of.residence_Woe <- mapvalues(as.character(demographic$Type.of.residence), from = IV$Tables$Type.of.residence$Type.of.residence, to = IV$Tables$Type.of.residence$WOE)
demographic$No.of.months.in.current.residence_Woe <- mapvalues(as.character(demographic$No.of.months.in.current.residence), from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence, to = IV$Tables$No.of.months.in.current.residence$WOE)
demographic$No.of.months.in.current.company_Woe <- mapvalues(as.character(demographic$No.of.months.in.current.company), from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company, to = IV$Tables$No.of.months.in.current.company$WOE)

str(demographic)

colnames(demographic[, 13:22])

WOE_DataFrame <- demographic[,13:22]
PerformanceDataFrame <- as.data.frame(demographic$Performance.Tag)
colnames(PerformanceDataFrame) <- c("Performance.Tag")

WOE_DataFrame <- cbind(PerformanceDataFrame, WOE_DataFrame)

head(WOE_DataFrame)

WOE_DataFrame$Age_Woe <- as.numeric(as.character(WOE_DataFrame$Age_Woe))
WOE_DataFrame$Gender_Woe <- as.numeric(WOE_DataFrame$Gender_Woe)
WOE_DataFrame$Marital.Status..at.the.time.of.application._Woe <- as.numeric(WOE_DataFrame$Marital.Status..at.the.time.of.application._Woe)
WOE_DataFrame$No.of.dependents_Woe <- as.numeric(WOE_DataFrame$No.of.dependents_Woe)
WOE_DataFrame$Income_Woe <- as.numeric(WOE_DataFrame$Income_Woe)
WOE_DataFrame$Education_Woe <- as.numeric(WOE_DataFrame$Education_Woe)
WOE_DataFrame$Profession_Woe <- as.numeric(WOE_DataFrame$Profession_Woe)
WOE_DataFrame$Type.of.residence_Woe <- as.numeric(WOE_DataFrame$Type.of.residence_Woe)
WOE_DataFrame$No.of.months.in.current.residence_Woe <- as.numeric(WOE_DataFrame$No.of.months.in.current.residence_Woe)
WOE_DataFrame$No.of.months.in.current.company_Woe <- as.numeric(WOE_DataFrame$No.of.months.in.current.company_Woe)

str(WOE_DataFrame)

WOE_DataFrame$Performance.Tag <- as.factor(WOE_DataFrame$Performance.Tag)

##################
##################
# MODEL BUILDING #
##################
##################

# The two types of models We need to build are as follows:
# Demographic data model: Build a model to predict the likelihood of default using only the demographic data. 
# This will give you a good idea of the predictive power of the application data. Obviously, 
# the final model will use the credit bureau data as well, though this model is an important part of 
# understanding the predictive power of application data.
# Now we will create the model on demographic data to see its predictabile power to predict the data.

# Creating Training & Testing data  
# ---------------------------------
set.seed(100)
trainindices= sample(1:nrow(WOE_DataFrame), 0.7*nrow(WOE_DataFrame))

# Separate training and testing data
# ----------------------------------
train = WOE_DataFrame[trainindices,]
test = WOE_DataFrame[-trainindices,]

#######################
# Logistic Regression #
#######################

# Model 1
# -------
model_1 <- glm(Performance.Tag~.,data=train,family = "binomial")
summary(model_1)

# Model 2
# -------
# used STEPAIC to find the best model
# model_2 <- stepAIC(model_1, direction = "both")
# summary(model_2)

# Model 3
# -------
model_3 <- glm(formula = Performance.Tag ~ Age_Woe + Gender_Woe + No.of.dependents_Woe + 
                 Income_Woe + Profession_Woe + No.of.months.in.current.residence_Woe + 
                 No.of.months.in.current.company_Woe, family = "binomial", 
               data = train)
summary(model_3)
vif(model_3)

# Model 4
# -------
model_4 <-glm(formula = Performance.Tag ~ Gender_Woe + No.of.dependents_Woe + Income_Woe + 
                 No.of.months.in.current.residence_Woe + 
                No.of.months.in.current.company_Woe, family = "binomial", data = train)
summary(model_4)
vif(model_4)

# Model 5
# -------
model_5 <-glm(formula = Performance.Tag ~ Gender_Woe  + Income_Woe + 
                No.of.months.in.current.residence_Woe + 
                No.of.months.in.current.company_Woe, family = "binomial", data = train)
summary(model_5)
vif(model_5)

# Model 6
# -------
model_6 <-glm(formula = Performance.Tag ~ Income_Woe + 
                No.of.months.in.current.residence_Woe + 
                No.of.months.in.current.company_Woe, family = "binomial", data = train)
summary(model_6)
vif(model_6)

# EVALUATING THE LOGISTIC MODEL
# -----------------------------

pred <- predict(model_6, newdata = test, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- test$Performance.Tag
mean(y_pred == y_act)
test[["Performance.Tag"]] <- factor(test[["Performance.Tag"]])
confusionMatrix(y_pred,test$Performance.Tag)

# Accuracy : 0.9595  
# Sensitivity : 1.0000          
# Specificity : 0.0000 

# OBSERVATION - Very Bad model AS sensitivity is 1 and Specificity is 0.
# ----------------------------------------------------------------------

# Credit DATA WOE REPLACEMENT
str(credit)

IV <- create_infotables(data=credit[,-1], y="Performance.Tag", parallel=FALSE)
head(IV)
IV_Value = data.frame(IV$Summary)

names <- IV$Summary$Variable
plots <- list()
for (i in 1:length(names)){
  print(IV$Summary[i,]$IV > 0.01)
  plots[[i]] <- plot_infotables(IV, names[i])
}

# Showing the top 18 variables
# ----------------------------
length(plots)
plots

plotFrame <- IV$Summary[order(-IV$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable, levels = plotFrame$Variable[order(-plotFrame$IV)])
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

plotFrame

creditbackup<- credit
credit <- creditbackup

credit$No.of.times.90.DPD.or.worse.in.last.6.months_Woe <- as.numeric(mapvalues(credit$No.of.times.90.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE))
credit$No.of.times.60.DPD.or.worse.in.last.6.months_Woe <- as.numeric(mapvalues(credit$No.of.times.60.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE))
credit$No.of.times.30.DPD.or.worse.in.last.6.months_Woe <- as.numeric(mapvalues(credit$No.of.times.30.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE))
credit$No.of.times.90.DPD.or.worse.in.last.12.months_Woe <- as.numeric(mapvalues(credit$No.of.times.90.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE))
credit$No.of.times.60.DPD.or.worse.in.last.12.months_Woe <- as.numeric(mapvalues(credit$No.of.times.60.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE))
credit$No.of.times.30.DPD.or.worse.in.last.12.months_Woe <- as.numeric(mapvalues(credit$No.of.times.30.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE))
credit$Avgas.CC.Utilization.in.last.12.months_Woe <- as.numeric(mapvalues(as.character(credit$Avgas.CC.Utilization.in.last.12.months), from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months, to = IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE))
credit$No.of.trades.opened.in.last.6.months_Woe <- as.numeric(mapvalues(credit$No.of.trades.opened.in.last.6.months, from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months, to = IV$Tables$No.of.trades.opened.in.last.6.months$WOE))
credit$No.of.trades.opened.in.last.12.months_Woe <- as.numeric(mapvalues(credit$No.of.trades.opened.in.last.12.months, from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months, to = IV$Tables$No.of.trades.opened.in.last.12.months$WOE))
credit$No.of.PL.trades.opened.in.last.6.months_Woe <- as.numeric(mapvalues(credit$No.of.PL.trades.opened.in.last.6.months, from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months, to = IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE))
credit$No.of.PL.trades.opened.in.last.12.months_Woe <- as.numeric(mapvalues(credit$No.of.PL.trades.opened.in.last.12.months, from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months, to = IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE))
credit$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._Woe <- as.numeric(mapvalues(credit$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE))
credit$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe <- as.numeric(mapvalues(credit$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE))
credit$Presence.of.open.home.loan_Woe <- as.numeric(mapvalues(as.character(credit$Presence.of.open.home.loan), from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan, to = IV$Tables$Presence.of.open.home.loan$WOE))
credit$Outstanding.Balance_Woe <- as.numeric(mapvalues(as.character(credit$Outstanding.Balance), from = IV$Tables$Outstanding.Balance$Outstanding.Balance, to = IV$Tables$Outstanding.Balance$WOE))
credit$Total.No.of.Trades_Woe <- as.numeric(mapvalues(as.character(credit$Total.No.of.Trades), from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades, to = IV$Tables$Total.No.of.Trades$WOE))
credit$Presence.of.open.auto.loan_Woe <- as.numeric(mapvalues(credit$Presence.of.open.auto.loan, from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan, to = IV$Tables$Presence.of.open.auto.loan$WOE))

woe_creditdata <- credit[,c(1,19:36)]
woe_demographicdata <- demographic[,c(1,12:22)]

str(woe_creditdata)
str(woe_demographicdata)

woe_demographicdata$Age_Woe <- as.numeric(as.character(woe_demographicdata$Age_Woe))
woe_demographicdata$Gender_Woe <- as.numeric(woe_demographicdata$Gender_Woe)
woe_demographicdata$Marital.Status..at.the.time.of.application._Woe <- as.numeric(woe_demographicdata$Marital.Status..at.the.time.of.application._Woe)
woe_demographicdata$No.of.dependents_Woe <- as.numeric(woe_demographicdata$No.of.dependents_Woe)
woe_demographicdata$Income_Woe <- as.numeric(woe_demographicdata$Income_Woe)
woe_demographicdata$Education_Woe <- as.numeric(woe_demographicdata$Education_Woe)
woe_demographicdata$Profession_Woe <- as.numeric(woe_demographicdata$Profession_Woe)
woe_demographicdata$Type.of.residence_Woe <- as.numeric(woe_demographicdata$Type.of.residence_Woe)
woe_demographicdata$No.of.months.in.current.residence_Woe <- as.numeric(woe_demographicdata$No.of.months.in.current.residence_Woe)
woe_demographicdata$No.of.months.in.current.company_Woe <- as.numeric(woe_demographicdata$No.of.months.in.current.company_Woe)

str(woe_demographicdata)

woe_demographicdata$Performance.Tag <- as.factor(woe_demographicdata$Performance.Tag)
woe_creditdata$Performance.Tag <- as.factor(woe_creditdata$Performance.Tag)

# We finally merged the demographic and credit WOE replaced data in one data frame
mergedData <- merge(woe_demographicdata, woe_creditdata, by ="Application.ID")

# Since Performance tag was in both datasets hence we evaluate if these columns are identical or different.
identical(mergedData[['Performance.Tag.y']],mergedData[['Performance.Tag.x']])

# SINCE BOTH COLUMNS ARE IDENTICAL HENCE WE REMOVE ONE OF THE COLUMNS AND RENAME THE OTHER COLUMN
mergedData$Performance.Tag.x <- NULL

# Removing one Performance Tag column . 
colnames(mergedData)[colnames(mergedData)=="Performance.Tag.y"]="Performance.Tag"

mergedData$Performance.Tag <- as.factor(mergedData$Performance.Tag)

str(mergedData)

# Droping the ApplicationId columns since it is not useful in model creation
finalmodeldata <- mergedData[,-1]

FinalSelectedModelData <- finalmodeldata

# Add another logistic regression model with unbalanced data

# Model Creation with complete data set
# -------------------------------------
set.seed(111)
train_new.index <- createDataPartition(finalmodeldata$Performance.Tag, p=0.7, list = FALSE)
train_new <- finalmodeldata[train_new.index,]
test_new <- finalmodeldata[-train_new.index,]

round(prop.table(table(finalmodeldata$Performance.Tag))*100)
# 96% value for 0 and 4% for +1. Showing Imbalanced classification of data.

round(prop.table(table(finalmodeldata$Performance.Tag))*100)
# 96% value for 0 and 4% for +1. Showing Imbalanced classification of data.

library(unbalanced)

data <- ubBalance(X= train_new[,-11], Y=as.factor(train_new[,11]), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
balanced_data <- cbind(data$X,data$Y)
summary(balanced_data)
colnames(balanced_data)[28] <- "Performance.Tag"

# Create task
train_new.task <- makeClassifTask(data = train_new,target = "Performance.Tag")
test_new.task <- makeClassifTask(data=test_new,target = "Performance.Tag")

# Logistic Model on Unbalanced data first
# ---------------------------------------

# Model using both demographic and credit bureau data: Build a model to predict default using both the data sets. 
# We may choose any type of model, though it is recommended to start with a logistic regression model first. 
# Further, you can choose any type of model. 

# Logistic Regression Model 
# -------------------------
train <- train_new

# Logistic Model 1
# ----------------
lm1 <- glm(Performance.Tag ~ ., family = "binomial", data = train)
summary(lm1)

# Logistic Model 2
# ----------------
# Using stepwise algorithm for removing insignificant variables 
# lm2 <- stepAIC(lm1, direction = "both")

# Logistic Model 3
# ----------------
lm3<- glm(formula = Performance.Tag ~ Age_Woe + Gender_Woe + No.of.months.in.current.company_Woe + 
            No.of.times.90.DPD.or.worse.in.last.6.months_Woe + No.of.times.30.DPD.or.worse.in.last.6.months_Woe + 
            No.of.times.90.DPD.or.worse.in.last.12.months_Woe + No.of.times.30.DPD.or.worse.in.last.12.months_Woe + 
            Avgas.CC.Utilization.in.last.12.months_Woe + No.of.trades.opened.in.last.12.months_Woe + 
            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe + 
            Outstanding.Balance_Woe, family = "binomial", data = train)
summary(lm3)
vif(lm3)
# removing  Age

# Logistic Model 4
# ----------------
lm4<- glm(formula = Performance.Tag ~ Gender_Woe + No.of.months.in.current.company_Woe + 
            No.of.times.90.DPD.or.worse.in.last.6.months_Woe + No.of.times.30.DPD.or.worse.in.last.6.months_Woe + 
            No.of.times.90.DPD.or.worse.in.last.12.months_Woe + No.of.times.30.DPD.or.worse.in.last.12.months_Woe + 
            Avgas.CC.Utilization.in.last.12.months_Woe + No.of.trades.opened.in.last.12.months_Woe + 
            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe + 
            Outstanding.Balance_Woe, family = "binomial", data = train)
summary(lm4)
vif(lm4)
# removing  due to high vif value , relatively lower significance -   No.of.times.60.DPD.or.worse.in.last.6.months  

# Logistic Model 5
# ----------------
lm5<- glm(formula = Performance.Tag ~ Gender_Woe + No.of.months.in.current.company_Woe + 
            No.of.times.90.DPD.or.worse.in.last.6.months_Woe + No.of.times.30.DPD.or.worse.in.last.6.months_Woe + 
            No.of.times.90.DPD.or.worse.in.last.12.months_Woe + No.of.times.30.DPD.or.worse.in.last.12.months_Woe + 
            Avgas.CC.Utilization.in.last.12.months_Woe + 
            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe + 
            Outstanding.Balance_Woe, family = "binomial", data = train)
summary(lm5)
vif(lm5)

# Logistic Model 6
# ----------------
lm6<- glm(formula = Performance.Tag ~ 
             No.of.times.30.DPD.or.worse.in.last.12.months_Woe + 
            Avgas.CC.Utilization.in.last.12.months_Woe + 
            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe + 
            Outstanding.Balance_Woe, family = "binomial", data = train)
summary(lm6)
vif(lm6)

# OBSERVATION - Significance is very high now for existing attributes.Lets take this model as final LR model for now.
# -------------------------------------------------------------------------------------------------------------------
finalmodel <- lm6


# Model Evaluation with Test Data
# -------------------------------

# Predicted probabilities  for test data
test_pred = predict(finalmodel, type = "response", newdata = test_new[,-1])


# Let's use the probability cutoff of 50%.
test_pred_default <- as.factor(ifelse(test_pred >= 0.50, 1,0))
test_actual_default <-  as.factor(ifelse(test_new$Performance.Tag==1,1,0))

conf_mtr_50_cutoff <- confusionMatrix(test_pred_default, test_actual_default)

conf_mtr_50_cutoff 

# Accuracy : 0.9578
# Sensitivity : 1.0000         
# Specificity : 0.0000 
# OBSERVATION - Above Lofistic Model is not good.
# -----------------------------------------------


# Get variable importance chart

library("FSelector")
var_imp <- generateFilterValuesData(train_new.task,  method = c("information.gain","chi.squared"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#Generate data synthetically to avoid errors related to explicitely mentioned probability
train <- balanced_data;
table(as.factor(train$Performance.Tag))

# Model using both demographic and credit bureau data: Build a model to predict default using both the data sets. 
# We may choose any type of model, though it is recommended to start with a logistic regression model first. 
# Further, you can choose any type of model. 

# Logistic Regression Model
# -------------------------

# Logistic Model 1
# ----------------
lm1 <- glm(Performance.Tag ~ ., family = "binomial", data = train)
summary(lm1)

# Logistic Model 2
# ----------------
# Using stepwise algorithm for removing insignificant variables 
#lm2 <- stepAIC(lm1, direction = "both")

# Logistic Model 3
# ----------------
lm3<- glm(Performance.Tag ~ Age_Woe + No.of.dependents_Woe + Income_Woe + 
            Education_Woe + Type.of.residence_Woe + No.of.months.in.current.company_Woe + 
            No.of.times.30.DPD.or.worse.in.last.6.months_Woe + Avgas.CC.Utilization.in.last.12.months_Woe + 
            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe + 
            Presence.of.open.home.loan_Woe + Outstanding.Balance_Woe + 
            Presence.of.open.auto.loan_Woe
                 , family = "binomial", data = train)
summary(lm3)
vif(lm3)
# removing  due to high vif value  - No.of.PL.trades.opened.in.last.6.months.1 

# Logistic Model 4
# ----------------
lm4<- glm(Performance.Tag ~ Age_Woe + No.of.dependents_Woe + Income_Woe + 
            Education_Woe + Type.of.residence_Woe + 
            No.of.times.30.DPD.or.worse.in.last.6.months_Woe + Avgas.CC.Utilization.in.last.12.months_Woe + 
            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe + 
            Presence.of.open.home.loan_Woe + Outstanding.Balance_Woe + 
            Presence.of.open.auto.loan_Woe
          , family = "binomial", data = train)
summary(lm4)
vif(lm4)

# Logistic Model 5
# ----------------
lm5<- glm(Performance.Tag ~ Age_Woe + No.of.dependents_Woe + Income_Woe + 
            Education_Woe + 
            No.of.times.30.DPD.or.worse.in.last.6.months_Woe + Avgas.CC.Utilization.in.last.12.months_Woe + 
            No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe + 
            Presence.of.open.home.loan_Woe + Outstanding.Balance_Woe + 
            Presence.of.open.auto.loan_Woe
          , family = "binomial", data = train)
summary(lm5)
vif(lm5)

# OBSERVATION - Significance is very high now for existing attributes.Lets take this model as final LR model for now.
finalmodel <- lm5


# Model Evaluation with Test Data
# -------------------------------
# Predicted probabilities  for test data
test_pred = predict(finalmodel, type = "response", newdata = test_new[])

# Let's use the probability cutoff of 50%.
test_pred_default <- as.factor(ifelse(test_pred >= 0.50, 1,0))
test_actual_default <-  as.factor(ifelse(test_new$Performance.Tag==1,1,0))

conf_mtr_50_cutoff <- confusionMatrix(test_pred_default, test_actual_default)

conf_mtr_50_cutoff 

#     0     1     -- Actual
#0 12541   342
#1   7534   542

# Accuracy : 0.586      
# Sensitivity : 0.62471        
# Specificity : 0.61312  
# OBSERVTION - Model evaluation
# Evaluate the models using relevant metrics and report the results. As a part of model validation, 
# predict the likelihood of default for the rejected candidates and assess whether the results correspond to your expectations.


# Let's find out the optimal probalility cutoff 
# ---------------------------------------------
perform_fn <- function(cutoff) 
{
  predicted_default <- as.factor(ifelse(test_pred >= cutoff, 1,0))
  conf <- confusionMatrix(predicted_default, test_actual_default)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.95,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
grid(50, lwd = 2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col= "blue",lwd=2)
box()
legend("topright", legend=c("Sensitivity","Specificity","Accuracy"),
       col=c(2,"darkgreen",4,"darkred"), cex=0.5,lwd=c(3,3,3,3), text.font=14,y.intersp = 0.3)

test_pred_optimal<- as.factor(ifelse(test_pred > 0.4992638, 1,0))
optimal_conf <- confusionMatrix(test_pred_optimal, test_actual_default)
optimal_conf

# for 52% optimal threshold,  
# accuracy = 0.6229, 
# specificity = 0.6165
# sensitivity = 0.6232

# So cutoff value is 0.4992638 for final model

##############################
##############################
# KS -statistic - Test Data ##
##############################
##############################

library(ROCR)

pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- ROCR::performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)   ## 0.239679

##########################
# KS-statistic is 23.96% #
##########################

# ROC Curve
# ---------
auc_ROCR <- ROCR::performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 

# Area under curve is : 0.6198395
# -------------------------------
pd <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(pd ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Logistic Regression Model",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))+
  annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc_ROCR, 3))) 

gini<-(auc_ROCR*2)-1
gini # 0.239679

library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

lift_decile_info = lift(test_actual_default, test_pred, groups = 10)

print(lift_decile_info)
#write.csv(lift_decile_info, "lift.csv", row.names = FALSE)

#######################
# Plotting Gain Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Gain),color='#FF6666', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Gain),color='#07843b',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")

#######################
# Plotting Lift Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#07843b', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#FF6666',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")

####################################
# Building Model for Decision tree #
####################################

set.seed(3033)
in_train <- createDataPartition(finalmodeldata$Performance.Tag, p=0.7, list = F)
training <- finalmodeldata[in_train,]
testing <- finalmodeldata[-in_train,]

data <- ubBalance(X= training[,-11], Y=as.factor(training[,11]), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
balanced_data <- cbind(data$X,data$Y)
summary(balanced_data)
colnames(balanced_data)[28] <- "Performance.Tag"

train_rf <- balanced_data
test_rf <- testing

table(train_rf$Performance.Tag)
# no   yes 
# 9283  8252 

table(test_rf$Performance.Tag)

#check classes distribution
prop.table(table(train_rf$Performance.Tag))
# no             yes 
# 0.5293983     0.4706017

# Let's build a model on given data.Building a decision tree with default hyperparameters
tree.model <- rpart(Performance.Tag ~ .,                     # formula
                    data = train_rf,                   # training data
                    method = "class")               # classification or regression

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "1")

# Accuracy : 0.9366
# Sensitivity : 0.042986         
# Specificity : 0.975990

# Change the algorithm to "information gain" instead of default "gini"
  tree.model <- rpart(Performance.Tag ~ .,                     # formula
                      data = train_rf,                   # training data
                      method = "class",               # classification or regression
                      parms = list(split = "information")
  )

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "1")

# Accuracy : 0.9366 
# Sensitivity : 0.044118         
# Specificity : 0.975890

tree.predict <- predict(tree.model, test_rf, type = "prob")

# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_default <- as.factor(ifelse(tree.predict[,2] >= cutoff, "1","0"))
  conf <- confusionMatrix(predicted_default, test_rf$Performance.Tag)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.95,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
grid(50, lwd = 2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col= "blue",lwd=2)
box()
legend("topright", legend=c("Sensitivity","Specificity","Accuracy"),
       col=c(2,"darkgreen",4,"darkred"), cex=0.5,lwd=c(3,3,3,3), text.font=14,y.intersp = 0.3)


test_pred_optimal<- as.factor(ifelse(tree.predict[,2] >= 0.3708387, 1,0))
optimal_conf <- confusionMatrix(test_pred_optimal, test_actual_default)
optimal_conf

# Accuracy : 0.5815
# Sensitivity : 0.58944         
# Specificity : 0.41290

############################################## 
# KS -statistic - Decision Tree - Test Data ##
##############################################

library(ROCR)
test_actual_default<- test_rf$Performance.Tag 
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- ROCR::performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #0.255077


# KS-statistic is 0% 

# ROC Curve
# ---------
auc_ROCR <- ROCR::performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 

# Area under curve is : 0.6275385
# -------------------------------
pd <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(pd ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Decision Tree Model",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))+
  annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc_ROCR, 3))) 

gini<-(auc_ROCR*2)-1
gini # 0.255077

library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

lift_decile_info = lift(test_actual_default, test_pred, groups = 10)

print(lift_decile_info)
#write.csv(lift_decile_info, "lift.csv", row.names = FALSE)

#######################
# Plotting Gain Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Gain),color='#FF6666', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Gain),color='#07843b',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")

#######################
# Plotting Lift Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#07843b', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#FF6666',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")

gc()

#######################
# Random Forest model #
#######################
rf_FinalData <- randomForest(Performance.Tag ~., 
                             data = train_rf, 
                             proximity = F, 
                             do.trace = T, 
                             mtry = 5,
                             ntree=1000)
summary(rf_FinalData)

# Make predictions on the test set
# --------------------------------
tree.predict <- predict(rf_FinalData, test_rf, type = "class")

# Evaluate the results
# --------------------
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "1") 

# Accuracy : 0.9479 
# -----------------

# This is at a standard cut-off Reference
# Prediction    no      yes
#         no    19851   868
#         yes   224     16

#Sensitivity : 0.0180995        
#Specificity : 0.9888418  

# In terms of probbability
rf_pred_FinalData <- predict(rf_FinalData, test_rf, type = "prob")

# Let's find out the optimal cutoff value for probalility with synthetic data
# Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_FinalData[, 2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# Calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# Plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]
cutoff_rf
# The plot shows that cutoff value of around 17.8% optimises sensitivity and accuracy
# The cut off is too low.
test_pred_optimal<- factor(ifelse(rf_pred_FinalData[, 2] >= 0.2673737, "1", "0"))
conf_rf <- confusionMatrix(test_pred_optimal, test_rf$Performance.Tag, positive = "1")
conf_rf

# Accuracy : 0.6276
# Sensitivity : 0.62330       
# Specificity : 0.62780

##############################################
# KS - statistic - Random Forest - Test Data #
##############################################

library(ROCR)
test_actual_default<-test_rf$Performance.Tag 
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- ROCR::performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #0.2510989


# KS-statistic is 25.10% 
# -----------------------

# ROC Curve
# ---------
auc_ROCR <- ROCR::performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 

# Area under curve is : 0.6255495
# -------------------------------
pd <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(pd ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Random Forest",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))+
  annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc_ROCR, 3))) 

gini<-(auc_ROCR*2)-1
gini #0.2510989

library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

lift_decile_info = lift(test_actual_default, test_pred, groups = 10)

print(lift_decile_info)
#write.csv(lift_decile_info, "lift.csv", row.names = FALSE)

#######################
# Plotting Gain Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Gain),color='#FF6666', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Gain),color='#07843b',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")

#######################
# Plotting Lift Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#07843b', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#FF6666',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")

CreditDataRejected$Performance.Tag <- 1
DemographicDataRejected$Performance.Tag <- 1

mergedDataRejected <- merge(DemographicDataRejected, CreditDataRejected, by = c("Application.ID", "Performance.Tag"))
mergedDataRejected$Performance.Tag <- as.factor(mergedDataRejected$Performance.Tag)

str(mergedDataRejected)

mergedDataRejected <- mergedDataRejected[,-1]

mergedDataRejected$Age_Woe <- as.numeric(mapvalues(mergedDataRejected$Age, from = IV_backup$Tables$Age$Age, to = IV_backup$Tables$Age$WOE))
mergedDataRejected$Gender_Woe <- as.numeric(mapvalues(as.character(mergedDataRejected$Gender), from = IV_backup$Tables$Gender$Gender, to = IV_backup$Tables$Gender$WOE))
mergedDataRejected$Marital.Status..at.the.time.of.application._Woe <- as.numeric(mapvalues(as.character(mergedDataRejected$Marital.Status..at.the.time.of.application.), from = IV_backup$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV_backup$Tables$Marital.Status..at.the.time.of.application.$WOE))
mergedDataRejected$No.of.dependents_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.dependents, from = IV_backup$Tables$No.of.dependents$No.of.dependents, to = IV_backup$Tables$No.of.dependents$WOE))
mergedDataRejected$Income_Woe <- as.numeric(mapvalues(mergedDataRejected$Income, from = IV_backup$Tables$Income$Income, to = IV_backup$Tables$Income$WOE))
mergedDataRejected$Education_Woe <- as.numeric(mapvalues(as.character(mergedDataRejected$Education), from = IV_backup$Tables$Education$Education, to = IV_backup$Tables$Education$WOE))
mergedDataRejected$Profession_Woe <- as.numeric(mapvalues(as.character(mergedDataRejected$Profession), from = IV_backup$Tables$Profession$Profession, to = IV_backup$Tables$Profession$WOE))
mergedDataRejected$Type.of.residence_Woe <- as.numeric(mapvalues(as.character(mergedDataRejected$Type.of.residence), from = IV_backup$Tables$Type.of.residence$Type.of.residence, to = IV_backup$Tables$Type.of.residence$WOE))
mergedDataRejected$No.of.months.in.current.residence_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.months.in.current.residence, from = IV_backup$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence, to = IV_backup$Tables$No.of.months.in.current.residence$WOE))
mergedDataRejected$No.of.months.in.current.company_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.months.in.current.company, from = IV_backup$Tables$No.of.months.in.current.company$No.of.months.in.current.company, to = IV_backup$Tables$No.of.months.in.current.company$WOE))
mergedDataRejected$No.of.times.90.DPD.or.worse.in.last.6.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.times.90.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE))
mergedDataRejected$No.of.times.60.DPD.or.worse.in.last.6.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.times.60.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE))
mergedDataRejected$No.of.times.30.DPD.or.worse.in.last.6.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.times.30.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE))
mergedDataRejected$No.of.times.90.DPD.or.worse.in.last.12.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.times.90.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE))
mergedDataRejected$No.of.times.60.DPD.or.worse.in.last.12.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.times.60.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE))
mergedDataRejected$No.of.times.30.DPD.or.worse.in.last.12.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.times.30.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE))
mergedDataRejected$Avgas.CC.Utilization.in.last.12.months_Woe <- as.numeric(mapvalues(mergedDataRejected$Avgas.CC.Utilization.in.last.12.months, from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months, to = IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE))
mergedDataRejected$No.of.trades.opened.in.last.6.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.trades.opened.in.last.6.months, from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months, to = IV$Tables$No.of.trades.opened.in.last.6.months$WOE))
mergedDataRejected$No.of.trades.opened.in.last.12.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.trades.opened.in.last.12.months, from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months, to = IV$Tables$No.of.trades.opened.in.last.12.months$WOE))
mergedDataRejected$No.of.PL.trades.opened.in.last.6.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.PL.trades.opened.in.last.6.months, from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months, to = IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE))
mergedDataRejected$No.of.PL.trades.opened.in.last.12.months_Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.PL.trades.opened.in.last.12.months, from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months, to = IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE))
mergedDataRejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE))
mergedDataRejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe <- as.numeric(mapvalues(mergedDataRejected$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE))
mergedDataRejected$Presence.of.open.home.loan_Woe <- as.numeric(mapvalues(mergedDataRejected$Presence.of.open.home.loan, from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan, to = IV$Tables$Presence.of.open.home.loan$WOE))
mergedDataRejected$Outstanding.Balance_Woe <- as.numeric(mapvalues(mergedDataRejected$Outstanding.Balance, from = IV$Tables$Outstanding.Balance$Outstanding.Balance, to = IV$Tables$Outstanding.Balance$WOE))
mergedDataRejected$Total.No.of.Trades_Woe <- as.numeric(mapvalues(mergedDataRejected$Total.No.of.Trades, from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades, to = IV$Tables$Total.No.of.Trades$WOE))
mergedDataRejected$Presence.of.open.auto.loan_Woe <- as.numeric(mapvalues(mergedDataRejected$Presence.of.open.auto.loan, from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan, to = IV$Tables$Presence.of.open.auto.loan$WOE))

str(mergedDataRejected)

sum(ncol(mergedDataRejected))

mergedDataRejected$Performance.Tag <- "yes"
mergedDataRejected$Performance.Tag <- as.factor(mergedDataRejected$Performance.Tag)

finaldataframerejecteddata <- mergedDataRejected[, c(1,29:55)]

finaldataframerejecteddataBackup <- finaldataframerejecteddata

tree.predict <- predict(rf_FinalData, finaldataframerejecteddata[,-1], type = "prob")

test_pred_optimal<- factor(ifelse(tree.predict[, 2] >= 0.2673737, "yes", "no"))
conf_rf <- confusionMatrix(finaldataframerejecteddata$Performance.Tag, test_pred_optimal, positive = "yes")
conf_rf

summary(as.factor(test_pred_optimal))

(1425- 68)/1425

1357/1425
# 95.23 percent of the Defaulters detected very well which is very good

finaldataframerejecteddata$Performance.Tag <- as.character(finaldataframerejecteddata$Performance.Tag)
finaldataframerejecteddata$Performance.Tag <- test_pred_optimal
finaldataframerejecteddata$Performance.Tag <- as.factor(finaldataframerejecteddata$Performance.Tag)

str(finalmodeldata)
str(finaldataframerejecteddata)

finaldataframerejecteddata$Performance.Tag <- as.factor(ifelse(finaldataframerejecteddata$Performance.Tag =="yes", 1, 0))

finalselecteddata <- finalmodeldata[,c(1:10, 12:28, 11)]
str(finalselecteddata)

finaldata <- rbind(finaldataframerejecteddata, finalselecteddata)

finaldata$Performance.Tag <- as.factor(finaldata$Performance.Tag)

set.seed(3033)
in_train <- createDataPartition(finaldata$Performance.Tag, p=0.7, list = F)
training <- finaldata[in_train,]
testing <- finaldata[-in_train,]

data <- ubBalance(X= training[,-1], Y=as.factor(training[,1]), type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
balanced_data <- cbind(data$X,data$Y)
summary(balanced_data)
colnames(balanced_data)[28] <- "Performance.Tag"

train_rf <- balanced_data
test_rf <- testing

summary(as.factor(train_rf$Performance.Tag))

# Random Forest model
# -------------------
gc()
train_rf$Performance.Tag<- as.factor(train_rf$Performance.Tag)

rf_FinalData <- randomForest(Performance.Tag ~., 
                             data = train_rf, 
                             proximity = F, 
                             do.trace = T, 
                             mtry = 5,
                             ntree= 2000)
summary(rf_FinalData)

# Make predictions on the test set
# --------------------------------
tree.predict <- predict(rf_FinalData, test_rf, type = "class")

# Evaluate the results
# --------------------
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "1") 


# This is at a standard cut-off
# Accuracy : 0.9046
#             Reference
# Prediction    no     yes
#         no    18967  912
#         yes   1128   379
#
# Sensitivity : 0.29357         
# Specificity : 0.94387

# In terms of probbability
rf_pred_FinalData <- predict(rf_FinalData, test_rf, type = "prob")

# Let's find out the optimal cutoff value for probalility with synthetic data
# Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_FinalData[, 2] >= cutoff, "1", "0"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "1")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# Creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# Calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# Plotting cutoffs
# ----------------
plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]
cutoff_rf

# 0.2772727 0.2871717
# The plot shows that cutoff value of around 17.8% optimises sensitivity and accuracy
# The cut off is too low.
test_pred_optimal<- factor(ifelse(rf_pred_FinalData[, 2] >= 0.2574747, "1", "0"))
conf_rf <- confusionMatrix(test_pred_optimal, test_rf$Performance.Tag, positive = "1")
conf_rf

#              Reference
#
# Prediction    no       yes
# no            13976    375
# yes           7907     656
#
# Accuracy : 0.7118
# Sensitivity : 0.66615       
# Specificity : 0.71471

##############################################
# KS - statistic - Random Forest - Test Data #
##############################################

library(ROCR)
test_actual_default<- test_rf$Performance.Tag 
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- ROCR::performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #0.3808554


# KS-statistic is 27% 
# -------------------

# ROC Curve
# ---------
auc_ROCR <- ROCR::performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 

# Area under curve is : 0.6904277
# -------------------------------
pd <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(pd ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Random Forest",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))+
  annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc_ROCR, 3))) 

gini<-(auc_ROCR*2)-1
gini # 0.3808554

library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

predicted_response <- as.factor(ifelse(test_pred >= 0.2574747, "1", "0"))

lift_decile_info = lift(test_actual_default, test_pred_optimal, groups = 10)

print(lift_decile_info)
#write.csv(lift_decile_info, "lift.csv", row.names = FALSE)

#######################
# Plotting Gain Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Gain),color='#FF6666', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Gain),color='#07843b',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")

#######################
# Plotting Lift Chart #
#######################

ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#07843b', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#FF6666',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")

##########################
##########################
# Application score card #
##########################
##########################
# Build an application scorecard with the good to bad odds of 10 to 1 at a score of 400 doubling every 20 points.  
# For the rejected population, calculate the application scores and assess the results. Compare the scores of the 
# Rejected population with the approved candidates and comment on the observations.
# On the basis of the scorecard, identify the cut-off score below which you would not grant credit cards to applicants.


test_rf$Prob <- predict(rf_FinalData, test_rf, type = "prob")

test_rf$log_odds <- log(test_rf$Prob/(1-test_rf$Prob))

PDO <- 20
BaseScore <- 400
Odds <- 10

# Calaculating Factor & Offset
# ----------------------------
Factor=PDO/log(2)

Offset=BaseScore-(Factor*log(Odds))

Offset
Factor

print("equation is : score = 333.5614 + (28.8539 * log(odds))")

test_rf$score <- 333.5614 + (28.8539 * test_rf$log_odds)
test_rf$score

predictedscore<- test_rf[, c("Performance.Tag", "score")]

summary(predictedscore$score)

View(test_rf[, c("Performance.Tag", "score")])


rejected_score=subset(predictedscore,predictedscore$Performance.Tag == "1")
selected_score=subset(predictedscore,predictedscore$Performance.Tag == "0")

summary(rejected_score$score)
# 0               1        
#Min.   :256.2   Min.   :180.8  
#1st Qu.:328.5   1st Qu.:296.5  
#Median :351.1   Median :316.0  
#Mean   :351.4   Mean   :315.7  
#3rd Qu.:370.6   3rd Qu.:338.6  
#Max.   :486.3   Max.   :411.0 

summary(selected_score$score)

 #0               1        
#Min.   :272.0   Min.   :160.8  
#1st Qu.:361.4   1st Qu.:262.5  
#Median :380.0   Median :287.1  
#Mean   :382.9   Mean   :284.2  
#3rd Qu.:404.6   3rd Qu.:305.7  
#Max.   :506.4   Max.   :395.1  

max(selected_score$score[is.finite(selected_score$score)])
#506.3663

cutoff_score=seq(201.0,506.4,1)

rejected_count2=NULL
selected_count2=NULL
for(i in seq(along=cutoff_score)){
  rejected_count2=c(rejected_count2,sum(rejected_score$score<cutoff_score[i]))
  selected_count2=c(selected_count2,sum(selected_score$score>cutoff_score[i]))
}
plot(cutoff_score,rejected_count2,type = "l")
par(new = T)
plot(cutoff_score, selected_count2,type = "l",axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 2, 'selected_count')
mtext(side=2,line= 2 ,'rejected_count')
mtext(side=1 ,line =2 ,"cut_off score")

#locator()

summary(as.factor(finaldata$Performance.Tag))
# 333.7068

###########################
# Plotting the lift chart #
###########################
# 
 lift <- function(labels , predicted_prob, groups=10) {
   if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
   if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
   helper = data.frame(cbind(labels , predicted_prob))
   helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
   gaintable = helper %>% group_by(bucket)  %>%
     summarise_at(vars(labels ), funs(total = n(),
                                      totalresp=sum(., na.rm = TRUE))) %>%
     mutate(Cumresp = cumsum(totalresp),
            Gain=Cumresp/sum(totalresp)*100,
            Cumlift=Gain/(bucket*(100/groups)),
            cumtotal = cumsum(total)
     )
   
   gaintable1 = helper %>% group_by(bucket)  %>%
     summarise_at(vars(predicted_prob ), funs(totalresp_pred=sum(., na.rm = TRUE))) %>% mutate(totalcumresp_pred = cumsum(totalresp_pred))
   
   gaintable<-cbind(gaintable,gaintable1[,-1])
   ratio_pred_actual <- gaintable[,9]/gaintable[,4]
   gaintable <- cbind(gaintable,ratio_pred_actual)
   return(gaintable)
   
 }
 

# We have to work on it to find business benefits
 
##############################################
# Create a Table of cumulative gain and lift #
##############################################
 
test_rf$perdict_default <- predict(rf_FinalData, test_rf, type = "class")
test_rf$response <- as.factor(ifelse(test_rf$perdict_default=="yes",1,0))
 
test_rf$Performance.Tag <- test_rf$Performance.Tag
 
LG = lift(test_rf$Performance.Tag, test_rf$response, groups = 10)
 
##############
# Gain Chart #
############## 
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
 
##############
# Lift Chart #
##############
plot(LG$cumtotal,LG$ratio_pred_actual,col="red",type="l",main="Lift Chart",xlab="Number Of Prospect Contacted",ylab = "Ratio of response rate using the model and without using the model")

# Let's say if you have spent 1Re for each customer
View(LG)



predict_unb_final=predict(rf_FinalData,newdata = mergedData,type = "prob")
predict_unb_final=predict_unb_final[,2]


###### PREPARING APPLICATION SCORECARD #######

Factor=20/log(2)
Offset=400-(Factor*log(10))
score=Offset+(Factor*log((1-predict_unb_final)/predict_unb_final))
mergedData$score=score

###### FINDING CUTOFF BELOW WHICH ALL WILL BE REJECTED #####

summary(mergedData$score)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#253.7    355.4   373.2   346.7   411.5   532.8482 

max(mergedData$score[is.finite(mergedData$score)])
#511.81

cutoff_score=seq(238.7,532.8482,1)


rejected_count2=NULL
selected_count2=NULL
for(i in seq(along=cutoff_score)){
  rejected_count2=c(rejected_count2,sum(rejected_score$score<cutoff_score[i]))
  selected_count2=c(selected_count2,sum(selected_score$score>cutoff_score[i]))
}
plot(cutoff_score,rejected_count2,type = "l")
par(new = T)
plot(cutoff_score, selected_count2,type = "l",axes=F, xlab=NA, ylab=NA)
axis(side = 4)
mtext(side = 4, line = 2, 'selected_count')
mtext(side=2,line= 2 ,'rejected_count')
mtext(side=1 ,line =2 ,"cut_off score")

#locator()

# 356 we got by woe binning below

mergedData$Prediction <- as.factor(ifelse(mergedData$score > 356 , 0, 1))

confusionMatrix(mergedData$Prediction,mergedData$Performance.Tag)

# point this locator on the graph where the lines are intersecting.
#Thus the optimal cutoff score is 356.


#Financial Benifits Calculation

#                                  Actual Defaults		
#                          Good Cust (0)	Bad Cust (1)	Total
# Predicted Defaults	     Good Cust (0)	56228 	  548 	 56776 
#                          Bad Cust (1)	  10689 	  2399 	 13088 
#                                Total	  66917  	 2947 	 69864 

# Accuracy	84.00%
# Sensitivity	84.03%
# Specificity	81.40%

# Credit Loss - Saved	

# Credit loss no model -	4%	
# Credit loss with model	0.8% ~ 1% (548/69864)
#   Credit Loss Saved	3%	(4.21 - 0.8) = 2.4%

# Revenue Loss	
# Good customers identified as bad		15% (10689/69864)
# So Revenue Loss		15%


predict_unb_final=predict(rf_FinalData,newdata = finaldataframerejecteddataBackup,type = "prob")
predict_unb_final=predict_unb_final[,2]


###### PREPARING APPLICATION SCORECARD #######

Factor=20/log(2)
Offset=400-(Factor*log(10))
score=Offset+(Factor*log((1-predict_unb_final)/predict_unb_final))
finaldataframerejecteddataBackup$score=score

###### FINDING CUTOFF BELOW WHICH ALL WILL BE REJECTED #####

summary(finaldataframerejecteddataBackup$score)

finaldataframerejecteddataBackup$Prediction <- as.factor(ifelse(finaldataframerejecteddataBackup$score > 356 , 0, 1))
finaldataframerejecteddataBackup$Performance.Tag <- as.factor(ifelse(finaldataframerejecteddataBackup$Performance.Tag == 'yes', 1, 0))

summary(finaldataframerejecteddataBackup$Prediction)

#library(woebin)
#library(scorecard)

#bins = woebin(finaldata, "Performance.Tag")
#dt_woe = woebin_ply(finaldata, bins)

#modelling on woe dataframe
#m = glm(Performance.Tag ~ ., family = binomial(), data = dt_woe)

#m_2 <- stepAIC(m, direction = "both")

#m_3 <- glm(Performance.Tag ~ Age_Woe_woe + No.of.dependents_Woe_woe + Income_Woe_woe + 
#             Education_Woe_woe + Profession_Woe_woe + No.of.months.in.current.residence_Woe_woe + 
#             No.of.months.in.current.company_Woe_woe + No.of.times.30.DPD.or.worse.in.last.6.months_Woe_woe + 
#             No.of.times.90.DPD.or.worse.in.last.12.months_Woe_woe + No.of.times.30.DPD.or.worse.in.last.12.months_Woe_woe + 
#             Avgas.CC.Utilization.in.last.12.months_Woe_woe + No.of.trades.opened.in.last.12.months_Woe_woe + 
#             No.of.PL.trades.opened.in.last.6.months_Woe_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._Woe_woe + 
#             No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe_woe + 
#             Outstanding.Balance_Woe_woe
#           , family = "binomial", data = dt_woe)

#summary(m_3)

#m_4 <- glm(Performance.Tag ~ Age_Woe_woe + No.of.dependents_Woe_woe + Income_Woe_woe + 
#             Education_Woe_woe + Profession_Woe_woe + No.of.months.in.current.residence_Woe_woe + 
#             No.of.months.in.current.company_Woe_woe + No.of.times.30.DPD.or.worse.in.last.6.months_Woe_woe + 
#             No.of.times.90.DPD.or.worse.in.last.12.months_Woe_woe + No.of.times.30.DPD.or.worse.in.last.12.months_Woe_woe + 
#             Avgas.CC.Utilization.in.last.12.months_Woe_woe + No.of.trades.opened.in.last.12.months_Woe_woe + 
#             No.of.PL.trades.opened.in.last.6.months_Woe_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._Woe_woe + 
#             No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe_woe 
#           
#           , family = "binomial", data = dt_woe)
#
#summary(m_4)

#vif(m_4)


# score ------
#card = scorecard(bins, m_4,points0 = 400,odds0 = 1/9,pdo = 20)

# credit score for only total score
#finaldata$score = scorecard_ply(finaldata, card)

#summary(finaldata$score)
## min = 356 , max = 472

#final_df_ordered_by_score<- finaldata[order(finaldata$score,decreasing = TRUE),"score"]
#final_df_ordered_by_score



###################################################
###################################################
# Assessing the financial benefit of your project #
###################################################

# 2.5 %(36/1425) of rejected candidates rejected by the bank are accepted by the model

# 15.37 %(10689/69864) of candidates accepted by the bank are rejected by the model. 


#Our model has rejected 15.37% of selected candidates. 

#We need to find out how many candidates rejected by the model have defaulted. 
# 2947/69864 = 4.21%

#Credit loss saved 
#-------------------------------------------------------------------------------------------------

#Total number of candidates selected by the bank but defaulted - 10689/69864 = 15.29% 
#No of candidates selected by the model and who defaulted - 548 
#No of candidates selected by the model - 56228
#% of candidates selected by the model and defaulted - 548/69864 = 0.78% 
#% of employees selected and defaulting before model= 4.21% 
#% of employees selected and defaulting after model=0.78% . 

#Credit loss saved = 3.43%


#Revenue loss
#----------------------------------------------------------------------------------
  
#Count of candidates rejected by the model who didn't default - 10689 
#Total count of candidates who didn't default - 66917
#Percentage of good candidates rejected by our model - 15.97%.

#So 15.97% percent is the revenue loss where we have identified good customers as bad . 



#Conclusion - 
#------------------------------------------------------------------------------
  
#Important variables that can be used to identify good customers 
#   from Random Forest Model: 
#  - Avgas.CC.Utilization.in.last.12.months 
#  - Outstanding.Balance 
#  - No.of.times.30.DPD.or.worse.in.last.6.months 
#  - No.of.times.90.DPD.or.worse.in.last.12.months 
#  - No.of.times.60.DPD.or.worse.in.last.6.months 
#  - No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.

# These variables are used while inquiring about customer before giving them loan.
# Thus We can conclude that the model has accurately predicted approximately 
# 84% of the performance Tag in the dataset.
# After tuning the model we have achieved the required accuracy of the model and 
# now model can be used to predict whether who will default and who will not default. 
# This could save a lot of resources of bank at the same time increasing efficiency.
# With the help of this model we found out that credit loss % was decreased when we used 
# this model from 4 to less than 1. 
# Model has performed accurately in rejecting the candidate who may default in future.
# This can save a lot of hours, money of the bank and at the same time increase the efficiency and 
# resources of the bank.

