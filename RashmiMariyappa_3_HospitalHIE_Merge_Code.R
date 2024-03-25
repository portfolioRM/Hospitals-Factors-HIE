#########################################
#Rashmi Mariyappa
#Health Analytics Class
#Merge Code
#Final Project


#########################################
#Load Libraries
library(tidyverse)
library(caret)
library(dplyr)
library(glmnet)
library(ggplot2)
library(tidyverse)
library(fastDummies)
library(ROCR)
#library(boot)
#library(class)


#########################################
#Quick Access Command

#Remove All Objects
rm(list=ls())

#Plot Margins
par(mfrow=c(1,1), mar=c(4,6,2.5,6))

#########################################
#Writing Files
#write.csv(dfCorr, 'C:/Users/rmari/...PerfCorrMat2.csv') #writing to exel


#########################################
#Read in File & Clean

#________________________________________
#HIMSS Demographic Data
demoDataH <- read.csv('C:/Users/rmari/...Demographics_HIMSS.csv', header = TRUE, sep = ",")
str(demoDataH) #checking type of each variable
summary(demoDataH)

demoSubData <- subset(demoDataH, select = c(5,4,6,22))
str(demoSubData)
summary(demoSubData)

demoSubData2 <- subset(demoDataH, select = c(1:6,22))
str(demoSubData)
summary(demoSubData)

#________________________________________
#HIMSS HIE Test
HIEDataH <- read.csv('C:/Users/rmari/...HIE_TEST.csv', header = TRUE, sep = ",")
HIEDataH <- subset(HIEDataH, select = c(4:8))
str(HIEDataH)
summary(demoSubData)


#HIMSS Merged Data
#df3 = merge(demoSubData, HIEDataH, by.x=c("CustomerId", "Hobby"), by.y=c("CustomerId", "like"))
df_test = merge(demoSubData, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"))
df3 = merge(demoSubData, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"), all.x=TRUE)


#________________________________________
#HMSS HIE Data
HIEDataH <- read.csv('C:/Users/rmari/...HIE_HMSS_FINAL.csv', header = TRUE, sep = ",")
HIEDataH <- subset(HIEDataH, select = c(4:7))
str(HIEDataH)
summary(demoSubData)

#HMSS Merged Data
#df3 = merge(demoSubData, HIEDataH, by.x=c("CustomerId", "Hobby"), by.y=c("CustomerId", "like"))
df3 = merge(demoSubData, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"))

df5 = merge(demoSubData2, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"), all.x=TRUE)

df6 <- subset(df5, select = c(1:7,12,13))

#________________________________________
#AHA Data Cleaning & Variable Selection

#Read In Files
AHAData <- read.csv('C:/Users/rmari/...AHA_2017.csv', header = TRUE, sep = ",")

#Merge Data to Obtain Subset of Hospitals
#df3 = merge(demoSubData, HIEDataH, by.x=c("CustomerId", "Hobby"), by.y=c("CustomerId", "like"))
#df5 = merge(demoSubData2, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"), all.x=TRUE)
df3 = merge(demoDataH, AHAData, by.x=c("CMSMedNo"), by.y=c("Medicare.Provider.ID")) #Merge with Demo HIMSS

#Examining Data to Exclude Variables
summary(df3$Practice..Physicians) #All Missing Values
summary(df3$Year) #All 2017
summary(df3$Health.care.system.ID) #1369
summary(df3$Emergency.Department...hospital) #955
summary(df3$Patient.representative.services...hospital)
summary(df3$Hosptial.unit.beds.set.up.and.staffed) #?
summary(df3$Medicare.Provider.Id) #4160
summary(df3$Robotic.surgery...hospital) #955
summary(df3$Total.gross.square.feet.of.your.physical.plant) #1354
summary(df3$Hospital.total.expenses..including.bad.debt) #3707
summary(df3$Full.time.physicians.and.dentists)
summary(df3$City.x)
summary(df3$CBSA.Type)
summary(df3$National.Provider.Number..Npi.)
summary(subAHADemo$Outpatient.Visits) #1785
summary(subAHADemo$Operating.Expense) #224
summary(subAHADemo$Admissions) #587
summary(subAHADemo$Admissions...As.Hosp.) #587
summary(subAHADemo$Certified.trauma.center...hospital) #955
summary(subAHADemo$ER.Visits) #683
summary(subAHADemo$Emergency.room.visits)

#Check
cityComp <- subset(subAHADemo2, select=c(8, 24))
bedComp <- subset(subAHADemo2, select = c(11, 12, 31, 32, 26, 25))
admissCom <- subset(subAHADemo2, select = c(17, 28, 37, 38))
orgNameComp <- subset(subAHADemo2, select = c(6, 18))
rm(orgNameComp)



#Subset of Variables
subAHADemo <- subset(df3, select = c(-8, -9, -12, -13, -14, -16, -17, -18, -19, -27, -29, -30, -31, -33, 
                                     -36, -37, -40, -41, -43, -44, -45, -48, -49, -53, -54, -56, -57, -60, 
                                     -61, -62, -63, -64, -68, -69, -70, -71, -72, -73, -74, -77, -78, -84, 
                                     -85, -86, -87, -88, -89, -100))

subAHADemo2 <- subset(subAHADemo, select = c(-14, -17, -18, -19, -22, -23, -24, -27, -28, -33, -34, -46, 
                                             -47, -48, -49, -53, -54, -55, -57, -58, -59, -60, -61, -62, 
                                             -63, -64, -65, -66, -67, -68, -69, -70, -71, -72, -73))




#________________________________________
#Merge All
df5 = merge(df3, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"), all.x=TRUE)
df7 = merge(df3, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"))

summary(df5)

hist(demoDataH$EMRAM_Stage, breaks=7, col="lightskyblue", xlab="", ylab="", main="")
counts <- table(df5$HIE_YN)
counts

#test
df_test = merge(demoDataH, HIEDataH, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"))
df5$HIE_YN[is.na(df5$HIE_YN)] <- 0 #fill in missing values with 0
df5$HIE_SUM[is.na(df5$HIE_SUM)] <- 0 #fill in missing values with 0
plot(df5$HIE_YN, df5$EMRAM_Stage)
HIEEMRAM <- table(df5$HIE_YN, df5$EMRAM_Stage)
HIEEMRAM

#________________________________________
#More Tech Score & Performance Matching
match <- subset(df5, select = c(1,2,6,7,22,41,42))
#techMatch = merge(TechData, match, by.x=c("Organization.Unique.Id), by.y=c("OrgID"))
testMatch = merge(match, TechData, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"))


#________________________________________
#Reading in Tech So Only Ones Match
#Tech Data
TechData <- read.csv('C:/Users/rmari/...Count Hospitals_TechnologySection_Details.csv', header = TRUE, sep = ",")

#________________________________________
#Reading in Performance
#Performance Data
match <- read.csv('C:/Users/rmari/...Match.csv', header = TRUE, sep = ",")
PerfData <- read.csv('C:/Users/rmari/...Copy of Count Hospitals _HCAHP.csv', header = TRUE, sep = ",")
perfMatch = merge(match, PerfData, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"))

#Correlation Matrix
PerfCorr <- read.csv('C:/Users/rmari/...Perf_HMSS_FINAL.csv', header = TRUE, sep = ",")
dfCorr <- round(as.data.frame(cor(PerfCorr, method = "pearson", use = "pairwise.complete.obs")), digits=3)



#########################################
#Variable Name Clean Up Demographic
names(demoDataH)[names(demoDataH) == 'Health.System.ID'] <- 'SysID'
names(demoDataH)[names(demoDataH) == 'Health.System'] <- 'SysName'
names(demoDataH)[names(demoDataH) == 'Health.System.ID2'] <- 'SysType'
names(demoDataH)[names(demoDataH) == 'CMS.Medicare.Number'] <- 'CMSMedNo'
names(demoDataH)[names(demoDataH) == 'Organization.Unique.Id'] <- 'OrgID'
names(demoDataH)[names(demoDataH) == 'Organization'] <- 'OrgName'
names(demoDataH)[names(demoDataH) == 'EMRAM.Stage..Validated.'] <- 'EMRAM_Stage'


#########################################
#Variable Exploration
summary(demoDataH$EMRAM.Stage..Validated.)
hist(demoDataH$EMRAM_Stage, breaks=7, col="lightskyblue", xlab="", ylab="", main="")
counts <- table(demoDataH$EMRAM_Stage)
counts

summary(df3$EMRAM_Stage)
hist(df3$EMRAM_Stage, breaks=7, col="lightskyblue", xlab="", ylab="", main="")
counts <- table(df3$EMRAM_Stage)
counts



#########################################
#Merging All Measures into One File
#Used Working Note Saved File #Demo <- read.csv('C:/Users/rmari/...CleanAHADemo38.csv', header = TRUE, sep = ",")
TechScore <- read.csv('C:/Users/rmari/...TechScore_HMSS_FINAL.csv', header = TRUE, sep = ",")
HIETarget <- read.csv('C:/Users/rmari/...HIE_HMSS_FINAL.csv', header = TRUE, sep = ",")
Perf <- read.csv('C:/Users/rmari/...Perf_HMSS_FINAL.csv', header = TRUE, sep = ",")

#Merge Demo & Tech Score
datFin1 = merge(subAHADemo2, TechScore, by.x=c("OrgID"), by.y=c("OrgID"), all.x=TRUE)

#Merge Prior & HIE
#Subset HIE
HIEsub <- subset(HIETarget, select = c(4,6,7))
datFin2 = merge(datFin1, HIEsub, by.x=c("OrgID"), by.y=c("Organization.Unique.Id"), all.x=TRUE)
#df5$HIE_YN[is.na(df5$HIE_YN)] <- 0 #fill in missing values with 0
#df5$HIE_SUM[is.na(df5$HIE_SUM)] <- 0 #fill in missing values with 0

#Merge Prior & HCAP
#Subset HCAP
perfSub <- subset(Perf, select = c(1,3,4))
datFin3 = merge(datFin2, perfSub, by.x=c("OrgID"), by.y=c("OrgID"), all.x=TRUE)

#Notes
#HIE: NA = 0
#Tech Score: NA = 0
#HCAP: NA = NA

##################################################
#Merge Just HIMSS
HIMMS_Demo <- read.csv('C:/Users/rmari/...Demographics_HIMSS.csv', header = TRUE, sep = ",")

HIMMS_DemoSub <- subset(HIMMS_Demo, select = c(3,5,6,7,11,15,20:22,24,26,31,34))
TechScore_New <- read.csv('C:/Users/rmari/...TechScore_All.csv', header = TRUE, sep = ",")
HIETarget <- read.csv('C:/Users/rmari/...HIE_HMSS_FINAL.csv', header = TRUE, sep = ",")

#Merge Demo & Tech Score
NewData1 = merge(HIMMS_DemoSub, TechScore_New, by.x=c("Organization.Unique.Id"), by.y=c("Row.Labels"), all.x=TRUE)

#Merge 
HIEsub <- subset(HIETarget, select = c(4,6,7))
NewData2 = merge(NewData1, HIEsub, by.x=c("Organization.Unique.Id"), by.y=c("Organization.Unique.Id"), all.x=TRUE)

count(NewData2[!complete.cases(NewData2$Outpatient.Visits),])     #2542 MV
count(NewData2[complete.cases(NewData2$Outpatient.Visits),])      #2952 V

summary(as.factor(NewData2$HIE_YN))

summary(NewData2$Organization.Primary.Service)
str(NewData2$Organization.Primary.Service)
NewGMS <- NewData2[NewData2$Organization.Primary.Service == "General Medical & Surgical", ]

count(NewGMS[complete.cases(NewGMS$Outpatient.Visits),])      #1684 V


##################################################
#Correlation Performed After Study was Complete for Curiosity
corrTech <- subset(NewData2, select = c(14:73,75))
str(corrTech)
summary(corrTech)

corrTech$HIE_YN[is.na(corrTech$HIE_YN)] <- 0           #fill in missing values with 0 HIE_YN
corrTech[is.na(corrTech)] <- 0

TechCORRMAT <- cor(corrTech, method = "pearson", use = "pairwise.complete.obs")
TechCORRMAT <- round(TechCORRMAT, digits=3) #rounding to 3 decimals
#write.csv(TechCORRMAT, 'C:/Users/rmari/...TechCORR.csv') #writing to exel

##################################################
