#########################################
#Rashmi Mariyappa
#Health Analytics Class
#Final Project


#########################################
#Load Libraries
library(tidyverse)
#library(caret) 
library(dplyr) 
#library(glmnet)
library(ggplot2)
library(tidyverse)
#library(fastDummies)
library(sm)
#library(ROCR) 
#library(boot) 
#library(class) 


#########################################
#Quick Access Command

#Remove All Objects
rm(list=ls())

#Plot Margins
par(mfrow=c(1,1), mar=c(4,6,2.5,6))
dev.off()

#Write File
#write.csv(stateDF, 'C:/Users/rmari/...stateHIE1.csv') #writing to exel


#########################################
#Read in File

#AHA HIMSS Hospital Data Clean #4160 Observations
hospD <- read.csv('C:/Users/rmari/...RMdata.csv', header = TRUE, sep = ",")
hospD <- subset(hospD, select = c(2:51))

str(hospD)       
summary(hospD)


#########################################
#Variable Name Clean Up
names(hospD)[names(hospD) == 'Organization.Primary.Service'] <- 'OrgPrimServ'
names(hospD)[names(hospD) == 'City.x'] <- 'City'
names(hospD)[names(hospD) == 'State.Province'] <- 'State'
names(hospD)[names(hospD) == 'Hospital..Organization.Control'] <- 'OrgControlType'
names(hospD)[names(hospD) == 'Beds..Licensed'] <- 'BedsLicenseH'
names(hospD)[names(hospD) == 'Beds..Staffed'] <- 'BedsStaffH'
names(hospD)[names(hospD) == 'Net.Patient.Revenues'] <- 'NetPatientRevH'
names(hospD)[names(hospD) == 'Operating.Expense'] <- 'OpExpenseH'
names(hospD)[names(hospD) == 'Total.Employees'] <- 'TotEmployeesH'
names(hospD)[names(hospD) == 'Total.Inpatient.Revenue'] <- 'TotInpatientRevH'
names(hospD)[names(hospD) == 'Control.Code...type.of.authority.responsible.for.establishing.policy.concerning.overall.operation.of.the.hospitals'] <- 'ControlCodeA'
names(hospD)[names(hospD) == 'Service.Code...category.best.describing.the.hospital.of.the.type.of.service.provided.to.the.majority.of.admissions'] <- 'ServiceCodeA'
names(hospD)[names(hospD) == 'Emergency.Department...hospital'] <- 'EmergencyDeptA'
names(hospD)[names(hospD) == 'Certified.trauma.center...hospital'] <- 'CertTraumaA'
names(hospD)[names(hospD) == 'Level.of.trauma.center'] <- 'TraumaLevelA'
names(hospD)[names(hospD) == 'Patient.representative.services...hospital'] <- 'PatientRepServA'
names(hospD)[names(hospD) == 'Admissions...As.Hosp.'] <- 'AdmissionsA'
names(hospD)[names(hospD) == 'Emergency.room.visits'] <- 'EmergencyRoomVisA'
names(hospD)[names(hospD) == 'Total.outpatient.visits'] <- 'TotOutpatientVisA'
names(hospD)[names(hospD) == 'Full.time.physicians.and.dentists'] <- 'FTPhysDenA'
names(hospD)[names(hospD) == 'Full.time.registered.nurses'] <- 'FTRegNurseA'
names(hospD)[names(hospD) == 'Full.time.licensed.practical.or.vocational.nurses'] <- 'FTVocNurseA'
names(hospD)[names(hospD) == 'Full.time.total.personnel'] <- 'FTPersTotA'
names(hospD)[names(hospD) == 'Total.full.time.hospital.unit.personnel'] <- 'FTHospPersTotA'
names(hospD)[names(hospD) == 'Bed.size.code'] <- 'BedSizeGrpA'
names(hospD)[names(hospD) == 'FTHospPersTot'] <- 'FTHospPersTotA'
names(hospD)[names(hospD) == 'Adjusted.Admissions'] <- 'AdjAdmissA'
names(hospD)[names(hospD) == 'Adjusted.patient.days'] <- 'AdjPatientDaysA'
names(hospD)[names(hospD) == 'Ranking.of.100.largest.cities'] <- 'Rank100LargeCity'
names(hospD)[names(hospD) == 'Accreditation.By.Jcaho'] <- 'AccredJC_A'
names(hospD)[names(hospD) == 'Member.of.Council.of.Teaching.Hospital.of.the.Association.of.American.Medical.Colleges..COTH.'] <- 'AccredCOTH_A'
names(hospD)[names(hospD) == 'CBSA.Type'] <- 'CBSATypeA'
names(hospD)[names(hospD) == 'Business.Intelligence...Clinical'] <- 'TS_BIClin'
names(hospD)[names(hospD) == 'CDSS..Clinical.Decision.Support.System'] <- 'TS_ClinDecSS'
names(hospD)[names(hospD) == 'Device..Intelligent.Medical.Device.Hubs'] <- 'TS_DevIntMedDevHub'
names(hospD)[names(hospD) == 'Dictation.with.Speech.Recognition'] <- 'TS_DictSpeechRec'
names(hospD)[names(hospD) == 'Mobile..WLAN'] <- 'TS_MobileWLAN'
names(hospD)[names(hospD) == 'RFID'] <- 'TS_RFID'
names(hospD)[names(hospD) == 'Telemedicine'] <- 'TS_Telemed'
names(hospD)[names(hospD) == 'Recommend.hospital...star.rating'] <- 'StarRate_Rec'
names(hospD)[names(hospD) == 'Overall.hospital.rating...star.rating'] <- 'StarRate_Overall'


#########################################
#Fill In Missing Values for HIE & Tech Score (HCAHP NAs Stay NA)

#HIE_YN
count(hospD[!complete.cases(hospD$HIE_YN),])     #1312 MV
summary(as.factor(hospD$HIE_YN))                 #208-0 / 2640-1 / 1312-NA = (1520 Should Be 0)
hospD$HIE_YN[is.na(hospD$HIE_YN)] <- 0           #fill in missing values with 0 HIE_YN
count(hospD[!complete.cases(hospD$HIE_YN),])     #0 MV
summary(as.factor(hospD$HIE_YN))                 #1520-0 / #2640-1

#HIE_SUM
summary(as.factor(hospD$HIE_SUM))                #208-0 / 1312-NA = (1520 Should Be 0)
hospD$HIE_SUM[is.na(hospD$HIE_SUM)] <- 0         #fill in missing values with 0 HIE_SUM
summary(as.factor(hospD$HIE_SUM))                #1520-0 /


#TechScore
summary(as.factor(hospD$TechScore))              #0-0 / 203-NA = (203 Should Be 0)
hospD$TechScore[is.na(hospD$TechScore)] <- 0     #fill in missing values with 0 HIE_SUM
summary(as.factor(hospD$TechScore))              #203-0 /

#Trauma Level
summary(as.factor(hospD$CertTraumaA))            #0-1790 1-1415  NA-955 
summary(as.factor(hospD$TraumaLevelA))           #1-222  2-309  3-489  4-366 5-1 NA-2773
hospD$TraumaLevelB <- hospD$TraumaLevelA
hospD$TraumaLevelB[hospD$CertTraumaA==0] <- 0
summary(as.factor(hospD$TraumaLevelB))              
testTrauma <- subset(hospD, select=c(20:22,51))  #NA=986 Some with Trauma Center NA on Level
rm(testTrauma)

#########################################
#Initial Exploratory Data

#Original HIE Split
hist(hospD$HIE_YN)      #2640/4160 = 63.46%

#Filter Data to Only Include General Medical & Surgical?
summary(hospD$OrgPrimServ)             #General Medical & Surgical 2277
summary(as.factor(hospD$ServiceCodeA)) #10 General Medical & Surgical 3653

#Explore Subset
genMedD <- hospD[hospD$ServiceCodeA == 10, ] #Subset to General Medical and Surgical AHA #3653
summary(as.factor(genMedD$HIE_YN))           #0-1263 1- 2390  TOT=3653 = 65.43%

#Graph Histogram for Presentation
breakdown <- as.data.frame(summary(as.factor(hospD$ServiceCodeA))) #10 General Medical & Surgical 3653
rm(breakdown)
breakdown <- read.csv('C:/Users/rmari/...Chart_GenMedBreakdown.csv', header = TRUE, sep = ",")
barplot(as.matrix(breakdown), col="yellowgreen",  las=2, cex.names=.7, ylim=c(0,4000))
dev.off()
rm(breakdown)

#########################################
#Missing Values
MV_hospital <- na.omit(genMedD) #2,688 Cases with Missing Values / 73% (73.583356)
rm(MV_hospital)

count(genMedD[!complete.cases(genMedD$City),])                 #0
count(genMedD[!complete.cases(genMedD$State),])                #0
count(genMedD[!complete.cases(genMedD$Rank100LargeCity),])     #0
count(genMedD[!complete.cases(genMedD$CBSATypeA),])            #0
count(genMedD[!complete.cases(genMedD$OrgControlType),])       #0
count(genMedD[!complete.cases(genMedD$ControlCodeA),])         #0
count(genMedD[!complete.cases(genMedD$AccredJC_A),])           #0
count(genMedD[!complete.cases(genMedD$AccredCOTH_A),])         #0
count(genMedD[!complete.cases(genMedD$BedsLicenseH),])         #0
count(genMedD[!complete.cases(genMedD$BedsStaffH),])           #0
count(genMedD[!complete.cases(genMedD$BedSizeGrpA),])          #0
count(genMedD[!complete.cases(genMedD$EMRAM_Stage),])          #0
count(genMedD[!complete.cases(genMedD$TechScore),])            #0
count(genMedD[!complete.cases(genMedD$PatientRepServA),])      #737
count(genMedD[!complete.cases(genMedD$TotOutpatientVisA),])    #0
count(genMedD[!complete.cases(genMedD$EmergencyRoomVisA),])    #0
count(genMedD[!complete.cases(genMedD$EmergencyDeptA),])       #737
count(genMedD[!complete.cases(genMedD$CertTraumaA),])          #737
count(genMedD[!complete.cases(genMedD$TraumaLevelA),])         #2296
count(genMedD[!complete.cases(genMedD$TraumaLevelB),])         #766
count(genMedD[!complete.cases(genMedD$OpExpenseH),])           #173
count(genMedD[!complete.cases(genMedD$TotEmployeesH),])        #3
count(genMedD[!complete.cases(genMedD$FTPhysDenA),])           #0
count(genMedD[!complete.cases(genMedD$FTRegNurseA),])          #0
count(genMedD[!complete.cases(genMedD$FTVocNurseA),])          #0
count(genMedD[!complete.cases(genMedD$FTPersTotA),])           #0
count(genMedD[!complete.cases(genMedD$FTHospPersTotA),])       #0
count(genMedD[!complete.cases(genMedD$AdmissionsA),])           #0
count(genMedD[!complete.cases(genMedD$AdjAdmissA),])           #0
count(genMedD[!complete.cases(genMedD$AdjPatientDaysA),])      #0
count(genMedD[!complete.cases(genMedD$HIE_YN),])               #0
count(genMedD[!complete.cases(genMedD$NetPatientRevH),])       #3
count(genMedD[!complete.cases(genMedD$TotInpatientRevH),])     #3
count(genMedD[!complete.cases(genMedD$StarRate_Overall),])     #917


#########################################
#More Exploration

#Technology Score
summary(as.factor(genMedD$TechScore))
hist(genMedD$TechScore)
plot(as.factor(genMedD$TechScore), col="mediumorchid2", ylim=c(0,1200))

#Shapiro-Wilk Test of Normality
shapiro.test(genMedD$TechScore)
#W = 0.93993, p-value < 2.2e-16

#Original HIE Split
hist(genMedD$BedSizeGrpA)
plot(genMedD$CBSATypeA)
plot(genMedD$OrgControlType)
hist(genMedD$AccredJC_A)
table(genMedD$AccredJC_A, genMedD$HIE_YN)
table(genMedD$AccredCOTH_A, genMedD$HIE_YN)
table(genMedD$EMRAM_Stage, genMedD$HIE_YN)

plot(genMedD$NetPatientRevH)
hist(genMedD$NetPatientRevH)
hist(genMedD$StarRate_Overall)

hist(hospD$HIE_YN)      
summary(hospD$OrgControlType)
plot(hospD$OrgControlType)
plot(hospD$ServiceCodeA)
str(hospD$ServiceCodeA)

#Exploration After Cleaning Recoding Transformaiton

#Control Type
plot(genMedD$AllCont, col="cadetblue3", ylim=c(0,2000))          #USED
summary(genMedD$AllCont)
table(genMedD$Gov, genMedD$HIE_YN)
table(genMedD$Church, genMedD$HIE_YN)
table(genMedD$Prof, genMedD$HIE_YN)
table(genMedD$MultiSys, genMedD$HIE_YN)
summary(as.factor(genMedD$MultiSys))
summary(as.factor(genMedD$HIE_YN))

#Emergency Department
plot((as.factor(genMedD$NewER)), col="tomato", ylim=c(0,4000))   #USED
summary(as.factor(genMedD$NewER))

#Urban Rural
#Change Color Pallet
col_Rur <- c("palegreen3", "gold2", "rosybrown1")       #USE FOR BAR
palette(col_Rur)

col_Rur1 <- c("palegreen3", "gold2", "lightpink1")
palette(col_Rur1)

col_Rur2 <- c("green3", "gold2", "hotpink1")            #USE for Dist
palette(col_Rur2)

#Histogram
#plot(genMedD$CBSATypeA, col="yellowgreen")
#plot(genMedD$CBSATypeA, col="palegreen2")       
#plot(genMedD$CBSATypeA, col="palegreen2")
plot(genMedD$CBSATypeA, col=col_Rur)                    #USED
#plot(genMedD$CBSATypeA, col=col_Rur1)
table(genMedD$CBSATypeA, genMedD$HIE_YN)
table(genMedD$CBSATypeA, genMedD$BedSizeGrpA)
summary(genMedD$CBSATypeA)

#Kernel Density Plot Compare
sm.density.compare(genMedD$BedSizeGrpA, genMedD$CBSATypeA, h=.75, lty=c(1,1,1), col=col_Rur2) #USED
sm.density.compare(genMedD$BedSizeGrpA, genMedD$CBSATypeA, h=.75, lty=c(1,1,1), col=col_Rur2, xlim=c(0,8))
#legend("topright", levels(genMedD$CBSATypeA), fill=2+(0:nlevels(genMedD$CBSATypeA)))

#GG Plot Density
#densityplot(~genMedD$BedSizeGrpA, group=genMedD$CBSATypeA, data=genMedD)
#ggplot(genMedD) + geom_density(aes(x=BedSizeGrpA, fill=CBSATypeA), alpha=0.2)
#ggplot(genMedD, aes(length, fill=BedSizeGrpA)) + geom_density(alpha=0.35)
#str(genMedD$BedSizeGrpA)

#Top100City
plot(as.factor(genMedD$Top100City_YN))
table(genMedD$Top100City_YN, genMedD$HIE_YN)
plot(jitter(genMedD$HIE_YN) ~ jitter(genMedD$Top100City_YN))

#Bed Size
plot(as.factor(genMedD$BedSizeGrpA), col="lightgoldenrod", ylim=c(0,1000))
summary(as.factor(genMedD$BedSizeGrpA))
plot(jitter(genMedD$HIE_YN) ~ jitter(genMedD$BedSizeGrpA))
table(genMedD$BedSizeGrpA, genMedD$HIE_YN)
df1 <- as.data.frame(as.matrix(table(genMedD$BedSizeGrpA, genMedD$HIE_YN)))
#rm(df1)

#Other Comparisons with HIE
summary(as.factor(genMedD$EMRAM_Stage))
table(genMedD$EMRAM_Stage, genMedD$HIE_YN)

summary(as.factor(genMedD$Tech_Score))
table(genMedD$EMRAM_Stage, genMedD$HIE_YN)

summary(as.factor(genMedD$PatientRepServA))
table(genMedD$PatientRepServA, genMedD$HIE_YN)


#########################################
#ADDED AVERAGES

#AVERAGES
NewHIE <- genMedD[which(genMedD$HIE_YN==1),]
NewNoHIE <- genMedD[which(genMedD$HIE_YN==0),]

summary(NewHIE$BedSizeGrpA)
summary(NewNoHIE$BedSizeGrpA)

summary(NewHIE$TotOutpatientVisA)
summary(NewNoHIE$TotOutpatientVisA)

summary(NewHIE$PercDr)
summary(NewNoHIE$PercDr)

summary(NewHIE$PercNurse)
summary(NewNoHIE$PercNurse)

summary(NewHIE$TechScore)
summary(NewNoHIE$TechScore)

summary(NewHIE$EMRAM_Stage)
summary(NewNoHIE$EMRAM_Stage)

#########################################
#Creating New Variables and Further Cleaning

#Creating Dummy Columns Template
#toDataSet <- dummy_cols(DataSet, select_columns = "VariableName", remove_first_dummy = FALSE)

#System Type
summary(genMedD$SysType)                                       #2421-Mulit   1232-Single
genMedD$MultiSys <- 0
genMedD$MultiSys[genMedD$SysType=="Multi"] <- 1
summary(as.factor(genMedD$MultiSys))             #Check:Okay   2421-1 1232-0

#Control Type
summary(as.factor(genMedD$ControlCodeA))
#G    #G   #G   #G   #G   #R   #NP  #FP  #FP  #FP  #G   #G
#12   13   14   15   16   21   23   31   32   33   44   47
#35  294   75   16  383  407 1907   13  102  418    1    2 

genMedD$Church <- 0
genMedD$Church[genMedD$ControlCodeA==21] <- 1
summary(as.factor(genMedD$Church))                 #Check:Okay   407-1 3246-0

genMedD$Gov <- 0
genMedD$Gov[genMedD$ControlCodeA==12 | genMedD$ControlCodeA==13 | genMedD$ControlCodeA==14 | 
            genMedD$ControlCodeA==15 | genMedD$ControlCodeA==16 | genMedD$ControlCodeA==44 |
            genMedD$ControlCodeA==47] <- 1
summary(as.factor(genMedD$Gov))                    #Check:Okay 1-806 0-2847  

genMedD$NonProf <- 0
genMedD$NonProf[genMedD$ControlCodeA==23]<- 1
summary(as.factor(genMedD$NonProf))                #Check:Okay 1-1907 0-1746  

genMedD$Prof <- 0
genMedD$Prof[genMedD$ControlCodeA==31 | genMedD$ControlCodeA==32 | genMedD$ControlCodeA==33] <- 1
summary(as.factor(genMedD$Prof))                   #Check:Okay 1-533 0-3120 

genMedD$AllCont <- 0
genMedD$AllCont[genMedD$Church==1] <- "Church"
genMedD$AllCont[genMedD$Gov==1] <- "Government"
genMedD$AllCont[genMedD$NonProf==1] <- "NonProfit"
genMedD$AllCont[genMedD$Prof==1] <- "ForProfit"
genMedD$AllCont <- as.factor(genMedD$AllCont)  
summary(genMedD$AllCont)
plot(genMedD$AllCont, ylim=c(0,2000))


#Recode Accreditation JC and Member COTH 1/2 to 1/0

summary(as.factor(genMedD$AccredJC_A))              #1-2252  2-1401 
genMedD$NewAccJC <- 0
genMedD$NewAccJC[genMedD$AccredJC_A==1] <- 1
summary(as.factor(genMedD$NewAccJC))                #Check:Okay 1-2252 0-1401

summary(as.factor(genMedD$AccredCOTH_A))            #1-190   2-3462
genMedD$NewMemCOTH <- 0
genMedD$NewMemCOTH[genMedD$AccredCOTH_A==1] <- 1
summary(as.factor(genMedD$NewMemCOTH))              #Check:Okay 1-190 0-3463


#Emergency Room
summary(as.factor(genMedD$EmergencyDeptA))          #0-31 1-2885  NA-737
numER <- genMedD$EmergencyRoomVisA
numER <- as.data.frame(numER)
numER$ER_YN <- 0
numER$ER_YN[numER$numER>0] <- 1
genMedD$NewER <- genMedD$EmergencyDeptA
genMedD$NewER[is.na(genMedD$NewER)] <- numER$ER_YN[is.na(genMedD$NewER)]
summary(as.factor(genMedD$NewER))                     #0-31 1-3622
testEmergency <- subset(genMedD, select=c(20,25,60))  
#rm(numER)
#rm(testEmergency)


#% Dr Staff
genMedD$PercDr <- (genMedD$FTPhysDenA / genMedD$FTPersTotA) * 100

#% Nurse Staff
genMedD$PercNurse <- ((genMedD$FTRegNurseA + genMedD$FTVocNurseA) / genMedD$FTPersTotA) * 100

#Top Cities
genMedD$Top100City_YN <- 0
genMedD$Top100City_YN[genMedD$Rank100LargeCity>0] <- 1

genMedD$Top10City_YN <- 0
genMedD$Top10City_YN[genMedD$Rank100LargeCity>0 & genMedD$Rank100LargeCity<11] <- 1

testRankCity <- subset(genMedD, select=c(35,63,64)) 
summary(as.factor(genMedD$Top100City_YN))             #0-3269  1-384 - use this
summary(as.factor(genMedD$Top10City_YN))              #0-3571  1-82 

#Rural
summary(genMedD$CBSATypeA)                            #Metro-2019   Micro-674   Rural-960 
genMedD <- dummy_cols(genMedD, select_columns = "CBSATypeA", remove_first_dummy = FALSE)
summary(as.factor(genMedD$CBSATypeA_Metro))           #Check:okay   0-1634 1-2019 
summary(as.factor(genMedD$CBSATypeA_Rural))           #Check:okay   0-2693 1-960 
summary(as.factor(genMedD$CBSATypeA_Micro))           #Check:okay   0-2979 1-674 

#Patient Rev / Bed Staffed Size
genMedD$NetRevSize <- genMedD$NetPatientRevH / genMedD$BedsStaffH


#########################################
#Correlation Analysis

#_________________________________
#All
#Subsetting Only Numeric Data
CorrMatGen <- subset(genMedD, select = c(11:13,15:17,21,23:34,46,51:56,58:67,48,50,14,68))
str(CorrMatGen)
dfCorrAll <- cor(CorrMatGen, method = "pearson", use = "pairwise.complete.obs")
dfCorrAllR <- round(dfCorrAll, digits=3) #rounding to 3 decimals

#Base Rate
summary(as.factor(genMedD$HIE_YN))      #0-1263 1-2390  BR=65.43%

#_________________________________
#Subsetting Only Bed Group Size 1 or 2
BSG12 <- genMedD[which(genMedD$BedSizeGrpA==1 | genMedD$BedSizeGrpA==2),]
cmBG12 <- subset(BSG12, select = c(54,53,56,55,52,63,66,65,67,58,59,32,26,60,23,61,62,46,13,48,68,50))
CorrBG12 <- cor(cmBG12, method = "pearson", use = "pairwise.complete.obs")
CorrBG12 <- round(CorrBG12, digits=3) #rounding to 3 decimals

#Base Rate
summary(as.factor(BSG12$HIE_YN))      #0-550 1-781  BR=58.68%

#_________________________________
#Subsetting Only Bed Group Size 3 or 4
BSG34 <- genMedD[which(genMedD$BedSizeGrpA==3 | genMedD$BedSizeGrpA==4),]
cmBG34 <- subset(BSG34, select = c(54,53,56,55,52,63,66,65,67,58,59,32,26,60,23,61,62,46,13,48,68,50))
CorrBG34 <- cor(cmBG34, method = "pearson", use = "pairwise.complete.obs")
CorrBG34 <- round(CorrBG34, digits=3) #rounding to 3 decimals

summary(as.factor(BSG34$HIE_YN))      #0-472 1-843  BR=64.11%

#_________________________________
#Subsetting Only Bed Group Size 5 or 6
BSG56 <- genMedD[which(genMedD$BedSizeGrpA==5 | genMedD$BedSizeGrpA==6),]
cmBG56 <- subset(BSG56, select = c(54,53,56,55,52,63,66,65,67,58,59,32,26,60,23,61,62,46,13,48,68,50))
CorrBG56 <- cor(cmBG56, method = "pearson", use = "pairwise.complete.obs")
CorrBG56 <- round(CorrBG56, digits=3) #rounding to 3 decimals

summary(as.factor(BSG56$HIE_YN))      #0-177 1-449  BR=71.73%

#_________________________________
#Subsetting Only Bed Group Size 7 or 8
BSG78 <- genMedD[which(genMedD$BedSizeGrpA==7 | genMedD$BedSizeGrpA==8),]
cmBG78 <- subset(BSG78, select = c(54,53,56,55,52,63,66,65,67,58,59,32,26,60,23,61,62,46,13,48,68,50))
CorrBG78 <- cor(cmBG78, method = "pearson", use = "pairwise.complete.obs")
CorrBG78 <- round(CorrBG78, digits=3) #rounding to 3 decimals

summary(as.factor(BSG78$HIE_YN))      #0-64 1-317  BR=83.20%

#_________________________________
#Subsetting Only Metro
CBSA_Metro <- genMedD[which(genMedD$CBSATypeA_Metro==1),]
cmMetro <- subset(CBSA_Metro, select = c(54,53,56,55,52,63,66,65,67,58,59,32,26,60,23,61,62,46,13,48,68,50))
CorrMetro <- cor(cmMetro, method = "pearson", use = "pairwise.complete.obs")
CorrMetro <- round(CorrMetro, digits=3) #rounding to 3 decimals

summary(as.factor(CBSA_Metro$HIE_YN))      #0-597 1-1422  BR=70.43% #=2019

#_________________________________
#Subsetting Only Rural
CBSA_Rural <- genMedD[which(genMedD$CBSATypeA_Rural==1),]
cmRural <- subset(CBSA_Rural, select = c(54,53,56,55,52,63,66,65,67,58,59,32,26,60,23,61,62,46,13,48,68,50))
CorrRural <- cor(cmRural, method = "pearson", use = "pairwise.complete.obs")
CorrRural <- round(CorrRural, digits=3) #rounding to 3 decimals

summary(as.factor(CBSA_Rural$HIE_YN))      #0-429 1-531 BR=55.31% #=960

#_________________________________
#Subsetting Only Micro
CBSA_Micro <- genMedD[which(genMedD$CBSATypeA_Micro==1),]
cmMicro <- subset(CBSA_Micro, select = c(54,53,56,55,52,63,66,65,67,58,59,32,26,60,23,61,62,46,13,48,68,50))
CorrMicro <- cor(cmMicro, method = "pearson", use = "pairwise.complete.obs")
CorrMicro <- round(CorrMicro, digits=3) #rounding to 3 decimals

summary(as.factor(CBSA_Micro$HIE_YN))      #0-237 1-437 BR=64.84% #=674

#*********************************
#Write Correlation Matrix to File
#write.csv(dfCorrAllR, 'C:/Users/rmari/...CorrAllGen1.csv') #writing to exel
#write.csv(CorrBG12, 'C:/Users/rmari/...CorrBG121.csv') #writing to exel
#write.csv(CorrBG34, 'C:/Users/rmari/...CorrBG341.csv') #writing to exel
#write.csv(CorrBG56, 'C:/Users/rmari/...CorrBG561.csv') #writing to exel
#write.csv(CorrMetro, 'C:/Users/rmari/...CorrMetro1.csv') #writing to exel
#write.csv(CorrRural, 'C:/Users/rmari/...CorrRural1.csv') #writing to exel
#write.csv(CorrMicro, 'C:/Users/rmari/...CorrMicro1.csv') #writing to exel
#rm(CBSA_Rural)
#write.csv(hospD, 'C:/Users/rmari/...backUpHD4160Y.csv') #writing to exel


#Significance Testing

#_________________________________
#All
cor.test(genMedD$Gov, genMedD$HIE_YN)               #t = -6.4379, df = 3651, p-value = 1.369e-10
cor.test(genMedD$Church, genMedD$HIE_YN)            #t = 8.2232, df = 3651, p-value = 2.725e-16
cor.test(genMedD$Prof, genMedD$HIE_YN)              #t = -22.943, df = 3651, p-value < 2.2e-16
cor.test(genMedD$NonProf, genMedD$HIE_YN)           #t = 15.86, df = 3651, p-value < 2.2e-16
cor.test(genMedD$MultiSys, genMedD$HIE_YN)          #t = 9.6885, df = 3651, p-value < 2.2e-16
cor.test(genMedD$Top100City_YN, genMedD$HIE_YN)     #t = 3.7224, df = 3651, p-value = 0.0002003
cor.test(genMedD$CBSATypeA_Rural, genMedD$HIE_YN)   #t = -7.7337, df = 3651, p-value = 1.342e-14
cor.test(genMedD$CBSATypeA_Metro, genMedD$HIE_YN)   #t = 7.1173, df = 3651, p-value = 1.319e-12
cor.test(genMedD$CBSATypeA_Micro, genMedD$HIE_YN)   #t = -0.35587, df = 3651, p-value = 0.722
cor.test(genMedD$NewAccJC, genMedD$HIE_YN)          #t = 4.635, df = 3651, p-value = 3.694e-06
cor.test(genMedD$NewMemCOTH, genMedD$HIE_YN)        #t = 7.6876, df = 3651, p-value = 1.915e-14
cor.test(genMedD$BedSizeGrpA, genMedD$HIE_YN)       #t = 9.9476, df = 3651, p-value < 2.2e-16
cor.test(genMedD$TotOutpatientVisA, genMedD$HIE_YN) #t = 11.902, df = 3651, p-value < 2.2e-16
cor.test(genMedD$PatientRepServA, genMedD$HIE_YN)   #t = 9.4333, df = 2914, p-value < 2.2e-16
cor.test(genMedD$PercDr, genMedD$HIE_YN)            #t = 5.7474, df = 3651, p-value = 9.806e-09
cor.test(genMedD$PercNurse, genMedD$HIE_YN)         #t = -2.6021, df = 3651, p-value = 0.009303
cor.test(genMedD$TechScore, genMedD$HIE_YN)         #t = 22.26, df = 3651, p-value < 2.2e-16
cor.test(genMedD$EMRAM_Stage, genMedD$HIE_YN)       #t = 16.984, df = 3651, p-value < 2.2e-16

#Check Number
table(genMedD$Gov, genMedD$HIE_YN)              
table(genMedD$Church, genMedD$HIE_YN)            
table(genMedD$Prof, genMedD$HIE_YN)              
table(genMedD$NonProf, genMedD$HIE_YN)           
table(genMedD$MultiSys, genMedD$HIE_YN)          
table(genMedD$Top100City_YN, genMedD$HIE_YN)     
table(genMedD$CBSATypeA_Rural, genMedD$HIE_YN)   
table(genMedD$CBSATypeA_Metro, genMedD$HIE_YN)   
table(genMedD$CBSATypeA_Micro, genMedD$HIE_YN)   
table(genMedD$NewAccJC, genMedD$HIE_YN)          
table(genMedD$NewMemCOTH, genMedD$HIE_YN)       
table(genMedD$BedSizeGrpA, genMedD$HIE_YN)       
table(genMedD$PatientRepServA, genMedD$HIE_YN)   
table(genMedD$TechScore, genMedD$HIE_YN)         
table(genMedD$EMRAM_Stage, genMedD$HIE_YN)       

#_________________________________
#Bed Size Group 1/2
cor.test(BSG12$Gov, BSG12$HIE_YN)                   #t = -5.3248, df = 1329, p-value = 1.186e-07
cor.test(BSG12$Church, BSG12$HIE_YN)                #t = 4.6573, df = 1329, p-value = 3.525e-06
cor.test(BSG12$Prof, BSG12$HIE_YN)                  #t = -9.2843, df = 1329, p-value < 2.2e-16
cor.test(BSG12$NonProf, BSG12$HIE_YN)               #t = 8.0825, df = 1329, p-value = 1.414e-15
cor.test(BSG12$MultiSys, BSG12$HIE_YN)              #t = 9.0014, df = 1329, p-value < 2.2e-16
cor.test(BSG12$Top100City_YN, BSG12$HIE_YN)         #t = -2.8923, df = 1329, p-value = 0.003887
cor.test(BSG12$CBSATypeA_Rural, BSG12$HIE_YN)       #t = -2.3566, df = 1329, p-value = 0.01859
cor.test(BSG12$CBSATypeA_Metro, BSG12$HIE_YN)       #t = 0.055081, df = 1329, p-value = 0.9561
cor.test(BSG12$CBSATypeA_Micro, BSG12$HIE_YN)       #t = 2.8437, df = 1329, p-value = 0.004527
cor.test(BSG12$NewAccJC, BSG12$HIE_YN)              #t = 2.8725, df = 1329, p-value = 0.004137
cor.test(BSG12$NewMemCOTH, BSG12$HIE_YN)            #NA
cor.test(BSG12$BedSizeGrpA, BSG12$HIE_YN)           #t = 1.5455, df = 1329, p-value = 0.1225
cor.test(BSG12$TotOutpatientVisA, BSG12$HIE_YN)     #t = 6.2371, df = 1329, p-value = 5.979e-10
cor.test(BSG12$PatientRepServA, BSG12$HIE_YN)       #t = 3.4468, df = 1032, p-value = 0.0005901
cor.test(BSG12$PercDr, BSG12$HIE_YN)                #t = 3.0647, df = 1329, p-value = 0.002223
cor.test(BSG12$PercNurse, BSG12$HIE_YN)             #t = -1.8506, df = 1329, p-value = 0.06444
cor.test(BSG12$TechScore, BSG12$HIE_YN)             #t = 11.745, df = 1329, p-value < 2.2e-16
cor.test(BSG12$EMRAM_Stage, BSG12$HIE_YN)           #t = 9.7957, df = 1329, p-value < 2.2e-16

#Check Number
table(BSG12$Gov, BSG12$HIE_YN)                   
table(BSG12$Church, BSG12$HIE_YN)                
table(BSG12$Prof, BSG12$HIE_YN)                  
table(BSG12$NonProf, BSG12$HIE_YN)              
table(BSG12$MultiSys, BSG12$HIE_YN)              
table(BSG12$Top100City_YN, BSG12$HIE_YN)         
table(BSG12$CBSATypeA_Rural, BSG12$HIE_YN)       
table(BSG12$CBSATypeA_Metro, BSG12$HIE_YN)       
table(BSG12$CBSATypeA_Micro, BSG12$HIE_YN)       
table(BSG12$NewAccJC, BSG12$HIE_YN)              
table(BSG12$NewMemCOTH, BSG12$HIE_YN)              #0 Member COTH            
table(BSG12$BedSizeGrpA, BSG12$HIE_YN)          
table(BSG12$PatientRepServA, BSG12$HIE_YN)      
table(BSG12$TechScore, BSG12$HIE_YN)             
table(BSG12$EMRAM_Stage, BSG12$HIE_YN)          

#_________________________________
#Bed Size Group 3/4
cor.test(BSG34$Gov, BSG34$HIE_YN)                   #t = -2.2262, df = 1313, p-value = 0.02617
cor.test(BSG34$Church, BSG34$HIE_YN)                #t = 4.8116, df = 1313, p-value = 1.67e-06
cor.test(BSG34$Prof, BSG34$HIE_YN)                  #t = -14.74, df = 1313, p-value < 2.2e-16
cor.test(BSG34$NonProf, BSG34$HIE_YN)               #t = 9.6578, df = 1313, p-value < 2.2e-16
cor.test(BSG34$MultiSys, BSG34$HIE_YN)              #t = 2.7769, df = 1313, p-value = 0.005566
cor.test(BSG34$Top100City_YN, BSG34$HIE_YN)         #t = 0.20179, df = 1313, p-value = 0.8401
cor.test(BSG34$CBSATypeA_Rural, BSG34$HIE_YN)       #t = -3.3798, df = 1313, p-value = 0.0007467
cor.test(BSG34$CBSATypeA_Metro, BSG34$HIE_YN)       #t = 3.0721, df = 1313, p-value = 0.00217
cor.test(BSG34$CBSATypeA_Micro, BSG34$HIE_YN)       #t = -0.53148, df = 1313, p-value = 0.5952
cor.test(BSG34$NewAccJC, BSG34$HIE_YN)              #t = -0.48223, df = 1313, p-value = 0.6297
cor.test(BSG34$NewMemCOTH, BSG34$HIE_YN)            #t = -0.41591, df = 1313, p-value = 0.6775
cor.test(BSG34$BedSizeGrpA, BSG34$HIE_YN)           #t = 0.2844, df = 1313, p-value = 0.7761
cor.test(BSG34$TotOutpatientVisA, BSG34$HIE_YN)     #t = 6.5974, df = 1313, p-value = 6.054e-11
cor.test(BSG34$PatientRepServA, BSG34$HIE_YN)       #t = 4.0555, df = 1011, p-value = 5.386e-05
cor.test(BSG34$PercDr, BSG34$HIE_YN)                #t = 2.5332, df = 1313, p-value = 0.01142
cor.test(BSG34$PercNurse, BSG34$HIE_YN)             #t = -1.4894, df = 1313, p-value = 0.1366
cor.test(BSG34$TechScore, BSG34$HIE_YN)             #t = 13.227, df = 1313, p-value < 2.2e-16
cor.test(BSG34$EMRAM_Stage, BSG34$HIE_YN)           #t = 9.2339, df = 1313, p-value < 2.2e-16

#Check Number
table(BSG34$Gov, BSG34$HIE_YN)                   
table(BSG34$Church, BSG34$HIE_YN)                
table(BSG34$Prof, BSG34$HIE_YN)                  
table(BSG34$NonProf, BSG34$HIE_YN)               
table(BSG34$MultiSys, BSG34$HIE_YN)              
table(BSG34$Top100City_YN, BSG34$HIE_YN)        
table(BSG34$CBSATypeA_Rural, BSG34$HIE_YN)      
table(BSG34$CBSATypeA_Metro, BSG34$HIE_YN)      
table(BSG34$CBSATypeA_Micro, BSG34$HIE_YN)       
table(BSG34$NewAccJC, BSG34$HIE_YN)              
table(BSG34$NewMemCOTH, BSG34$HIE_YN)               #only 2 Member COTH          
table(BSG34$BedSizeGrpA, BSG34$HIE_YN)           
table(BSG34$PatientRepServA, BSG34$HIE_YN)       
table(BSG34$TechScore, BSG34$HIE_YN)            
table(BSG34$EMRAM_Stage, BSG34$HIE_YN)           

#_________________________________
#Bed Size Group 5/6
cor.test(BSG56$Gov, BSG56$HIE_YN)                   #t = 0.65216, df = 624, p-value = 0.5145
cor.test(BSG56$Church, BSG56$HIE_YN)                #t = 3.9113, df = 624, p-value = 0.0001019
cor.test(BSG56$Prof, BSG56$HIE_YN)                  #t = -14.721, df = 624, p-value < 2.2e-16
cor.test(BSG56$NonProf, BSG56$HIE_YN)               #t = 6.9578, df = 624, p-value = 8.762e-12
cor.test(BSG56$MultiSys, BSG56$HIE_YN)              #t = -0.51285, df = 624, p-value = 0.6082
cor.test(BSG56$Top100City_YN, BSG56$HIE_YN)         #t = 1.0342, df = 624, p-value = 0.3014
cor.test(BSG56$CBSATypeA_Rural, BSG56$HIE_YN)       #t = -0.96712, df = 624, p-value = 0.3339
cor.test(BSG56$CBSATypeA_Metro, BSG56$HIE_YN)       #t = 0.9142, df = 624, p-value = 0.361
cor.test(BSG56$CBSATypeA_Micro, BSG56$HIE_YN)       #t = -0.67277, df = 624, p-value = 0.5013
cor.test(BSG56$NewAccJC, BSG56$HIE_YN)              #t = -1.8227, df = 624, p-value = 0.06883
cor.test(BSG56$NewMemCOTH, BSG56$HIE_YN)            #t = 1.415, df = 624, p-value = 0.1576
cor.test(BSG56$BedSizeGrpA, BSG56$HIE_YN)           #t = 2.2707, df = 624, p-value = 0.0235
cor.test(BSG56$TotOutpatientVisA, BSG56$HIE_YN)     #t = 4.6878, df = 624, p-value = 3.393e-06
cor.test(BSG56$PatientRepServA, BSG56$HIE_YN)       #t = 4.0071, df = 516, p-value = 7.052e-05
cor.test(BSG56$PercDr, BSG56$HIE_YN)                #t = 3.5431, df = 624, p-value = 0.000425
cor.test(BSG56$PercNurse, BSG56$HIE_YN)             #t = -3.8579, df = 624, p-value = 0.0001263
cor.test(BSG56$TechScore, BSG56$HIE_YN)             #t = 8.6553, df = 624, p-value < 2.2e-16
cor.test(BSG56$EMRAM_Stage, BSG56$HIE_YN)           #t = 5.1353, df = 624, p-value = 3.77e-07

#Check Number
table(BSG56$Gov, BSG56$HIE_YN)                   
table(BSG56$Church, BSG56$HIE_YN)               
table(BSG56$Prof, BSG56$HIE_YN)                 
table(BSG56$NonProf, BSG56$HIE_YN)               
table(BSG56$MultiSys, BSG56$HIE_YN)              
table(BSG56$Top100City_YN, BSG56$HIE_YN)        
table(BSG56$CBSATypeA_Rural, BSG56$HIE_YN)          #Only 4 Rural Hospitals       
table(BSG56$CBSATypeA_Metro, BSG56$HIE_YN)       
table(BSG56$CBSATypeA_Micro, BSG56$HIE_YN)       
table(BSG56$NewAccJC, BSG56$HIE_YN)             
table(BSG56$NewMemCOTH, BSG56$HIE_YN)          
table(BSG56$BedSizeGrpA, BSG56$HIE_YN)           
table(BSG56$PatientRepServA, BSG56$HIE_YN)       
table(BSG56$TechScore, BSG56$HIE_YN)           
table(BSG56$EMRAM_Stage, BSG56$HIE_YN)           


#_________________________________
#Bed Size Group 7/8
cor.test(BSG78$Gov, BSG78$HIE_YN)                   #t = 1.045, df = 379, p-value = 0.2967
cor.test(BSG78$Church, BSG78$HIE_YN)                #t = 1.3222, df = 379, p-value = 0.1869
cor.test(BSG78$Prof, BSG78$HIE_YN)                  #t = -12.497, df = 379, p-value < 2.2e-16
cor.test(BSG78$NonProf, BSG78$HIE_YN)               #t = 4.8857, df = 379, p-value = 1.523e-06
cor.test(BSG78$MultiSys, BSG78$HIE_YN)              #t = -0.34415, df = 379, p-value = 0.7309
cor.test(BSG78$Top100City_YN, BSG78$HIE_YN)         #t = -0.12462, df = 379, p-value = 0.9009
cor.test(BSG78$CBSATypeA_Rural, BSG78$HIE_YN)       #t = -3.1894, df = 379, p-value = 0.001544
cor.test(BSG78$CBSATypeA_Metro, BSG78$HIE_YN)       #t = 1.584, df = 379, p-value = 0.114
cor.test(BSG78$CBSATypeA_Micro, BSG78$HIE_YN)       #t = 0.0086443, df = 379, p-value = 0.9931
cor.test(BSG78$NewAccJC, BSG78$HIE_YN)              #t = 0.26043, df = 379, p-value = 0.7947
cor.test(BSG78$NewMemCOTH, BSG78$HIE_YN)            #t = 4.5382, df = 379, p-value = 7.628e-06
cor.test(BSG78$BedSizeGrpA, BSG78$HIE_YN)           #t = 2.0766, df = 379, p-value = 0.03851
cor.test(BSG78$TotOutpatientVisA, BSG78$HIE_YN)     #t = 4.1238, df = 379, p-value = 4.582e-05
cor.test(BSG78$PatientRepServA, BSG78$HIE_YN)       #t = 0.99381, df = 349, p-value = 0.321
cor.test(BSG78$PercDr, BSG78$HIE_YN)                #t = 2.9387, df = 379, p-value = 0.003497
cor.test(BSG78$PercNurse, BSG78$HIE_YN)             #t = -5.1525, df = 379, p-value = 4.143e-07
cor.test(BSG78$TechScore, BSG78$HIE_YN)             #t = 5.6979, df = 379, p-value = 2.44e-08
cor.test(BSG78$EMRAM_Stage, BSG78$HIE_YN)           #t = 2.8798, df = 379, p-value = 0.004205

#Check Number
table(BSG78$Gov, BSG78$HIE_YN)                 
table(BSG78$Church, BSG78$HIE_YN)                
table(BSG78$Prof, BSG78$HIE_YN)                  
table(BSG78$NonProf, BSG78$HIE_YN)              
table(BSG78$MultiSys, BSG78$HIE_YN)              
table(BSG78$Top100City_YN, BSG78$HIE_YN)        
table(BSG78$CBSATypeA_Rural, BSG78$HIE_YN)          #Only 2 Rural     
table(BSG78$CBSATypeA_Metro, BSG78$HIE_YN)          #Only 8 Not Metro
table(BSG78$CBSATypeA_Micro, BSG78$HIE_YN)          #only 6 Micro
table(BSG78$NewAccJC, BSG78$HIE_YN)              
table(BSG78$NewMemCOTH, BSG78$HIE_YN)            
table(BSG78$BedSizeGrpA, BSG78$HIE_YN)           
table(BSG78$PatientRepServA, BSG78$HIE_YN)          #Only 3 without Patient Rep Serv    
table(BSG78$TechScore, BSG78$HIE_YN)             
table(BSG78$EMRAM_Stage, BSG78$HIE_YN) 

#_________________________________
#Plot
boxplot(genMedD$TotOutpatientVisA)                  #Okay

#########################################
#Split Into Test Train Data

#_________________________________
#All
set.seed(123)
train_samp <- createDataPartition(y=genMedD$HIE_YN, p=0.8, list = FALSE)
train_All  <- genMedD[train_samp, ] 
test_All <- genMedD[-train_samp, ]

genMedD2 <-genMedD
genMedD2$BGBin <- 0
genMedD2$BGBin[genMedD$BedSizeGrpA==1 | genMedD$BedSizeGrpA==2] <- 1
genMedD2$BGBin[genMedD$BedSizeGrpA==3 | genMedD$BedSizeGrpA==4] <- 2
genMedD2$BGBin[genMedD$BedSizeGrpA==5 | genMedD$BedSizeGrpA==6] <- 3
genMedD2$BGBin[genMedD$BedSizeGrpA==7 | genMedD$BedSizeGrpA==8] <- 4
summary(as.factor(genMedD2$BGBin))
genMedD2$BGBin <- (as.factor(genMedD2$BGBin))
summary(genMedD2$BGBin)
genMedD2 <- dummy_cols(genMedD2, select_columns = "BGBin", remove_first_dummy = FALSE)
summary(as.factor(genMedD2$BGBin_4))
summary(as.factor(genMedD2$BGBin_3))
summary(as.factor(genMedD2$BGBin_2))
summary(as.factor(genMedD2$BGBin_1))

genMedD2$EMRAM_Bin <- 0
genMedD2$EMRAM_Bin[genMedD2$EMRAM_Stage==6 | genMedD2$EMRAM_Stage==7] <- 1
summary(as.factor(genMedD2$EMRAM_Stage))
summary(as.factor(genMedD2$EMRAM_Bin))

set.seed(123)
train_samp <- createDataPartition(y=genMedD2$HIE_YN, p=0.8, list = FALSE)
train_All  <- genMedD2[train_samp, ] 
test_All <- genMedD2[-train_samp, ]


#_________________________________
#Bed Group Size 12
set.seed(123)
train_samp <- createDataPartition(y=BSG12$HIE_YN, p=0.8, list = FALSE)
train_BSG12  <- BSG12[train_samp, ] 
test_BSG12 <- BSG12[-train_samp, ]


#_________________________________
#Bed Group Size 34
set.seed(123)
train_samp <- createDataPartition(y=BSG34$HIE_YN, p=0.8, list = FALSE)
train_BSG34  <- BSG34[train_samp, ] 
test_BSG34 <- BSG34[-train_samp, ]

#_________________________________
#Bed Group Size 56
set.seed(123)
train_samp <- createDataPartition(y=BSG56$HIE_YN, p=0.8, list = FALSE)
train_BSG56  <- BSG56[train_samp, ] 
test_BSG56 <- BSG56[-train_samp, ]

#_________________________________
#Bed Group Size 78
set.seed(123)
train_samp <- createDataPartition(y=BSG78$HIE_YN, p=0.8, list = FALSE)
train_BSG78  <- BSG78[train_samp, ] 
test_BSG78 <- BSG78[-train_samp, ]

#########################################
#Build Models

#_________________________________
#All

#All Variables  AUC = 0.8244494 
All_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                         NewAccJC + NewMemCOTH + BedSizeGrpA + TotOutpatientVisA + PercDr + PercNurse +
                         TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_Mod)


##All Bed Siz Group Bin

#Test
All_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                 NewAccJC + NewMemCOTH + BedSizeGrpA + TotOutpatientVisA + PercDr + PercNurse +
                 TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_Mod)


#New
All_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                 NewAccJC + NewMemCOTH + BGBin_2 + BGBin_3 + BGBin_4 + TotOutpatientVisA + PercDr + PercNurse +
                 TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_Mod)


#ADDED Model EMRAM - ALL DATA
#New

names(genMedD2)[names(genMedD2) == 'BGBin_2'] <- 'BGBin_B'
names(genMedD2)[names(genMedD2) == 'BGBin_3'] <- 'BGBin_C'
names(genMedD2)[names(genMedD2) == 'BGBin_4'] <- 'BGBin_D'


All_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                 NewAccJC + NewMemCOTH + BGBin_B + BGBin_C + BGBin_D + TotOutpatientVisA + PercDr + PercNurse +
                 TechScore + EMRAM_Bin, data=genMedD2, family = binomial)
summary(All_Mod)




#Test
All_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                 NewAccJC + NewMemCOTH + BedSizeGrpA + TotOutpatientVisA + PercDr + PercNurse +
                 TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_Mod)



#Exp 1
All_Mod1 <- glm(HIE_YN ~ Gov + Prof + Church + MultiSys + CBSATypeA_Rural + NewMemCOTH + BedSizeGrpA + 
                         TotOutpatientVisA + PercNurse + TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_Mod1)

#Exp 2
All_Mod2 <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + CBSATypeA_Rural + NewMemCOTH + 
                          TotOutpatientVisA + PercNurse + TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_Mod2)


#Exp 3
All_Mod4 <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + CBSATypeA_Rural + NewAccJC + TotOutpatientVisA +
                          PercNurse + TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_Mod4)

#Only Sig Variables #AUC = 0.8233369
All_ModSig <- glm(HIE_YN ~  Gov + Prof + MultiSys + CBSATypeA_Rural + NewAccJC + TotOutpatientVisA +
                          PercNurse + TechScore + EMRAM_Stage, data=train_All, family = binomial)
summary(All_ModSig)  


#Predictions on Test Data
fitAll <- predict(All_ModSig,newdata=test_All, type='response')

#ROC Curve
pr <- prediction(fitAll, test_All$HIE_YN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col="magenta")

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Seed 123
#0.8244494 Model with All Variables
#0.8233369 Model with Only Sig Variables
#0.8249776 Model Exp 1
#0.8249856 Model Exp 2 Current

#Seed 321
#0.8107324 with All Variables
#0.8110209 only Sig Variables
#0.8109191 Sig + Church
#0.8107833 with Exp 1 
#0.8107833 with Exp 2

#Extracting Coefficients
coefAll_Sig <-as.data.frame(summary(All_Mod)$coefficients)
write.csv(coefAll_Sig, 'C:/Users/rmari/...ModAll_Prof.csv') #writing to exel
#rm(All_Mod)

#_________________________________
#Bed Size Group 1 or 2

#All Variables 
BSG12_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                 NewAccJC + TotOutpatientVisA + PercDr + PercNurse +
                 TechScore + EMRAM_Stage, data=train_BSG12, family = binomial)
summary(BSG12_Mod)

#Block Model #Block - 0.7621264
BSG12_ModBM <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                   NewAccJC + TotOutpatientVisA + PercDr + TechScore + EMRAM_Stage, data=train_BSG12, family = binomial)
summary(BSG12_ModBM)

#Exp 1
BSG12_Mod1 <- glm(HIE_YN ~  Gov + Prof + MultiSys  + CBSATypeA_Rural +
                    NewAccJC + TotOutpatientVisA + PercNurse + TechScore + EMRAM_Stage, data=train_BSG12, family = binomial)
summary(BSG12_Mod1)

#Sig Variables - #Sig - 0.763046
BSG12_ModSig <- glm(HIE_YN ~  Prof + MultiSys + TotOutpatientVisA + PercNurse +
                    TechScore + EMRAM_Stage, data=train_BSG12, family = binomial)
summary(BSG12_ModSig)

#Predictions on Test Data
fitAll12 <- predict(BSG12_ModBM,newdata=test_BSG12, type='response')

#ROC Curve
pr <- prediction(fitAll12, test_BSG12$HIE_YN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col="magenta")

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#All - 0.760977
#Block - 0.7621264
#Exp 1 - 0.7612644
#Sig - 0.763046

#Extracting Coefficients
#coefBSG12_BM <-as.data.frame(summary(BSG12_ModBM)$coefficients)
#write.csv(coefBSG12_BM, 'C:/Users/rmari/...ModBSG12_BMX.csv') #writing to exel

#_________________________________
#Bed Size Group 3 or 4

#All Variables 
BSG34_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                   NewAccJC + TotOutpatientVisA + PercDr + PercNurse +
                   TechScore + EMRAM_Stage, data=train_BSG34, family = binomial)
summary(BSG34_Mod)


#Block Model #AUC - 0.7785596
BSG34_ModBM <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + CBSATypeA_Rural + TotOutpatientVisA 
                             + PercDr + TechScore + EMRAM_Stage, data=train_BSG34, family = binomial)
summary(BSG34_ModBM)

#Exp 1
BSG34_Mod1 <- glm(HIE_YN ~  Gov + Prof + MultiSys + NewAccJC + TotOutpatientVisA  + CBSATypeA_Rural +
                            TechScore + EMRAM_Stage, data=train_BSG34, family = binomial)
summary(BSG34_Mod1)

#Model Sig Values #AUC - 0.7756623
BSG34_ModSig <- glm(HIE_YN ~  Gov + Prof + MultiSys + TotOutpatientVisA + NewAccJC + 
                              TechScore + EMRAM_Stage, data=train_BSG34, family = binomial)
summary(BSG34_ModSig)


#Predictions on Test Data
fitAll34 <- predict(BSG34_ModSig, newdata=test_BSG34, type='response')

#ROC Curve
pr <- prediction(fitAll34, test_BSG34$HIE_YN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col="magenta")

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#AUC
#All - 0.7767266
#BM - 0.7785596
#Exp1 - 0.7770222
#Sig - 0.7756623


#Extracting Coefficients
#coefBSG34_Sig <-as.data.frame(summary(BSG34_ModSig)$coefficients)
#write.csv(coefBSG34_Sig, 'C:/Users/rmari/...ModBSG34_SigX.csv') #writing to excel


#_________________________________
#Bed Size Group 5 or 6

#All Variables 
BSG56_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN +
                   NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                   TechScore + EMRAM_Stage, data=train_BSG56, family = binomial)
summary(BSG56_Mod)


#Block Model 
BSG56_ModBM <- glm(HIE_YN ~  Gov + Prof + Church + TotOutpatientVisA + PercDr + PercNurse +
                   TechScore + EMRAM_Stage, data=train_BSG56, family = binomial)
summary(BSG56_ModBM)

#Exp1
BSG56_Mod1 <- glm(HIE_YN ~  Prof + Church + PercNurse +
                     TechScore + EMRAM_Stage, data=train_BSG56, family = binomial)
summary(BSG56_Mod1)

#Sig
BSG56_ModSig <- glm(HIE_YN ~  Prof + Church + PercDr + TechScore + EMRAM_Stage, data=train_BSG56, family = binomial)
summary(BSG56_ModSig)

#Sig2
BSG56_ModSig2 <- glm(HIE_YN ~  Prof + Church + TechScore + EMRAM_Stage, data=train_BSG56, family = binomial)
summary(BSG56_ModSig2)

#Predictions on Test Data
fitAll56 <- predict(BSG56_ModBM, newdata=test_BSG56, type='response')

#ROC Curve
pr <- prediction(fitAll56, test_BSG56$HIE_YN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col="magenta")

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#AUC
#All - 0.8148479
#BM - 0.8154442
#Exp1 - 0.8136553
#Sig - 0.8144007
#Sig2 - 0.8152952


#Extracting Coefficients
#coefBSG56_Sig <-as.data.frame(summary(BSG56_ModSig2)$coefficients)
#write.csv(coefBSG56_BM, 'C:/Users/rmari/...ModBSG56_BMX.csv') #writing to exel

#_________________________________
#Bed Size Group 7 or 8

#All Variables 
BSG78_Mod <- glm(HIE_YN ~  Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                   NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                   TechScore + EMRAM_Stage, data=train_BSG78, family = binomial)
summary(BSG78_Mod)


#Block Model
BSG78_ModBM <- glm(HIE_YN ~  Prof + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                   TechScore + EMRAM_Stage, data=train_BSG78, family = binomial)
summary(BSG78_ModBM)

#Sig
BSG78_ModSig <- glm(HIE_YN ~  Prof + TotOutpatientVisA + TechScore, data=train_BSG78, family = binomial)
summary(BSG78_ModSig)


#Predictions on Test Data
fitAll78 <- predict(BSG78_ModSig, newdata=test_BSG78, type='response')

#ROC Curve
pr <- prediction(fitAll78, test_BSG78$HIE_YN)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col="magenta")

#AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#AUC
#All - 0.6587413
#BM - 0.6657343
#Sig - 0.6475524

#Extracting Coefficients
#coefBSG78_BM <-as.data.frame(summary(BSG78_ModBM)$coefficients)
#write.csv(coefBSG78_BM, 'C:/Users/rmari/...ModBSG78_BMX.csv') #writing to exel





#####################################
#Performance Models

#_____________________________________
#Net Revenue Size - Performance
#_____________________________________
#All
#Net Revenue size
hist(genMedD$NetRevSize)
plot(genMedD$HIE_YN, genMedD$NetRevSize)
hist(P_allMVO_LNRS$lnNetRevSZ)

#Remove Missing Values & Log
allMVtemp <- genMedD[complete.cases(genMedD[ , 68]),]  #Remove Missing Values
allMVtemp[1712, 68] <- 0
allMVtemp$lnNetRevSZ <- log(1 + allMVtemp$NetRevSize)

#Remove Outliers
P_allMVO_LNRS <- allMVtemp[which(allMVtemp$lnNetRevSZ >0),]

#Add Bed Size Group
P_allMVO_LNRS2 <- P_allMVO_LNRS
P_allMVO_LNRS2$BGBin <- 0
P_allMVO_LNRS2$BGBin[P_allMVO_LNRS2$BedSizeGrpA==1 | P_allMVO_LNRS2$BedSizeGrpA==2] <- 1
P_allMVO_LNRS2$BGBin[P_allMVO_LNRS2$BedSizeGrpA==3 | P_allMVO_LNRS2$BedSizeGrpA==4] <- 2
P_allMVO_LNRS2$BGBin[P_allMVO_LNRS2$BedSizeGrpA==5 | P_allMVO_LNRS2$BedSizeGrpA==6] <- 3
P_allMVO_LNRS2$BGBin[P_allMVO_LNRS2$BedSizeGrpA==7 | P_allMVO_LNRS2$BedSizeGrpA==8] <- 4
summary(as.factor(P_allMVO_LNRS2$BGBin))
P_allMVO_LNRS2$BGBin <- (as.factor(P_allMVO_LNRS2$BGBin))
summary(P_allMVO_LNRS2$BGBin)
P_allMVO_LNRS2 <- dummy_cols(P_allMVO_LNRS2, select_columns = "BGBin", remove_first_dummy = FALSE)
summary(as.factor(P_allMVO_LNRS2$BGBin_4))
summary(as.factor(P_allMVO_LNRS2$BGBin_3))
summary(as.factor(P_allMVO_LNRS2$BGBin_2))
summary(as.factor(P_allMVO_LNRS2$BGBin_1))


#Kernel Density Plot
d <- density(allMVtemp$NetRevSize) # returns the density data logged
plot(d)                            # plots the results
polygon(d, col="powderblue", border="cadetblue")

d <- density(allMVtemp$lnNetRevSZ) # returns the density data natural log
plot(d)                            # plots the results
polygon(d, col="powderblue", border="cadetblue")


d <- density(P_allMVO_LNRS$lnNetRevSZ) # returns the density data natural log
plot(d)                                # plots the results
polygon(d, col="powderblue", border="cadetblue")
polygon(d, col="plum2", border="orchid")

#Shapiro-Wilk Test of Normality
shapiro.test(genMedD$NetRevSize)

#Shapiro-Wilk Test of Normality
shapiro.test(allMVtemp$lnNetRevSZ)

#Shapiro-Wilk Test of Normality
shapiro.test(P_allMVO_LNRS$lnNetRevSZ)

#Preliminary Model Checking for Outliers
PerfNRS_Mod <- glm(lnNetRevSZ ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                                NewAccJC + NewMemCOTH + BedSizeGrpA + PercDr + PercNurse +
                                TechScore + EMRAM_Stage, data=P_allMVO_LNRS)

summary(PerfNRS_Mod)
plot(PerfNRS_Mod)


#All**************************

mean(P_allMVO_LNRS$lnNetRevSZ) #13.78594

PerfNRS_Mod1 <- glm(lnNetRevSZ ~ HIE_YN, data=P_allMVO_LNRS)
summary(PerfNRS_Mod1)
cv_error <- cv.glm(P_allMVO_LNRS, PerfNRS_Mod1, K=10) #0.4479068 0.4478847
cv_error$delta


PerfNRS_Mod2 <- glm(lnNetRevSZ ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                     NewAccJC + NewMemCOTH + BedSizeGrpA + TotOutpatientVisA + PercDr + PercNurse +
                     TechScore + EMRAM_Stage, data=P_allMVO_LNRS)
summary(PerfNRS_Mod2)
cv_error <- cv.glm(P_allMVO_LNRS, PerfNRS_Mod2, K=10) #0.3649328 0.3647218
cv_error$delta

names(P_allMVO_LNRS2)[names(P_allMVO_LNRS2) == 'BGBin_2'] <- 'BGBin_B'
names(P_allMVO_LNRS2)[names(P_allMVO_LNRS2) == 'BGBin_3'] <- 'BGBin_C'
names(P_allMVO_LNRS2)[names(P_allMVO_LNRS2) == 'BGBin_4'] <- 'BGBin_D'

#BSG Bin
PerfNRS_Mod2 <- glm(lnNetRevSZ ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                      NewAccJC + NewMemCOTH + BGBin_B + BGBin_C + BGBin_D + TotOutpatientVisA + PercDr + PercNurse +
                      TechScore + EMRAM_Stage, data=P_allMVO_LNRS2)
summary(PerfNRS_Mod2)
cv_error <- cv.glm(P_allMVO_LNRS2, PerfNRS_Mod2, K=10) #0.3645456 0.3643354
cv_error$delta


#BSG12**************************

P_BG12_LNRS <- P_allMVO_LNRS[which(P_allMVO_LNRS$BedSizeGrpA==1 | P_allMVO_LNRS$BedSizeGrpA==2),]
mean(P_BG12_LNRS$lnNetRevSZ) #13.63274

PerfNRS_Mod1 <- glm(lnNetRevSZ ~ HIE_YN, data=P_BG12_LNRS)
summary(PerfNRS_Mod1)
cv_error <- cv.glm(P_BG12_LNRS, PerfNRS_Mod1, K=10) #0.6517760 0.6516309
cv_error$delta


PerfNRS_Mod2 <- glm(lnNetRevSZ ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                      NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                      TechScore + EMRAM_Stage, data=P_BG12_LNRS)
summary(PerfNRS_Mod2)
cv_error <- cv.glm(P_BG12_LNRS, PerfNRS_Mod2, K=10) #0.5021810 0.5012748
cv_error$delta
warnings()


#BSG34**************************

P_BG34_LNRS <- P_allMVO_LNRS[which(P_allMVO_LNRS$BedSizeGrpA==3 | P_allMVO_LNRS$BedSizeGrpA==4),]
mean(P_BG34_LNRS$lnNetRevSZ) #13.76818

PerfNRS_Mod1 <- glm(lnNetRevSZ ~ HIE_YN, data=P_BG34_LNRS)
summary(PerfNRS_Mod1)
cv_error <- cv.glm(P_BG34_LNRS, PerfNRS_Mod1, K=10) #0.3704906 0.3704528
cv_error$delta


PerfNRS_Mod2 <- glm(lnNetRevSZ ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                      NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                      TechScore + EMRAM_Stage, data=P_BG34_LNRS)
summary(PerfNRS_Mod2)
cv_error <- cv.glm(P_BG34_LNRS, PerfNRS_Mod2, K=10) # 0.3216117 0.3211491
cv_error$delta


#BSG56**************************

P_BG56_LNRS <- P_allMVO_LNRS[which(P_allMVO_LNRS$BedSizeGrpA==5 | P_allMVO_LNRS$BedSizeGrpA==6),]
mean(P_BG56_LNRS$lnNetRevSZ) #13.88923

PerfNRS_Mod1 <- glm(lnNetRevSZ ~ HIE_YN, data=P_BG56_LNRS)
summary(PerfNRS_Mod1)
cv_error <- cv.glm(P_BG56_LNRS, PerfNRS_Mod1, K=10) #0.1948570 0.1947874
cv_error$delta

PerfNRS_Mod2 <- glm(lnNetRevSZ ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                      NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                      TechScore + EMRAM_Stage, data=P_BG56_LNRS)
summary(PerfNRS_Mod2)
cv_error <- cv.glm(P_BG56_LNRS, PerfNRS_Mod2, K=10) #0.1694055 0.1688717
cv_error$delta


#BSG78**************************

P_BG78_LNRS <- P_allMVO_LNRS[which(P_allMVO_LNRS$BedSizeGrpA==7 | P_allMVO_LNRS$BedSizeGrpA==8),]
mean(P_BG78_LNRS$lnNetRevSZ) #14.21349

PerfNRS_Mod1 <- glm(lnNetRevSZ ~ HIE_YN, data=P_BG78_LNRS)
summary(PerfNRS_Mod1)
cv_error <- cv.glm(P_BG78_LNRS, PerfNRS_Mod1, K=10) #0.2198095 0.2196479
cv_error$delta

PerfNRS_Mod2 <- glm(lnNetRevSZ ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                      NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                      TechScore + EMRAM_Stage, data=P_BG78_LNRS)
summary(PerfNRS_Mod2)
cv_error <- cv.glm(P_BG78_LNRS, PerfNRS_Mod2, K=10) #0.1535153 0.1525689
cv_error$delta


#Extracting Coefficients
coefAll_1 <-as.data.frame(summary(PerfNRS_Mod2)$coefficients)
#write.csv(coefAll_1, 'C:/Users/rmari/...BG78_LNRS_2.csv') #writing to exel

#_____________________________________
#Star Rating - Performance
#_____________________________________
#All

#Star Rating
hist(genMedD$StarRate_Overall)
plot(as.factor(genMedD$StarRate_Overall), col="magenta")
plot(as.factor(genMedD$StarRate_Overall), col="plum2", ylim=c(0,1500))
summary(as.factor(genMedD$StarRate_Overall))
plot(jitter(genMedD$HIE_YN, 0.25), jitter(genMedD$StarRate_Overall, 0.25))

#Remove Missing Values
allStartemp <- genMedD[complete.cases(genMedD[ , 50]),]  #Remove Missing Values
allStartemp2 <- genMedD2[complete.cases(genMedD2[ , 50]),]  #Remove Missing Values

#Shapiro-Wilk Test of Normality
shapiro.test(allStartemp$StarRate_Overall)


#All**************************

mean(allStartemp$StarRate_Overall) #3.516813

PerfStar_Mod1 <- glm(StarRate_Overall ~ HIE_YN, data=allStartemp)
summary(PerfStar_Mod1)
cv_error <- cv.glm(allStartemp, PerfStar_Mod1, K=10) #0.6933217 0.6932861
cv_error$delta


PerfStar_Mod2 <- glm(StarRate_Overall ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                      NewAccJC + NewMemCOTH + BedSizeGrpA + TotOutpatientVisA + PercDr + PercNurse +
                      TechScore + EMRAM_Stage, data=allStartemp)
summary(PerfStar_Mod2)
cv_error <- cv.glm(allStartemp, PerfStar_Mod2, K=10) #0.6197436 0.6192369
cv_error$delta

#Bed Size Group Bin

names(allStartemp2)[names(allStartemp2) == 'BGBin_2'] <- 'BGBin_B'
names(allStartemp2)[names(allStartemp2) == 'BGBin_3'] <- 'BGBin_C'
names(allStartemp2)[names(allStartemp2) == 'BGBin_4'] <- 'BGBin_D'



PerfStar_Mod2 <- glm(StarRate_Overall ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                       NewAccJC + NewMemCOTH + BGBin_B + BGBin_C + BGBin_D + TotOutpatientVisA + PercDr + PercNurse +
                       TechScore + EMRAM_Stage, data=allStartemp2)
summary(PerfStar_Mod2)
cv_error <- cv.glm(allStartemp2, PerfStar_Mod2, K=10) #0.6082835 0.6077835
cv_error$delta


#BSG12**************************

S_BG12_Perf <- allStartemp[which(allStartemp$BedSizeGrpA==1 | allStartemp$BedSizeGrpA==2),]
mean(S_BG12_Perf$StarRate_Overall) #3.959391

PerfStar_Mod1 <- glm(StarRate_Overall ~ HIE_YN, data=S_BG12_Perf)
summary(PerfStar_Mod1)
cv_error <- cv.glm(S_BG12_Perf, PerfStar_Mod1, K=10) #0.5527885 0.5526381
cv_error$delta


PerfStar_Mod2 <- glm(StarRate_Overall ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                       NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                       TechScore + EMRAM_Stage, data=S_BG12_Perf)
summary(PerfStar_Mod2)
cv_error <- cv.glm(S_BG12_Perf, PerfStar_Mod2, K=10) #0.5523537 0.5508335
cv_error$delta

#BSG34**************************

S_BG34_Perf <- allStartemp[which(allStartemp$BedSizeGrpA==3 | allStartemp$BedSizeGrpA==4),]
mean(S_BG34_Perf$StarRate_Overall) #3.402972

PerfStar_Mod1 <- glm(StarRate_Overall ~ HIE_YN, data=S_BG34_Perf)
summary(PerfStar_Mod1)
cv_error <- cv.glm(S_BG34_Perf, PerfStar_Mod1, K=10) #0.6986793 0.6985132
cv_error$delta


PerfStar_Mod2 <- glm(StarRate_Overall ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                       NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                       TechScore + EMRAM_Stage, data=S_BG34_Perf)
summary(PerfStar_Mod2)
cv_error <- cv.glm(S_BG34_Perf, PerfStar_Mod2, K=10) #0.6599145 0.6586549
cv_error$delta



#BSG56**************************

S_BG56_Perf <- allStartemp[which(allStartemp$BedSizeGrpA==5 | allStartemp$BedSizeGrpA==6),]
mean(S_BG56_Perf$StarRate_Overall) #3.336013

PerfStar_Mod1 <- glm(StarRate_Overall ~ HIE_YN, data=S_BG56_Perf)
summary(PerfStar_Mod1)
cv_error <- cv.glm(S_BG56_Perf, PerfStar_Mod1, K=10) #0.6300875 0.6298311
cv_error$delta


PerfStar_Mod2 <- glm(StarRate_Overall ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                       NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                       TechScore + EMRAM_Stage, data=S_BG56_Perf)
summary(PerfStar_Mod2)
cv_error <- cv.glm(S_BG56_Perf, PerfStar_Mod2, K=10) #0.6024242 0.6003150
cv_error$delta


#BSG78**************************

S_BG78_Perf <- allStartemp[which(allStartemp$BedSizeGrpA==7 | allStartemp$BedSizeGrpA==8),]
mean(S_BG78_Perf$StarRate_Overall) #3.467018

PerfStar_Mod1 <- glm(StarRate_Overall ~ HIE_YN, data=S_BG78_Perf)
summary(PerfStar_Mod1)
cv_error <- cv.glm(S_BG78_Perf, PerfStar_Mod1, K=10) #0.5950589 0.5947211
cv_error$delta


PerfStar_Mod2 <- glm(StarRate_Overall ~ HIE_YN + Gov + Prof + Church + MultiSys + Top100City_YN + CBSATypeA_Rural +
                       NewAccJC + NewMemCOTH + TotOutpatientVisA + PercDr + PercNurse +
                       TechScore + EMRAM_Stage, data=S_BG78_Perf)
summary(PerfStar_Mod2)
cv_error <- cv.glm(S_BG78_Perf, PerfStar_Mod2, K=10) #0.5309216 0.5283018
cv_error$delta


#Extracting Coefficients
coefAll_1 <-as.data.frame(summary(PerfStar_Mod2)$coefficients)
#write.csv(coefAll_1, 'C:/Users/rmari/...BG78_Star_2.csv') #writing to exel






