##Assessing viral load by various factors in MAST data 01/02/2021
library(dplyr)
library(KernSmooth)

memory.limit(size=56000000000)
load("data/2021-09-08/COVIDPILLAR2_VCam_MastP2.RData")
data1<-mast
rm("mast")
data1$AppointmentDate<-as.Date(as.character(data1$AppointmentDate))
##order observations by appointment date.
data1<-data1[with(data1,order(PseudoID,AppointmentDate)),]
data1$SpecimenProcessedDate<-as.Date(as.character(data1$SpecimenProcessedDate))

##Only consider dates after 6th November
data1<-data1[data1$AppointmentDate>="2020-11-06" & data1$AppointmentDate<="2021-09-08",]

##Define LFT tests
data1$TypeofTest<-"LFT"
data1$TypeofTest[data1$TestResult=="SCT:1240581000000104"]<-"PCR"
data1$TypeofTest[data1$TestResult=="SCT:1240591000000102"]<-"PCR"
data1$TypeofTest[data1$TestResult=="SCT:1321691000000102"]<-"PCR"
data1$TypeofTest<-as.factor(data1$TypeofTest)
##Also coded in "TestKit" but this was just to check they agree with the results codes.

##Count number of PCR tests
f1<-function(x){sum(x=="PCR")}
check1<-aggregate(TypeofTest~PseudoID,f1,data=data1)
names(check1)<-c("PseudoID","TotalPCR")
data1<-left_join(data1,check1)

##Count number of LFT tests
f2<-function(x){sum(x=="LFT")}
check2<-aggregate(TypeofTest~PseudoID,f2,data=data1)
names(check2)<-c("PseudoID","TotalLFT")
data1<-left_join(data1,check2)

##Need to code Symptomatic or not... coded in "TestReason" or "CovidSymptomatic"
data1$SYMPTOMS<-"No"
data1$SYMPTOMS[data1$TestReason=="symptomatic-citizen"]<-"Yes"
data1$SYMPTOMS[data1$TestReason=="symptomatic-essential-worker"]<-"Yes"
data1$SYMPTOMS[data1$TestReason=="I have coronavirus symptoms"]<-"Yes"
data1$SYMPTOMS[data1$CovidSymptomatic=="true"]<-"Yes"
data1$SYMPTOMS<-as.factor(data1$SYMPTOMS)

##Calculate mean of available Ct values.
data1$Ch1Cq<-as.numeric(as.character(data1$Ch1Cq))
data1$Ch2Cq<-as.numeric(as.character(data1$Ch2Cq))
data1$Ch3Cq<-as.numeric(as.character(data1$Ch3Cq))

f1<-function(x){mean(x,na.rm=TRUE)}
data1$CTRes<-apply(data1[,c("Ch2Cq","Ch3Cq","Ch1Cq")],1,f1)

##Determine Home Tests
data1$HomeTest<-"Test Centre"
data1$HomeTest[data1$TypeofTest=="PCR" & data1$TestCentreID=="HOM"]<-"Home"
data1$HomeTest[data1$TypeofTest=="LFT" & data1$ProcessingLabCode %in% c("LFDSelfTest","SelfServe","LFDSelfImageReader")]<-"Home"
data1$HomeTest<-as.factor(data1$HomeTest)

##restrict to LSOA's in Cheshire and Merseyside
LSOAdata<-read.csv("data/LiverpoolLSOAcodes.csv",header=TRUE,na.strings = c("NULL","NaN",""))
Areas<-c("Liverpool","Wirral","Sefton","Cheshire East","Cheshire West and Chester","Warrington","St. Helens","Knowsley","Halton")
LSOA1<-subset(LSOAdata,LSOAdata$LAD19NM %in% Areas)$LSOA11CD ##All 1562 LSOA areas in Cheshire and Merseyside

data1<-subset(data1,data1$LLSOA2011 %in% LSOA1)

##Remove any tests at IM, IMU or ACCORAQ, Randox, NP or SW - Immensa labs don't test for S gene and ACCORAQ tests for E gene.
data1A<-subset(data1,data1$ProcessingLabCode %in% c("IM","IMU","ACCORAQ","Randox","NP","SW","HLS"))
data1<-subset(data1,!(data1$ProcessingLabCode %in% c("IM","IMU","ACCORAQ","Randox","NP","SW","HLS")))
data1C<-subset(data1,data1$Ch3Target=="MS2")
data1D<-subset(data1,data1$Ch1Target=="N gene") ##317 tests
data1<-subset(data1,data1$Ch3Target!="MS2")
data1<-subset(data1,data1$Ch1Target!="N gene")

##Define variant type. Assumption of Delta is based on a reference given in the manuscript.
data1$NewVOC<-"Unknown"
data1$NewVOC[!is.na(data1$Ch1Cq)&!is.na(data1$Ch2Cq)&is.na(data1$Ch3Cq)]<-"Alpha"
data1$NewVOC[!is.na(data1$Ch1Cq)&!is.na(data1$Ch2Cq)&!is.na(data1$Ch3Cq)]<-"Wild Type"
data1$NewVOC[!is.na(data1$Ch1Cq)&!is.na(data1$Ch2Cq)&!is.na(data1$Ch3Cq)& data1$AppointmentDate>="2021-04-01"]<-"Delta"
data1$NewVOC<-as.factor(data1$NewVOC)

data1$Ethnicity1<-as.factor(data1$Ethnicity)
check1<-cbind(levels(data1$Ethnicity1),c("Black","Asian","Black","Another ethnic group","Another ethnic group","Mixed or multiple ethnic groups","White","Another ethnic group",
"Mixed or multiple ethnic groups","Asian","Asian","Mixed or multiple ethnic groups","Mixed or multiple ethnic groups","Black","White","Black","Asian","Asian",
"White","White","Mixed or multiple ethnic groups","Asian","Prefer not to say","White"))
levels(data1$Ethnicity1)<-c("Black","Asian","Black","Another ethnic group","Another ethnic group","Mixed or multiple ethnic groups","White","Another ethnic group",
"Mixed or multiple ethnic groups","Asian","Asian","Mixed or multiple ethnic groups","Mixed or multiple ethnic groups","Black","White","Black","Asian","Asian",
"White","White","Mixed or multiple ethnic groups","Asian","Prefer not to say","White")

##Define time period (I called these months originally, although the boundaries changed as this work progressed but it seemed easier jsut to have a short label).
data1$Period<-"November"
data1$Period[data1$AppointmentDate>="2020-12-03" & data1$AppointmentDate<"2021-01-05"]<-"December"
data1$Period[data1$AppointmentDate>="2021-01-05" & data1$AppointmentDate<"2021-03-08"]<-"January"
data1$Period[data1$AppointmentDate>="2021-03-08"& data1$AppointmentDate<"2021-05-17"]<-"March"
data1$Period[data1$AppointmentDate>="2021-05-17"]<-"June"
data1$Period<-as.factor(data1$Period)

data1<-data1[,c("PseudoID","Age","Gender","Ethnicity","LLSOA2011","LowerTierLocalAuthority","TestReason","TestID","TestCentreID","AppointmentDate",
"AdministrationMethod","TestType","TestLocation","SpecimenID","TestResultDesc","Ch1Target","Ch1Result","Ch1Cq","Ch2Target","Ch2Result","Ch2Cq","Ch3Target",
"Ch3Result","Ch3Cq","FK_Patient_Link_ID","TypeofTest","TotalPCR","TotalLFT","SYMPTOMS","CTRes","HomeTest","NewVOC","Ethnicity1","Period")]

save(data1,data1A,data1C,check1,file="CTDatasetSep22.RData")

##Merge vaccine data into Ct values analysis.

##Load vaccine data
load("data/2021-10-20/Immunisations_20OCt2021.RData")

##Limit to people with at least one completed vaccined dose.
data2A<-subset(data2,data2$Status=="completed" & data2$VaccinationProcedure %in% c("1324681000000101","1324691000000104","1362591000000103")) 
##Note that there are small numbers of individuals who appear to have CovidDoseNumber=1 or 2, but they don't have the relevant SNOMED code.

##Sort vaccine date.
data2A$DateTimeAdministered<-as.Date(as.character(data2A$DateTimeAdministered))
##order observations by appointment date.
data2A<-data2A[with(data2A,order(FK_Patient_Link_ID,DateTimeAdministered)),]

##Restrict to dates before or on 8th September.
data2A<-subset(data2A,data2A$DateTimeAdministered<="2021-09-08")
data2B<-subset(data2A,data2A$FK_Patient_Link_ID!=-1)

##Create a wide vaccine dataset.
data2D<-data2B[,c("FK_Patient_Link_ID","Age.At.Imm","DateTimeAdministered","VaccinationProcedure","Manufacturer","SourceDeceasedFlag","SourceDeathDate")]            

FirstDose<-subset(data2D,data2D$VaccinationProcedure=="1324681000000101")
SecondDose<-subset(data2D,data2D$VaccinationProcedure=="1324691000000104")
Booster<-subset(data2D,data2D$VaccinationProcedure=="1362591000000103")

FirstDose<-FirstDose[!duplicated(FirstDose$FK_Patient_Link_ID, fromLast=F),]
SecondDose<-SecondDose[!duplicated(SecondDose$FK_Patient_Link_ID, fromLast=F),]
Booster<-Booster[!duplicated(Booster$FK_Patient_Link_ID, fromLast=F),]

names(SecondDose)<-c("FK_Patient_Link_ID","Age.At.Dose2","DateDose2","VaccinationProcedureDose2","ManufacturerDose2","SourceDeceasedFlagDose2","SourceDeathDateDose2")
names(Booster)<-c("FK_Patient_Link_ID","Age.At.Booster","DateBooster","VaccinationProcedureBooster","ManufacturerBooster","SourceDeceasedFlagBooster",
"SourceDeathDateBooster")

VaccineData<-merge(FirstDose,SecondDose,by="FK_Patient_Link_ID",all=TRUE)
##7320 indiviuals who appear to have a second dose but not a first.

##There are 662 individuals who appear to have a Booster jab but we've no record of a first or second dose. Not sure what to do with them.
VaccineData<-merge(VaccineData,Booster,by="FK_Patient_Link_ID",all=TRUE)

##If they have a dose 2 but no first dose, we'll assume it was a first dose
s1<-which(is.na(VaccineData$DateTimeAdministered) & !is.na(VaccineData$DateDose2))
VaccineData[s1,2:5]<-VaccineData[s1,8:11]
VaccineData[s1,8:11]<-NA

##If date of dose 2 is before date of dose 1, switch them around - 212 cases.
s2<-which(VaccineData$DateTimeAdministered>VaccineData$DateDose2)
tmp1<-VaccineData[s2,2:5]
VaccineData[s2,2:5]<-VaccineData[s2,8:11]
VaccineData[s2,8:11]<-tmp1

VaccineData<-VaccineData[,c("FK_Patient_Link_ID","DateTimeAdministered","DateDose2","DateBooster")]

VaccineData<-subset(VaccineData,VaccineData$FK_Patient_Link_ID %in% unique(data1$FK_Patient_Link_ID))

data1<-merge(data1,VaccineData,by="FK_Patient_Link_ID",all=TRUE)

##Create vaccine variable
data1$VacDoses<-0
data1$VacDoses[!is.na(data1$DateTimeAdministered) & data1$DateTimeAdministered<data1$AppointmentDate]<-1
data1$VacDoses[!is.na(data1$DateTimeAdministered) & data1$DateDose2<data1$AppointmentDate]<-2
data1$VacDoses<-as.factor(data1$VacDoses)
summary(data1$VacDoses)
##Only a few individuals had received boosters y 8th September so this was not considered as a category in this analysis.

##Only consider patients who can be reliably matched to vaccine data - remove if FK_Patient_Link_ID==-1
##This removes 106977/2230504 (4.7%), 9291/217606 (4.3%), and 8606/206947 (4.2%) of pos PCRs with Ct values.
data1<-subset(data1,data1$FK_Patient_Link_ID!=-1)

##Select only positive PCRs in MAST from 6th November onwards
data2<-subset(data1,data1$TypeofTest=="PCR")
data3<-subset(data2,data2$TestResultDesc=="Positive")
data3a<-subset(data2,data2$TestResultDesc=="Negative")
data3b<-subset(data2,data2$TestResultDesc=="Void")
##Currently 217606 positive tests and 206825 unique ids.
##2230504 PCR tests on 953917 individuals

##Only keep individuals with Ct values recorded - need to check later if there are differences between these groups?
data4<-subset(data3,!is.na(data3$CTRes))
##206947 tests on 197060 individuals.
#tmp<-count(data4,PseudoID)
#names(tmp)<-c("id","TotalObs")
#tmpA<-count(tmp,TotalObs)
##4684 with 2 Ct values, 402 with 3, 83 with 4, 5 with 5, 1 with 6 and 2 with 7, 1 with 8.


save(data4,file="VaccineMergedPositives.RData")

##Take the first observation per individual and compare viral loads....
data5<-data4[!duplicated(data4$PseudoID, fromLast=F),]

##Count number of LFTs in the 14 days prior to a positive.
data1LFT<-subset(data1,data1$TypeofTest=="LFT" & data1$PseudoID %in% data5$PseudoID)
t1<-vector(length=nrow(data5))
p1<-vector(length=nrow(data5))

for(i in 1:nrow(data5)){
t1[i]<-sum(data1LFT$PseudoID==data5$PseudoID[i] & data1LFT$AppointmentDate<=data5$AppointmentDate[i] & data1LFT$AppointmentDate>=(data5$AppointmentDate[i]-14))
p1[i]<-sum(data1LFT$PseudoID==data5$PseudoID[i] & data1LFT$AppointmentDate<=data5$AppointmentDate[i] & data1LFT$AppointmentDate>=(data5$AppointmentDate[i]-14) & data1LFT$TestResultDesc=="lft positive")
}

t2<-t1
t2[t2>1]<-2
t2<-as.factor(t2)
summary(t2)

p2<-p1
p2[p2>0]<-1
p2<-as.factor(p2)
summary(p2)

data5$LFT14Prev<-t2
data5$LFT14Pos<-p2
save(data5,file="LFTMergedPositives.RData")

data5$age.factor<-"5-11"
data5$age.factor[data5$Age>=12&data5$Age<=18]<-"12-18"
data5$age.factor[data5$Age>=19&data5$Age<=29]<-"19-29"
data5$age.factor[data5$Age>=30&data5$Age<=39]<-"30-39"
data5$age.factor[data5$Age>=40&data5$Age<=49]<-"40-49"
data5$age.factor[data5$Age>=50&data5$Age<=59]<-"50-59"
data5$age.factor[data5$Age>=60&data5$Age<=69]<-"60-69"
data5$age.factor[data5$Age>=70]<-"70+"
data5$age.factor<-as.factor(data5$age.factor)

##by age
##Omit any ages <5 - they shouldn't be tested so likely to be data entry errors.
data5a<-subset(data5,data5$Age>=5 & data5$Age<=112)

summary(data5a$age.factor)
## [pasted console output, commented out for parsing] 12-18 19-29 30-39 40-49  5-11 50-59 60-69   70+ 
## [pasted console output, commented out for parsing] 22383 42802 33788 26681 12799 25479 13141  7829
x1<-bkde(data5a$CTRes[data5a$age.factor=="5-11"])
x2<-bkde(data5a$CTRes[data5a$age.factor=="12-18"])
x3<-bkde(data5a$CTRes[data5a$age.factor=="19-29"])
x4<-bkde(data5a$CTRes[data5a$age.factor=="30-39"])
x5<-bkde(data5a$CTRes[data5a$age.factor=="40-49"])
x6<-bkde(data5a$CTRes[data5a$age.factor=="50-59"])
x7<-bkde(data5a$CTRes[data5a$age.factor=="60-69"])
x8<-bkde(data5a$CTRes[data5a$age.factor=="70+"])

plot(x1,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by age",lwd=2)
lines(x2,col="green",lwd=2)
lines(x3,col="red",lwd=2)
lines(x4,col="blue",lwd=2)
lines(x5,col="orange",lwd=2)
lines(x6,col="pink",lwd=2)
lines(x7,col="purple",lwd=2)
lines(x8,col="darkorange",lwd=2)
lines(x9,col="steelblue",lwd=2)
legend("topright",legend=c("5-11","12-18","19-29","30-39","40-49","50-59","60-69","70-79","80+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange","steelblue"),lty=1)

library(devEMF)
#emf(file = "CtSupplementaryS3Revised.emf", width=12,height=6.5,emfPlus = TRUE)
tiff(filename = "CtSupplementaryS3Revised.tiff",width = 12, height = 6, units = "in",compression = "lzw",res = 600)
par(mfrow=c(1,2))
boxplot(data5a$CTRes[data5a$age.factor=="5-11"],data5a$CTRes[data5a$age.factor=="12-18"],data5a$CTRes[data5a$age.factor=="19-29"],
data5a$CTRes[data5a$age.factor=="30-39"],data5a$CTRes[data5a$age.factor=="40-49"],data5a$CTRes[data5a$age.factor=="50-59"],
data5a$CTRes[data5a$age.factor=="60-69"],data5a$CTRes[data5a$age.factor=="70+"],
col=c("grey","green","red","blue","orange","pink","purple","darkorange"),
names=c("5-11","12-18","19-29","30-39","40-49","50-59","60-69","70+"),
xlab="Age (in years)",ylab="Mean Cycle Threshold",cex.axis=0.8)

##Loess line for age...
LoessAge<-loess.smooth(data5a$Age,data5a$CTRes,span=.7,family="gaussian")
plot(LoessAge,lwd=2,xlab="Age",ylab="Mean Cycle Threshold",col="steelblue4",type="l",ylim=c(18,22))
dev.off()

##by sex
x10<-bkde(data5$CTRes[data5$Gender=="Female"])
x11<-bkde(data5$CTRes[data5$Gender=="Male"])

summary(as.factor(data5$Gender))
## [pasted console output, commented out for parsing]           Female    Male unknown 
## [pasted console output, commented out for parsing]       3   98192   90460     166

plot(x10,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by sex",lwd=2)
lines(x11,col="green",lwd=2)
legend("topright",legend=c("Females","Males"),col=c("black","green"),lty=1,lwd=2)

##by new variant/old variant
summary(data5$NewVOC)
## [pasted console output, commented out for parsing]     Alpha     Delta   Unknown Wild Type 
## [pasted console output, commented out for parsing]     58491     94861      1945     33524

x12<-bkde(data5$CTRes[data5$NewVOC=="Unknown"])
x13<-bkde(data5$CTRes[data5$NewVOC=="Wild Type"])
x13a<-bkde(data5$CTRes[data5$NewVOC=="Alpha"])
x13b<-bkde(data5$CTRes[data5$NewVOC=="Delta"])

plot(x12,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.2),main="Mean Ct value by variant",lwd=2)
lines(x13,col="green",lwd=2)
lines(x13a,col="red",lwd=2)
lines(x13b,col="blue",lwd=2)
legend("topright",legend=c("Unknown","Wild Type","Alpha","Delta"),col=c("black","green","red","blue"),lty=1,lwd=2)

##by ethnicity
## [pasted console output, commented out for parsing]                           Black                           Asian            Another ethnic group Mixed or multiple ethnic groups                           White               Prefer not to say 
## [pasted console output, commented out for parsing]                            1708                            4035                            1331                            2769                          172864                            6114 
 
x14<-bkde(data5$CTRes[data5$Ethnicity1=="White"])
x15<-bkde(data5$CTRes[data5$Ethnicity1=="Black"])
x16<-bkde(data5$CTRes[data5$Ethnicity1=="Asian"])
x17<-bkde(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"])
x18<-bkde(data5$CTRes[data5$Ethnicity1=="Another ethnic group"])
x19<-bkde(data5$CTRes[data5$Ethnicity1=="Prefer not to say"])

plot(x14,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by ethnicity",lwd=2)
lines(x15,col="green",lwd=2)
lines(x16,col="red",lwd=2)
lines(x17,col="blue",lwd=2)
lines(x18,col="orange",lwd=2)
lines(x19,col="pink",lwd=2)
legend("topright",legend=c("White","Black","Asian","Mixed or multiple ethnic groups","Another ethnic group",
"Prefer not to say"),col=c("black","green","red","blue","orange","pink"),lty=1,lwd=2)
                                              
##By Symptoms
summary(data5$SYMPTOMS)
## [pasted console output, commented out for parsing]     No    Yes 
## [pasted console output, commented out for parsing]  41222 147599 

x20<-bkde(data5$CTRes[data5$SYMPTOMS=="Yes"])
x21<-bkde(data5$CTRes[data5$SYMPTOMS=="No"])

plot(x20,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by symptoms",lwd=2)
lines(x21,col="green",lwd=2)
legend("topright",legend=c("Symptomatic","Asymptomatic"),col=c("black","green"),lty=1,lwd=2)

##By Test Location
summary(data5$HomeTest)
## [pasted console output, commented out for parsing]        Home Test Centre 
## [pasted console output, commented out for parsing]       37888      150933  
x22<-bkde(data5$CTRes[data5$HomeTest=="Home"])
x23<-bkde(data5$CTRes[data5$HomeTest=="Test Centre"])
#x24<-bkde(data5$CTRes[data5$TestLocation=="drivein"])
#x25<-bkde(data5$CTRes[data5$TestLocation==""])

plot(x22,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by test type",lwd=2)
lines(x23,col="green",lwd=2)
#lines(x24,col="red",lwd=2)
#lines(x25,col="blue",lwd=2)
#legend("topright",legend=c("Home","Walk in","Drive in","Not known"),col=c("black","green","red","blue"),lty=1,lwd=2)

##By Processing lab code.
#AP = Alderley Park
#GLS = Glasgow
#IM = Immensa
#MK = Milton Keynes
#CB = Cambridge
#Randox = Randox

#         AP         GLS          MK          CB         HSL    ACCORAZK                 
#      84686       14720        1125          87           4           1  

#x26<-bkde(data5$CTRes[data5$ProcessingLabCode=="AP"])
#x27<-bkde(data5$CTRes[data5$ProcessingLabCode=="GLS"])
#x29<-bkde(data5$CTRes[data5$ProcessingLabCode=="MK"])

#plot(x26,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by Processing laboratory",lwd=2)
#lines(x27,col="green",lwd=2)
#lines(x29,col="blue",lwd=2)
#legend("topright",legend=c("Alderley Park","Glasgow","Milton Keynes"),col=c("black","green","red","blue"),lty=1,lwd=2)

##Administration method
summary(as.factor(data5$AdministrationMethod))
## [pasted console output, commented out for parsing]                          health_care_professional                     self 
## [pasted console output, commented out for parsing]                    31352                     8363                   149106

x30<-bkde(data5$CTRes[data5$AdministrationMethod=="health_care_professional"])
x31<-bkde(data5$CTRes[data5$AdministrationMethod=="self"])
x32<-bkde(data5$CTRes[data5$AdministrationMethod==""])

plot(x30,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by Administration method",lwd=2)
lines(x31,col="green",lwd=2)
lines(x32,col="red",lwd=2)
legend("topright",legend=c("Healthcare professional","Self administered","Not known"),col=c("black","green","red"),lty=1,lwd=2)

##appointment date
summary(data5$Period)
## [pasted console output, commented out for parsing] December  January     June    March November 
## [pasted console output, commented out for parsing]    23619    52894    96506     4206    11596 

x33<-bkde(data5$CTRes[data5$Period=="November"])
x34<-bkde(data5$CTRes[data5$Period=="December"])
x35<-bkde(data5$CTRes[data5$Period=="January"])
x35a<-bkde(data5$CTRes[data5$Period=="March"])
x35b<-bkde(data5$CTRes[data5$Period=="June"])

plot(x33,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.1),main="Mean Ct value by Appointment Date",lwd=2)
lines(x34,col="green",lwd=2)
lines(x35,col="red",lwd=2)
lines(x35a,col="blue",lwd=2)
lines(x35a,col="orange",lwd=2)
legend("topright",legend=c("6th November - 2nd December","3rd December - 4th January","5th January - 7th March","8th March - 16th May","17th May - 8th September"),
col=c("black","green","red","blue","orange"),lty=1,lwd=2)

##Number of LFTs in previous days
summary(data5$LFT14Prev)
## [pasted console output, commented out for parsing]      0      1      2 
## [pasted console output, commented out for parsing] 143379  32399  13043
x36<-bkde(data5$CTRes[data5$LFT14Prev=="0"])
x37<-bkde(data5$CTRes[data5$LFT14Prev=="1"])
x38<-bkde(data5$CTRes[data5$LFT14Prev=="2"])

plot(x36,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.15),main="Mean Ct value by LFT in past 14 days",lwd=2)
lines(x37,col="green",lwd=2)
lines(x38,col="red",lwd=2)
legend("topright",legend=c("No LFTs","1 LFT","2+ LFTs"),col=c("black","green","red","blue","orange"),lty=1,lwd=2)

##Number of Vaccine Doses
summary(data5$VacDoses)
## [pasted console output, commented out for parsing]      0      1      2 
## [pasted console output, commented out for parsing] 136259  20874  31688

x39<-bkde(data5$CTRes[data5$VacDoses=="0"])
x40<-bkde(data5$CTRes[data5$VacDoses=="1"])
x41<-bkde(data5$CTRes[data5$VacDoses=="2"])

plot(x39,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.15),main="Mean Ct value by LFT in past 14 days",lwd=2)
lines(x40,col="green",lwd=2)
lines(x41,col="red",lwd=2)
legend("topright",legend=c("No Doses","1 Dose","2 Doses"),col=c("black","green","red"),lty=1,lwd=2)

##regression model to explore the influence of these various factors on Ct values - do they change if we use mixed model to account for LSOA level???
data6<-subset(data5,data5$Gender!=""& data5$Age>=5 & data5$Age<112)
summary(data6$Period)
## [pasted console output, commented out for parsing] December  January     June    March November 
## [pasted console output, commented out for parsing]    23271    51608    94523     4112    11385

data6$Ethnicity1<-relevel(data6$Ethnicity1,ref="White")
data6$age.factor<-relevel(data6$age.factor,ref="30-39")
data6$HomeTest<-relevel(data6$HomeTest,ref="Test Centre")
data6$Period<-relevel(data6$Period,ref="November")
data6$AdministrationMethod<-relevel(as.factor(data6$AdministrationMethod),ref="health_care_professional")
data6$NewVOC<-relevel(data6$NewVOC,ref="Wild Type")
data6$Gender<-as.factor(data6$Gender)
levels(data6$Gender)<-c("Female","Male",NA)
data6$AgeSquared<-data6$Age^2

##166 with missing gender.

mod1<-lm(CTRes~Age + AgeSquared+Gender+SYMPTOMS+Ethnicity1+HomeTest+NewVOC+Period+AdministrationMethod+ProLab,data=data6)
MultivariateResults<-cbind(summary(mod1)$coefficients[,1], confint(mod1),summary(mod1)$coefficients[,4])
write.csv(MultivariateResults,file="MultivariateResultsJuly2021.csv")

##Univariate analysis
##Quantile regression
library(quantreg)
##Univariate analysis
mod2<-rq(CTRes~age.factor,data=data6,ci=TRUE)
mod3<-rq(CTRes~Gender,data=data6,ci=TRUE)
mod4<-rq(CTRes~SYMPTOMS,data=data6,ci=TRUE)
mod5<-rq(CTRes~Ethnicity1,data=data6,ci=TRUE)
mod6<-rq(CTRes~HomeTest,data=data6,ci=TRUE)
mod7<-rq(CTRes~NewVOC,data=data6,ci=TRUE)
mod8<-rq(CTRes~Period,data=data6,ci=TRUE)
mod9<-rq(CTRes~AdministrationMethod,data=data6,ci=TRUE)
mod10<-rq(CTRes~LFT14Prev,data=data6,ci=TRUE)
mod11<-rq(CTRes~VacDoses,data=data6,ci=TRUE)
UnivariateResults<-rbind(
cbind(round(mod2$coefficients[,1],digits=2),paste("(",round(mod2$coefficients[,2],digits=2),", ",round(mod2$coefficients[,3],digits=2),")",sep=""),round(summary(mod2)$coefficients[,4],digits=3)),
cbind(round(mod3$coefficients[,1],digits=2),paste("(",round(mod3$coefficients[,2],digits=2),", ",round(mod3$coefficients[,3],digits=2),")",sep=""),round(summary(mod3)$coefficients[,4],digits=3)),
cbind(round(mod4$coefficients[,1],digits=2),paste("(",round(mod4$coefficients[,2],digits=2),", ",round(mod4$coefficients[,3],digits=2),")",sep=""),round(summary(mod4)$coefficients[,4],digits=3)),
cbind(round(mod5$coefficients[,1],digits=2),paste("(",round(mod5$coefficients[,2],digits=2),", ",round(mod5$coefficients[,3],digits=2),")",sep=""),round(summary(mod5)$coefficients[,4],digits=3)),
cbind(round(mod6$coefficients[,1],digits=2),paste("(",round(mod6$coefficients[,2],digits=2),", ",round(mod6$coefficients[,3],digits=2),")",sep=""),round(summary(mod6)$coefficients[,4],digits=3)),
cbind(round(mod7$coefficients[,1],digits=2),paste("(",round(mod7$coefficients[,2],digits=2),", ",round(mod7$coefficients[,3],digits=2),")",sep=""),round(summary(mod7)$coefficients[,4],digits=3)),
cbind(round(mod8$coefficients[,1],digits=2),paste("(",round(mod8$coefficients[,2],digits=2),", ",round(mod8$coefficients[,3],digits=2),")",sep=""),round(summary(mod8)$coefficients[,4],digits=3)),
cbind(round(mod9$coefficients[,1],digits=2),paste("(",round(mod9$coefficients[,2],digits=2),", ",round(mod9$coefficients[,3],digits=2),")",sep=""),round(summary(mod9)$coefficients[,4],digits=3)),
cbind(round(mod10$coefficients[,1],digits=2),paste("(",round(mod10$coefficients[,2],digits=2),", ",round(mod10$coefficients[,3],digits=2),")",sep=""),round(summary(mod10)$coefficients[,4],digits=3)),
cbind(round(mod11$coefficients[,1],digits=2),paste("(",round(mod11$coefficients[,2],digits=2),", ",round(mod11$coefficients[,3],digits=2),")",sep=""),round(summary(mod11)$coefficients[,4],digits=3))
)
write.csv(UnivariateResults,file="UnivariateResultsQuantileAug2022.csv")

quantmod <- rq(CTRes ~ age.factor*NewVOC +Gender+SYMPTOMS+Ethnicity1+HomeTest+Period+AdministrationMethod+LFT14Prev+VacDoses,data=data6,ci=TRUE)
summary(quantmod)$coefficients
ResultsQ<-cbind(
round(quantmod$coefficients[,1],digits=2),
paste("(",round(quantmod$coefficients[,2],digits=2),", ",round(quantmod$coefficients[,3],digits=2),")",sep=""),
round(summary(quantmod)$coefficients[,4],digits=3))
write.csv(ResultsQ,file="QuantileRegressionModelAug2022.csv")

##Plot median gene values over time...
##Select Positive PCR
tmp1<-count(data2, AppointmentDate)
tmp2<-count(data3, AppointmentDate)
colnames(tmp2)<-c("AppointmentDate","Positives")
tmp3<-left_join(tmp1,tmp2)

fmedian<-function(x){median(x,na.rm=TRUE)}
check1<-aggregate(CTRes~AppointmentDate,fmedian,data=data6)
names(check1)<-c("AppointmentDate","CTRes")
tmp3<-left_join(tmp3,check1)

check2<-aggregate(Ch1Cq~AppointmentDate,fmedian,data=data6)
names(check2)<-c("AppointmentDate","ORF1ab")
tmp3<-left_join(tmp3,check2)

check3<-aggregate(Ch2Cq~AppointmentDate,fmedian,data=data6)
names(check3)<-c("AppointmentDate","Ngene")
tmp3<-left_join(tmp3,check3)

check4<-aggregate(Ch3Cq~AppointmentDate,fmedian,data=data6)
names(check4)<-c("AppointmentDate","Sgene")
tmp3<-left_join(tmp3,check4)

##Plot the positive cases
library(devEMF)
#emf(file = "CtFigure1.emf", width=12,height=6.5,emfPlus = FALSE)
tiff(filename = "CtFigureSupplementary1Revised.tiff",width = 12, height = 6, units = "in",compression = "lzw",res = 600)
par(mar=c(5, 4, 7, 6) + 0.1)
x<-barplot(tmp3$n[1:306],col="steelblue",xlab="",ylab="Positive PCR Counts",ylim=c(0,20000), xaxt="n")
labs <-format(tmp3$AppointmentDate, format="%m-%d")[1:306]
text(cex=0.8, x=x[seq(1,306,by=7)], y=-2000, labs[seq(1,306,by=7)], xpd=TRUE, srt=90)
#legend("top", inset = c(0,-0.2),legend=c("Proportion of PCR tests positive",expression("Median C"[q]*" value"),expression("Median N gene C"[T]),
#expression("Median S gene C"[T]),expression("Median ORF1ab C"[T])),col=c("blue","green","orange","red","purple"),pch=15,xpd=TRUE)
legend("top", inset = c(0,-0.2),legend=c("Proportion of PCR tests positive",expression("Median C"[q]*" value")),col=c("red","green"),pch=15,xpd=TRUE)

##Add the proportion of positive PCR on separate axis
par(new=TRUE)
plot(tmp3$AppointmentDate[1:306],(tmp3$Positives[1:306]/tmp3$n[1:306]),ylab="",xlab="",axes=FALSE,ylim=c(0,1),col="red",type="l",lwd=2)
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,line=-1.5,xpd=TRUE)
mtext("Proportion",side=4,line=0.5)

par(new=TRUE)
plot(tmp3$AppointmentDate[1:306],tmp3$CTRes[1:306],ylab="",xlab="",axes=FALSE,ylim=c(15,30),col="green",type="l",lwd=2)
axis(4,at=seq(15,30,by=5),las=1,cex=1.5,col="black",line=2.5,xpd=TRUE)
mtext(expression("Median C"[q]*" Value"),side=4,line=5)
dev.off()

##Is the age effect due to sampling type?
tmp1<-table(data6$age.factor,data6$AdministrationMethod,data6$NewVOC)
wildTypeMethod<-tmp1[,,1]
AlphaMethod<-tmp1[,,2]
DeltaMethod<-tmp1[,,3]
write.csv(wildTypeMethod,file="wildTypeMethodSep2022.csv")
write.csv(AlphaMethod,file="AlphaMethodSep2022.csv")
write.csv(DeltaMethod,file="DeltaMethodSep2022.csv")

x1A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
x2A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
x3A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
x4A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
x5A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
x6A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
x7A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
x8A<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

x1B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
x2B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
x3B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
x4B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
x5B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
x6B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
x7B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
x8B<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

x1C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8C<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8D<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8E<-bkde(data6$CTRes[data6$AdministrationMethod=="health_care_professional" & data6$NewVOC=="Delta" & data6$age.factor=="70+"])

x1F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8F<-bkde(data6$CTRes[data6$AdministrationMethod=="self" & data6$NewVOC=="Delta" & data6$age.factor=="70+"])

tiff(filename = "CtSupplementaryFigure5Revised.tiff",width = 8, height = 11, units = "in",compression = "lzw",res = 600)
par(mfrow=c(3,2))
plot(x1A,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Wild Type",ylim=c(0,0.1),main="Health Care Professional Administered",lwd=2)
lines(x2A,col="green",lwd=2)
lines(x3A,col="red",lwd=2)
lines(x4A,col="blue",lwd=2)
lines(x5A,col="orange",lwd=2)
lines(x6A,col="pink",lwd=2)
lines(x7A,col="purple",lwd=2)
lines(x8A,col="darkorange",lwd=2)

plot(x1B,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.1),main="Self Administered",lwd=2)
lines(x2B,col="green",lwd=2)
lines(x3B,col="red",lwd=2)
lines(x4B,col="blue",lwd=2)
lines(x5B,col="orange",lwd=2)
lines(x6B,col="pink",lwd=2)
lines(x7B,col="purple",lwd=2)
lines(x8B,col="darkorange",lwd=2)
legend("topright",legend=c("<12","12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x1C,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Alpha",ylim=c(0,0.1),main="",lwd=2)
lines(x2C,col="green",lwd=2)
lines(x3C,col="red",lwd=2)
lines(x4C,col="blue",lwd=2)
lines(x5C,col="orange",lwd=2)
lines(x6C,col="pink",lwd=2)
lines(x7C,col="purple",lwd=2)
lines(x8C,col="darkorange",lwd=2)

plot(x1D,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.1),main="",lwd=2)
lines(x2D,col="green",lwd=2)
lines(x3D,col="red",lwd=2)
lines(x4D,col="blue",lwd=2)
lines(x5D,col="orange",lwd=2)
lines(x6D,col="pink",lwd=2)
lines(x7D,col="purple",lwd=2)
lines(x8D,col="darkorange",lwd=2)

plot(x1E,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Delta",ylim=c(0,0.12),main="",lwd=2)
lines(x2E,col="green",lwd=2)
lines(x3E,col="red",lwd=2)
lines(x4E,col="blue",lwd=2)
lines(x5E,col="orange",lwd=2)
lines(x6E,col="pink",lwd=2)
lines(x7E,col="purple",lwd=2)
lines(x8E,col="darkorange",lwd=2)

plot(x1F,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.12),main="",lwd=2)
lines(x2F,col="green",lwd=2)
lines(x3F,col="red",lwd=2)
lines(x4F,col="blue",lwd=2)
lines(x5F,col="orange",lwd=2)
lines(x6F,col="pink",lwd=2)
lines(x7F,col="purple",lwd=2)
lines(x8F,col="darkorange",lwd=2)
dev.off()

##Age and symptom status
tmp1<-table(data6$age.factor,data6$SYMPTOMS,data6$NewVOC)
wildTypeSymptoms<-tmp1[,,1]
AlphaSymptoms<-tmp1[,,2]
DeltaSymptoms<-tmp1[,,3]
write.csv(wildTypeSymptoms,file="wildTypeSymptomsSep2022.csv")
write.csv(AlphaSymptoms,file="AlphaSymptomsSep2022.csv")
write.csv(DeltaSymptoms,file="DeltaSymptomsSep2022.csv")

x1A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
x2A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
x3A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
x4A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
x5A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
x6A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
x7A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
x8A<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

x1B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
x2B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
x3B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
x4B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
x5B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
x6B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
x7B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
x8B<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

x1C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8C<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8D<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8E<-bkde(data6$CTRes[data6$SYMPTOMS=="Yes" & data6$NewVOC=="Delta" & data6$age.factor=="70+"])

x1F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8F<-bkde(data6$CTRes[data6$SYMPTOMS=="No" & data6$NewVOC=="Delta" & data6$age.factor=="70+"])

#emf(file = "CtFigure4.emf", width=12,height=6.5,emfPlus = FALSE)
tiff(filename = "CtSupplementaryFigure4Revised.tiff",width = 8, height = 11, units = "in",compression = "lzw",res = 600)
par(mfrow=c(3,2))
plot(x1A,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Wild Type",ylim=c(0,0.1),main="Symptomatic",lwd=2)
lines(x2A,col="green",lwd=2)
lines(x3A,col="red",lwd=2)
lines(x4A,col="blue",lwd=2)
lines(x5A,col="orange",lwd=2)
lines(x6A,col="pink",lwd=2)
lines(x7A,col="purple",lwd=2)
lines(x8A,col="darkorange",lwd=2)

plot(x1B,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.1),main="Asymptomatic",lwd=2)
lines(x2B,col="green",lwd=2)
lines(x3B,col="red",lwd=2)
lines(x4B,col="blue",lwd=2)
lines(x5B,col="orange",lwd=2)
lines(x6B,col="pink",lwd=2)
lines(x7B,col="purple",lwd=2)
lines(x8B,col="darkorange",lwd=2)
legend("topright",legend=c("<12","12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x1C,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Alpha",ylim=c(0,0.1),main="",lwd=2)
lines(x2C,col="green",lwd=2)
lines(x3C,col="red",lwd=2)
lines(x4C,col="blue",lwd=2)
lines(x5C,col="orange",lwd=2)
lines(x6C,col="pink",lwd=2)
lines(x7C,col="purple",lwd=2)
lines(x8C,col="darkorange",lwd=2)

plot(x1D,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.1),main="",lwd=2)
lines(x2D,col="green",lwd=2)
lines(x3D,col="red",lwd=2)
lines(x4D,col="blue",lwd=2)
lines(x5D,col="orange",lwd=2)
lines(x6D,col="pink",lwd=2)
lines(x7D,col="purple",lwd=2)
lines(x8D,col="darkorange",lwd=2)

plot(x1E,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Delta",ylim=c(0,0.12),main="",lwd=2)
lines(x2E,col="green",lwd=2)
lines(x3E,col="red",lwd=2)
lines(x4E,col="blue",lwd=2)
lines(x5E,col="orange",lwd=2)
lines(x6E,col="pink",lwd=2)
lines(x7E,col="purple",lwd=2)
lines(x8E,col="darkorange",lwd=2)

plot(x1F,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.12),main="",lwd=2)
lines(x2F,col="green",lwd=2)
lines(x3F,col="red",lwd=2)
lines(x4F,col="blue",lwd=2)
lines(x5F,col="orange",lwd=2)
lines(x6F,col="pink",lwd=2)
lines(x7F,col="purple",lwd=2)
lines(x8F,col="darkorange",lwd=2)
dev.off()

##Is age effect explained by willingness to test.
table(data6$age.factor,data6$Period)
## [pasted console output, commented out for parsing]         November December January  June March
## [pasted console output, commented out for parsing]   30-39     2078     4405   10079 16420   803
## [pasted console output, commented out for parsing]   12-18     1160     1618    3193 15899   513
## [pasted console output, commented out for parsing]   19-29     1915     5318   10375 24410   784
## [pasted console output, commented out for parsing]   40-49     1851     3722    8292 12146   670
## [pasted console output, commented out for parsing]   5-11       560      780    2124  8928   407
## [pasted console output, commented out for parsing]   50-59     2101     4076    9625  9128   549
## [pasted console output, commented out for parsing]   60-69     1073     2186    5030  4622   230
## [pasted console output, commented out for parsing]   70+        647     1166    2890  2970   156

x1C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="5-11"])
x2C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="12-18"])
x3C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="19-29"])
x4C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="30-39"])
x5C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="40-49"])
x6C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="50-59"])
x7C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="60-69"])
x8C<-bkde(data6$CTRes[data6$Period=="November" & data6$age.factor=="70+"])

x1D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="5-11"])
x2D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="12-18"])
x3D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="19-29"])
x4D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="30-39"])
x5D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="40-49"])
x6D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="50-59"])
x7D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="60-69"])
x8D<-bkde(data6$CTRes[data6$Period=="December" & data6$age.factor=="70+"])

x1E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="5-11"])
x2E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="12-18"])
x3E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="19-29"])
x4E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="30-39"])
x5E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="40-49"])
x6E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="50-59"])
x7E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="60-69"])
x8E<-bkde(data6$CTRes[data6$Period=="January" & data6$age.factor=="70+"])

x1F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="5-11"])
x2F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="12-18"])
x3F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="19-29"])
x4F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="30-39"])
x5F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="40-49"])
x6F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="50-59"])
x7F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="60-69"])
x8F<-bkde(data6$CTRes[data6$Period=="March" & data6$age.factor=="70+"])

x1G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="5-11"])
x2G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="12-18"])
x3G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="19-29"])
x4G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="30-39"])
x5G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="40-49"])
x6G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="50-59"])
x7G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="60-69"])
x8G<-bkde(data6$CTRes[data6$Period=="June" & data6$age.factor=="70+"])

tiff(filename = "CtSupplementaryFigure6Revised.tiff",width = 11, height = 7.5, units = "in",compression = "lzw",res = 600)
m<-rbind(c(1,1,2,2,3,3),c(6,4,4,5,5,6))
layout(m)

#par(mfrow=c(2,3))
plot(x1C,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.1),main="6th November - 2nd December",lwd=2)
lines(x2C,col="green",lwd=2)
lines(x3C,col="red",lwd=2)
lines(x4C,col="blue",lwd=2)
lines(x5C,col="orange",lwd=2)
lines(x6C,col="pink",lwd=2)
lines(x7C,col="purple",lwd=2)
lines(x8C,col="darkorange",lwd=2)

plot(x1D,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.1),main="3rd December - 4th January",lwd=2)
lines(x2D,col="green",lwd=2)
lines(x3D,col="red",lwd=2)
lines(x4D,col="blue",lwd=2)
lines(x5D,col="orange",lwd=2)
lines(x6D,col="pink",lwd=2)
lines(x7D,col="purple",lwd=2)
lines(x8D,col="darkorange",lwd=2)
legend("topright",legend=c("<12","12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x1E,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.1),main="5th January - 7th March",lwd=2)
lines(x2E,col="green",lwd=2)
lines(x3E,col="red",lwd=2)
lines(x4E,col="blue",lwd=2)
lines(x5E,col="orange",lwd=2)
lines(x6E,col="pink",lwd=2)
lines(x7E,col="purple",lwd=2)
lines(x8E,col="darkorange",lwd=2)

plot(x1F,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.1),main="8th March - 16th May",lwd=2)
lines(x2F,col="green",lwd=2)
lines(x3F,col="red",lwd=2)
lines(x4F,col="blue",lwd=2)
lines(x5F,col="orange",lwd=2)
lines(x6F,col="pink",lwd=2)
lines(x7F,col="purple",lwd=2)
lines(x8F,col="darkorange",lwd=2)

plot(x1G,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.12),main="17th May - 8th September",lwd=2)
lines(x2G,col="green",lwd=2)
lines(x3G,col="red",lwd=2)
lines(x4G,col="blue",lwd=2)
lines(x5G,col="orange",lwd=2)
lines(x6G,col="pink",lwd=2)
lines(x7G,col="purple",lwd=2)
lines(x8G,col="darkorange",lwd=2)
dev.off()

##Does age effect change by suspected Variant
## [pasted console output, commented out for parsing]         Wild Type Alpha Delta Unknown
## [pasted console output, commented out for parsing]   30-39      6224 11106 16157     298
## [pasted console output, commented out for parsing]   12-18      2576  3971 15642     194
## [pasted console output, commented out for parsing]   19-29      6602 11824 24029     347
## [pasted console output, commented out for parsing]   40-49      5231  9207 11933     310
## [pasted console output, commented out for parsing]   5-11       1259  2657  8729     154
## [pasted console output, commented out for parsing]   50-59      5972 10252  8983     272
## [pasted console output, commented out for parsing]   60-69      3169  5258  4564     150
## [pasted console output, commented out for parsing]   70+        1924  2821  2930     154

x1C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
x2C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
x3C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
x4C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
x5C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
x6C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
x7C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
x8C<-bkde(data6$CTRes[data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

x1D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8D<-bkde(data6$CTRes[data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8E<-bkde(data6$CTRes[data6$NewVOC=="Delta" & data6$age.factor=="70+"])

x1F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="5-11"])
x2F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="12-18"])
x3F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="19-29"])
x4F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="30-39"])
x5F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="40-49"])
x6F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="50-59"])
x7F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="60-69"])
x8F<-bkde(data6$CTRes[data6$NewVOC=="Unknown" & data6$age.factor=="70+"])

emf(file = "CtFigure2Revised.emf", width=11,height=5,emfPlus = TRUE,coordDPI=600)
#tiff(filename = "CtFigure2Revised.tiff",width = 11, height = 5, units = "in",compression = "lzw",res = 600)
par(mfrow=c(1,3))
plot(x1C,type="l",xlab=expression("Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.1),main="Suspected Wild-Type",lwd=2)
lines(x2C,col="green",lwd=2)
lines(x3C,col="red",lwd=2)
lines(x4C,col="blue",lwd=2)
lines(x5C,col="orange",lwd=2)
lines(x6C,col="pink",lwd=2)
lines(x7C,col="purple",lwd=2)
lines(x8C,col="darkorange",lwd=2)

plot(x1D,type="l",xlab=expression("Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.1),main="Suspected Alpha",lwd=2)
lines(x2D,col="green",lwd=2)
lines(x3D,col="red",lwd=2)
lines(x4D,col="blue",lwd=2)
lines(x5D,col="orange",lwd=2)
lines(x6D,col="pink",lwd=2)
lines(x7D,col="purple",lwd=2)
lines(x8D,col="darkorange",lwd=2)
legend("topright",legend=c("<12","12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x1E,type="l",xlab=expression("Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.12),main="Suspected Delta",lwd=2)
lines(x2E,col="green",lwd=2)
lines(x3E,col="red",lwd=2)
lines(x4E,col="blue",lwd=2)
lines(x5E,col="orange",lwd=2)
lines(x6E,col="pink",lwd=2)
lines(x7E,col="purple",lwd=2)
lines(x8E,col="darkorange",lwd=2)
dev.off()

##Age and Vaccine and Variant - probably need to remove 5-11 year olds since they are not really getting 2 doses...
tmp1<-table(data6$age.factor,data6$VacDoses,data6$NewVOC)
#wildTypeSymptoms<-tmp1[,,1]
AlphaSymptoms<-tmp1[,,2]
DeltaSymptoms<-tmp1[,,3]
#write.csv(wildTypeSymptoms,file="wildTypeSymptoms.csv")
write.csv(AlphaSymptoms,file="AlphaDoses.csv")
write.csv(DeltaSymptoms,file="DeltaDoses.csv")

#x1A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
#x2A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
#x3A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
#x4A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
#x5A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
#x6A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
#x7A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
#x8A<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

#x1B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
#x2B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
#x3B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
#x4B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
#x5B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
#x6B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
#x7B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
#x8B<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

#x1C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="5-11"])
#x2C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="12-18"])
#x3C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="19-29"])
#x4C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="30-39"])
#x5C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="40-49"])
#x6C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="50-59"])
#x7C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="60-69"])
#x8C<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Wild Type" & data6$age.factor=="70+"])

x1D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8D<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8E<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="5-11"])
x2F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="12-18"])
x3F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="19-29"])
x4F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="30-39"])
x5F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="40-49"])
x6F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="50-59"])
x7F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="60-69"])
x8F<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Alpha" & data6$age.factor=="70+"])

x1G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8G<-bkde(data6$CTRes[data6$VacDoses=="0" & data6$NewVOC=="Delta" & data6$age.factor=="70+"])

x1H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8H<-bkde(data6$CTRes[data6$VacDoses=="1" & data6$NewVOC=="Delta" & data6$age.factor=="70+"])

x1I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="5-11"])
x2I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="12-18"])
x3I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="19-29"])
x4I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="30-39"])
x5I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="40-49"])
x6I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="50-59"])
x7I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="60-69"])
x8I<-bkde(data6$CTRes[data6$VacDoses=="2" & data6$NewVOC=="Delta" & data6$age.factor=="70+"])

#emf(file = "CtFigure4.emf", width=12,height=6.5,emfPlus = FALSE)
tiff(filename = "CtSupplementaryFigure7Revised.tiff",width = 8, height = 11, units = "in",compression = "lzw",res = 600)
par(mfrow=c(2,3))

plot(x2D,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Alpha",ylim=c(0,0.1),main="No Doses",lwd=2)
lines(x2D,col="green",lwd=2)
lines(x3D,col="red",lwd=2)
lines(x4D,col="blue",lwd=2)
lines(x5D,col="orange",lwd=2)
lines(x6D,col="pink",lwd=2)
lines(x7D,col="purple",lwd=2)
lines(x8D,col="darkorange",lwd=2)

plot(x2E,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.12),main="1 Dose",lwd=2)
lines(x2E,col="green",lwd=2)
lines(x3E,col="red",lwd=2)
lines(x4E,col="blue",lwd=2)
lines(x5E,col="orange",lwd=2)
lines(x6E,col="pink",lwd=2)
lines(x7E,col="purple",lwd=2)
lines(x8E,col="darkorange",lwd=2)

plot(x3F,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.12),main="2 Doses",lwd=2)
#lines(x2F,col="green",lwd=2)
lines(x3F,col="red",lwd=2)
lines(x4F,col="blue",lwd=2)
lines(x5F,col="orange",lwd=2)
lines(x6F,col="pink",lwd=2)
lines(x7F,col="purple",lwd=2)
lines(x8F,col="darkorange",lwd=2)
legend("topright",legend=c("12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x2G,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Suspected Delta",ylim=c(0,0.12),main="",lwd=2)
lines(x2G,col="green",lwd=2)
lines(x3G,col="red",lwd=2)
lines(x4G,col="blue",lwd=2)
lines(x5G,col="orange",lwd=2)
lines(x6G,col="pink",lwd=2)
lines(x7G,col="purple",lwd=2)
lines(x8G,col="darkorange",lwd=2)

plot(x2H,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.12),main="",lwd=2)
lines(x2H,col="green",lwd=2)
lines(x3H,col="red",lwd=2)
lines(x4H,col="blue",lwd=2)
lines(x5H,col="orange",lwd=2)
lines(x6H,col="pink",lwd=2)
lines(x7H,col="purple",lwd=2)
lines(x8H,col="darkorange",lwd=2)

plot(x2I,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="",ylim=c(0,0.12),main="",lwd=2)
lines(x2I,col="green",lwd=2)
lines(x3I,col="red",lwd=2)
lines(x4I,col="blue",lwd=2)
lines(x5I,col="orange",lwd=2)
lines(x6I,col="pink",lwd=2)
lines(x7I,col="purple",lwd=2)
lines(x8I,col="darkorange",lwd=2)
dev.off()

##Summary Table
Total<-c(nrow(data2),
paste(nrow(data3)," (",round(nrow(data3)/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3a)," (",round(nrow(data3a)/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3b)," (",round(nrow(data3b)/nrow(data2),digits=2),")",sep=""),
length(unique(data2$PseudoID)),length(unique(data3$PseudoID)),
paste(round(summary(data5$CTRes)[3],digits=1)," (",round(summary(data5$CTRes)[2],digits=1),",",round(summary(data5$CTRes)[5],digits=1),")",sep=""),
paste(sum(data5$CTRes<18.3)," (",round(sum(data5$CTRes<18.3)/nrow(data5),digits=2),")",sep=""),
paste(sum(data5$CTRes>=18.3 &data5$CTRes<24.4)," (",round(sum(data5$CTRes>=18.3 &data5$CTRes<24.4)/nrow(data5),digits=2),")",sep=""),
paste(sum(data5$CTRes>=24.4 &data5$CTRes<30.5)," (",round(sum(data5$CTRes>=24.4 &data5$CTRes<30.5)/nrow(data5),digits=2),")",sep=""),
paste(sum(data5$CTRes>=30.5)," (",round(sum(data5$CTRes>=30.5)/nrow(data5),digits=2),")",sep=""))

Age12<-c(paste(nrow(data2[data2$Age>5&data2$Age<12,])," (",round(nrow(data2[data2$Age>5&data2$Age<12,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>5&data3$Age<12,])," (",round(nrow(data3[data3$Age>5&data3$Age<12,])/nrow(data2[data2$Age>5&data2$Age<12,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>5&data3a$Age<12,])," (",round(nrow(data3a[data3a$Age>5&data3a$Age<12,])/nrow(data2[data2$Age>5&data2$Age<12,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>5&data3b$Age<12,])," (",round(nrow(data3b[data3b$Age>5&data3b$Age<12,])/nrow(data2[data2$Age>5&data2$Age<12,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>5&data2$Age<12])),
length(unique(data3$PseudoID[data3$Age>5&data3$Age<12])),
paste(round(summary(data5$CTRes[data5$Age>5&data5$Age<12])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>5&data5$Age<12])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>5&data5$Age<12])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>5&data5$Age<12]<18.3)," (",round(sum(data5$CTRes[data5$Age>5&data5$Age<12]<18.3)/nrow(data5[data5$Age>5&data5$Age<12,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>5&data5$Age<12]>=18.3 &data5$CTRes[data5$Age>5&data5$Age<12]<24.4)," (",round(sum(data5$CTRes[data5$Age>5&data5$Age<12]>=18.3 &data5$CTRes[data5$Age>5&data5$Age<12]<24.4)/nrow(data5[data5$Age>5&data5$Age<12,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>5&data5$Age<12]>=24.4 &data5$CTRes[data5$Age>5&data5$Age<12]<30.5)," (",round(sum(data5$CTRes[data5$Age>5&data5$Age<12]>=24.4 &data5$CTRes[data5$Age>5&data5$Age<12]<30.5)/nrow(data5[data5$Age>5&data5$Age<12,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>5&data5$Age<12]>=30.5)," (",round(sum(data5$CTRes[data5$Age>5&data5$Age<12]>=30.5)/nrow(data5[data5$Age>5&data5$Age<12,]),digits=2),")",sep=""))

Age12.18<-c(
paste(nrow(data2[data2$Age>=12&data2$Age<=18,])," (",round(nrow(data2[data2$Age>=12&data2$Age<=18,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>=12&data3$Age<=18,])," (",round(nrow(data3[data3$Age>=12&data3$Age<=18,])/nrow(data2[data2$Age>=12&data2$Age<=18,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>=12&data3a$Age<=18,])," (",round(nrow(data3a[data3a$Age>=12&data3a$Age<=18,])/nrow(data2[data2$Age>=12&data2$Age<=18,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>=12&data3b$Age<=18,])," (",round(nrow(data3b[data3b$Age>=12&data3b$Age<=18,])/nrow(data2[data2$Age>=12&data2$Age<=18,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>=12&data2$Age<=18])),
length(unique(data3$PseudoID[data3$Age>=12&data3$Age<=18])),
paste(round(summary(data5$CTRes[data5$Age>=12&data5$Age<=18])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>=12&data5$Age<=18])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>=12&data5$Age<=18])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]<18.3)," (",round(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]<18.3)/nrow(data5[data5$Age>=12&data5$Age<=18,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]>=18.3 &data5$CTRes[data5$Age>=12&data5$Age<=18]<24.4)," (",round(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]>=18.3 &data5$CTRes[data5$Age>=12&data5$Age<=18]<24.4)/nrow(data5[data5$Age>=12&data5$Age<=18,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]>=24.4 &data5$CTRes[data5$Age>=12&data5$Age<=18]<30.5)," (",round(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]>=24.4 &data5$CTRes[data5$Age>=12&data5$Age<=18]<30.5)/nrow(data5[data5$Age>=12&data5$Age<=18,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]>=30.5)," (",round(sum(data5$CTRes[data5$Age>=12&data5$Age<=18]>=30.5)/nrow(data5[data5$Age>=12&data5$Age<=18,]),digits=2),")",sep=""))

Age19.29<-c(
paste(nrow(data2[data2$Age>=19&data2$Age<=29,])," (",round(nrow(data2[data2$Age>=19&data2$Age<=29,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>=19&data3$Age<=29,])," (",round(nrow(data3[data3$Age>=19&data3$Age<=29,])/nrow(data2[data2$Age>=19&data2$Age<=29,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>=19&data3a$Age<=29,])," (",round(nrow(data3a[data3a$Age>=19&data3a$Age<=29,])/nrow(data2[data2$Age>=19&data2$Age<=29,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>=19&data3b$Age<=29,])," (",round(nrow(data3b[data3b$Age>=19&data3b$Age<=29,])/nrow(data2[data2$Age>=19&data2$Age<=29,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>=19&data2$Age<=29])),
length(unique(data3$PseudoID[data3$Age>=19&data3$Age<=29])),
paste(round(summary(data5$CTRes[data5$Age>=19&data5$Age<=29])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>=19&data5$Age<=29])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>=19&data5$Age<=29])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]<18.3)," (",round(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]<18.3)/nrow(data5[data5$Age>=19&data5$Age<=29,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]>=18.3 &data5$CTRes[data5$Age>=19&data5$Age<=29]<24.4)," (",round(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]>=18.3 &data5$CTRes[data5$Age>=19&data5$Age<=29]<24.4)/nrow(data5[data5$Age>=19&data5$Age<=29,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]>=24.4 &data5$CTRes[data5$Age>=19&data5$Age<=29]<30.5)," (",round(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]>=24.4 &data5$CTRes[data5$Age>=19&data5$Age<=29]<30.5)/nrow(data5[data5$Age>=19&data5$Age<=29,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]>=30.5)," (",round(sum(data5$CTRes[data5$Age>=19&data5$Age<=29]>=30.5)/nrow(data5[data5$Age>=19&data5$Age<=29,]),digits=2),")",sep=""))

Age30.39<-c(
paste(nrow(data2[data2$Age>=30&data2$Age<=39,])," (",round(nrow(data2[data2$Age>=30&data2$Age<=39,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>=30&data3$Age<=39,])," (",round(nrow(data3[data3$Age>=30&data3$Age<=39,])/nrow(data2[data2$Age>=30&data2$Age<=39,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>=30&data3a$Age<=39,])," (",round(nrow(data3a[data3a$Age>=30&data3a$Age<=39,])/nrow(data2[data2$Age>=30&data2$Age<=39,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>=30&data3b$Age<=39,])," (",round(nrow(data3b[data3b$Age>=30&data3b$Age<=39,])/nrow(data2[data2$Age>=30&data2$Age<=39,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>=30&data2$Age<=39])),
length(unique(data3$PseudoID[data3$Age>=30&data3$Age<=39])),
paste(round(summary(data5$CTRes[data5$Age>=30&data5$Age<=39])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>=30&data5$Age<=39])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>=30&data5$Age<=39])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]<18.3)," (",round(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]<18.3)/nrow(data5[data5$Age>=30&data5$Age<=39,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]>=18.3 &data5$CTRes[data5$Age>=30&data5$Age<=39]<24.4)," (",round(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]>=18.3 &data5$CTRes[data5$Age>=30&data5$Age<=39]<24.4)/nrow(data5[data5$Age>=30&data5$Age<=39,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]>=24.4 &data5$CTRes[data5$Age>=30&data5$Age<=39]<30.5)," (",round(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]>=24.4 &data5$CTRes[data5$Age>=30&data5$Age<=39]<30.5)/nrow(data5[data5$Age>=30&data5$Age<=39,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]>=30.5)," (",round(sum(data5$CTRes[data5$Age>=30&data5$Age<=39]>=30.5)/nrow(data5[data5$Age>=30&data5$Age<=39,]),digits=2),")",sep=""))

Age40.49<-c(
paste(nrow(data2[data2$Age>=40&data2$Age<=49,])," (",round(nrow(data2[data2$Age>=40&data2$Age<=49,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>=40&data3$Age<=49,])," (",round(nrow(data3[data3$Age>=40&data3$Age<=49,])/nrow(data2[data2$Age>=40&data2$Age<=49,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>=40&data3a$Age<=49,])," (",round(nrow(data3a[data3a$Age>=40&data3a$Age<=49,])/nrow(data2[data2$Age>=40&data2$Age<=49,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>=40&data3b$Age<=49,])," (",round(nrow(data3b[data3b$Age>=40&data3b$Age<=49,])/nrow(data2[data2$Age>=40&data2$Age<=49,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>=40&data2$Age<=49])),
length(unique(data3$PseudoID[data3$Age>=40&data3$Age<=49])),
paste(round(summary(data5$CTRes[data5$Age>=40&data5$Age<=49])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>=40&data5$Age<=49])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>=40&data5$Age<=49])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]<18.3)," (",round(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]<18.3)/nrow(data5[data5$Age>=40&data5$Age<=49,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]>=18.3 &data5$CTRes[data5$Age>=40&data5$Age<=49]<24.4)," (",round(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]>=18.3 &data5$CTRes[data5$Age>=40&data5$Age<=49]<24.4)/nrow(data5[data5$Age>=40&data5$Age<=49,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]>=24.4 &data5$CTRes[data5$Age>=40&data5$Age<=49]<30.5)," (",round(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]>=24.4 &data5$CTRes[data5$Age>=40&data5$Age<=49]<30.5)/nrow(data5[data5$Age>=40&data5$Age<=49,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]>=30.5)," (",round(sum(data5$CTRes[data5$Age>=40&data5$Age<=49]>=30.5)/nrow(data5[data5$Age>=40&data5$Age<=49,]),digits=2),")",sep=""))

Age50.59<-c(
paste(nrow(data2[data2$Age>=50&data2$Age<=59,])," (",round(nrow(data2[data2$Age>=50&data2$Age<=59,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>=50&data3$Age<=59,])," (",round(nrow(data3[data3$Age>=50&data3$Age<=59,])/nrow(data2[data2$Age>=50&data2$Age<=59,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>=50&data3a$Age<=59,])," (",round(nrow(data3a[data3a$Age>=50&data3a$Age<=59,])/nrow(data2[data2$Age>=50&data2$Age<=59,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>=50&data3b$Age<=59,])," (",round(nrow(data3b[data3b$Age>=50&data3b$Age<=59,])/nrow(data2[data2$Age>=50&data2$Age<=59,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>=50&data2$Age<=59])),
length(unique(data3$PseudoID[data3$Age>=50&data3$Age<=59])),
paste(round(summary(data5$CTRes[data5$Age>=50&data5$Age<=59])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>=50&data5$Age<=59])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>=50&data5$Age<=59])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]<18.3)," (",round(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]<18.3)/nrow(data5[data5$Age>=50&data5$Age<=59,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]>=18.3 &data5$CTRes[data5$Age>=50&data5$Age<=59]<24.4)," (",round(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]>=18.3 &data5$CTRes[data5$Age>=50&data5$Age<=59]<24.4)/nrow(data5[data5$Age>=50&data5$Age<=59,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]>=24.4 &data5$CTRes[data5$Age>=50&data5$Age<=59]<30.5)," (",round(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]>=24.4 &data5$CTRes[data5$Age>=50&data5$Age<=59]<30.5)/nrow(data5[data5$Age>=50&data5$Age<=59,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]>=30.5)," (",round(sum(data5$CTRes[data5$Age>=50&data5$Age<=59]>=30.5)/nrow(data5[data5$Age>=50&data5$Age<=59,]),digits=2),")",sep=""))

Age60.69<-c(
paste(nrow(data2[data2$Age>=60&data2$Age<=69,])," (",round(nrow(data2[data2$Age>=60&data2$Age<=69,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>=60&data3$Age<=69,])," (",round(nrow(data3[data3$Age>=60&data3$Age<=69,])/nrow(data2[data2$Age>=60&data2$Age<=69,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>=60&data3a$Age<=69,])," (",round(nrow(data3a[data3a$Age>=60&data3a$Age<=69,])/nrow(data2[data2$Age>=60&data2$Age<=69,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>=60&data3b$Age<=69,])," (",round(nrow(data3b[data3b$Age>=60&data3b$Age<=69,])/nrow(data2[data2$Age>=60&data2$Age<=69,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>=60&data2$Age<=69])),
length(unique(data3$PseudoID[data3$Age>=60&data3$Age<=69])),
paste(round(summary(data5$CTRes[data5$Age>=60&data5$Age<=69])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>=60&data5$Age<=69])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>=60&data5$Age<=69])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]<18.3)," (",round(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]<18.3)/nrow(data5[data5$Age>=60&data5$Age<=69,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]>=18.3 &data5$CTRes[data5$Age>=60&data5$Age<=69]<24.4)," (",round(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]>=18.3 &data5$CTRes[data5$Age>=60&data5$Age<=69]<24.4)/nrow(data5[data5$Age>=60&data5$Age<=69,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]>=24.4 &data5$CTRes[data5$Age>=60&data5$Age<=69]<30.5)," (",round(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]>=24.4 &data5$CTRes[data5$Age>=60&data5$Age<=69]<30.5)/nrow(data5[data5$Age>=60&data5$Age<=69,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]>=30.5)," (",round(sum(data5$CTRes[data5$Age>=60&data5$Age<=69]>=30.5)/nrow(data5[data5$Age>=60&data5$Age<=69,]),digits=2),")",sep=""))

Age80<-c(
paste(nrow(data2[data2$Age>=70,])," (",round(nrow(data2[data2$Age>=70,])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Age>=70,])," (",round(nrow(data3[data3$Age>=70,])/nrow(data2[data2$Age>=70,]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Age>=70,])," (",round(nrow(data3a[data3a$Age>=70,])/nrow(data2[data2$Age>=70,]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Age>=70,])," (",round(nrow(data3b[data3b$Age>=70,])/nrow(data2[data2$Age>=70,]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Age>=70])),
length(unique(data3$PseudoID[data3$Age>=70])),
paste(round(summary(data5$CTRes[data5$Age>=70])[3],digits=1)," (",round(summary(data5$CTRes[data5$Age>=70])[2],digits=1),",",
round(summary(data5$CTRes[data5$Age>=70])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=70]<18.3)," (",round(sum(data5$CTRes[data5$Age>=70]<18.3)/nrow(data5[data5$Age>=70,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=70]>=18.3 &data5$CTRes[data5$Age>=70]<24.4)," (",round(sum(data5$CTRes[data5$Age>=70]>=18.3 &data5$CTRes[data5$Age>=70]<24.4)/nrow(data5[data5$Age>=70,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=70]>=24.4 &data5$CTRes[data5$Age>=70]<30.5)," (",round(sum(data5$CTRes[data5$Age>=70]>=24.4 &data5$CTRes[data5$Age>=70]<30.5)/nrow(data5[data5$Age>=70,]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Age>=70]>=30.5)," (",round(sum(data5$CTRes[data5$Age>=70]>=30.5)/nrow(data5[data5$Age>=70,]),digits=2),")",sep=""))

Male<-c(
paste(nrow(data2[data2$Gender=="Male",])," (",round(nrow(data2[data2$Gender=="Male",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Gender=="Male",])," (",round(nrow(data3[data3$Gender=="Male",])/nrow(data2[data2$Gender=="Male",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Gender=="Male",])," (",round(nrow(data3a[data3a$Gender=="Male",])/nrow(data2[data2$Gender=="Male",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Gender=="Male",])," (",round(nrow(data3b[data3b$Gender=="Male",])/nrow(data2[data2$Gender=="Male",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Gender=="Male"])),
length(unique(data3$PseudoID[data3$Gender=="Male"])),
paste(round(summary(data5$CTRes[data5$Gender=="Male"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Gender=="Male"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Gender=="Male"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Male"]<18.3)," (",round(sum(data5$CTRes[data5$Gender=="Male"]<18.3)/nrow(data5[data5$Gender=="Male",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Male"]>=18.3 &data5$CTRes[data5$Gender=="Male"]<24.4)," (",round(sum(data5$CTRes[data5$Gender=="Male"]>=18.3 &data5$CTRes[data5$Gender=="Male"]<24.4)/nrow(data5[data5$Gender=="Male",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Male"]>=24.4 &data5$CTRes[data5$Gender=="Male"]<30.5)," (",round(sum(data5$CTRes[data5$Gender=="Male"]>=24.4 &data5$CTRes[data5$Gender=="Male"]<30.5)/nrow(data5[data5$Gender=="Male",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Male"]>=30.5)," (",round(sum(data5$CTRes[data5$Gender=="Male"]>=30.5)/nrow(data5[data5$Gender=="Male",]),digits=2),")",sep=""))

Female<-c(
paste(nrow(data2[data2$Gender=="Female",])," (",round(nrow(data2[data2$Gender=="Female",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Gender=="Female",])," (",round(nrow(data3[data3$Gender=="Female",])/nrow(data2[data2$Gender=="Female",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Gender=="Female",])," (",round(nrow(data3a[data3a$Gender=="Female",])/nrow(data2[data2$Gender=="Female",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Gender=="Female",])," (",round(nrow(data3b[data3b$Gender=="Female",])/nrow(data2[data2$Gender=="Female",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Gender=="Female"])),
length(unique(data3$PseudoID[data3$Gender=="Female"])),
paste(round(summary(data5$CTRes[data5$Gender=="Female"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Gender=="Female"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Gender=="Female"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Female"]<18.3)," (",round(sum(data5$CTRes[data5$Gender=="Female"]<18.3)/nrow(data5[data5$Gender=="Female",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Female"]>=18.3 &data5$CTRes[data5$Gender=="Female"]<24.4)," (",round(sum(data5$CTRes[data5$Gender=="Female"]>=18.3 &data5$CTRes[data5$Gender=="Female"]<24.4)/nrow(data5[data5$Gender=="Female",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Female"]>=24.4 &data5$CTRes[data5$Gender=="Female"]<30.5)," (",round(sum(data5$CTRes[data5$Gender=="Female"]>=24.4 &data5$CTRes[data5$Gender=="Female"]<30.5)/nrow(data5[data5$Gender=="Female",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Gender=="Female"]>=30.5)," (",round(sum(data5$CTRes[data5$Gender=="Female"]>=30.5)/nrow(data5[data5$Gender=="Female",]),digits=2),")",sep=""))

White<-c(
paste(nrow(data2[data2$Ethnicity1=="White",])," (",round(nrow(data2[data2$Ethnicity1=="White",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Ethnicity1=="White",])," (",round(nrow(data3[data3$Ethnicity1=="White",])/nrow(data2[data2$Ethnicity1=="White",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Ethnicity1=="White",])," (",round(nrow(data3a[data3a$Ethnicity1=="White",])/nrow(data2[data2$Ethnicity1=="White",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Ethnicity1=="White",])," (",round(nrow(data3b[data3b$Ethnicity1=="White",])/nrow(data2[data2$Ethnicity1=="White",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Ethnicity1=="White"])),
length(unique(data3$PseudoID[data3$Ethnicity1=="White"])),
paste(round(summary(data5$CTRes[data5$Ethnicity1=="White"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Ethnicity1=="White"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Ethnicity1=="White"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="White"]<18.3)," (",round(sum(data5$CTRes[data5$Ethnicity1=="White"]<18.3)/nrow(data5[data5$Ethnicity1=="White",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="White"]>=18.3 &data5$CTRes[data5$Ethnicity1=="White"]<24.4)," (",round(sum(data5$CTRes[data5$Ethnicity1=="White"]>=18.3 &data5$CTRes[data5$Ethnicity1=="White"]<24.4)/nrow(data5[data5$Ethnicity1=="White",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="White"]>=24.4 &data5$CTRes[data5$Ethnicity1=="White"]<30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="White"]>=24.4 &data5$CTRes[data5$Ethnicity1=="White"]<30.5)/nrow(data5[data5$Ethnicity1=="White",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="White"]>=30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="White"]>=30.5)/nrow(data5[data5$Ethnicity1=="White",]),digits=2),")",sep=""))

Black<-c(
paste(nrow(data2[data2$Ethnicity1=="Black",])," (",round(nrow(data2[data2$Ethnicity1=="Black",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Ethnicity1=="Black",])," (",round(nrow(data3[data3$Ethnicity1=="Black",])/nrow(data2[data2$Ethnicity1=="Black",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Ethnicity1=="Black",])," (",round(nrow(data3a[data3a$Ethnicity1=="Black",])/nrow(data2[data2$Ethnicity1=="Black",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Ethnicity1=="Black",])," (",round(nrow(data3b[data3b$Ethnicity1=="Black",])/nrow(data2[data2$Ethnicity1=="Black",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Ethnicity1=="Black"])),
length(unique(data3$PseudoID[data3$Ethnicity1=="Black"])),
paste(round(summary(data5$CTRes[data5$Ethnicity1=="Black"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Ethnicity1=="Black"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Ethnicity1=="Black"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Black"]<18.3)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Black"]<18.3)/nrow(data5[data5$Ethnicity1=="Black",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Black"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Black"]<24.4)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Black"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Black"]<24.4)/nrow(data5[data5$Ethnicity1=="Black",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Black"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Black"]<30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Black"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Black"]<30.5)/nrow(data5[data5$Ethnicity1=="Black",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Black"]>=30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Black"]>=30.5)/nrow(data5[data5$Ethnicity1=="Black",]),digits=2),")",sep=""))

Asian<-c(
paste(nrow(data2[data2$Ethnicity1=="Asian",])," (",round(nrow(data2[data2$Ethnicity1=="Asian",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Ethnicity1=="Asian",])," (",round(nrow(data3[data3$Ethnicity1=="Asian",])/nrow(data2[data2$Ethnicity1=="Asian",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Ethnicity1=="Asian",])," (",round(nrow(data3a[data3a$Ethnicity1=="Asian",])/nrow(data2[data2$Ethnicity1=="Asian",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Ethnicity1=="Asian",])," (",round(nrow(data3b[data3b$Ethnicity1=="Asian",])/nrow(data2[data2$Ethnicity1=="Asian",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Ethnicity1=="Asian"])),
length(unique(data3$PseudoID[data3$Ethnicity1=="Asian"])),
paste(round(summary(data5$CTRes[data5$Ethnicity1=="Asian"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Ethnicity1=="Asian"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Ethnicity1=="Asian"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Asian"]<18.3)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Asian"]<18.3)/nrow(data5[data5$Ethnicity1=="Asian",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Asian"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Asian"]<24.4)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Asian"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Asian"]<24.4)/nrow(data5[data5$Ethnicity1=="Asian",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Asian"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Asian"]<30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Asian"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Asian"]<30.5)/nrow(data5[data5$Ethnicity1=="Asian",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Asian"]>=30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Asian"]>=30.5)/nrow(data5[data5$Ethnicity1=="Asian",]),digits=2),")",sep=""))

Mixed<-c(
paste(nrow(data2[data2$Ethnicity1=="Mixed or multiple ethnic groups",])," (",round(nrow(data2[data2$Ethnicity1=="Mixed or multiple ethnic groups",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Ethnicity1=="Mixed or multiple ethnic groups",])," (",round(nrow(data3[data3$Ethnicity1=="Mixed or multiple ethnic groups",])/nrow(data2[data2$Ethnicity1=="Mixed or multiple ethnic groups",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Ethnicity1=="Mixed or multiple ethnic groups",])," (",round(nrow(data3a[data3a$Ethnicity1=="Mixed or multiple ethnic groups",])/nrow(data2[data2$Ethnicity1=="Mixed or multiple ethnic groups",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Ethnicity1=="Mixed or multiple ethnic groups",])," (",round(nrow(data3b[data3b$Ethnicity1=="Mixed or multiple ethnic groups",])/nrow(data2[data2$Ethnicity1=="Mixed or multiple ethnic groups",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Ethnicity1=="Mixed or multiple ethnic groups"])),
length(unique(data3$PseudoID[data3$Ethnicity1=="Mixed or multiple ethnic groups"])),
paste(round(summary(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]<18.3)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]<18.3)/nrow(data5[data5$Ethnicity1=="Mixed or multiple ethnic groups",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]<24.4)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]<24.4)/nrow(data5[data5$Ethnicity1=="Mixed or multiple ethnic groups",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]<30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]<30.5)/nrow(data5[data5$Ethnicity1=="Mixed or multiple ethnic groups",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]>=30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Mixed or multiple ethnic groups"]>=30.5)/nrow(data5[data5$Ethnicity1=="Mixed or multiple ethnic groups",]),digits=2),")",sep=""))

Other<-c(
paste(nrow(data2[data2$Ethnicity1=="Another ethnic group",])," (",round(nrow(data2[data2$Ethnicity1=="Another ethnic group",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Ethnicity1=="Another ethnic group",])," (",round(nrow(data3[data3$Ethnicity1=="Another ethnic group",])/nrow(data2[data2$Ethnicity1=="Another ethnic group",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Ethnicity1=="Another ethnic group",])," (",round(nrow(data3a[data3a$Ethnicity1=="Another ethnic group",])/nrow(data2[data2$Ethnicity1=="Another ethnic group",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Ethnicity1=="Another ethnic group",])," (",round(nrow(data3b[data3b$Ethnicity1=="Another ethnic group",])/nrow(data2[data2$Ethnicity1=="Another ethnic group",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Ethnicity1=="Another ethnic group"])),
length(unique(data3$PseudoID[data3$Ethnicity1=="Another ethnic group"])),
paste(round(summary(data5$CTRes[data5$Ethnicity1=="Another ethnic group"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Ethnicity1=="Another ethnic group"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Ethnicity1=="Another ethnic group"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]<18.3)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]<18.3)/nrow(data5[data5$Ethnicity1=="Another ethnic group",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Another ethnic group"]<24.4)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Another ethnic group"]<24.4)/nrow(data5[data5$Ethnicity1=="Another ethnic group",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Another ethnic group"]<30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Another ethnic group"]<30.5)/nrow(data5[data5$Ethnicity1=="Another ethnic group",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]>=30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Another ethnic group"]>=30.5)/nrow(data5[data5$Ethnicity1=="Another ethnic group",]),digits=2),")",sep=""))

PNS<-c(
paste(nrow(data2[data2$Ethnicity1=="Prefer not to say",])," (",round(nrow(data2[data2$Ethnicity1=="Prefer not to say",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Ethnicity1=="Prefer not to say",])," (",round(nrow(data3[data3$Ethnicity1=="Prefer not to say",])/nrow(data2[data2$Ethnicity1=="Prefer not to say",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Ethnicity1=="Prefer not to say",])," (",round(nrow(data3a[data3a$Ethnicity1=="Prefer not to say",])/nrow(data2[data2$Ethnicity1=="Prefer not to say",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Ethnicity1=="Prefer not to say",])," (",round(nrow(data3b[data3b$Ethnicity1=="Prefer not to say",])/nrow(data2[data2$Ethnicity1=="Prefer not to say",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Ethnicity1=="Prefer not to say"])),
length(unique(data3$PseudoID[data3$Ethnicity1=="Prefer not to say"])),
paste(round(summary(data5$CTRes[data5$Ethnicity1=="Prefer not to say"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Ethnicity1=="Prefer not to say"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Ethnicity1=="Prefer not to say"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]<18.3)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]<18.3)/nrow(data5[data5$Ethnicity1=="Prefer not to say",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Prefer not to say"]<24.4)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]>=18.3 &data5$CTRes[data5$Ethnicity1=="Prefer not to say"]<24.4)/nrow(data5[data5$Ethnicity1=="Prefer not to say",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Prefer not to say"]<30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]>=24.4 &data5$CTRes[data5$Ethnicity1=="Prefer not to say"]<30.5)/nrow(data5[data5$Ethnicity1=="Prefer not to say",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]>=30.5)," (",round(sum(data5$CTRes[data5$Ethnicity1=="Prefer not to say"]>=30.5)/nrow(data5[data5$Ethnicity1=="Prefer not to say",]),digits=2),")",sep=""))

SymptomsYes<-c(
paste(nrow(data2[data2$SYMPTOMS=="Yes",])," (",round(nrow(data2[data2$SYMPTOMS=="Yes",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$SYMPTOMS=="Yes",])," (",round(nrow(data3[data3$SYMPTOMS=="Yes",])/nrow(data2[data2$SYMPTOMS=="Yes",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$SYMPTOMS=="Yes",])," (",round(nrow(data3a[data3a$SYMPTOMS=="Yes",])/nrow(data2[data2$SYMPTOMS=="Yes",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$SYMPTOMS=="Yes",])," (",round(nrow(data3b[data3b$SYMPTOMS=="Yes",])/nrow(data2[data2$SYMPTOMS=="Yes",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$SYMPTOMS=="Yes"])),
length(unique(data3$PseudoID[data3$SYMPTOMS=="Yes"])),
paste(round(summary(data5$CTRes[data5$SYMPTOMS=="Yes"])[3],digits=1)," (",round(summary(data5$CTRes[data5$SYMPTOMS=="Yes"])[2],digits=1),",",
round(summary(data5$CTRes[data5$SYMPTOMS=="Yes"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$SYMPTOMS=="Yes"]<18.3)," (",round(sum(data5$CTRes[data5$SYMPTOMS=="Yes"]<18.3)/nrow(data5[data5$SYMPTOMS=="Yes",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$SYMPTOMS=="Yes"]>=18.3 &data5$CTRes[data5$SYMPTOMS=="Yes"]<24.4)," (",round(sum(data5$CTRes[data5$SYMPTOMS=="Yes"]>=18.3 &data5$CTRes[data5$SYMPTOMS=="Yes"]<24.4)/nrow(data5[data5$SYMPTOMS=="Yes",]),digits=2),")",sep=""))  ## [closing bracket added: the script as sent ended this c( ) block with a comma]

SymptomsNo<-c(
paste(nrow(data2[data2$SYMPTOMS=="No",])," (",round(nrow(data2[data2$SYMPTOMS=="No",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$SYMPTOMS=="No",])," (",round(nrow(data3[data3$SYMPTOMS=="No",])/nrow(data2[data2$SYMPTOMS=="No",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$SYMPTOMS=="No",])," (",round(nrow(data3a[data3a$SYMPTOMS=="No",])/nrow(data2[data2$SYMPTOMS=="No",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$SYMPTOMS=="No",])," (",round(nrow(data3b[data3b$SYMPTOMS=="No",])/nrow(data2[data2$SYMPTOMS=="No",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$SYMPTOMS=="No"])),
length(unique(data3$PseudoID[data3$SYMPTOMS=="No"])),
paste(round(summary(data5$CTRes[data5$SYMPTOMS=="No"])[3],digits=1)," (",round(summary(data5$CTRes[data5$SYMPTOMS=="No"])[2],digits=1),",",
round(summary(data5$CTRes[data5$SYMPTOMS=="No"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$SYMPTOMS=="No"]<18.3)," (",round(sum(data5$CTRes[data5$SYMPTOMS=="No"]<18.3)/nrow(data5[data5$SYMPTOMS=="No",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$SYMPTOMS=="No"]>=18.3 &data5$CTRes[data5$SYMPTOMS=="No"]<24.4)," (",round(sum(data5$CTRes[data5$SYMPTOMS=="No"]>=18.3 &data5$CTRes[data5$SYMPTOMS=="No"]<24.4)/nrow(data5[data5$SYMPTOMS=="No",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$SYMPTOMS=="No"]>=24.4 &data5$CTRes[data5$SYMPTOMS=="No"]<30.5)," (",round(sum(data5$CTRes[data5$SYMPTOMS=="No"]>=24.4 &data5$CTRes[data5$SYMPTOMS=="No"]<30.5)/nrow(data5[data5$SYMPTOMS=="No",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$SYMPTOMS=="No"]>=30.5)," (",round(sum(data5$CTRes[data5$SYMPTOMS=="No"]>=30.5)/nrow(data5[data5$SYMPTOMS=="No",]),digits=2),")",sep=""))

Home<-c(
paste(nrow(data2[data2$HomeTest=="Home",])," (",round(nrow(data2[data2$HomeTest=="Home",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$HomeTest=="Home",])," (",round(nrow(data3[data3$HomeTest=="Home",])/nrow(data2[data2$HomeTest=="Home",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$HomeTest=="Home",])," (",round(nrow(data3a[data3a$HomeTest=="Home",])/nrow(data2[data2$HomeTest=="Home",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$HomeTest=="Home",])," (",round(nrow(data3b[data3b$HomeTest=="Home",])/nrow(data2[data2$HomeTest=="Home",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$HomeTest=="Home"])),
length(unique(data3$PseudoID[data3$HomeTest=="Home"])),
paste(round(summary(data5$CTRes[data5$HomeTest=="Home"])[3],digits=1)," (",round(summary(data5$CTRes[data5$HomeTest=="Home"])[2],digits=1),",",
round(summary(data5$CTRes[data5$HomeTest=="Home"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Home"]<18.3)," (",round(sum(data5$CTRes[data5$HomeTest=="Home"]<18.3)/nrow(data5[data5$HomeTest=="Home",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Home"]>=18.3 &data5$CTRes[data5$HomeTest=="Home"]<24.4)," (",round(sum(data5$CTRes[data5$HomeTest=="Home"]>=18.3 &data5$CTRes[data5$HomeTest=="Home"]<24.4)/nrow(data5[data5$HomeTest=="Home",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Home"]>=24.4 &data5$CTRes[data5$HomeTest=="Home"]<30.5)," (",round(sum(data5$CTRes[data5$HomeTest=="Home"]>=24.4 &data5$CTRes[data5$HomeTest=="Home"]<30.5)/nrow(data5[data5$HomeTest=="Home",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Home"]>=30.5)," (",round(sum(data5$CTRes[data5$HomeTest=="Home"]>=30.5)/nrow(data5[data5$HomeTest=="Home",]),digits=2),")",sep=""))

TestCentre<-c(
paste(nrow(data2[data2$HomeTest=="Test Centre",])," (",round(nrow(data2[data2$HomeTest=="Test Centre",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$HomeTest=="Test Centre",])," (",round(nrow(data3[data3$HomeTest=="Test Centre",])/nrow(data2[data2$HomeTest=="Test Centre",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$HomeTest=="Test Centre",])," (",round(nrow(data3a[data3a$HomeTest=="Test Centre",])/nrow(data2[data2$HomeTest=="Test Centre",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$HomeTest=="Test Centre",])," (",round(nrow(data3b[data3b$HomeTest=="Test Centre",])/nrow(data2[data2$HomeTest=="Test Centre",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$HomeTest=="Test Centre"])),
length(unique(data3$PseudoID[data3$HomeTest=="Test Centre"])),
paste(round(summary(data5$CTRes[data5$HomeTest=="Test Centre"])[3],digits=1)," (",round(summary(data5$CTRes[data5$HomeTest=="Test Centre"])[2],digits=1),",",
round(summary(data5$CTRes[data5$HomeTest=="Test Centre"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Test Centre"]<18.3)," (",round(sum(data5$CTRes[data5$HomeTest=="Test Centre"]<18.3)/nrow(data5[data5$HomeTest=="Test Centre",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Test Centre"]>=18.3 &data5$CTRes[data5$HomeTest=="Test Centre"]<24.4)," (",round(sum(data5$CTRes[data5$HomeTest=="Test Centre"]>=18.3 &data5$CTRes[data5$HomeTest=="Test Centre"]<24.4)/nrow(data5[data5$HomeTest=="Test Centre",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Test Centre"]>=24.4 &data5$CTRes[data5$HomeTest=="Test Centre"]<30.5)," (",round(sum(data5$CTRes[data5$HomeTest=="Test Centre"]>=24.4 &data5$CTRes[data5$HomeTest=="Test Centre"]<30.5)/nrow(data5[data5$HomeTest=="Test Centre",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$HomeTest=="Test Centre"]>=30.5)," (",round(sum(data5$CTRes[data5$HomeTest=="Test Centre"]>=30.5)/nrow(data5[data5$HomeTest=="Test Centre",]),digits=2),")",sep=""))

HCP<-c(
paste(nrow(data2[data2$AdministrationMethod=="health_care_professional",])," (",round(nrow(data2[data2$AdministrationMethod=="health_care_professional",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$AdministrationMethod=="health_care_professional",])," (",round(nrow(data3[data3$AdministrationMethod=="health_care_professional",])/nrow(data2[data2$AdministrationMethod=="health_care_professional",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$AdministrationMethod=="health_care_professional",])," (",round(nrow(data3a[data3a$AdministrationMethod=="health_care_professional",])/nrow(data2[data2$AdministrationMethod=="health_care_professional",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$AdministrationMethod=="health_care_professional",])," (",round(nrow(data3b[data3b$AdministrationMethod=="health_care_professional",])/nrow(data2[data2$AdministrationMethod=="health_care_professional",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$AdministrationMethod=="health_care_professional"])),
length(unique(data3$PseudoID[data3$AdministrationMethod=="health_care_professional"])),
paste(round(summary(data5$CTRes[data5$AdministrationMethod=="health_care_professional"])[3],digits=1)," (",round(summary(data5$CTRes[data5$AdministrationMethod=="health_care_professional"])[2],digits=1),",",
round(summary(data5$CTRes[data5$AdministrationMethod=="health_care_professional"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]<18.3)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]<18.3)/nrow(data5[data5$AdministrationMethod=="health_care_professional",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]>=18.3 &data5$CTRes[data5$AdministrationMethod=="health_care_professional"]<24.4)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]>=18.3 &data5$CTRes[data5$AdministrationMethod=="health_care_professional"]<24.4)/nrow(data5[data5$AdministrationMethod=="health_care_professional",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]>=24.4 &data5$CTRes[data5$AdministrationMethod=="health_care_professional"]<30.5)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]>=24.4 &data5$CTRes[data5$AdministrationMethod=="health_care_professional"]<30.5)/nrow(data5[data5$AdministrationMethod=="health_care_professional",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]>=30.5)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="health_care_professional"]>=30.5)/nrow(data5[data5$AdministrationMethod=="health_care_professional",]),digits=2),")",sep=""))

Self<-c(
paste(nrow(data2[data2$AdministrationMethod=="self",])," (",round(nrow(data2[data2$AdministrationMethod=="self",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$AdministrationMethod=="self",])," (",round(nrow(data3[data3$AdministrationMethod=="self",])/nrow(data2[data2$AdministrationMethod=="self",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$AdministrationMethod=="self",])," (",round(nrow(data3a[data3a$AdministrationMethod=="self",])/nrow(data2[data2$AdministrationMethod=="self",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$AdministrationMethod=="self",])," (",round(nrow(data3b[data3b$AdministrationMethod=="self",])/nrow(data2[data2$AdministrationMethod=="self",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$AdministrationMethod=="self"])),
length(unique(data3$PseudoID[data3$AdministrationMethod=="self"])),
paste(round(summary(data5$CTRes[data5$AdministrationMethod=="self"])[3],digits=1)," (",round(summary(data5$CTRes[data5$AdministrationMethod=="self"])[2],digits=1),",",
round(summary(data5$CTRes[data5$AdministrationMethod=="self"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="self"]<18.3)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="self"]<18.3)/nrow(data5[data5$AdministrationMethod=="self",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="self"]>=18.3 &data5$CTRes[data5$AdministrationMethod=="self"]<24.4)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="self"]>=18.3 &data5$CTRes[data5$AdministrationMethod=="self"]<24.4)/nrow(data5[data5$AdministrationMethod=="self",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="self"]>=24.4 &data5$CTRes[data5$AdministrationMethod=="self"]<30.5)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="self"]>=24.4 &data5$CTRes[data5$AdministrationMethod=="self"]<30.5)/nrow(data5[data5$AdministrationMethod=="self",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod=="self"]>=30.5)," (",round(sum(data5$CTRes[data5$AdministrationMethod=="self"]>=30.5)/nrow(data5[data5$AdministrationMethod=="self",]),digits=2),")",sep=""))

Admin<-c(
paste(nrow(data2[data2$AdministrationMethod=="",])," (",round(nrow(data2[data2$AdministrationMethod=="",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$AdministrationMethod=="",])," (",round(nrow(data3[data3$AdministrationMethod=="",])/nrow(data2[data2$AdministrationMethod=="",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$AdministrationMethod=="",])," (",round(nrow(data3a[data3a$AdministrationMethod=="",])/nrow(data2[data2$AdministrationMethod=="",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$AdministrationMethod=="",])," (",round(nrow(data3b[data3b$AdministrationMethod=="",])/nrow(data2[data2$AdministrationMethod=="",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$AdministrationMethod==""])),
length(unique(data3$PseudoID[data3$AdministrationMethod==""])),
paste(round(summary(data5$CTRes[data5$AdministrationMethod==""])[3],digits=1)," (",round(summary(data5$CTRes[data5$AdministrationMethod==""])[2],digits=1),",",
round(summary(data5$CTRes[data5$AdministrationMethod==""])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod==""]<18.3)," (",round(sum(data5$CTRes[data5$AdministrationMethod==""]<18.3)/nrow(data5[data5$AdministrationMethod=="",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod==""]>=18.3 &data5$CTRes[data5$AdministrationMethod==""]<24.4)," (",round(sum(data5$CTRes[data5$AdministrationMethod==""]>=18.3 &data5$CTRes[data5$AdministrationMethod==""]<24.4)/nrow(data5[data5$AdministrationMethod=="",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod==""]>=24.4 &data5$CTRes[data5$AdministrationMethod==""]<30.5)," (",round(sum(data5$CTRes[data5$AdministrationMethod==""]>=24.4 &data5$CTRes[data5$AdministrationMethod==""]<30.5)/nrow(data5[data5$AdministrationMethod=="",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$AdministrationMethod==""]>=30.5)," (",round(sum(data5$CTRes[data5$AdministrationMethod==""]>=30.5)/nrow(data5[data5$AdministrationMethod=="",]),digits=2),")",sep=""))

November<-c(
paste(nrow(data2[data2$Period=="November",])," (",round(nrow(data2[data2$Period=="November",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Period=="November",])," (",round(nrow(data3[data3$Period=="November",])/nrow(data2[data2$Period=="November",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Period=="November",])," (",round(nrow(data3a[data3a$Period=="November",])/nrow(data2[data2$Period=="November",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Period=="November",])," (",round(nrow(data3b[data3b$Period=="November",])/nrow(data2[data2$Period=="November",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Period=="November"])),
length(unique(data3$PseudoID[data3$Period=="November"])),
paste(round(summary(data5$CTRes[data5$Period=="November"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Period=="November"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Period=="November"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="November"]<18.3)," (",round(sum(data5$CTRes[data5$Period=="November"]<18.3)/nrow(data5[data5$Period=="November",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="November"]>=18.3 &data5$CTRes[data5$Period=="November"]<24.4)," (",round(sum(data5$CTRes[data5$Period=="November"]>=18.3 &data5$CTRes[data5$Period=="November"]<24.4)/nrow(data5[data5$Period=="November",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="November"]>=24.4 &data5$CTRes[data5$Period=="November"]<30.5)," (",round(sum(data5$CTRes[data5$Period=="November"]>=24.4 &data5$CTRes[data5$Period=="November"]<30.5)/nrow(data5[data5$Period=="November",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="November"]>=30.5)," (",round(sum(data5$CTRes[data5$Period=="November"]>=30.5)/nrow(data5[data5$Period=="November",]),digits=2),")",sep=""))

December<-c(
paste(nrow(data2[data2$Period=="December",])," (",round(nrow(data2[data2$Period=="December",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Period=="December",])," (",round(nrow(data3[data3$Period=="December",])/nrow(data2[data2$Period=="December",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Period=="December",])," (",round(nrow(data3a[data3a$Period=="December",])/nrow(data2[data2$Period=="December",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Period=="December",])," (",round(nrow(data3b[data3b$Period=="December",])/nrow(data2[data2$Period=="December",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Period=="December"])),
length(unique(data3$PseudoID[data3$Period=="December"])),
paste(round(summary(data5$CTRes[data5$Period=="December"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Period=="December"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Period=="December"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="December"]<18.3)," (",round(sum(data5$CTRes[data5$Period=="December"]<18.3)/nrow(data5[data5$Period=="December",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="December"]>=18.3 &data5$CTRes[data5$Period=="December"]<24.4)," (",round(sum(data5$CTRes[data5$Period=="December"]>=18.3 &data5$CTRes[data5$Period=="December"]<24.4)/nrow(data5[data5$Period=="December",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="December"]>=24.4 &data5$CTRes[data5$Period=="December"]<30.5)," (",round(sum(data5$CTRes[data5$Period=="December"]>=24.4 &data5$CTRes[data5$Period=="December"]<30.5)/nrow(data5[data5$Period=="December",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="December"]>=30.5)," (",round(sum(data5$CTRes[data5$Period=="December"]>=30.5)/nrow(data5[data5$Period=="December",]),digits=2),")",sep=""))

January<-c(
paste(nrow(data2[data2$Period=="January",])," (",round(nrow(data2[data2$Period=="January",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Period=="January",])," (",round(nrow(data3[data3$Period=="January",])/nrow(data2[data2$Period=="January",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Period=="January",])," (",round(nrow(data3a[data3a$Period=="January",])/nrow(data2[data2$Period=="January",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Period=="January",])," (",round(nrow(data3b[data3b$Period=="January",])/nrow(data2[data2$Period=="January",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Period=="January"])),
length(unique(data3$PseudoID[data3$Period=="January"])),
paste(round(summary(data5$CTRes[data5$Period=="January"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Period=="January"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Period=="January"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="January"]<18.3)," (",round(sum(data5$CTRes[data5$Period=="January"]<18.3)/nrow(data5[data5$Period=="January",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="January"]>=18.3 &data5$CTRes[data5$Period=="January"]<24.4)," (",round(sum(data5$CTRes[data5$Period=="January"]>=18.3 &data5$CTRes[data5$Period=="January"]<24.4)/nrow(data5[data5$Period=="January",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="January"]>=24.4 &data5$CTRes[data5$Period=="January"]<30.5)," (",round(sum(data5$CTRes[data5$Period=="January"]>=24.4 &data5$CTRes[data5$Period=="January"]<30.5)/nrow(data5[data5$Period=="January",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="January"]>=30.5)," (",round(sum(data5$CTRes[data5$Period=="January"]>=30.5)/nrow(data5[data5$Period=="January",]),digits=2),")",sep=""))

March<-c(
paste(nrow(data2[data2$Period=="March",])," (",round(nrow(data2[data2$Period=="March",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Period=="March",])," (",round(nrow(data3[data3$Period=="March",])/nrow(data2[data2$Period=="March",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Period=="March",])," (",round(nrow(data3a[data3a$Period=="March",])/nrow(data2[data2$Period=="March",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Period=="March",])," (",round(nrow(data3b[data3b$Period=="March",])/nrow(data2[data2$Period=="March",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Period=="March"])),
length(unique(data3$PseudoID[data3$Period=="March"])),
paste(round(summary(data5$CTRes[data5$Period=="March"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Period=="March"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Period=="March"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="March"]<18.3)," (",round(sum(data5$CTRes[data5$Period=="March"]<18.3)/nrow(data5[data5$Period=="March",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="March"]>=18.3 &data5$CTRes[data5$Period=="March"]<24.4)," (",round(sum(data5$CTRes[data5$Period=="March"]>=18.3 &data5$CTRes[data5$Period=="March"]<24.4)/nrow(data5[data5$Period=="March",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="March"]>=24.4 &data5$CTRes[data5$Period=="March"]<30.5)," (",round(sum(data5$CTRes[data5$Period=="March"]>=24.4 &data5$CTRes[data5$Period=="March"]<30.5)/nrow(data5[data5$Period=="March",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="March"]>=30.5)," (",round(sum(data5$CTRes[data5$Period=="March"]>=30.5)/nrow(data5[data5$Period=="March",]),digits=2),")",sep=""))

June<-c(
paste(nrow(data2[data2$Period=="June",])," (",round(nrow(data2[data2$Period=="June",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$Period=="June",])," (",round(nrow(data3[data3$Period=="June",])/nrow(data2[data2$Period=="June",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$Period=="June",])," (",round(nrow(data3a[data3a$Period=="June",])/nrow(data2[data2$Period=="June",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$Period=="June",])," (",round(nrow(data3b[data3b$Period=="June",])/nrow(data2[data2$Period=="June",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$Period=="June"])),
length(unique(data3$PseudoID[data3$Period=="June"])),
paste(round(summary(data5$CTRes[data5$Period=="June"])[3],digits=1)," (",round(summary(data5$CTRes[data5$Period=="June"])[2],digits=1),",",
round(summary(data5$CTRes[data5$Period=="June"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="June"]<18.3)," (",round(sum(data5$CTRes[data5$Period=="March"]<18.3)/nrow(data5[data5$Period=="June",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="June"]>=18.3 &data5$CTRes[data5$Period=="June"]<24.4)," (",round(sum(data5$CTRes[data5$Period=="June"]>=18.3 &data5$CTRes[data5$Period=="June"]<24.4)/nrow(data5[data5$Period=="June",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="June"]>=24.4 &data5$CTRes[data5$Period=="June"]<30.5)," (",round(sum(data5$CTRes[data5$Period=="June"]>=24.4 &data5$CTRes[data5$Period=="June"]<30.5)/nrow(data5[data5$Period=="June",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$Period=="June"]>=30.5)," (",round(sum(data5$CTRes[data5$Period=="June"]>=30.5)/nrow(data5[data5$Period=="June",]),digits=2),")",sep=""))

VocUnknown<-c(
paste(nrow(data2[data2$NewVOC=="Unknown",])," (",round(nrow(data2[data2$NewVOC=="Unknown",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$NewVOC=="Unknown",])," (",round(nrow(data3[data3$NewVOC=="Unknown",])/nrow(data2[data2$NewVOC=="Unknown",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$NewVOC=="Unknown",])," (",round(nrow(data3a[data3a$NewVOC=="Unknown",])/nrow(data2[data2$NewVOC=="Unknown",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$NewVOC=="Unknown",])," (",round(nrow(data3b[data3b$NewVOC=="Unknown",])/nrow(data2[data2$NewVOC=="Unknown",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$NewVOC=="Unknown"])),
length(unique(data3$PseudoID[data3$NewVOC=="Unknown"])),
paste(round(summary(data5$CTRes[data5$NewVOC=="Unknown"])[3],digits=1)," (",round(summary(data5$CTRes[data5$NewVOC=="Unknown"])[2],digits=1),",",
round(summary(data5$CTRes[data5$NewVOC=="Unknown"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Unknown"]<18.3)," (",round(sum(data5$CTRes[data5$NewVOC=="Unknown"]<18.3)/nrow(data5[data5$NewVOC=="Unknown",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Unknown"]>=18.3 &data5$CTRes[data5$NewVOC=="Unknown"]<24.4)," (",round(sum(data5$CTRes[data5$NewVOC=="Unknown"]>=18.3 &data5$CTRes[data5$NewVOC=="Unknown"]<24.4)/nrow(data5[data5$NewVOC=="Unknown",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Unknown"]>=24.4 &data5$CTRes[data5$NewVOC=="Unknown"]<30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Unknown"]>=24.4 &data5$CTRes[data5$NewVOC=="Unknown"]<30.5)/nrow(data5[data5$NewVOC=="Unknown",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Unknown"]>=30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Unknown"]>=30.5)/nrow(data5[data5$NewVOC=="Unknown",]),digits=2),")",sep=""))

VocWildType<-c(
paste(nrow(data2[data2$NewVOC=="Wild Type",])," (",round(nrow(data2[data2$NewVOC=="Wild Type",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$NewVOC=="Wild Type",])," (",round(nrow(data3[data3$NewVOC=="Wild Type",])/nrow(data2[data2$NewVOC=="Wild Type",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$NewVOC=="Wild Type",])," (",round(nrow(data3a[data3a$NewVOC=="Wild Type",])/nrow(data2[data2$NewVOC=="Wild Type",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$NewVOC=="Wild Type",])," (",round(nrow(data3b[data3b$NewVOC=="Wild Type",])/nrow(data2[data2$NewVOC=="Wild Type",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$NewVOC=="Wild Type"])),
length(unique(data3$PseudoID[data3$NewVOC=="Wild Type"])),
paste(round(summary(data5$CTRes[data5$NewVOC=="Wild Type"])[3],digits=1)," (",round(summary(data5$CTRes[data5$NewVOC=="Wild Type"])[2],digits=1),",",
round(summary(data5$CTRes[data5$NewVOC=="Wild Type"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Wild Type"]<18.3)," (",round(sum(data5$CTRes[data5$NewVOC=="Wild Type"]<18.3)/nrow(data5[data5$NewVOC=="Wild Type",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Wild Type"]>=18.3 &data5$CTRes[data5$NewVOC=="Wild Type"]<24.4)," (",round(sum(data5$CTRes[data5$NewVOC=="Wild Type"]>=18.3 &data5$CTRes[data5$NewVOC=="Wild Type"]<24.4)/nrow(data5[data5$NewVOC=="Wild Type",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Wild Type"]>=24.4 &data5$CTRes[data5$NewVOC=="Wild Type"]<30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Wild Type"]>=24.4 &data5$CTRes[data5$NewVOC=="Wild Type"]<30.5)/nrow(data5[data5$NewVOC=="Wild Type",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Wild Type"]>=30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Wild Type"]>=30.5)/nrow(data5[data5$NewVOC=="Wild Type",]),digits=2),")",sep=""))

VocAlpha<-c(
paste(nrow(data2[data2$NewVOC=="Alpha",])," (",round(nrow(data2[data2$NewVOC=="Alpha",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$NewVOC=="Alpha",])," (",round(nrow(data3[data3$NewVOC=="Alpha",])/nrow(data2[data2$NewVOC=="Alpha",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$NewVOC=="Alpha",])," (",round(nrow(data3a[data3a$NewVOC=="Alpha",])/nrow(data2[data2$NewVOC=="Alpha",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$NewVOC=="Alpha",])," (",round(nrow(data3b[data3b$NewVOC=="Alpha",])/nrow(data2[data2$NewVOC=="Alpha",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$NewVOC=="Alpha"])),
length(unique(data3$PseudoID[data3$NewVOC=="Alpha"])),
paste(round(summary(data5$CTRes[data5$NewVOC=="Alpha"])[3],digits=1)," (",round(summary(data5$CTRes[data5$NewVOC=="Alpha"])[2],digits=1),",",
round(summary(data5$CTRes[data5$NewVOC=="Alpha"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Alpha"]<18.3)," (",round(sum(data5$CTRes[data5$NewVOC=="Alpha"]<18.3)/nrow(data5[data5$NewVOC=="Alpha",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Alpha"]>=18.3 &data5$CTRes[data5$NewVOC=="Alpha"]<24.4)," (",round(sum(data5$CTRes[data5$NewVOC=="Alpha"]>=18.3 &data5$CTRes[data5$NewVOC=="Alpha"]<24.4)/nrow(data5[data5$NewVOC=="Alpha",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Alpha"]>=24.4 &data5$CTRes[data5$NewVOC=="Alpha"]<30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Alpha"]>=24.4 &data5$CTRes[data5$NewVOC=="Alpha"]<30.5)/nrow(data5[data5$NewVOC=="Alpha",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Alpha"]>=30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Alpha"]>=30.5)/nrow(data5[data5$NewVOC=="Alpha",]),digits=2),")",sep=""))

VocDelta<-c(
paste(nrow(data2[data2$NewVOC=="Delta",])," (",round(nrow(data2[data2$NewVOC=="Delta",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$NewVOC=="Delta",])," (",round(nrow(data3[data3$NewVOC=="Delta",])/nrow(data2[data2$NewVOC=="Delta",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$NewVOC=="Delta",])," (",round(nrow(data3a[data3a$NewVOC=="Delta",])/nrow(data2[data2$NewVOC=="Delta",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$NewVOC=="Delta",])," (",round(nrow(data3b[data3b$NewVOC=="Delta",])/nrow(data2[data2$NewVOC=="Delta",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$NewVOC=="Delta"])),
length(unique(data3$PseudoID[data3$NewVOC=="Delta"])),
paste(round(summary(data5$CTRes[data5$NewVOC=="Delta"])[3],digits=1)," (",round(summary(data5$CTRes[data5$NewVOC=="Delta"])[2],digits=1),",",
round(summary(data5$CTRes[data5$NewVOC=="Delta"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Delta"]<18.3)," (",round(sum(data5$CTRes[data5$NewVOC=="Delta"]<18.3)/nrow(data5[data5$NewVOC=="Delta",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Delta"]>=18.3 &data5$CTRes[data5$NewVOC=="Delta"]<24.4)," (",round(sum(data5$CTRes[data5$NewVOC=="Delta"]>=18.3 &data5$CTRes[data5$NewVOC=="Delta"]<24.4)/nrow(data5[data5$NewVOC=="Delta",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Delta"]>=24.4 &data5$CTRes[data5$NewVOC=="Delta"]<30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Delta"]>=24.4 &data5$CTRes[data5$NewVOC=="Delta"]<30.5)/nrow(data5[data5$NewVOC=="Delta",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$NewVOC=="Delta"]>=30.5)," (",round(sum(data5$CTRes[data5$NewVOC=="Delta"]>=30.5)/nrow(data5[data5$NewVOC=="Delta",]),digits=2),")",sep=""))

Dose0<-c(
paste(nrow(data2[data2$VacDoses=="0",])," (",round(nrow(data2[data2$VacDoses=="0",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$VacDoses=="0",])," (",round(nrow(data3[data3$VacDoses=="0",])/nrow(data2[data2$VacDoses=="0",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$VacDoses=="0",])," (",round(nrow(data3a[data3a$VacDoses=="0",])/nrow(data2[data2$VacDoses=="0",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$VacDoses=="0",])," (",round(nrow(data3b[data3b$VacDoses=="0",])/nrow(data2[data2$VacDoses=="0",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$VacDoses=="0"])),
length(unique(data3$PseudoID[data3$VacDoses=="0"])),
paste(round(summary(data5$CTRes[data5$VacDoses=="0"])[3],digits=1)," (",round(summary(data5$CTRes[data5$VacDoses=="0"])[2],digits=1),",",
round(summary(data5$CTRes[data5$VacDoses=="0"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="0"]<18.3)," (",round(sum(data5$CTRes[data5$VacDoses=="0"]<18.3)/nrow(data5[data5$VacDoses=="0",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="0"]>=18.3 &data5$CTRes[data5$VacDoses=="0"]<24.4)," (",round(sum(data5$CTRes[data5$VacDoses=="0"]>=18.3 &data5$CTRes[data5$VacDoses=="0"]<24.4)/nrow(data5[data5$VacDoses=="0",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="0"]>=24.4 &data5$CTRes[data5$VacDoses=="0"]<30.5)," (",round(sum(data5$CTRes[data5$VacDoses=="0"]>=24.4 &data5$CTRes[data5$VacDoses=="0"]<30.5)/nrow(data5[data5$VacDoses=="0",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="0"]>=30.5)," (",round(sum(data5$CTRes[data5$VacDoses=="0"]>=30.5)/nrow(data5[data5$VacDoses=="0",]),digits=2),")",sep=""))

Dose1<-c(
paste(nrow(data2[data2$VacDoses=="1",])," (",round(nrow(data2[data2$VacDoses=="1",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$VacDoses=="1",])," (",round(nrow(data3[data3$VacDoses=="1",])/nrow(data2[data2$VacDoses=="1",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$VacDoses=="1",])," (",round(nrow(data3a[data3a$VacDoses=="1",])/nrow(data2[data2$VacDoses=="1",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$VacDoses=="1",])," (",round(nrow(data3b[data3b$VacDoses=="1",])/nrow(data2[data2$VacDoses=="1",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$VacDoses=="1"])),
length(unique(data3$PseudoID[data3$VacDoses=="1"])),
paste(round(summary(data5$CTRes[data5$VacDoses=="1"])[3],digits=1)," (",round(summary(data5$CTRes[data5$VacDoses=="1"])[2],digits=1),",",
round(summary(data5$CTRes[data5$VacDoses=="1"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="1"]<18.3)," (",round(sum(data5$CTRes[data5$VacDoses=="1"]<18.3)/nrow(data5[data5$VacDoses=="1",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="1"]>=18.3 &data5$CTRes[data5$VacDoses=="1"]<24.4)," (",round(sum(data5$CTRes[data5$VacDoses=="1"]>=18.3 &data5$CTRes[data5$VacDoses=="1"]<24.4)/nrow(data5[data5$VacDoses=="1",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="1"]>=24.4 &data5$CTRes[data5$VacDoses=="1"]<30.5)," (",round(sum(data5$CTRes[data5$VacDoses=="1"]>=24.4 &data5$CTRes[data5$VacDoses=="1"]<30.5)/nrow(data5[data5$VacDoses=="1",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="1"]>=30.5)," (",round(sum(data5$CTRes[data5$VacDoses=="1"]>=30.5)/nrow(data5[data5$VacDoses=="1",]),digits=2),")",sep=""))

Dose2<-c(
paste(nrow(data2[data2$VacDoses=="2",])," (",round(nrow(data2[data2$VacDoses=="2",])/nrow(data2),digits=2),")",sep=""),
paste(nrow(data3[data3$VacDoses=="2",])," (",round(nrow(data3[data3$VacDoses=="2",])/nrow(data2[data2$VacDoses=="2",]),digits=2),")",sep=""),
paste(nrow(data3a[data3a$VacDoses=="2",])," (",round(nrow(data3a[data3a$VacDoses=="2",])/nrow(data2[data2$VacDoses=="2",]),digits=2),")",sep=""),
paste(nrow(data3b[data3b$VacDoses=="2",])," (",round(nrow(data3b[data3b$VacDoses=="2",])/nrow(data2[data2$VacDoses=="2",]),digits=2),")",sep=""),
length(unique(data2$PseudoID[data2$VacDoses=="2"])),
length(unique(data3$PseudoID[data3$VacDoses=="2"])),
paste(round(summary(data5$CTRes[data5$VacDoses=="2"])[3],digits=1)," (",round(summary(data5$CTRes[data5$VacDoses=="2"])[2],digits=1),",",
round(summary(data5$CTRes[data5$VacDoses=="2"])[5],digits=1),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="2"]<18.3)," (",round(sum(data5$CTRes[data5$VacDoses=="2"]<18.3)/nrow(data5[data5$VacDoses=="2",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="2"]>=18.3 &data5$CTRes[data5$VacDoses=="2"]<24.4)," (",round(sum(data5$CTRes[data5$VacDoses=="2"]>=18.3 &data5$CTRes[data5$VacDoses=="2"]<24.4)/nrow(data5[data5$VacDoses=="2",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="2"]>=24.4 &data5$CTRes[data5$VacDoses=="2"]<30.5)," (",round(sum(data5$CTRes[data5$VacDoses=="2"]>=24.4 &data5$CTRes[data5$VacDoses=="2"]<30.5)/nrow(data5[data5$VacDoses=="2",]),digits=2),")",sep=""),
paste(sum(data5$CTRes[data5$VacDoses=="2"]>=30.5)," (",round(sum(data5$CTRes[data5$VacDoses=="2"]>=30.5)/nrow(data5[data5$VacDoses=="2",]),digits=2),")",sep=""))

DemographicsTable<-rbind(Total,Age12,Age12.18,Age19.29,Age30.39,Age40.49,Age50.59,Age60.69,Age80,Male,Female,White,Black,Asian,Mixed,Other,PNS,
SymptomsYes,SymptomsNo,Home,TestCentre,HCP,Self,Admin,November,December,January,March,June,VocUnknown,VocWildType,VocAlpha,VocDelta,Dose0,Dose1,Dose2)

write.csv(DemographicsTable,file="CtDemographicsSep2022.csv")

##Kruskal Wallis tests for differences in Ct values between age groups
#Perform the Kruskal-Wallis test
#ch1<-shapiro.test(data6$CTRes)
library(FSA)
m1<-kruskal.test(CTRes ~ age.factor, data=data6)
print(m1)
##Kruskal-Wallis chi-squared = 704.94, df = 8, p-value < 2.2e-16
posthocs1<-dunnTest(CTRes ~ age.factor, data=data6, method="bonferroni")
print(posthocs1)
write.csv(posthocs1$res,file="AgePosthocs.csv")
#posthocs2<-dscfAllPairsTest(CTRes ~ age.factor, data=data6)
#print(posthocs2)
#posthoc3<-wilcox.test(CTRes ~ age.factor, data=data6, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
#print(m1)
#posthoc4<-dunn.test(data6$CTRes,data6$age.factor,method="bonferroni")

##sex
m2<-wilcox.test(CTRes ~ Gender, data=data6, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m2)
##W = 946954928, p-value < 2.2e-16

##Ethnicity
m3<-kruskal.test(CTRes ~ Ethnicity1, data=data6)
print(m3)
##Kruskal-Wallis chi-squared = 102, df = 5, p-value < 2.2e-16
posthocs3<-dunnTest(CTRes ~ Ethnicity1, data=data6, method="bonferroni")
print(posthocs3)
write.csv(posthocs3$res,file="EthnicityPosthocs.csv")

##Symptoms
m4<-wilcox.test(CTRes ~ SYMPTOMS, data=data6, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m4)
##W = 607879637, p-value < 2.2e-16

##Test Type
m5<-kruskal.test(CTRes ~ TestLocation, data=data6)
print(m5)
##Kruskal-Wallis chi-squared = 148, df = 3, p-value < 2.2e-16
posthocs5<-dunnTest(CTRes ~ TestLocation, data=data6, method="bonferroni")
print(posthocs5)
write.csv(posthocs5$res,file="TestTypePosthocs.csv")

##Processing Lab
m6<-kruskal.test(CTRes ~ ProLab, data=data6)
print(m6)
##Kruskal-Wallis chi-squared = 1974.7, df = 4, p-value < 2.2e-16
posthocs6<-dunnTest(CTRes ~ ProLab, data=data6, method="bonferroni")
print(posthocs6)
write.csv(posthocs6$res,file="LabPosthocs.csv")

##Administration Method
m7<-kruskal.test(CTRes ~ AdministrationMethod, data=data6)
print(m7)
##Kruskal-Wallis chi-squared = 413.9, df = 2, p-value < 2.2e-16
posthocs7<-dunnTest(CTRes ~ AdministrationMethod, data=data6, method="bonferroni")
print(posthocs7)
write.csv(posthocs7$res,file="MethodPosthocs.csv")

##Test Period
m8<-kruskal.test(CTRes ~ Period, data=data6)
print(m8)
##Kruskal-Wallis chi-squared = 396.13, df = 2, p-value < 2.2e-16
posthocs8<-dunnTest(CTRes ~ Period, data=data6, method="bonferroni")
print(posthocs8)
write.csv(posthocs8$res,file="PeriodPosthocs.csv")

##New Voc?
m9<-wilcox.test(CTRes ~ NewVOC, data=data6, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
print(m9)
##W = 1041345096, p-value < 2.2e-16















##Explore longitudinal models?
s1<-subset(data3,data3$PseudoID %in% tmp[31925,1])
s2<-subset(data3,data3$PseudoID %in% tmp[43651,1])
s3<-subset(data3,data3$PseudoID %in% tmp[85898,1])
plot(s1$AppointmentDate,s1$CTRes,xlim=c(min(s1$AppointmentDate,s2$AppointmentDate,s3$AppointmentDate),max(s1$AppointmentDate,s2$AppointmentDate,s3$AppointmentDate)))
points(s2$AppointmentDate,s2$CTRes,pch=16,col="red")
points(s3$AppointmentDate,s3$CTRes,pch=16,col="blue")

emf(file = "CtFigure1Revised.emf", width=8.27,height=11.69,emfPlus = FALSE,coordDPI=600)
#tiff(filename = "CtFigure1Revised.tiff",width = 8.27, height = 11.69, units = "in",compression = "lzw",res = 600)
#par(mfrow=c(4,3))
#m<-rbind(c(1,1,2,2,3,3),c(6,4,4,5,5,6))
m<-rbind(c(1,2,3),c(4,5,6),c(7,8,9),c(11,10,12))
layout(m)

plot(x1,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.11),main="Age",lwd=2)
lines(x2,col="green",lwd=2)
lines(x3,col="red",lwd=2)
lines(x4,col="blue",lwd=2)
lines(x5,col="orange",lwd=2)
lines(x6,col="pink",lwd=2)
lines(x7,col="purple",lwd=2)
lines(x8,col="darkorange",lwd=2)
legend("topright",legend=c("5-11","12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x10,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.1),main="Sex",lwd=2)
lines(x11,col="green",lwd=2)
legend("topright",legend=c("Females","Males"),col=c("black","green"),lty=1,lwd=2)

plot(x12,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.32),main="Variant",lwd=2)
lines(x13,col="green",lwd=2)
lines(x13a,col="red",lwd=2)
lines(x13b,col="blue",lwd=2)
legend("topright",legend=c("Unknown Variant","Suspected Wild Type","Suspected Alpha","Suspected Delta"),col=c("black","green","red","blue"),lty=1,lwd=2)

plot(x14,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.18),main="Ethnicity",lwd=2)
lines(x15,col="green",lwd=2)
lines(x16,col="red",lwd=2)
lines(x17,col="blue",lwd=2)
lines(x18,col="orange",lwd=2)
lines(x19,col="pink",lwd=2)
legend("topright",legend=c("White","Black","Asian","Mixed or multiple ethnic groups","Another ethnic group",
"Prefer not to say"),col=c("black","green","red","blue","orange","pink"),lty=1,lwd=2,cex=0.8)

plot(x20,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.1),main="Symptoms",lwd=2)
lines(x21,col="green",lwd=2)
legend("topright",legend=c("Symptomatic","Asymptomatic"),col=c("black","green"),lty=1,lwd=2)

plot(x22,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.11),main="Home Test",lwd=2)
lines(x23,col="green",lwd=2)
#lines(x24,col="red",lwd=2)
#lines(x25,col="blue",lwd=2)
legend("topright",legend=c("Home Test","Not Home Test"),col=c("black","green"),lty=1,lwd=2)

#plot(x26,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.1),main="Processing laboratory",lwd=2)
#lines(x27,col="green",lwd=2)
##lines(x28,col="red",lwd=2)
#lines(x29,col="blue",lwd=2)
#legend("topright",legend=c("Alderley Park","Glasgow","Milton Keynes"),col=c("black","green","blue"),lty=1,lwd=2)
plot(x36,type="l",xlab="Cycle Threshold (Ct)",ylab="Density",ylim=c(0,0.15),main="LFTs in past 14 days",lwd=2)
lines(x37,col="green",lwd=2)
lines(x38,col="red",lwd=2)
legend("topright",legend=c("No LFTs","1 LFT","2+ LFTs"),col=c("black","green","red"),lty=1,lwd=2)

plot(x30,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.16),main="Administration method",lwd=2)
lines(x31,col="green",lwd=2)
lines(x32,col="red",lwd=2)
legend("topright",legend=c("Healthcare professional","Self administered","Not known"),col=c("black","green","red"),lty=1,lwd=2)

plot(x33,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.22),main="Appointment Date",lwd=2)
lines(x34,col="green",lwd=2)
lines(x35,col="red",lwd=2)
lines(x35a,col="blue",lwd=2)
lines(x35b,col="orange",lwd=2)
legend("topright",legend=c("6th Nov - 2nd Dec","3rd Dec - 4th Jan","5th Jan - 7th March","8th March - 16th May","17th May - 8th September"),
col=c("black","green","red","blue","orange"),lty=1,lwd=2)

plot(x39,type="l",xlab=expression("Mean Cycle Threshold (Cq)"),ylab="Density",ylim=c(0,0.12),main="Vaccination Doses",lwd=2)
lines(x40,col="green",lwd=2)
lines(x41,col="red",lwd=2)
legend("topright",legend=c("No Doses","1 Dose","2 Doses"),col=c("black","green","red"),lty=1,lwd=2)
dev.off()

tiff(filename = "CtFigureS2Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
par(mfrow=c(1,2))
hist(data5$CTRes,xlab=expression("Mean Cycle Threshold (C"[q]*")"),col="orange",main="")
hist(data5a$Age,xlab=expression("Age at test date (years)"),col="steelblue",main="")
dev.off()


##Plot mean Ct by VOC over time
data5$YearWeek<-format(data5$AppointmentDate,format="%Y-%U")

fmedian<-function(x){median(x,na.rm=TRUE)}
check1<-aggregate(CTRes~YearWeek,fmedian,data=subset(data5,data5$NewVOC=="Wild Type"))
names(check1)<-c("YearWeek","WildType")
check2<-aggregate(CTRes~YearWeek,fmedian,data=subset(data5,data5$NewVOC=="Alpha"))
names(check2)<-c("YearWeek","Alpha")
check3<-aggregate(CTRes~YearWeek,fmedian,data=subset(data5,data5$NewVOC=="Delta"))
names(check3)<-c("YearWeek","Delta")
check4<-aggregate(CTRes~YearWeek,fmedian,data=subset(data5,data5$NewVOC=="Unknown"))
names(check4)<-c("YearWeek","Unknown")
check5<-count(subset(data5,data5$NewVOC=="Wild Type"),YearWeek)
names(check5)<-c("YearWeek","nWild")
check6<-count(subset(data5,data5$NewVOC=="Alpha"),YearWeek)
names(check6)<-c("YearWeek","nAlpha")
check7<-count(subset(data5,data5$NewVOC=="Delta"),YearWeek)
names(check7)<-c("YearWeek","nDelta")
check8<-count(subset(data5,data5$NewVOC=="Unknown"),YearWeek)
names(check8)<-c("YearWeek","nUnknown")

tmp1<-count(data5, YearWeek)
tmp2<-left_join(tmp1,check1)
tmp3<-left_join(tmp2,check2)
tmp4<-left_join(tmp3,check3)
tmp5<-left_join(tmp4,check4)
tmp5<-left_join(tmp5,check5)
tmp5<-left_join(tmp5,check6)
tmp5<-left_join(tmp5,check7)
tmp5<-left_join(tmp5,check8)

emf(file = "CtFigure4Revised.emf", width=10,height=6,emfPlus = TRUE,coordDPI=600)
#tiff(filename = "CtFigure4Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
plot(1:length(tmp5$YearWeek),tmp5$WildType,ylab=expression("Median Cq Value"),xlab="", xaxt="n",ylim=c(6,40),col="chartreuse3",type="l",lwd=2)
labs <-tmp5$YearWeek
text(cex=0.8, x=1:nrow(tmp5), y=2, labs, xpd=TRUE, srt=90)
points(1:length(tmp5$YearWeek),tmp5$Alpha,col="steelblue",type="l",lwd=2)
points(1:length(tmp5$YearWeek),tmp5$Delta,col="chocolate2",type="l",lwd=2)
#points(1:length(tmp5$YearWeek),tmp5$Unknown,col="darkgoldenrod2",type="l",lwd=2)
text(x=25.15,y=15.5,"Number of individuals with Suspected Wild-Type")
text(cex=0.9, x=(1:length(tmp5$YearWeek))[seq(1,nrow(tmp5),by=3)], y=14, tmp5$nWild[seq(1,nrow(tmp5),by=3)], xpd=TRUE, srt=0)
text(x=25.15,y=11.5,"Number of individuals with Suspected Alpha")
text(cex=0.9, x=(1:length(tmp5$YearWeek))[seq(1,nrow(tmp5),by=3)], y=10, tmp5$nAlpha[seq(1,nrow(tmp5),by=3)], xpd=TRUE, srt=0)
text(x=25.15,y=7.5,"Number of individuals with Suspected Delta")
text(cex=0.9, x=(1:length(tmp5$YearWeek))[seq(1,nrow(tmp5),by=3)], y=6, tmp5$nDelta[seq(1,nrow(tmp5),by=3)], xpd=TRUE, srt=0)
#text(x=25.15,y=7,"Number of individuals with Unknown Variant")
#text(cex=0.9, x=(1:length(tmp5$YearWeek))[seq(1,nrow(tmp5),by=3)], y=6, tmp5$nUnknown[seq(1,nrow(tmp5),by=3)], xpd=TRUE, srt=0)
legend("topleft",legend=c("Suspected Wild-Type","Suspected Alpha","Suspected Delta"),col=c("chartreuse3","steelblue","chocolate2"),
lty=1,lwd=2,ncol=2)
dev.off()


##Gene values by s gene drop out or not.
xO<-bkde(data5$Ch1Cq[!is.na(data5$Ch1Cq)])
xN<-bkde(data5$Ch2Cq[!is.na(data5$Ch2Cq)])
xS<-bkde(data5$Ch3Cq[!is.na(data5$Ch3Cq)])
XO1<-bkde(data5$Ch1Cq[!is.na(data5$Ch1Cq)&is.na(data5$Ch3Cq)&!is.na(data5$Ch2Cq)])
XO2<-bkde(data5$Ch1Cq[!is.na(data5$Ch1Cq)&!is.na(data5$Ch3Cq)&!is.na(data5$Ch2Cq)])
XN1<-bkde(data5$Ch2Cq[!is.na(data5$Ch2Cq)&is.na(data5$Ch3Cq)&!is.na(data5$Ch1Cq)])
XN2<-bkde(data5$Ch2Cq[!is.na(data5$Ch2Cq)&!is.na(data5$Ch3Cq)&!is.na(data5$Ch1Cq)])

tiff(filename = "CtSupplementaryFigure8Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
par(mfrow=c(1,3))
plot(xO,type="l",xlab="Cycle Threshold",ylab="Density",ylim=c(0,0.12),main="Cycle Threshold value by gene target",lwd=2)
lines(xN,col="green",lwd=2)
lines(xS,col="red",lwd=2)
legend("topright",legend=c("ORF1ab","N gene","S gene"),col=c("black","green","red"),lty=1,lwd=2)

plot(XO1,type="l",xlab="Cycle Threshold",ylab="Density",ylim=c(0,0.12),main="ORF1ab",lwd=2)
lines(XO2,col="green",lwd=2)
legend("topright",legend=c("S gene dropout","S gene present"),col=c("black","green"),lty=1,lwd=2)

plot(XN1,type="l",xlab="Cycle Threshold",ylab="Density",ylim=c(0,0.12),main="N gene",lwd=2)
lines(XN2,col="green",lwd=2)
legend("topright",legend=c("S gene dropout","S gene present"),col=c("black","green"),lty=1,lwd=2)
dev.off()

##proprotion of tests with 3 positives or S gene dropout over time.
library(gplots)
library(PropCIs)

tmp5$Triple<-ifelse(!is.na(tmp5$nWild),tmp5$nWild,0)+ifelse(!is.na(tmp5$nDelta),tmp5$nDelta,0)

##calculate confidence intervals
TripleConf<-AlphaConf<-NULL
for(i in 1:nrow(tmp5)){
TripleConf<-rbind(TripleConf,as.numeric(exactci(tmp5$Triple[i],tmp5$n[i],conf.level=0.95)$conf.int))
AlphaConf<-rbind(AlphaConf,as.numeric(exactci(tmp5$nAlpha[i],tmp5$n[i],conf.level=0.95)$conf.int))
}

tiff(filename = "CtSupplementaryFigure9Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
plotCI(1:nrow(tmp5),tmp5$Triple/tmp5$n,li=TripleConf[,1],ui=TripleConf[,2],pch=16,
xlab="",col="chartreuse4",ylab="Proportion of Positive PCRs",ylim=c(0,1),main="Proportion of triple positives over time", xaxt="n",gap=0,lwd=1,las=1)
labs <-tmp5$YearWeek
text(cex=0.8, x=1:nrow(tmp5), y=-0.1, labs, xpd=TRUE, srt=90)
plotCI(1:nrow(tmp5),tmp5$nAlpha/tmp5$n,li=AlphaConf[,1],ui=AlphaConf[,2],pch=16,xlab="",col="cornflowerblue",ylab="", xaxt="n",gap=0,lwd=1,add=TRUE)
legend("right",legend=c("Triple positive","S gene dropout"),col=c("chartreuse4","cornflowerblue"),ncol=1,pch=16)
dev.off()


##Odds ratios plots for quantile model.
##Plot Ct values for different genes depending on whether S gene present or not...
library(rms)
library("gridExtra")  

##Odds ratios plots for quantile model.
dd <- datadist(data6)
options(datadist='dd')
quantmodNew2 <- Rq(CTRes ~ age.factor*NewVOC +Gender+SYMPTOMS+Ethnicity1+HomeTest+Period+AdministrationMethod+LFT14Prev+VacDoses,data=data6)

w1<-contrast(quantmodNew2, a=list(NewVOC="Wild Type", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w1$age.factor<-levels(data6$age.factor)
w1<-as.data.frame(w1[c('age.factor','Contrast','Lower','Upper')])

w2<-contrast(quantmodNew2, a=list(NewVOC="Alpha", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w2$age.factor<-levels(data6$age.factor)
w2<-as.data.frame(w2[c('age.factor','Contrast','Lower','Upper')])

w3<-contrast(quantmodNew2, a=list(NewVOC="Delta", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w3$age.factor<-levels(data6$age.factor)
w3<-as.data.frame(w3[c('age.factor','Contrast','Lower','Upper')])

w1<-w1[c(5,2,3,1,4,6,7,8),]
w2<-w2[c(5,2,3,1,4,6,7,8),]
w3<-w3[c(5,2,3,1,4,6,7,8),]
library(gplots)
emf(file = "CtFigure3Revised.emf", width=10,height=6,emfPlus = TRUE,coordDPI=600)
#tiff(filename = "CtFigure3Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
plotCI(1:8,w1$Contrast,li=w1$Lower,ui=w1$Upper,pch=16,xlab="",col="steelblue3",ylab="Effect Size (Cycle Thresholds)",ylim=c(-10,4),main="", xaxt="n",gap=0,lwd=1,las=1)
labs <-w1$age.factor
text(cex=1, x=1:8, y=-11, labs, xpd=TRUE, srt=0)
text(cex=1, x=4, y=-12, "Age (Years)", xpd=TRUE, srt=0)
plotCI(1:8,w2$Contrast,li=w2$Lower,ui=w2$Upper,pch=16,xlab="",col="chartreuse3",gap=0,lwd=1,las=1,add=TRUE)
plotCI(1:8,w3$Contrast,li=w3$Lower,ui=w3$Upper,pch=16,xlab="",col="chocolate3",gap=0,lwd=1,las=1,add=TRUE)
legend("topright",legend=c("Wild Type","Alpha","Delta"),col=c("steelblue3","chartreuse3","chocolate3"),ncol=1,pch=16)
dev.off()


##Sensitivity analysis
##1. Only reliable variant calling.
##Select confident Delta Period - dates after 1st May 2021
dataDelta<-subset(data6,data6$AppointmentDate>="2021-05-01")

Ctseq<-seq(10,35,length=100)
DropProp<-CTmean<-vector(length=99)

for(i in 1:99){
sub1<-subset(dataDelta,dataDelta$CTRes>=Ctseq[i] & dataDelta$CTRes<Ctseq[(i+1)])
DropProp[i]<-sum(is.na(sub1$Ch3Cq))/nrow(sub1)
CTmean[i]<-(Ctseq[i]+Ctseq[(i+1)])/2
}

tiff(filename = "CtSupplementaryFigure10Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
plot(CTmean,DropProp,type="l",xlab="Cycle Threshold",ylab="Proportion with S gene dropout")
abline(h=0.05,lty=2,lwd=2)
dev.off()
#Ct of approximately 31 is the threshold at which we get over 5% SGTF

##Select subset with CT<31
dataCT<-subset(data6,data6$CTRes<31)

##Quantile Regression Model
quantmodCT <- rq(CTRes ~ age.factor*NewVOC +Gender+SYMPTOMS+Ethnicity1+HomeTest+Period+AdministrationMethod+LFT14Prev+VacDoses,data=dataCT,ci=TRUE)
summary(quantmodCT)$coefficients
ResultsQ<-cbind(
round(quantmodCT$coefficients[,1],digits=2),
paste("(",round(quantmodCT$coefficients[,2],digits=2),", ",round(quantmodCT$coefficients[,3],digits=2),")",sep=""),
round(summary(quantmodCT)$coefficients[,4],digits=3))
write.csv(ResultsQ,file="QuantileRegressionModelAug2022_SensitivityALPHA.csv")

##Effect Size Plot
dd <- datadist(dataCT)
options(datadist='dd')
quantmodNew2CT <- Rq(CTRes ~ age.factor*NewVOC +Gender+SYMPTOMS+Ethnicity1+HomeTest+Period+AdministrationMethod+LFT14Prev+VacDoses,data=dataCT)

w1<-contrast(quantmodNew2CT, a=list(NewVOC="Wild Type", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w1$age.factor<-levels(dataCT$age.factor)
w1<-as.data.frame(w1[c('age.factor','Contrast','Lower','Upper')])

w2<-contrast(quantmodNew2CT, a=list(NewVOC="Alpha", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w2$age.factor<-levels(dataCT$age.factor)
w2<-as.data.frame(w2[c('age.factor','Contrast','Lower','Upper')])

w3<-contrast(quantmodNew2CT, a=list(NewVOC="Delta", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w3$age.factor<-levels(dataCT$age.factor)
w3<-as.data.frame(w3[c('age.factor','Contrast','Lower','Upper')])

w1<-w1[c(5,2,3,1,4,6,7,8),]
w2<-w2[c(5,2,3,1,4,6,7,8),]
w3<-w3[c(5,2,3,1,4,6,7,8),]

tiff(filename = "CtSupplementaryFigure11Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
plotCI(1:8,w1$Contrast,li=w1$Lower,ui=w1$Upper,pch=16,xlab="",col="steelblue3",ylab="Effect Size (Cycle Thresholds)",ylim=c(-10,4),main="", xaxt="n",gap=0,lwd=1,las=1)
labs <-w1$age.factor
text(cex=1, x=1:8, y=-11, labs, xpd=TRUE, srt=0)
text(cex=1, x=4, y=-12, "Age (Years)", xpd=TRUE, srt=0)
plotCI(1:8,w2$Contrast,li=w2$Lower,ui=w2$Upper,pch=16,xlab="",col="chartreuse3",gap=0,lwd=1,las=1,add=TRUE)
plotCI(1:8,w3$Contrast,li=w3$Lower,ui=w3$Upper,pch=16,xlab="",col="chocolate3",gap=0,lwd=1,las=1,add=TRUE)
legend("topright",legend=c("Wild Type","Alpha","Delta"),col=c("steelblue3","chartreuse3","chocolate3"),ncol=1,pch=16)
dev.off()

##Variant Plot.
x1C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="5-11"])
x2C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="12-18"])
x3C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="19-29"])
x4C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="30-39"])
x5C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="40-49"])
x6C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="50-59"])
x7C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="60-69"])
x8C<-bkde(dataCT$CTRes[dataCT$NewVOC=="Wild Type" & dataCT$age.factor=="70+"])

x1D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="5-11"])
x2D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="12-18"])
x3D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="19-29"])
x4D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="30-39"])
x5D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="40-49"])
x6D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="50-59"])
x7D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="60-69"])
x8D<-bkde(dataCT$CTRes[dataCT$NewVOC=="Alpha" & dataCT$age.factor=="70+"])

x1E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="5-11"])
x2E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="12-18"])
x3E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="19-29"])
x4E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="30-39"])
x5E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="40-49"])
x6E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="50-59"])
x7E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="60-69"])
x8E<-bkde(dataCT$CTRes[dataCT$NewVOC=="Delta" & dataCT$age.factor=="70+"])

tiff(filename = "CtSupplementaryFigure12Revised.tiff",width = 11, height = 5, units = "in",compression = "lzw",res = 600)
par(mfrow=c(1,3))
plot(x1C,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.1),main="Suspected Wild-Type",lwd=2)
lines(x2C,col="green",lwd=2)
lines(x3C,col="red",lwd=2)
lines(x4C,col="blue",lwd=2)
lines(x5C,col="orange",lwd=2)
lines(x6C,col="pink",lwd=2)
lines(x7C,col="purple",lwd=2)
lines(x8C,col="darkorange",lwd=2)

plot(x1D,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.1),main="Suspected Alpha",lwd=2)
lines(x2D,col="green",lwd=2)
lines(x3D,col="red",lwd=2)
lines(x4D,col="blue",lwd=2)
lines(x5D,col="orange",lwd=2)
lines(x6D,col="pink",lwd=2)
lines(x7D,col="purple",lwd=2)
lines(x8D,col="darkorange",lwd=2)
legend("topright",legend=c("<12","12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x1E,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.12),main="Suspected Delta",lwd=2)
lines(x2E,col="green",lwd=2)
lines(x3E,col="red",lwd=2)
lines(x4E,col="blue",lwd=2)
lines(x5E,col="orange",lwd=2)
lines(x6E,col="pink",lwd=2)
lines(x7E,col="purple",lwd=2)
lines(x8E,col="darkorange",lwd=2)
dev.off()

##2. Confirmatory PCRs.
##Select subset with positive LFT in 14 days before PCR
dataPos<-subset(data6,data6$LFT14Pos==1)
dataPos<-subset(dataPos,dataPos$NewVOC!="Unknown")
levels(dataPos$LFT14Prev)<-c("1","1","2")
dataPos<-dataPos[,-37]

##Quantile Regression Model
quantmodPos <- rq(CTRes ~ age.factor*NewVOC +Gender+SYMPTOMS+Ethnicity1+HomeTest+Period+AdministrationMethod+VacDoses,data=dataPos,ci=TRUE)
summary(quantmodPos)$coefficients
ResultsQ<-cbind(
round(quantmodPos$coefficients[,1],digits=2),
paste("(",round(quantmodPos$coefficients[,2],digits=2),", ",round(quantmodPos$coefficients[,3],digits=2),")",sep=""),
round(summary(quantmodPos)$coefficients[,4],digits=3))
write.csv(ResultsQ,file="QuantileRegressionModelAug2022_SensitivityConfirmatory.csv")

##Effect Size Plot
dd <- datadist(dataPos)
options(datadist='dd')
quantmodNew2Pos <- Rq(CTRes ~ age.factor*NewVOC +Gender+SYMPTOMS+Ethnicity1+HomeTest+Period+AdministrationMethod+VacDoses,data=dataPos)

w1<-contrast(quantmodNew2Pos, a=list(NewVOC="Wild Type", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w1$age.factor<-levels(dataPos$age.factor)
w1<-as.data.frame(w1[c('age.factor','Contrast','Lower','Upper')])

w2<-contrast(quantmodNew2Pos, a=list(NewVOC="Alpha", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w2$age.factor<-levels(dataPos$age.factor)
w2<-as.data.frame(w2[c('age.factor','Contrast','Lower','Upper')])

w3<-contrast(quantmodNew2Pos, a=list(NewVOC="Delta", age.factor=levels(data6$age.factor)),b=list(NewVOC="Wild Type", age.factor="30-39"),
type='individual')
w3$age.factor<-levels(dataPos$age.factor)
w3<-as.data.frame(w3[c('age.factor','Contrast','Lower','Upper')])

w1<-w1[c(5,2,3,1,4,6,7,8),]
w2<-w2[c(5,2,3,1,4,6,7,8),]
w3<-w3[c(5,2,3,1,4,6,7,8),]

tiff(filename = "CtSupplementaryFigure13Revised.tiff",width = 10, height = 6, units = "in",compression = "lzw",res = 600)
plotCI(1:8,w1$Contrast,li=w1$Lower,ui=w1$Upper,pch=16,xlab="",col="steelblue3",ylab="Effect Size (Cycle Thresholds)",ylim=c(-10,4),main="", xaxt="n",gap=0,lwd=1,las=1)
labs <-w1$age.factor
text(cex=1, x=1:8, y=-11, labs, xpd=TRUE, srt=0)
text(cex=1, x=4, y=-12, "Age (Years)", xpd=TRUE, srt=0)
plotCI(1:8,w2$Contrast,li=w2$Lower,ui=w2$Upper,pch=16,xlab="",col="chartreuse3",gap=0,lwd=1,las=1,add=TRUE)
plotCI(1:8,w3$Contrast,li=w3$Lower,ui=w3$Upper,pch=16,xlab="",col="chocolate3",gap=0,lwd=1,las=1,add=TRUE)
legend("topright",legend=c("Wild Type","Alpha","Delta"),col=c("steelblue3","chartreuse3","chocolate3"),ncol=1,pch=16)
dev.off()

##Variant Plot.
x1C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="5-11"])
x2C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="12-18"])
x3C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="19-29"])
x4C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="30-39"])
x5C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="40-49"])
x6C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="50-59"])
x7C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="60-69"])
x8C<-bkde(dataPos$CTRes[dataPos$NewVOC=="Wild Type" & dataPos$age.factor=="70+"])

x1D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="5-11"])
x2D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="12-18"])
x3D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="19-29"])
x4D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="30-39"])
x5D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="40-49"])
x6D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="50-59"])
x7D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="60-69"])
x8D<-bkde(dataPos$CTRes[dataPos$NewVOC=="Alpha" & dataPos$age.factor=="70+"])

x1E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="5-11"])
x2E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="12-18"])
x3E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="19-29"])
x4E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="30-39"])
x5E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="40-49"])
x6E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="50-59"])
x7E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="60-69"])
x8E<-bkde(dataPos$CTRes[dataPos$NewVOC=="Delta" & dataPos$age.factor=="70+"])

tiff(filename = "CtSupplementaryFigure14Revised.tiff",width = 11, height = 5, units = "in",compression = "lzw",res = 600)
par(mfrow=c(1,3))
plot(x1C,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.15),main="Suspected Wild-Type",lwd=2)
lines(x2C,col="green",lwd=2)
lines(x3C,col="red",lwd=2)
lines(x4C,col="blue",lwd=2)
lines(x5C,col="orange",lwd=2)
lines(x6C,col="pink",lwd=2)
lines(x7C,col="purple",lwd=2)
lines(x8C,col="darkorange",lwd=2)

plot(x1D,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.15),main="Suspected Alpha",lwd=2)
lines(x2D,col="green",lwd=2)
lines(x3D,col="red",lwd=2)
lines(x4D,col="blue",lwd=2)
lines(x5D,col="orange",lwd=2)
lines(x6D,col="pink",lwd=2)
lines(x7D,col="purple",lwd=2)
lines(x8D,col="darkorange",lwd=2)
legend("topright",legend=c("<12","12-18","19-29","30-39","40-49","50-59","60-69","70+"),lwd=2,col=c("black","green","red","blue","orange","pink","purple","darkorange"),lty=1)

plot(x1E,type="l",xlab=expression("Cycle Threshold (C"[q]*")"),ylab="Density",ylim=c(0,0.15),main="Suspected Delta",lwd=2)
lines(x2E,col="green",lwd=2)
lines(x3E,col="red",lwd=2)
lines(x4E,col="blue",lwd=2)
lines(x5E,col="orange",lwd=2)
lines(x6E,col="pink",lwd=2)
lines(x7E,col="purple",lwd=2)
lines(x8E,col="darkorange",lwd=2)
dev.off()


##Contrasts TAble Supplementarys S2

s1<-contrast(quantmodNew2,list(age.factor='5-11',NewVOC='Alpha'),list(age.factor='30-39',NewVOC='Alpha'))
s2<-contrast(quantmodNew2,list(age.factor='12-18',NewVOC='Alpha'),list(age.factor='30-39',NewVOC='Alpha'))
s3<-contrast(quantmodNew2,list(age.factor='19-29',NewVOC='Alpha'),list(age.factor='30-39',NewVOC='Alpha'))
s4<-contrast(quantmodNew2,list(age.factor='40-49',NewVOC='Alpha'),list(age.factor='30-39',NewVOC='Alpha'))
s5<-contrast(quantmodNew2,list(age.factor='50-59',NewVOC='Alpha'),list(age.factor='30-39',NewVOC='Alpha'))
s6<-contrast(quantmodNew2,list(age.factor='60-69',NewVOC='Alpha'),list(age.factor='30-39',NewVOC='Alpha'))
s7<-contrast(quantmodNew2,list(age.factor='70+',NewVOC='Alpha'),list(age.factor='30-39',NewVOC='Alpha'))

s8<-contrast(quantmodNew2,list(age.factor='5-11',NewVOC='Delta'),list(age.factor='30-39',NewVOC='Delta'))
s9<-contrast(quantmodNew2,list(age.factor='12-18',NewVOC='Delta'),list(age.factor='30-39',NewVOC='Delta'))
s10<-contrast(quantmodNew2,list(age.factor='19-29',NewVOC='Delta'),list(age.factor='30-39',NewVOC='Delta'))
s11<-contrast(quantmodNew2,list(age.factor='40-49',NewVOC='Delta'),list(age.factor='30-39',NewVOC='Delta'))
s12<-contrast(quantmodNew2,list(age.factor='50-59',NewVOC='Delta'),list(age.factor='30-39',NewVOC='Delta'))
s13<-contrast(quantmodNew2,list(age.factor='60-69',NewVOC='Delta'),list(age.factor='30-39',NewVOC='Delta'))
s14<-contrast(quantmodNew2,list(age.factor='70+',NewVOC='Delta'),list(age.factor='30-39',NewVOC='Delta'))


ContrastResults<-rbind(
c(s1$Contrast,s1$Lower,s1$Upper,s1$Pvalue),
c(s2$Contrast,s2$Lower,s2$Upper,s2$Pvalue),
c(s3$Contrast,s3$Lower,s3$Upper,s3$Pvalue),
c(s4$Contrast,s4$Lower,s4$Upper,s4$Pvalue),
c(s5$Contrast,s5$Lower,s5$Upper,s5$Pvalue),
c(s6$Contrast,s6$Lower,s6$Upper,s6$Pvalue),
c(s7$Contrast,s7$Lower,s7$Upper,s7$Pvalue),
c(s8$Contrast,s8$Lower,s8$Upper,s8$Pvalue),
c(s9$Contrast,s9$Lower,s9$Upper,s9$Pvalue),
c(s10$Contrast,s10$Lower,s10$Upper,s10$Pvalue),
c(s11$Contrast,s11$Lower,s11$Upper,s11$Pvalue),
c(s12$Contrast,s12$Lower,s12$Upper,s12$Pvalue),
c(s13$Contrast,s13$Lower,s13$Upper,s13$Pvalue),
c(s14$Contrast,s14$Lower,s14$Upper,s14$Pvalue)
)
write.csv(ContrastResults,file="AgeVocContrastsSep2022.csv")

