##Testing in school aged children, investigations.
library(dplyr)
library(KernSmooth)

memory.limit(size=56000000000)
load("data/2021-08-06/COVIDPILLAR2_VCam_MastP2.RData")
data1$AppointmentDate<-as.Date(as.character(data1$AppointmentDate))
##order observations by appointment date.
data1<-data1[with(data1,order(PseudoID,AppointmentDate)),]
data1$SpecimenProcessedDate<-as.Date(as.character(data1$SpecimenProcessedDate))

##Only consider dates after 6th November
data1<-data1[data1$AppointmentDate>="2020-11-06",]

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

##Remove any tests at IM, IMU or ACCORAQ - Immensa labs don't test for S gene and ACCORAQ tests for E gene.
#data1<-subset(data1,!(data1$ProcessingLab %in% c("IM","IMU","ACCORAQ","Randox")))

data1$Ch1Cq<-as.numeric(as.character(data1$Ch1Cq))
data1$Ch2Cq<-as.numeric(as.character(data1$Ch2Cq))
data1$Ch3Cq<-as.numeric(as.character(data1$Ch3Cq))
#data1$Ch3Cq[data1$Ch3Target=="MS2"]<-NA
f1<-function(x){mean(x,na.rm=TRUE)}
data1$CTRes<-apply(data1[,c("Ch2Cq","Ch3Cq","Ch1Cq")],1,f1)

data1$Ethnicity1<-as.factor(data1$Ethnicity)
levels(data1$Ethnicity1)<-c("Black","Asian","Black","Another ethnic group","Another ethnic group","Mixed or multiple ethnic groups","White","Asian",
"Mixed or multiple ethnic groups","Asian","Asian","Mixed or multiple ethnic groups","Mixed or multiple ethnic groups","Black","White","Black","Asian","Asian",
"White","White","Mixed or multiple ethnic groups","Asian","Prefer not to say","White")

check1<-cbind(levels(data1$Ethnicity1),c("Black","Asian","Black","Another ethnic group","Another ethnic group","Mixed or multiple ethnic groups","White","Asian",
"Mixed or multiple ethnic groups","Asian","Asian","Mixed or multiple ethnic groups","Mixed or multiple ethnic groups","Black","White","Black","Asian","Asian",
"White","White","Mixed or multiple ethnic groups","Asian","Prefer not to say","White"))

data1$NewVOC<-"Unknown"
data1$NewVOC[!is.na(data1$Ch1Cq)&!is.na(data1$Ch2Cq)&is.na(data1$Ch3Cq)]<-"Alpha"
data1$NewVOC[!is.na(data1$Ch1Cq)&!is.na(data1$Ch2Cq)&!is.na(data1$Ch3Cq)]<-"Wild Type"
data1$NewVOC[!is.na(data1$Ch1Cq)&!is.na(data1$Ch2Cq)&!is.na(data1$Ch3Cq)& data1$AppointmentDate>="2021-04-01"]<-"Delta"
data1$NewVOC<-as.factor(data1$NewVOC)

##restrict to LSOA's in Cheshire and Merseyside
LSOAdata<-read.csv("data/LiverpoolLSOAcodes.csv",header=TRUE,na.strings = c("NULL","NaN",""))
Areas<-c("Liverpool","Wirral","Sefton","Cheshire East","Cheshire West and Chester","Warrington","St. Helens","Knowsley","Halton")
LSOA1<-subset(LSOAdata,LSOAdata$LAD19NM %in% Areas)$LSOA11CD ##All 1562 LSOA areas in Cheshire and Merseyside

data1<-subset(data1,data1$LLSOA2011 %in% LSOA1)

##Then need confirmatory PCRs
##Can this be improved using the PHE positives and negatives data...
#data2<-read.delim("data/2021-06-15/PHE_Positive.txt",header = TRUE,quote="")
#load("data/2021-06-15/PHE_Negative.RData")
#data3<-data4
#rm(data4)
#data2$SpecimenDate<-as.Date(as.character(data2$SpecimenDate))
#data3$SpecimenDate<-as.Date(as.character(data3$SpecimenDate))

##Only consider dates before 31st July 2021
data1<-data1[data1$AppointmentDate<="2021-07-31",]

##Restrict to 5-18 year olds...
PrimaryS<-subset(data1,data1$Age>=5 & data1$Age<=11)
SecondaryS<-subset(data1,data1$Age>11& data1$Age<=18)

sub1<-subset(PrimaryS,PrimaryS$TypeofTest=="LFT")
sub2<-subset(PrimaryS,PrimaryS$TypeofTest=="PCR")
sub3<-subset(SecondaryS,SecondaryS$TypeofTest=="LFT")
sub4<-subset(SecondaryS,SecondaryS$TypeofTest=="PCR")


##Plot number of LFTs per day and positivity and void percentage.
library(RcppRoll)

tmp1<-count(sub3, AppointmentDate)
tmp2<-count(subset(sub3,sub3$TestResultDesc=="lft insufficent"),AppointmentDate)
names(tmp2)<-c("AppointmentDate","void")
tmp3<-count(subset(sub3,sub3$TestResultDesc=="lft positive"),AppointmentDate)
names(tmp3)<-c("AppointmentDate","positive")
tmp1<-left_join(tmp1,tmp2,by="AppointmentDate")
tmp1<-left_join(tmp1,tmp3,by="AppointmentDate")

tmp1$PropPos<-roll_sum(tmp1$positive,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)
tmp1$PropVoid<-roll_sum(tmp1$void,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)

#library(devEMF)
tiff("SchoolsFigure1.tiff",units="in",width=12.5,height=7.8,res=300,compression="lzw")
par(mfrow=c(2,1))
#emf(file = "ConfirmatoryPCRs.emf", width=12,height=6.5,emfPlus = FALSE)
x<-barplot(tmp1$n,col="blue",xlab="",ylab="Daily Test Counts",ylim=c(0,40000),main="Lateral Flow tests in 12-18 year olds", xaxt="n",las=1,cex.axis=0.8)
labs <-format(tmp1$AppointmentDate, format="%m-%d")
text(cex=0.8, x=x[seq(1,268,by=7)], y=-5000, labs[seq(1,268,by=7)], xpd=TRUE, srt=90)
par(new=TRUE)
plot(tmp1$AppointmentDate,tmp1$PropVoid,ylab="",xlab="",axes=FALSE,ylim=c(0,0.05),col="orange",type="l",lwd=2)
axis(4,at=seq(0,0.05,by=0.01),las=1,cex=1.5,line=-1.5,xpd=TRUE)
points(tmp1$AppointmentDate,tmp1$PropPos,col="green",type="l",lwd=2)
mtext("Proportion",side=4,line=1)
legend("topright",legend=c("Positive (7-day avrg)","Void (7-day avrg)"),col=c("green","orange"),pch=15,inset=c(0.1,0))
#dev.off()

tmp1<-count(sub1, AppointmentDate)
tmp2<-count(subset(sub1,sub1$TestResultDesc=="lft insufficent"),AppointmentDate)
names(tmp2)<-c("AppointmentDate","void")
tmp3<-count(subset(sub1,sub1$TestResultDesc=="lft positive"),AppointmentDate)
names(tmp3)<-c("AppointmentDate","positive")
tmp1<-left_join(tmp1,tmp2,by="AppointmentDate")
tmp1<-left_join(tmp1,tmp3,by="AppointmentDate")
tmp1$PropPos<-roll_sum(tmp1$positive,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)
tmp1$PropVoid<-roll_sum(tmp1$void,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)
x<-barplot(tmp1$n,col="blue",xlab="",ylab="Daily Test Counts",ylim=c(0,4500),main="Lateral Flow tests in 5-11 year olds", xaxt="n",las=1,cex.axis=0.8)
labs <-format(tmp1$AppointmentDate, format="%m-%d")
text(cex=0.8, x=x[seq(1,267,by=7)], y=-500, labs[seq(1,267,by=7)], xpd=TRUE, srt=90)
par(new=TRUE)
plot(tmp1$AppointmentDate,tmp1$PropVoid,ylab="",xlab="",axes=FALSE,ylim=c(0,0.1),col="orange",type="l",lwd=2)
axis(4,at=seq(0,0.1,by=0.02),las=1,cex=1.5,line=-1.5,xpd=TRUE)
points(tmp1$AppointmentDate,tmp1$PropPos,col="green",type="l",lwd=2)
mtext("Proportion",side=4,line=1)
legend("topright",legend=c("Positive (7-day avrg)","Void (7-day avrg)"),col=c("green","orange"),pch=15,inset=c(0.1,0))
dev.off()


##Plot number of PCRs per day and positivity and void percentage.
tmp1<-count(sub4, AppointmentDate)
tmp2<-count(subset(sub4,sub4$TestResultDesc=="Void"),AppointmentDate)
names(tmp2)<-c("AppointmentDate","void")
tmp3<-count(subset(sub4,sub4$TestResultDesc=="Positive"),AppointmentDate)
names(tmp3)<-c("AppointmentDate","positive")
tmp1<-left_join(tmp1,tmp2,by="AppointmentDate")
tmp1<-left_join(tmp1,tmp3,by="AppointmentDate")
tmp1$PropPos<-roll_sum(tmp1$positive,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)
tmp1$PropVoid<-roll_sum(tmp1$void,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)
#emf(file = "ConfirmatoryPCRs.emf", width=12,height=6.5,emfPlus = FALSE)
par(mfrow=c(2,1))
x<-barplot(tmp1$n,col="blue",xlab="",ylab="Daily Test Counts",ylim=c(0,2500),main="PCR tests in 12-18 year olds", xaxt="n",las=1,cex.axis=0.8)
labs <-format(tmp1$AppointmentDate, format="%m-%d")
text(cex=0.8, x=x[seq(1,268,by=7)], y=-500, labs[seq(1,268,by=7)], xpd=TRUE, srt=90)
par(new=TRUE)
plot(tmp1$AppointmentDate,(tmp1$void/tmp1$n),ylab="",xlab="",axes=FALSE,ylim=c(0,0.4),col="orange",type="l",lwd=2)
axis(4,at=seq(0,0.4,by=0.1),las=1,cex=1.5,line=-1.5,xpd=TRUE)
points(tmp1$AppointmentDate,tmp1$PropPos,col="green",type="l",lwd=2)
mtext("Proportion",side=4,line=0.5)
legend("topright",legend=c("Positive (7-day avrg)","Void (7-day avrg)"),col=c("green","orange"),pch=15,inset=c(0.3,0))

tmp1<-count(sub2, AppointmentDate)
tmp2<-count(subset(sub2,sub2$TestResultDesc=="Void"),AppointmentDate)
names(tmp2)<-c("AppointmentDate","void")
tmp3<-count(subset(sub2,sub2$TestResultDesc=="Positive"),AppointmentDate)
names(tmp3)<-c("AppointmentDate","positive")
tmp1<-left_join(tmp1,tmp2,by="AppointmentDate")
tmp1<-left_join(tmp1,tmp3,by="AppointmentDate")
tmp1$PropPos<-roll_sum(tmp1$positive,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)
tmp1$PropVoid<-roll_sum(tmp1$void,n=7, align = "right", fill = NA,na.rm=TRUE)/roll_sum(tmp1$n,n=7, align = "right", fill = NA,na.rm=TRUE)
#emf(file = "ConfirmatoryPCRs.emf", width=12,height=6.5,emfPlus = FALSE)
x<-barplot(tmp1$n,col="blue",xlab="",ylab="Daily Test Counts",ylim=c(0,1500),main="PCR tests in 5-11 year olds", xaxt="n",las=1,cex.axis=0.8)
labs <-format(tmp1$AppointmentDate, format="%m-%d")
text(cex=0.8, x=x[seq(1,268,by=7)], y=-300, labs[seq(1,268,by=7)], xpd=TRUE, srt=90)
par(new=TRUE)
plot(tmp1$AppointmentDate,tmp1$PropVoid,ylab="",xlab="",axes=FALSE,ylim=c(0,0.4),col="orange",type="l",lwd=2)
axis(4,at=seq(0,0.4,by=0.1),las=1,cex=1.5,line=-1.5,xpd=TRUE)
points(tmp1$AppointmentDate,tmp1$PropPos,col="green",type="l",lwd=2)
mtext("Proportion",side=4,line=0.5)
legend("topright",legend=c("Positive (7-day avrg)","Void (7-day avrg)"),col=c("green","orange"),pch=15,inset=c(0.3,0))

##Number of individuals who had a frist LFT.
sub1a<-sub1[!duplicated(sub1$PseudoID, fromLast=F),]
N1P<-nrow(sub1a)
N1P
sub3a<-sub3[!duplicated(sub3$PseudoID, fromLast=F),]
N1S<-nrow(sub3a)
N1S
##number of VOIDS and positives
L0P<-sum(sub1a$TestResultDesc=="lft insufficent")
L0S<-sum(sub3a$TestResultDesc=="lft insufficent")

L1P<-sum(sub1a$TestResultDesc=="lft positive")
L1S<-sum(sub3a$TestResultDesc=="lft positive")

L0P/N1P
L1P/N1P
L0S/N1S
L1S/N1S

##Secondary Schools
SUB1<-subset(sub3,sub3$TestResultDesc=="lft positive")
SUB1$TestRecordCreatedDate<-strptime(SUB1$TestRecordCreatedDate,format='%Y-%m-%d %H:%M')
id1<-unique(SUB1$PseudoID)
##Select all individuals within main dataset who have at least one positive LFT
SUB2<-subset(data1,data1$PseudoID %in% id1)
SUB2$TestRecordCreatedDate<-strptime(SUB2$TestRecordCreatedDate,format='%Y-%m-%d %H:%M')
SUB2<-SUB2[,c("PseudoID","AppointmentDate","SYMPTOMS","TestCentreID","TestResultDesc","NewVOC","TypeofTest","TestRecordCreatedDate","Age","Gender","Ethnicity1","ProcessingLabCode","LLSOA2011","CTRes")]

##Set up vectors with 1 row per patient...
HasConfPCR<-vector(length=length(id1))
DateConfPCR<-vector(length=length(id1))
DatePositiveLFT<-vector(length=length(id1))
PCRResult<-vector(length=length(id1))
HasSymptoms<-vector(length=length(id1))
PCRTestCentre<-vector(length=length(id1))
LFTTestCentre<-vector(length=length(id1))
Age<-vector(length=length(id1))
PCRCt<-vector(length=length(id1))
PCRVoC<-vector(length=length(id1))
Ethnicity<-vector(length=length(id1))
Sex<-vector(length=length(id1))
LFTProcessingLab<-vector(length=length(id1))
PCRProcessingLab<-vector(length=length(id1))
LSOA<-vector(length=length(id1))
TotalLFTPrev<-vector(length=length(id1))
HasConfLFT<-vector(length=length(id1))
DateConfLFT<-vector(length=length(id1))
ConfLFTResult<-vector(length=length(id1))

##loop to check if they have a confirmatory PCR, if so, 
for(i in 1:length(id1)){
t1<-subset(SUB2,SUB2$PseudoID==id1[i])
if(nrow(t1)==1){
   HasConfPCR[i]<-"No"
   HasConfLFT[i]<-"No"
   DatePositiveLFT[i]<-as.Date(t1$AppointmentDate,format="%Y-%m-%d",origin="1970-01-01")
   HasSymptoms[i]<-t1$SYMPTOMS
   LFTTestCentre[i]<-as.character(t1$TestCentreID)
   Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=12 & t1$Age<=18][1]
   Sex[i]<-as.character(t1$Gender)
   Ethnicity[i]<-as.character(t1$Ethnicity1)
   LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode)
   LSOA[i]<-as.character(t1$LLSOA2011)
   TotalLFTPrev[i]<-0
   }
if(nrow(t1)>1){
s1<-which(t1$TestResultDesc=="lft positive")
s2<-which(t1$TypeofTest=="PCR" & t1$AppointmentDate>=t1$AppointmentDate[s1[1]])
s2a<-which(t1$TypeofTest=="LFT" & t1$TestRecordCreatedDate>t1$TestRecordCreatedDate[s1[1]] & t1$AppointmentDate<=t1$AppointmentDate[s1[1]]+2)
   if(length(s2a)==0){HasConfLFT[i]<-"No"}
   if(length(s2a)>0){
      HasConfLFT[i]<-"Yes"
      DateConfLFT[i]<-t1$AppointmentDate[s2a[1]]
      ConfLFTResult[i]<-t1$TestResultDesc[s2a[1]]
   }
   if(length(s2)==0){
      HasConfPCR[i]<-"No"
      DatePositiveLFT[i]<-t1$AppointmentDate[s1[1]]
      HasSymptoms[i]<-t1$SYMPTOMS[s1[1]]
      LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[1]])
      Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=12 & t1$Age<=18][1]
      Sex[i]<-as.character(t1$Gender[s1[1]])
      Ethnicity[i]<-as.character(t1$Ethnicity1[s1[1]])
      LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode[s1[1]])
      LSOA[i]<-as.character(t1$LLSOA2011[s1[1]])
      TotalLFTPrev[i]<-sum(t1$TypeofTest=="LFT" & t1$AppointmentDate<t1$AppointmentDate[s1[1]] & t1$AppointmentDate>=(t1$AppointmentDate[s1[1]]-14))
      }
   if(length(s2)>0){
     for(k in 1:length(s1)){
        s3<-which(t1$TypeofTest=="PCR" & (t1$AppointmentDate-t1$AppointmentDate[s1[k]])<=2 & (t1$AppointmentDate-t1$AppointmentDate[s1[k]])>=0)
        if(length(s3)>0& HasConfPCR[i]!="Yes"){ 
        HasConfPCR[i]<-"Yes"
        DateConfPCR[i]<-t1$AppointmentDate[s3[1]]
        DatePositiveLFT[i]<-t1$AppointmentDate[s1[k]]
        PCRResult[i]<-t1$TestResultDesc[s3[1]]
        HasSymptoms[i]<-t1$SYMPTOMS[s1[k]]
        LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[k]])
        PCRTestCentre[i]<-as.character(t1$TestCentreID[s3[1]])
        Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=12 & t1$Age<=18][1]
        Sex[i]<-as.character(t1$Gender[s1[k]])
        Ethnicity[i]<-as.character(t1$Ethnicity1[s1[k]])
        LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode[s1[k]])
        LSOA[i]<-as.character(t1$LLSOA2011[s1[k]])
        PCRProcessingLab[i]<-as.character(t1$ProcessingLabCode[s3[1]])
        PCRCt[i]<-t1$CTRes[s3[1]]
        PCRVoC[i]<-t1$NewVOC[s3[1]]
        TotalLFTPrev[i]<-sum(t1$TypeofTest=="LFT" & t1$AppointmentDate<t1$AppointmentDate[s1[k]] & t1$AppointmentDate>=(t1$AppointmentDate[s1[k]]-14))
        }
        if(length(s3)==0 & HasConfPCR[i]!="Yes"){ 
        HasConfPCR[i]<-"No"
        DatePositiveLFT[i]<-t1$AppointmentDate[s1[k]]
        HasSymptoms[i]<-t1$SYMPTOMS[s1[k]]
        LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[k]])
        Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=12 & t1$Age<=18][1]
        Sex[i]<-as.character(t1$Gender[s1[k]])
        Ethnicity[i]<-as.character(t1$Ethnicity1[s1[k]])
        LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode[s1[k]])
        LSOA[i]<-as.character(t1$LLSOA2011[s1[k]])
        TotalLFTPrev[i]<-sum(t1$TypeofTest=="LFT" & t1$AppointmentDate<t1$AppointmentDate[s1[k]] & t1$AppointmentDate>=(t1$AppointmentDate[s1[k]]-14))
        }
     }
   }
}
}

newdata<-cbind(id1,HasConfPCR,DateConfPCR,DatePositiveLFT,PCRResult,HasSymptoms,LFTTestCentre,PCRTestCentre,Age,Sex,Ethnicity,LSOA,LFTProcessingLab,
PCRProcessingLab,PCRCt,PCRVoC,TotalLFTPrev,HasConfLFT,DateConfLFT,ConfLFTResult)
newdata<-as.data.frame(newdata)
newdata$Newdate<-as.numeric(as.character(newdata$DatePositiveLFT))
newdata$LFTDate<-as.Date(newdata$Newdate,format="%Y-%m-%d",origin="1970-01-01")
newdata$Newdate2<-as.numeric(as.character(newdata$DateConfPCR))
newdata$PCRDate<-as.Date(newdata$Newdate2,format="%Y-%m-%d",origin="1970-01-01")
newdata$diff<-as.numeric(newdata$PCRDate-newdata$LFTDate)
newdata$NewdateLFT<-as.numeric(as.character(newdata$DateConfLFT))
newdata$ConfLFTDate<-as.Date(newdata$NewdateLFT,format="%Y-%m-%d",origin="1970-01-01")
newdata$diffLFT<-as.numeric(newdata$ConfLFTDate-newdata$LFTDate)
check1<-subset(newdata,newdata$HasConfPCR=="Yes")

##1103/1903 (58.0%) have confirmatory PCR in secondary school children within 5 days.
##311/522 (59.6%) have confirmatory PCR in primary school children within 5 days.


##How many positive LFTs had a confirmatory PCR...
P1<-sum(newdata$HasConfPCR=="Yes")
P1<-sum(newdata$HasConfPCR=="Yes" & newdata$diff<=2)
P1
P1/nrow(newdata)

##How many confirmatory PCR's were negative.
P1neg<-sum(newdata$PCRResult==5)
P1neg<-sum(newdata$PCRResult==5 & newdata$diff<=2)
P1neg
P1neg/P1

##How many confirmatory PCR's were Positive
P1pos<-sum(newdata$PCRResult==6)
P1pos<-sum(newdata$PCRResult==6 & newdata$diff<=2)
P1pos
P1pos/P1

##How many confirmatory PCR's were VOID
P1v<-sum(newdata$PCRResult==7)
P1v<-sum(newdata$PCRResult==7 & newdata$diff<=2)
P1v
P1v/P1


##Primary School Modelling
##Primary Schools
SUB1<-subset(sub1,sub1$TestResultDesc=="lft positive")

SUB1$TestRecordCreatedDate<-strptime(SUB1$TestRecordCreatedDate,format='%Y-%m-%d %H:%M')
id1<-unique(SUB1$PseudoID)
##Select all individuals within main dataset who have at least one positive LFT
SUB2<-subset(data1,data1$PseudoID %in% id1)
SUB2$TestRecordCreatedDate<-strptime(SUB2$TestRecordCreatedDate,format='%Y-%m-%d %H:%M')
SUB2<-SUB2[,c("PseudoID","AppointmentDate","SYMPTOMS","TestCentreID","TestResultDesc","NewVOC","TypeofTest","TestRecordCreatedDate","Age","Gender","Ethnicity1","ProcessingLabCode","LLSOA2011","CTRes")]

##Set up vectors with 1 row per patient...
HasConfPCR<-vector(length=length(id1))
DateConfPCR<-vector(length=length(id1))
DatePositiveLFT<-vector(length=length(id1))
PCRResult<-vector(length=length(id1))
HasSymptoms<-vector(length=length(id1))
PCRTestCentre<-vector(length=length(id1))
LFTTestCentre<-vector(length=length(id1))
Age<-vector(length=length(id1))
PCRCt<-vector(length=length(id1))
PCRVoC<-vector(length=length(id1))
Ethnicity<-vector(length=length(id1))
Sex<-vector(length=length(id1))
LFTProcessingLab<-vector(length=length(id1))
PCRProcessingLab<-vector(length=length(id1))
LSOA<-vector(length=length(id1))
TotalLFTPrev<-vector(length=length(id1))
HasConfLFT<-vector(length=length(id1))
DateConfLFT<-vector(length=length(id1))
ConfLFTResult<-vector(length=length(id1))

##loop to check if they have a confirmatory PCR, if so, 
for(i in 1:length(id1)){
t1<-subset(SUB2,SUB2$PseudoID==id1[i])
if(nrow(t1)==1){
   HasConfPCR[i]<-"No"
   HasConfLFT[i]<-"No"
   DatePositiveLFT[i]<-as.Date(t1$AppointmentDate,format="%Y-%m-%d",origin="1970-01-01")
   HasSymptoms[i]<-t1$SYMPTOMS
   LFTTestCentre[i]<-as.character(t1$TestCentreID)
   Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=5 & t1$Age<=11][1]
   Sex[i]<-as.character(t1$Gender)
   Ethnicity[i]<-as.character(t1$Ethnicity1)
   LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode)
   LSOA[i]<-as.character(t1$LLSOA2011)
   TotalLFTPrev[i]<-0
   }
if(nrow(t1)>1){
s1<-which(t1$TestResultDesc=="lft positive")
s2<-which(t1$TypeofTest=="PCR" & t1$AppointmentDate>=t1$AppointmentDate[s1[1]])
s2a<-which(t1$TypeofTest=="LFT" & t1$TestRecordCreatedDate>t1$TestRecordCreatedDate[s1[1]] & t1$AppointmentDate<=t1$AppointmentDate[s1[1]]+2)
   if(length(s2a)==0){HasConfLFT[i]<-"No"}
   if(length(s2a)>0){
      HasConfLFT[i]<-"Yes"
      DateConfLFT[i]<-t1$AppointmentDate[s2a[1]]
      ConfLFTResult[i]<-t1$TestResultDesc[s2a[1]]
   }
   if(length(s2)==0){
      HasConfPCR[i]<-"No"
      DatePositiveLFT[i]<-t1$AppointmentDate[s1[1]]
      HasSymptoms[i]<-t1$SYMPTOMS[s1[1]]
      LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[1]])
      Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=5 & t1$Age<=11][1]
      Sex[i]<-as.character(t1$Gender[s1[1]])
      Ethnicity[i]<-as.character(t1$Ethnicity1[s1[1]])
      LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode[s1[1]])
      LSOA[i]<-as.character(t1$LLSOA2011[s1[1]])
      TotalLFTPrev[i]<-sum(t1$TypeofTest=="LFT" & t1$AppointmentDate<t1$AppointmentDate[s1[1]] & t1$AppointmentDate>=(t1$AppointmentDate[s1[1]]-14))
      }
   if(length(s2)>0){
     for(k in 1:length(s1)){
        s3<-which(t1$TypeofTest=="PCR" & (t1$AppointmentDate-t1$AppointmentDate[s1[k]])<=2 & (t1$AppointmentDate-t1$AppointmentDate[s1[k]])>=0)
        if(length(s3)>0& HasConfPCR[i]!="Yes"){ 
        HasConfPCR[i]<-"Yes"
        DateConfPCR[i]<-t1$AppointmentDate[s3[1]]
        DatePositiveLFT[i]<-t1$AppointmentDate[s1[k]]
        PCRResult[i]<-t1$TestResultDesc[s3[1]]
        HasSymptoms[i]<-t1$SYMPTOMS[s1[k]]
        LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[k]])
        PCRTestCentre[i]<-as.character(t1$TestCentreID[s3[1]])
        Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=5 & t1$Age<=11][1]
        Sex[i]<-as.character(t1$Gender[s1[k]])
        Ethnicity[i]<-as.character(t1$Ethnicity1[s1[k]])
        LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode[s1[k]])
        LSOA[i]<-as.character(t1$LLSOA2011[s1[k]])
        PCRProcessingLab[i]<-as.character(t1$ProcessingLabCode[s3[1]])
        PCRCt[i]<-t1$CTRes[s3[1]]
        PCRVoC[i]<-t1$NewVOC[s3[1]]
        TotalLFTPrev[i]<-sum(t1$TypeofTest=="LFT" & t1$AppointmentDate<t1$AppointmentDate[s1[k]] & t1$AppointmentDate>=(t1$AppointmentDate[s1[k]]-14))
        }
        if(length(s3)==0 & HasConfPCR[i]!="Yes"){ 
        HasConfPCR[i]<-"No"
        DatePositiveLFT[i]<-t1$AppointmentDate[s1[k]]
        HasSymptoms[i]<-t1$SYMPTOMS[s1[k]]
        LFTTestCentre[i]<-as.character(t1$TestCentreID[s1[k]])
        Age[i]<-t1$Age[!is.na(t1$Age) & t1$Age>=5 & t1$Age<=11][1]
        Sex[i]<-as.character(t1$Gender[s1[k]])
        Ethnicity[i]<-as.character(t1$Ethnicity1[s1[k]])
        LFTProcessingLab[i]<-as.character(t1$ProcessingLabCode[s1[k]])
        LSOA[i]<-as.character(t1$LLSOA2011[s1[k]])
        TotalLFTPrev[i]<-sum(t1$TypeofTest=="LFT" & t1$AppointmentDate<t1$AppointmentDate[s1[k]] & t1$AppointmentDate>=(t1$AppointmentDate[s1[k]]-14))
        }
     }
   }
}
}

newdataP<-cbind(id1,HasConfPCR,DateConfPCR,DatePositiveLFT,PCRResult,HasSymptoms,LFTTestCentre,PCRTestCentre,Age,Sex,Ethnicity,LSOA,LFTProcessingLab,
PCRProcessingLab,PCRCt,PCRVoC,TotalLFTPrev,HasConfLFT,DateConfLFT,ConfLFTResult)
newdataP<-as.data.frame(newdataP)
newdataP$Newdate<-as.numeric(as.character(newdataP$DatePositiveLFT))
newdataP$LFTDate<-as.Date(newdataP$Newdate,format="%Y-%m-%d",origin="1970-01-01")
newdataP$Newdate2<-as.numeric(as.character(newdataP$DateConfPCR))
newdataP$PCRDate<-as.Date(newdataP$Newdate2,format="%Y-%m-%d",origin="1970-01-01")
newdataP$diff<-as.numeric(newdataP$PCRDate-newdataP$LFTDate)
newdataP$NewdateLFT<-as.numeric(as.character(newdataP$DateConfLFT))
newdataP$ConfLFTDate<-as.Date(newdataP$NewdateLFT,format="%Y-%m-%d",origin="1970-01-01")
newdataP$diffLFT<-as.numeric(newdataP$ConfLFTDate-newdataP$LFTDate)
check1<-subset(newdataP,newdataP$HasConfPCR=="Yes")

##1103/1903 (58.0%) have confirmatory PCR in secondary school children within 5 days.
##377/599 (59.6%) have confirmatory PCR in primary school children within 5 days.

##How many positive LFTs had a confirmatory PCR...
P1<-sum(newdataP$HasConfPCR=="Yes")
P1<-sum(newdataP$HasConfPCR=="Yes" & newdataP$diff<=2)
P1
P1/nrow(newdataP)

##How many confirmatory PCR's were negative.
P1neg<-sum(newdataP$PCRResult==5)
P1neg<-sum(newdataP$PCRResult==5 & newdataP$diff<=2)
P1neg
P1neg/P1

##How many confirmatory PCR's were Positive
P1pos<-sum(newdataP$PCRResult==6)
P1pos<-sum(newdataP$PCRResult==6 & newdataP$diff<=2)
P1pos
P1pos/P1

##How many confirmatory PCR's were VOID
P1v<-sum(newdataP$PCRResult==7)
P1v<-sum(newdataP$PCRResult==7 & newdataP$diff<=2)
P1v
P1v/P1

##Plot number of positive LFT per day with how many were confirmed and how many confirmatory positive.

tmpA<-count(sub3, AppointmentDate)
tmp1<-count(newdata, LFTDate)
tmp2<-count(subset(newdata,newdata$HasConfPCR=="Yes"),LFTDate)
names(tmp2)<-c("LFTDate","ConfPCR")
tmp3<-count(subset(newdata,newdata$HasConfPCR=="Yes"&newdata$PCRResult=="Positive"),LFTDate)
names(tmp3)<-c("LFTDate","PosPCR")
tmp1<-left_join(tmp1,tmp2,by="LFTDate")
tmp1<-left_join(tmp1,tmp3,by="LFTDate")
names(tmp1)<-c("AppointmentDate","NumPos","ConfPCR","PosPCR")
tmpA<-left_join(tmpA,tmp1,by="AppointmentDate")
tmpA$ConfPCR[is.na(tmpA$ConfPCR)]<-0
tmpA$PosPCR[is.na(tmpA$PosPCR)]<-0

tiff("SchoolsFigure2.tiff",units="in",width=12.5,height=7.8,res=300,compression="lzw")
par(mfrow=c(2,1))
x<-barplot(tmpA$NumPos,col="black",xlab="",ylab="Daily Test Counts",ylim=c(0,150),main="Positive lateral flow tests in 12-18 year olds", xaxt="n",las=1,cex.axis=0.8)
barplot(tmpA$ConfPCR,col="orange",xaxt="n",add=T,las=1,cex.axis=0.8)
barplot(tmpA$PosPCR,col="blue",xaxt="n",add=T,las=1,cex.axis=0.8)
labs <-format(tmpA$AppointmentDate, format="%m-%d")
text(cex=0.8, x=x[seq(1,268,by=7)], y=-15, labs[seq(1,268,by=7)], xpd=TRUE, srt=90)
legend("topleft",legend=c("Confirmatory PCR positive","Confirmatory PCR negative/void","No confirmatory PCR"),col=c("blue","orange","black"),pch=15)

tmpA<-count(sub1, AppointmentDate)
tmp1<-count(newdataP, LFTDate)
tmp2<-count(subset(newdataP,newdataP$HasConfPCR=="Yes"),LFTDate)
names(tmp2)<-c("LFTDate","ConfPCR")
tmp3<-count(subset(newdataP,newdataP$HasConfPCR=="Yes"&newdataP$PCRResult=="Positive"),LFTDate)
names(tmp3)<-c("LFTDate","PosPCR")
tmp1<-left_join(tmp1,tmp2,by="LFTDate")
tmp1<-left_join(tmp1,tmp3,by="LFTDate")
names(tmp1)<-c("AppointmentDate","NumPos","ConfPCR","PosPCR")
tmpA<-left_join(tmpA,tmp1,by="AppointmentDate")
tmpA$ConfPCR[is.na(tmpA$ConfPCR)]<-0
tmpA$PosPCR[is.na(tmpA$PosPCR)]<-0
x<-barplot(tmpA$NumPos,col="black",xlab="",ylab="Daily Test Counts",ylim=c(0,60),main="Positive lateral flow tests in 5-11 year olds", xaxt="n",las=1,cex.axis=0.8)
barplot(tmpA$ConfPCR,col="orange",xaxt="n",add=T,las=1,cex.axis=0.8)
barplot(tmpA$PosPCR,col="blue",xaxt="n",add=T,las=1,cex.axis=0.8)
labs <-format(tmpA$AppointmentDate, format="%m-%d")
text(cex=0.8, x=x[seq(1,268,by=7)], y=-10, labs[seq(1,268,by=7)], xpd=TRUE, srt=90)
legend("topleft",legend=c("Confirmatory PCR positive","Confirmatory PCR negative/void","No confirmatory PCR"),col=c("blue","orange","black"),pch=15)
dev.off()


##Modelling..
##Merge in deprivation indices.
##Load in deprivations qunitile data
dataMark<-read.csv("data/imd2019.csv",header=TRUE,na.strings = c("NULL","NaN",""))
dataMark<-dataMark[,c("�..lsoa_code","imd_decile","LAD_name")]
colnames(dataMark)<-c("LSOA","imd_decile","LAD_name")
newdata<-merge(newdata,dataMark,by="LSOA",all.x=TRUE)
newdata$LAD_name<-droplevels(newdata$LAD_name)
newdata$imd_decile<-as.factor(newdata$imd_decile)
levels(newdata$imd_decile)<-c("1","1","2","2","3","3","4","4","5","5")
newdata$imd_decile<-relevel(newdata$imd_decile,ref="5")

levels(newdata$Sex)<-c("Female","Male",NA)
newdata$Age<-as.numeric(as.character(newdata$Age))
newdata$SelfTest<-"No"
newdata$SelfTest[newdata$LFTProcessingLab=="LFDSelfTest"]<-"Yes"
newdata$SelfTest[newdata$LFTProcessingLab=="SelfServe"]<-"Yes"
newdata$Ethnicity<-relevel(newdata$Ethnicity,ref="White")
newdata$TotalLFTPrev<-as.numeric(as.character(newdata$TotalLFTPrev))

newdataP<-merge(newdataP,dataMark,by="LSOA",all.x=TRUE)
newdataP$LAD_name<-droplevels(newdataP$LAD_name)
newdataP$imd_decile<-as.factor(newdataP$imd_decile)
levels(newdataP$imd_decile)<-c("1","1","2","2","3","3","4","4","5","5")
newdataP$imd_decile<-relevel(newdataP$imd_decile,ref="5")

levels(newdataP$Sex)<-c("Female","Male",NA)
newdataP$Age<-as.numeric(as.character(newdataP$Age))
newdataP$SelfTest<-"No"
newdataP$SelfTest[newdataP$LFTProcessingLab=="LFDSelfTest"]<-"Yes"
newdataP$SelfTest[newdataP$LFTProcessingLab=="SelfServe"]<-"Yes"
newdataP$Ethnicity<-relevel(newdataP$Ethnicity,ref="White")
newdataP$TotalLFTPrev<-as.numeric(as.character(newdataP$TotalLFTPrev))

##Model 1 - What affects uptake of confirmatory PCR.
newdata$Age<-newdata$Age-15
newdataP$Age<-newdataP$Age-8
model1<-glm(HasConfPCR~Age+Sex+Ethnicity+imd_decile+TotalLFTPrev + SelfTest,family=binomial,data=newdata) 
cis<-confint(model1)
Params<-cbind(exp(model1$coefficients),exp(cis),coef(summary(model1))[,4])
write.csv(Params,file="SecondaryConfirmatoryV2.csv")

model2<-glm(HasConfPCR~Age+Sex+Ethnicity+imd_decile+TotalLFTPrev + SelfTest,family=binomial,data=newdataP) 
cis<-confint(model2)
Params<-cbind(exp(model2$coefficients),exp(cis),coef(summary(model2))[,4])
write.csv(Params,file="PrimaryConfirmatoryV2.csv")


##Model 2 - What affects correctness of adjudications.
newdata2<-subset(newdata,newdata$HasConfPCR=="Yes")
newdata2$PCRAgreed<-"No"
newdata2$PCRAgreed[newdata2$PCRResult=="Positive"]<-"Yes"
newdata2$PCRAgreed<-as.factor(newdata2$PCRAgreed)
levels(newdata2$Ethnicity)<-c("White","Non-White","Non-White","Non-White","Non-White","Prefer not to say")

newdata2P<-subset(newdataP,newdataP$HasConfPCR=="Yes")
newdata2P$PCRAgreed<-"No"
newdata2P$PCRAgreed[newdata2P$PCRResult=="Positive"]<-"Yes"
newdata2P$PCRAgreed<-as.factor(newdata2P$PCRAgreed)
levels(newdata2P$Ethnicity)<-c("White","Non-White","Non-White","Non-White","Non-White","Prefer not to say")

model3<-glm(PCRAgreed~Age+Sex+Ethnicity+imd_decile+TotalLFTPrev + SelfTest,family=binomial,data=newdata2)
cis<-confint(model3)
Params<-cbind(exp(model3$coefficients),exp(cis),coef(summary(model3))[,4])
write.csv(Params,file="CorrectAdjudicationsSecondaryV2.csv")

model4<-glm(PCRAgreed~Age+Sex+Ethnicity+imd_decile+TotalLFTPrev + SelfTest,family=binomial,data=newdata2P)
cis<-confint(model4)
Params<-cbind(exp(model4$coefficients),exp(cis),coef(summary(model4))[,4])
write.csv(Params,file="CorrectAdjudicationsPrimaryV2.csv")

##Model 3 - what affects uptake of multiple LFTs
#CHECK1<-count(sub3,PseudoID)
#CHECK1<-aggregate(~PseudoID,f2,data=data1)
#names(check2)<-c("PseudoID","TotalLFT")
#data1<-left_join(data1,check2)
#newdata<-merge(newdata,dataMark,by="LSOA",all.x=TRUE)
#newdata$LAD_name<-droplevels(newdata$LAD_name)
#newdata$imd_decile<-as.factor(newdata$imd_decile)
#levels(newdata$imd_decile)<-c("1","1","2","2","3","3","4","4","5","5")
#newdata$imd_decile<-relevel(newdata$imd_decile,ref="5")
#
#levels(newdata$Sex)<-c(NA,"Female","Male",NA)
#newdata$Age<-as.numeric(as.character(newdata$Age))
#newdata$Age[newdata$Age==0]<-NA
#newdata$SelfTest<-"No"
#newdata$SelfTest[newdata$LFTProcessingLab=="LFDSelfTest"]<-"Yes"
#newdata$SelfTest[newdata$LFTProcessingLab=="SelfServe"]<-"Yes"
#newdata$Ethnicity<-relevel(newdata$Ethnicity,ref="White")




##Plot CT values by age with colours for correct or not?

##Plot false positive rate by week?

##Plot proportion correct by days between test.


##Plot Ct value v age coloured by correct/incorrect.
newdata$PCRCt<-as.numeric(as.character(newdata$PCRCt))
newdataP$PCRCt<-as.numeric(as.character(newdataP$PCRCt))
newdata$Age<-as.numeric(as.character(newdata$Age))
newdataP$Age<-as.numeric(as.character(newdataP$Age))

CombinedData<-rbind(newdata,newdataP)
Totals<-Correct<-vector(length=14)

for(i in 5:18){
Totals[(i-4)]<-sum(CombinedData$Age==i & CombinedData$HasConfPCR=="Yes",na.rm=TRUE)
Correct[(i-4)]<-sum(CombinedData$Age==i& CombinedData$HasConfPCR=="Yes" & CombinedData$PCRResult=="Positive",na.rm=TRUE)
}

plot(5:18,Correct/Totals,ylim=c(0,1),xlab="Age (Years)",pch=16,ylab="Proportion",main="Agreement between Positive LFT and PCR by age")
library(PropCIs)
PropCorrectCI<-NULL
for(i in 1:14){
PropCorrectCI<-rbind(PropCorrectCI,as.numeric(exactci(Correct[i],Totals[i],conf.level=0.95)$conf.int))
}
library(gplots)
plotCI(5:18,Correct/Totals,ui=PropCorrectCI[,2],li=PropCorrectCI[,1], xlab="Age (years)",ylab="Proportion",,main="Agreement between Positive LFT and PCR by age",
,ylim=c(0,1),pch=16,col="steelblue",gap=0)

##Accuracy by days between +LFT and PCR
TotalDiffS<-TotalDiffP<-CorrectDiffS<-CorrectDiffP<-vector(length=6)

for(i in 0:5){
TotalDiffS[(i+1)]<-sum(newdata$HasConfPCR=="Yes" & newdata$diff==i)
CorrectDiffS[(i+1)]<-sum(newdata$HasConfPCR=="Yes" & newdata$diff==i & newdata$PCRResult==6)
TotalDiffP[(i+1)]<-sum(newdataP$HasConfPCR=="Yes" & newdataP$diff==i)
CorrectDiffP[(i+1)]<-sum(newdataP$HasConfPCR=="Yes" & newdataP$diff==i & newdataP$PCRResult==6)
}

par(mfrow=c(1,2))
plot(0:5,CorrectDiffP/TotalDiffP,pch=16,xlab="Time between Positive LFT and PCR (days)",ylab="Proportion Identified as positive by PCR",main="Primary School aged children")
plot(0:5,CorrectDiffS/TotalDiffS,pch=16,xlab="Time between Positive LFT and PCR (days)",ylab="Proportion Identified as positive by PCR",main="Secondary School aged children")

##Graph of false positive rate over time....
x_month <- format(newdata$PCRDate, "%m")

FPRate<-subset(newdata,newdata$HasConfPCR=="Yes")
FPRate$month<-format(FPRate$PCRDate, "%b-%Y")
FPRateP<-subset(newdataP,newdataP$HasConfPCR=="Yes")
FPRateP$month<-format(FPRateP$PCRDate, "%b-%Y")

FPRate$month<-format(FPRate$PCRDate, "%b-%Y")
PrimaryS$month<-format(PrimaryS$AppointmentDate, "%b-%Y") 
SecondaryS$month<-format(SecondaryS$AppointmentDate, "%b-%Y") 
newdata$month<-format(newdata$LFTDate, "%b-%Y")
newdataP$month<-format(newdataP$LFTDate, "%b-%Y")

m1<-unique(FPRate$month)
MonthTotalS<-MonthFalsePosS<-MonthTotalP<-MonthFalsePosP<-vector(length=length(m1))
MonthTotalTests<-MonthTotalTestsP<-MonthTotalPos<-MonthTotalPosP<-vector(length=length(m1))

for(i in 1:length(m1)){
MonthTotalS[i]<-sum(FPRate$month==m1[i])
MonthFalsePosS[i]<-sum(FPRate$month==m1[i] & FPRate$PCRResult=="Negative")
MonthTotalP[i]<-sum(FPRateP$month==m1[i])
MonthFalsePosP[i]<-sum(FPRateP$month==m1[i] & FPRateP$PCRResult=="Negative")
MonthTotalTests[i]<-sum(SecondaryS$month==m1[i]) 
MonthTotalTestsP[i]<-sum(PrimaryS$month==m1[i])
MonthTotalPos[i]<-sum(SecondaryS$month==m1[i] & SecondaryS$TestResultDesc %in% c("lft positive","Positive"))
MonthTotalPosP[i]<-sum(PrimaryS$month==m1[i] & PrimaryS$TestResultDesc %in% c("lft positive","Positive"))
} 
## [pasted console output or note, commented out for parsing] 5,4,2,9,7,8,6,1,3
## [pasted console output or note, commented out for parsing] Monthly false positive rates varied between 4.7% and 35.3% in 12-18-year olds and 0% and 25.6% in 5-11-year olds.
plot(1:9,MonthFalsePosS[c(5,4,2,9,7,8,6,1,3)]/MonthTotalS[c(5,4,2,9,7,8,6,1,3)],pch=16,xlab="",col="green",ylab="False Positive Proportion",
ylim=c(0,1),main="False Positive Rates over time", xaxt="n")
text(cex=1, x=1:9, y=-0.1, m1[c(5,4,2,9,7,8,6,1,3)], xpd=TRUE, srt=0)
points(1:9,MonthFalsePosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalP[c(5,4,2,9,7,8,6,1,3)],xlab="",col="red",pch=16)
points(1:9,MonthFalsePosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalP[c(5,4,2,9,7,8,6,1,3)],xlab="",col="red",type="l",lwd=2)
points(1:9,MonthFalsePosS[c(5,4,2,9,7,8,6,1,3)]/MonthTotalS[c(5,4,2,9,7,8,6,1,3)],xlab="",col="green",type="l",lwd=2)
points(1:9,MonthTotalPos[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTests[c(5,4,2,9,7,8,6,1,3)],col="blue",pch=16)
points(1:9,MonthTotalPos[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTests[c(5,4,2,9,7,8,6,1,3)],col="blue",type="l",lwd=2)
points(1:9,MonthTotalPosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTestsP[c(5,4,2,9,7,8,6,1,3)],col="orange",pch=16)
points(1:9,MonthTotalPosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTestsP[c(5,4,2,9,7,8,6,1,3)],col="orange",type="l",lwd=2)
legend("topleft",legend=c("Age 5-11 False Positive Rate","Age 12-18 False Positive Rate","Age 5-11 Positive test Rate","Age 12-18 Positive test Rate"),
col=c("red","green","orange","blue"),pch=15)


FPCISec<-NULL
FPCIPrim<-NULL
PCISec<-NULL
PCIPrim<-NULL
for(i in 1:9){
FPCISec<-rbind(FPCISec,as.numeric(exactci(MonthFalsePosS[i],MonthTotalS[i],conf.level=0.95)$conf.int))
FPCIPrim<-rbind(FPCIPrim,as.numeric(exactci(MonthFalsePosP[i],MonthTotalP[i],conf.level=0.95)$conf.int))
PCISec<-rbind(PCISec,as.numeric(exactci(MonthTotalPos[i],MonthTotalTests[i],conf.level=0.95)$conf.int))
PCIPrim<-rbind(PCIPrim,as.numeric(exactci(MonthTotalPosP[i],MonthTotalTestsP[i],conf.level=0.95)$conf.int))
}


plotCI(MonthTotalPos/MonthTotalTests,MonthFalsePosS/MonthTotalS,ui=FPCISec[,2],li=FPCISec[,1], xlab="Positive Test Rate",ylab="False Positive Rate",main="Agreement between Positive LFT and PCR by age",
,ylim=c(0,1),pch=16,col="chartreuse4",gap=0)
plotCI(MonthTotalPosP/MonthTotalTestsP,MonthFalsePosP/MonthTotalP,ui=FPCIPrim[,2],li=FPCIPrim[,1],pch=16,col="chocolate2",gap=0,add=TRUE)

tab1<-table(newdata$imd_decile,newdata$TotalLFTPrev)
tabprop<-100*round(prop.table(table(newdata$imd_decile,newdata$TotalLFTPrev),margin=1),digits=2)

tab2<-tab1
for(i in 1:nrow(tab1)){
for(j in 1:ncol(tab1)){
tab2[i,j]<-paste(tab1[i,j],"(",tabprop[i,j],"%)",sep="")
}
}

write.csv(tab2,file="SecondarySchoolDeprivationPreviousLFT.csv")


tab1<-table(newdataP$imd_decile,newdataP$TotalLFTPrev)
tabprop<-100*round(prop.table(table(newdataP$imd_decile,newdataP$TotalLFTPrev),margin=1),digits=2)

tab2<-tab1
for(i in 1:nrow(tab1)){
for(j in 1:ncol(tab1)){
tab2[i,j]<-paste(tab1[i,j],"(",tabprop[i,j],"%)",sep="")
}
}

write.csv(tab2,file="PrimarySchoolDeprivationPreviousLFT.csv")

##Confidence intervals for monthly false positive rates.

m<-rbind(c(1,1,1,1),c(2,2,2,2),c(3,3,4,4))
layout(m)
plotCI(1:9,MonthFalsePosS[c(5,4,2,9,7,8,6,1,3)]/MonthTotalS[c(5,4,2,9,7,8,6,1,3)],li=FPCISec[c(5,4,2,9,7,8,6,1,3),1],ui=FPCISec[c(5,4,2,9,7,8,6,1,3),2],pch=16,
xlab="",col="chartreuse4",ylab="Discordant Positive Proportion",ylim=c(0,1),main="Discordant Positive Proportion over time", xaxt="n",gap=0,lwd=1)
text(cex=1, x=1:9, y=-0.1, m1[c(5,4,2,9,7,8,6,1,3)], xpd=TRUE, srt=0)
plotCI(0.2+1:9,MonthFalsePosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalP[c(5,4,2,9,7,8,6,1,3)],li=FPCIPrim[c(5,4,2,9,7,8,6,1,3),1],ui=FPCIPrim[c(5,4,2,9,7,8,6,1,3),2],pch=16,
col="chocolate2",add=TRUE,gap=0,lwd=1)
#points(1:9,MonthFalsePosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalP[c(5,4,2,9,7,8,6,1,3)],xlab="",col="chocolate2",pch=16)
#points(0.2+1:9,MonthFalsePosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalP[c(5,4,2,9,7,8,6,1,3)],xlab="",col="chocolate2",type="l",lwd=1.5)
#points(1:9,MonthFalsePosS[c(5,4,2,9,7,8,6,1,3)]/MonthTotalS[c(5,4,2,9,7,8,6,1,3)],xlab="",col="chartreuse4",type="l",lwd=1.5)
legend("topright",legend=c("Age 5-11 Discordant Positive propotion","Age 12-18 Discordant Positive proportion"),col=c("chocolate2","chartreuse4"),pch=15)

plotCI(1:9,MonthTotalPos[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTests[c(5,4,2,9,7,8,6,1,3)],li=PCISec[c(5,4,2,9,7,8,6,1,3),1],ui=PCISec[c(5,4,2,9,7,8,6,1,3),2],
pch=16,xlab="",col="cornflowerblue",ylab="Proportion Positive",ylim=c(0,0.15),main="Proportion of tests positive over time", xaxt="n",gap=0,lwd=1)
text(cex=1, x=1:9, y=-0.01, m1[c(5,4,2,9,7,8,6,1,3)], xpd=TRUE, srt=0)
plotCI(0.2+1:9,MonthTotalPosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTestsP[c(5,4,2,9,7,8,6,1,3)],li=PCIPrim[c(5,4,2,9,7,8,6,1,3),1],ui=PCIPrim[c(5,4,2,9,7,8,6,1,3),2],
col="darkgoldenrod3",add=TRUE,gap=0,pch=16,lwd=1)
#points(1:9,MonthTotalPos[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTests[c(5,4,2,9,7,8,6,1,3)],col="cornflowerblue",pch=16)
#points(1:9,MonthTotalPos[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTests[c(5,4,2,9,7,8,6,1,3)],col="cornflowerblue",type="l",lwd=2)
#points(1:9,MonthTotalPosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTestsP[c(5,4,2,9,7,8,6,1,3)],col="darkgoldenrod3",pch=16)
#points(1:9,MonthTotalPosP[c(5,4,2,9,7,8,6,1,3)]/MonthTotalTestsP[c(5,4,2,9,7,8,6,1,3)],col="darkgoldenrod3",type="l",lwd=2)
legend("topleft",legend=c("Age 5-11 Positive test Rate","Age 12-18 Positive test Rate"),col=c("darkgoldenrod3","cornflowerblue"),pch=15)

plotCI(5:18,Correct/Totals,ui=PropCorrectCI[,2],li=PropCorrectCI[,1], xlab="Age (years)",ylab="Concordant Positive Proportion",main="Agreement between Positive LFT and PCR by age",
,ylim=c(0.7,1),pch=16,col="steelblue",gap=0)

plotCI(MonthTotalPos/MonthTotalTests,MonthFalsePosS/MonthTotalS,ui=FPCISec[,2],li=FPCISec[,1], xlab="Proportion positive",ylab="Discordant Positive proportion",
main="Discordant positive proportion by test positivity",ylim=c(0,0.6),pch=16,col="chartreuse4",gap=0)
plotCI(MonthTotalPosP/MonthTotalTestsP,MonthFalsePosP/MonthTotalP,ui=FPCIPrim[,2],li=FPCIPrim[,1],pch=16,col="chocolate2",gap=0,add=TRUE)
legend("topright",legend=c("Age 5-11","Age 12-18"),col=c("chocolate2","chartreuse4"),pch=16)

##Calculate proportion of tests with confirmatory PCR by age.
AgeTotals<-vector(length=14)
AgePropConfirmedCI<-NULL

for(i in 5:18){
AgeTotals[(i-4)]<-sum(CombinedData$Age==i ,na.rm=TRUE)
AgePropConfirmedCI<-rbind(AgePropConfirmedCI,as.numeric(exactci(Totals[(i-4)],AgeTotals[(i-4)],conf.level=0.95)$conf.int))
}

plotCI(5:18,Totals/AgeTotals,ui=AgePropConfirmedCI[,2],li=AgePropConfirmedCI[,1], xlab="Age (years)",ylab="Proportion with Confirmatory PCR",
main="Confirmatory PCR following positive LFT by age",,ylim=c(0,1),pch=16,col="steelblue",gap=0)


##Table of Self Test false positive rates by month.
##Count number of tests per month - 


m1<-unique(newdata$month)

##MonthTotalTests has total number of tests per month
##MontTotalPos has total positives per month.

TotalMonth<-TotalSelfTest<-TotalSelfTestConfirmed<-TotalSelfTestCorrect<-TotalTestCentre<-TotalTestCentreConfirmed<-
TotalTestCentreCorrect<-MonthTotalTests<-MonthTotalPos<-vector(length=length(m1))
for(i in 1:length(m1)){
##Count total number of positive LFT tests per month
TotalMonth[i]<-sum(newdata$month==m1[i])
##Count number of self tests per month
TotalSelfTest[i]<-sum(newdata$month==m1[i]&newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))
##Count Self Tests with Confirmatory PCR
TotalSelfTestConfirmed[i]<-sum(newdata$month==m1[i]&newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")& newdata$HasConfPCR=="Yes")
##Count Self Tests with correct Confirmatory PCR
TotalSelfTestCorrect[i]<-sum(newdata$month==m1[i]&newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")& newdata$HasConfPCR=="Yes" & newdata$PCRResult=="Positive")
##Count number of non self tests per month.
TotalTestCentre[i]<-sum(newdata$month==m1[i] & !(newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")))
##Count Centre Tests with Confirmatory PCR
TotalTestCentreConfirmed[i]<-sum(newdata$month==m1[i]&!(newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))& newdata$HasConfPCR=="Yes")
##Count Centre Tests with correct Confirmatory PCR
TotalTestCentreCorrect[i]<-sum(newdata$month==m1[i]&!(newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))& newdata$HasConfPCR=="Yes" & newdata$PCRResult=="Positive")
MonthTotalTests[i]<-sum(SecondaryS$month==m1[i]) 
MonthTotalPos[i]<-sum(SecondaryS$month==m1[i] & SecondaryS$TestResultDesc %in% c("lft positive","Positive"))
}

c(8,2,6,4,3,7,9,1,5)

plot(1:9,MonthTotalPos[c(8,2,6,4,3,7,9,1,5)]/MonthTotalTests[c(8,2,6,4,3,7,9,1,5)],type="l",xlab="",col="green",ylab="Proportion",
ylim=c(0,1),main="", xaxt="n",lwd=2)
text(cex=1, x=1:9, y=-0.1, m1[c(8,2,6,4,3,7,9,1,5)], xpd=TRUE, srt=0)
lines(1:9,TotalSelfTest[c(8,2,6,4,3,7,9,1,5)]/TotalMonth[c(8,2,6,4,3,7,9,1,5)],col="blue",lwd=2)
lines(1:9,TotalSelfTestConfirmed[c(8,2,6,4,3,7,9,1,5)]/TotalSelfTest[c(8,2,6,4,3,7,9,1,5)],col="red",lwd=2)
lines(1:9,TotalSelfTestCorrect[c(8,2,6,4,3,7,9,1,5)]/TotalSelfTestConfirmed[c(8,2,6,4,3,7,9,1,5)],col="orange",lwd=2)
#lines(1:9,TotalTestCentreConfirmed[c(8,2,6,4,3,7,9,1,5)]/TotalTestCentre[c(8,2,6,4,3,7,9,1,5)],col="pink",lwd=2)
#lines(1:9,TotalTestCentreCorrect[c(8,2,6,4,3,7,9,1,5)]/TotalTestCentreConfirmed[c(8,2,6,4,3,7,9,1,5)],col="purple",lwd=2)
legend("topleft",legend=c("Proportion of all tests positive","Proportion of positive LFTs that are self tests",
"Self Tests with confirmatory PCR","Self Test with confirmatory PCR positive"),
col=c("green","blue","red","orange"),pch=15,inset=c(0.01,0.2))

SelfTestTable<-cbind(m1[c(8,2,6,4,3,7,9,1,5)],MonthTotalTests[c(8,2,6,4,3,7,9,1,5)],MonthTotalPos[c(8,2,6,4,3,7,9,1,5)],
MonthTotalPos[c(8,2,6,4,3,7,9,1,5)]/MonthTotalTests[c(8,2,6,4,3,7,9,1,5)],TotalSelfTest[c(8,2,6,4,3,7,9,1,5)],
TotalSelfTest[c(8,2,6,4,3,7,9,1,5)]/TotalMonth[c(8,2,6,4,3,7,9,1,5)],TotalSelfTestConfirmed[c(8,2,6,4,3,7,9,1,5)],
TotalSelfTestConfirmed[c(8,2,6,4,3,7,9,1,5)]/TotalSelfTest[c(8,2,6,4,3,7,9,1,5)],TotalSelfTestCorrect[c(8,2,6,4,3,7,9,1,5)],
TotalSelfTestCorrect[c(8,2,6,4,3,7,9,1,5)]/TotalSelfTestConfirmed[c(8,2,6,4,3,7,9,1,5)])

write.csv(SelfTestTable,file="SelfTestInvestigations.csv")

##HOw many 5-11 and 12-18 year olds registered in LIverpool in March 2021

Demographics<-read.delim("data/2021-03-05/Demographics.txt",header = TRUE,quote="")
Demographics1<-read.delim("data/2021-03-29/Demographics.txt",header = TRUE,quote="")

s1<-subset(sub1,sub1$AppointmentDate>="2021-03-08" & sub1$AppointmentDate<="2021-03-19")
length(unique(s1$PseudoID))

s2<-subset(sub3,sub3$AppointmentDate>="2021-03-08" & sub3$AppointmentDate<="2021-03-19")
length(unique(s2$PseudoID))

sum(Demographics$Patient.Age>=12 & Demographics$Patient.Age<=18,na.rm=TRUE)

##Table of tests and results by month.
##Secondary School Aged
newdata$month<-format(newdata$LFTDate, "%b-%Y")
newdataP$month<-format(newdataP$LFTDate, "%b-%Y")
sub1$month<-format(sub1$AppointmentDate, "%b-%Y")
sub3$month<-format(sub3$AppointmentDate, "%b-%Y")
newdata$month<-factor(newdata$month,levels=c("Nov-2020","Dec-2020","Jan-2021","Feb-2021","Mar-2021","Apr-2021","May-2021","Jun-2021","Jul-2021"))
newdataP$month<-factor(newdataP$month,levels=c("Nov-2020","Dec-2020","Jan-2021","Feb-2021","Mar-2021","Apr-2021","May-2021","Jun-2021","Jul-2021"))
sub1$month<-factor(sub1$month,levels=c("Nov-2020","Dec-2020","Jan-2021","Feb-2021","Mar-2021","Apr-2021","May-2021","Jun-2021","Jul-2021"))
sub3$month<-factor(sub3$month,levels=c("Nov-2020","Dec-2020","Jan-2021","Feb-2021","Mar-2021","Apr-2021","May-2021","Jun-2021","Jul-2021"))
SecondaryS$month<-format(SecondaryS$AppointmentDate, "%b-%Y")
SecondaryS$month<-factor(SecondaryS$month,levels=c("Nov-2020","Dec-2020","Jan-2021","Feb-2021","Mar-2021","Apr-2021","May-2021","Jun-2021","Jul-2021"))
PrimaryS$month<-format(PrimaryS$AppointmentDate, "%b-%Y")
PrimaryS$month<-factor(PrimaryS$month,levels=c("Nov-2020","Dec-2020","Jan-2021","Feb-2021","Mar-2021","Apr-2021","May-2021","Jun-2021","Jul-2021"))

m1<-levels(newdata$month)

TotalLFT<-TotalLFTInd<-TotalLFTPos<-TotalConfPCR<-TotalConfPCRPos<-TotalConfPCRNeg<-TotalConfPCRVoid<-TotalSelfTest<-TotalSelfTestConfirmed<-
TotalSelfTestCorrect<-MonthTotalTests<-MonthTotalPos<-TotalCentreTest<-TotalCentreTestConfirmed<-
TotalCentreTestCorrect<-vector(length=length(m1))

for(i in 1:length(m1)){
TotalLFT[i]<-sum(sub3$month==m1[i])
TotalLFTInd[i]<-length(unique(subset(sub3,sub3$month==m1[i])$PseudoID))
TotalLFTPos[i]<-sum(newdata$month==m1[i])
TotalConfPCR[i]<-sum(newdata$month==m1[i] & newdata$HasConfPCR=="Yes")
TotalConfPCRPos[i]<-sum(newdata$month==m1[i] & newdata$HasConfPCR=="Yes" & newdata$PCRResult=="Positive")
TotalConfPCRNeg[i]<-sum(newdata$month==m1[i] & newdata$HasConfPCR=="Yes" & newdata$PCRResult=="Negative")
TotalConfPCRVoid[i]<-sum(newdata$month==m1[i] & newdata$HasConfPCR=="Yes" & newdata$PCRResult=="Void")
TotalSelfTest[i]<-sum(newdata$month==m1[i]&newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))
TotalSelfTestConfirmed[i]<-sum(newdata$month==m1[i]&newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")& newdata$HasConfPCR=="Yes")
TotalSelfTestCorrect[i]<-sum(newdata$month==m1[i]&newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")& newdata$HasConfPCR=="Yes" & newdata$PCRResult=="Positive")
TotalCentreTest[i]<-sum(newdata$month==m1[i]&!(newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")))
TotalCentreTestConfirmed[i]<-sum(newdata$month==m1[i]&!(newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))& newdata$HasConfPCR=="Yes")
TotalCentreTestCorrect[i]<-sum(newdata$month==m1[i]&!(newdata$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))& newdata$HasConfPCR=="Yes" & newdata$PCRResult=="Positive")
MonthTotalTests[i]<-sum(SecondaryS$month==m1[i])
MonthTotalPos[i]<-sum(SecondaryS$month==m1[i]& SecondaryS$TestResultDesc %in% c("lft positive","Positive")) 
#MonthTotalTests[i]<-length(unique(subset(SecondaryS,SecondaryS$month==m1[i])$PseudoID))
#MonthTotalPos[i]<-length(unique(subset(SecondaryS,SecondaryS$month==m1[i]& SecondaryS$TestResultDesc %in% c("lft positive","Positive"))$PseudoID)) 
}

EpochsTable<-cbind(
m1,
paste(round(100*MonthTotalPos/MonthTotalTests,digit=2), "%",sep=""),
TotalLFT,
TotalLFTInd,
paste(TotalLFTPos, " (", round(100*TotalLFTPos/TotalLFTInd,digit=2), "%)",sep=""),
paste(TotalConfPCR, " (", round(100*TotalConfPCR/TotalLFTPos,digit=2), "%)",sep=""),
paste(TotalConfPCRPos, " (", round(100*TotalConfPCRPos/TotalConfPCR,digit=2), "%)",sep=""),
paste(TotalConfPCRNeg, " (", round(100*TotalConfPCRNeg/TotalConfPCR,digit=2), "%)",sep=""),
paste(TotalConfPCRVoid, " (", round(100*TotalConfPCRVoid/TotalConfPCR,digit=2), "%)",sep=""),
paste(TotalSelfTest, " (", round(100*TotalSelfTest/TotalLFTPos,digits=2), "%)",sep=""),
paste(TotalSelfTestConfirmed, " (", round(100*TotalSelfTestConfirmed/TotalSelfTest,digits=2), "%)",sep=""),
paste(TotalSelfTestCorrect, " (", round(100*TotalSelfTestCorrect/TotalSelfTestConfirmed,digits=2), "%)",sep=""),
paste(TotalCentreTest, " (", round(100*TotalCentreTest/TotalLFTPos,digits=2), "%)",sep=""),
paste(TotalCentreTestConfirmed, " (", round(100*TotalCentreTestConfirmed/TotalCentreTest,digits=2), "%)",sep=""),
paste(TotalCentreTestCorrect, " (", round(100*TotalCentreTestCorrect/TotalCentreTestConfirmed,digits=2), "%)",sep=""))

colnames(EpochsTable)<-c("Month","Positive Test Proportion","Total Registered LFTs","Total Individuals with LFT","Total LFT positive","Total Confirmatory PCR",
"Confirmatory PCR Positive","Confirmatory PCR Negative","Confirmatory PCR Void","Total LFT positive home tests","Confirmed LFT positive home tests","positive home test with positive PCR",
"Total LFT positives at test centres","Confirmed LFT positives from test centres","LFT positives from test centre with positive PCR")

write.csv(EpochsTable,file="SecondaryEpochs.csv")

##Primary SChools
TotalLFT<-TotalLFTInd<-TotalLFTPos<-TotalConfPCR<-TotalConfPCRPos<-TotalConfPCRNeg<-TotalConfPCRVoid<-TotalSelfTest<-TotalSelfTestConfirmed<-
TotalSelfTestCorrect<-MonthTotalTests<-MonthTotalPos<-TotalCentreTest<-TotalCentreTestConfirmed<-
TotalCentreTestCorrect<-vector(length=length(m1))

for(i in 1:length(m1)){
TotalLFT[i]<-sum(sub1$month==m1[i])
TotalLFTInd[i]<-length(unique(subset(sub1,sub1$month==m1[i])$PseudoID))
TotalLFTPos[i]<-sum(newdataP$month==m1[i])
TotalConfPCR[i]<-sum(newdataP$month==m1[i] & newdataP$HasConfPCR=="Yes")
TotalConfPCRPos[i]<-sum(newdataP$month==m1[i] & newdataP$HasConfPCR=="Yes" & newdataP$PCRResult=="Positive")
TotalConfPCRNeg[i]<-sum(newdataP$month==m1[i] & newdataP$HasConfPCR=="Yes" & newdataP$PCRResult=="Negative")
TotalConfPCRVoid[i]<-sum(newdataP$month==m1[i] & newdataP$HasConfPCR=="Yes" & newdataP$PCRResult=="Void")
TotalSelfTest[i]<-sum(newdataP$month==m1[i]&newdataP$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))
TotalSelfTestConfirmed[i]<-sum(newdataP$month==m1[i]&newdataP$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")& newdataP$HasConfPCR=="Yes")
TotalSelfTestCorrect[i]<-sum(newdataP$month==m1[i]&newdataP$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")& newdataP$HasConfPCR=="Yes" & newdataP$PCRResult=="Positive")
TotalCentreTest[i]<-sum(newdataP$month==m1[i]&!(newdataP$LFTProcessingLab %in% c("LFDSelfTest","SelfServe")))
TotalCentreTestConfirmed[i]<-sum(newdataP$month==m1[i]&!(newdataP$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))& newdataP$HasConfPCR=="Yes")
TotalCentreTestCorrect[i]<-sum(newdataP$month==m1[i]&!(newdataP$LFTProcessingLab %in% c("LFDSelfTest","SelfServe"))& newdataP$HasConfPCR=="Yes" & newdataP$PCRResult=="Positive")
MonthTotalTests[i]<-sum(PrimaryS$month==m1[i])
MonthTotalPos[i]<-sum(PrimaryS$month==m1[i]& PrimaryS$TestResultDesc %in% c("lft positive","Positive")) 
}

EpochsTable<-cbind(
m1,
paste(round(100*MonthTotalPos/MonthTotalTests,digit=2), "%",sep=""),
TotalLFT,
TotalLFTInd,
paste(TotalLFTPos, " (", round(100*TotalLFTPos/TotalLFTInd,digit=2), "%)",sep=""),
paste(TotalConfPCR, " (", round(100*TotalConfPCR/TotalLFTPos,digit=2), "%)",sep=""),
paste(TotalConfPCRPos, " (", round(100*TotalConfPCRPos/TotalConfPCR,digit=2), "%)",sep=""),
paste(TotalConfPCRNeg, " (", round(100*TotalConfPCRNeg/TotalConfPCR,digit=2), "%)",sep=""),
paste(TotalConfPCRVoid, " (", round(100*TotalConfPCRVoid/TotalConfPCR,digit=2), "%)",sep=""),
paste(TotalSelfTest, " (", round(100*TotalSelfTest/TotalLFTPos,digits=2), "%)",sep=""),
paste(TotalSelfTestConfirmed, " (", round(100*TotalSelfTestConfirmed/TotalSelfTest,digits=2), "%)",sep=""),
paste(TotalSelfTestCorrect, " (", round(100*TotalSelfTestCorrect/TotalSelfTestConfirmed,digits=2), "%)",sep=""),
paste(TotalCentreTest, " (", round(100*TotalCentreTest/TotalLFTPos,digits=2), "%)",sep=""),
paste(TotalCentreTestConfirmed, " (", round(100*TotalCentreTestConfirmed/TotalCentreTest,digits=2), "%)",sep=""),
paste(TotalCentreTestCorrect, " (", round(100*TotalCentreTestCorrect/TotalCentreTestConfirmed,digits=2), "%)",sep=""))

colnames(EpochsTable)<-c("Month","Positive Test Proportion","Total Registered LFTs","Total Individuals with LFT","Total LFT positive","Total Confirmatory PCR",
"Confirmatory PCR Positive","Confirmatory PCR Negative","Confirmatory PCR Void","Total LFT positive home tests","Confirmed LFT positive home tests","positive home test with positive PCR",
"Total LFT positives at test centres","Confirmed LFT positives from test centres","LFT positives from test centre with positive PCR")

write.csv(t(EpochsTable),file="PrimaryEpochs.csv")

b<-t1 %>%
  group_by(month) %>%
  summarise(count = n_distinct(PseudoID))

##Number of tests

##check linearity of TotalLFT Prev and agreement.
TotalTests<-TotalPositive<-vector(length=10)
POSci<-NULL
TotalTestsP<-TotalPositiveP<-vector(length=10)
POSciP<-NULL

for(i in 1:10){
TotalTests[i]<-sum(newdata2$TotalLFTPrev==(i-1))
TotalPositive[i]<-sum(newdata2$TotalLFTPrev==(i-1)&newdata2$PCRResult=="Positive")
POSci<-rbind(POSci,as.numeric(exactci(TotalPositive[i],TotalTests[i],conf.level=0.95)$conf.int))
TotalTestsP[i]<-sum(newdata2P$TotalLFTPrev==(i-1))
TotalPositiveP[i]<-sum(newdata2P$TotalLFTPrev==(i-1)&newdata2P$PCRResult=="Positive")
POSciP<-rbind(POSciP,as.numeric(exactci(TotalPositiveP[i],TotalTestsP[i],conf.level=0.95)$conf.int))
}

plotCI(0:9,TotalPositive/TotalTests,ui=POSci[,2],li=POSci[,1], xlab="Number of LFTs in previous 14 days",ylab="Proportion with positive confirmatory PCR",
main="",ylim=c(0,1),pch=16,col="steelblue",gap=0)
plotCI(0.1+0:9,TotalPositiveP/TotalTestsP,ui=POSciP[,2],li=POSciP[,1],
ylim=c(0,1),pch=16,col="chocolate2",gap=0,add=T)

