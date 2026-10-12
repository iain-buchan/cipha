##Key worker release...

data_police<-read.csv("MerseyPolice.csv",header=TRUE,na.strings = c("NULL","NaN",""))
data_police<-data_police[2:1359,1:32]

##Count number of LFT's per individual.
levels(data_police$X)<-c("No","No","No","No","Yes","Yes","Yes")
levels(data_police$X.2)<-c("No","No","No","No","Yes","Yes","Yes")
levels(data_police$X.4)<-c("No","No","No","No","Yes","Yes","Yes")
levels(data_police$X.6)<-c("No","No","No","No","Yes","Yes","Yes")
levels(data_police$X.8)<-c("No","No","No","Yes","Yes","Yes")
levels(data_police$X.10)<-c("No","No","No","Yes","Yes","Yes","Yes")
levels(data_police$X.12)<-c("No","No","Yes","Yes","Yes","Yes","Yes")
summary(data_police$X)
summary(data_police$X.2)
summary(data_police$X.4)
summary(data_police$X.6)
summary(data_police$X.8)
summary(data_police$X.10)
summary(data_police$X.12)

##Sort PCR Result
levels(data_police$PCR.Result)<-c(NA,"inconclusive","Negative","Negative",NA,"Positive","Positive")

##Sort LFT Results
levels(data_police$X.1)<-c(NA,"Negative","Negative","Negative","Negative","Positive","Positive",NA)
levels(data_police$X.3)<-c(NA,"Negative","Negative","Negative","Positive",NA)
levels(data_police$X.5)<-c(NA,"Negative","Negative","Negative","Negative","No","No","Positive",NA)
levels(data_police$X.7)<-c(NA,"Negative","Negative","Negative","Negative","Positive",NA)
levels(data_police$X.9)<-c(NA,"Negative","Negative","Negative","Negative","Negative","Positive","Positive",NA)
levels(data_police$X.11)<-c(NA,"Negative","Negative","Negative","Positive","Positive",NA)
levels(data_police$X.13)<-c(NA,"Negative","Negative","Negative","No","Positive",NA)

summary(data_police$X.1)
summary(data_police$X.3)
summary(data_police$X.5)
summary(data_police$X.7)
summary(data_police$X.9)
summary(data_police$X.11)
summary(data_police$X.13)

##Count number of LFTs per individual
data_police$TotalLFT<-rowSums(data_police[,c("X","X.2","X.4","X.6","X.8","X.10","X.12")]=="Yes",na.rm=TRUE)

##Assess prediciton accurary
data_police$LFTSummary<-"Negative"
data_police$LFTSummary[data_police$X.1=="Positive"|data_police$X.3=="Positive"|data_police$X.5=="Positive"|data_police$X.7=="Positive"|
data_police$X.9=="Positive"|data_police$X.11=="Positive"|data_police$X.13=="Positive"]<-"Positive"
data_police$LFTSummary<-as.factor(data_police$LFTSummary)

PoliceRes<-table(data_police$LFTSummary,data_police$PCR.Result,useNA="ifany")
write.csv(PoliceRes,file="PoliceResultsAccuracyV2.csv")

##Missing PCRs:
##Withdrawn due to missing LFT mid pilot: 3
##Scheme withdrawn: 8
##HOusehold Member tests positive: 3
##Missing test: 36
##Developed Symptoms: 1+1+1+1=4
##Decided to withdraw and self isolate during the pilot: 3

data_police$Day.Zero..Date.of.Exposure.<-as.Date(as.character(data_police$Day.Zero..Date.of.Exposure.),format="%d/%m/%Y")
data_police$Date.Identified.as.a.Contact<-as.Date(as.character(data_police$Date.Identified.as.a.Contact),format="%d/%m/%Y")
data_police$ExposureGap<-as.numeric(data_police$Date.Identified.as.a.Contact-data_police$Day.Zero..Date.of.Exposure)

library(dplyr)
tmp1<-count(data_police,ExposureGap)
x<-barplot(tmp1$n,col="steelblue",xlab="Days between exposure and identification as a contact of a case",ylab="Number of Individuals",ylim=c(0,200),names=tmp1$ExposureGap)

data_police$ExpectedVisits<-8-data_police$ExposureGap
data_police$ExpminusObs<-data_police$ExpectedVisits - data_police$TotalLFT

#sub2<-subset(sub1,!is.na(sub1$PCR.Result))
#sub3<-subset(data_police,data_police$PCR.Result=="Negative")
#sum(data_police$ExpectedVisits)
#sum(data_police$TotalLFT)
#tmp2<-count(data_police,ExpminusObs)
#x<-barplot(tmp2$n,col="steelblue",xlab="Missed LFTs",ylab="Number of Individuals",ylim=c(0,200))

table(data_police$ExposureGap,data_police$TotalLFT)
##ID 31195 appears to have a missing day 5 LFT only. IS this correct?
##ID 32816 and 47032 appear to have missing day 1 LFT but all others present, though no PCR. Is date identified correct? Why would they have missing first day LFT 
##and then all others?
##IDs 44695 and 47880 appear to have no LFT information at any point. NO PCR either?
##ID 48508 has one LFT on day 4 but no other results.
##IDs 23947 and 35709 have LFTs on day before identified as contact. Is the date identified incorrect?
##ID 31014 has LFT 2 days before date identified as a contact. Is this date incorrect?
##ID 46139 has no tests recorded.

subset(data_police,data_police$ExposureGap==0&data_police$TotalLFT==2)
summary(data_police$Age)
summary(data_police$Gender)
summary(data_police$Ethnicity)

#########################################################################################################
#########################################################################################################
##Merseyfire
data_fire<-read.csv("data/2021-09-01/MerseyFire.csv",header=TRUE,na.strings = c("NULL","NaN",""))
#data_fire<-read.csv("MerseyFire.csv",header=TRUE,na.strings = c("NULL","NaN",""))
data_fire<-data_fire[2:92,]

##Fire 63 tested positive before the scheme so has no results - remove from analysis
data_fire<-data_fire[-63,]

##Fire 18, 6 and 37 did not get PCR results back
data_fire$PCR.Result[6]<-NA
data_fire$PCR.Result[37]<-NA


library(lubridate)

fmts <- c("%d/%m/%Y", "%d.%m.%Y", "%d.%m.%y")
data_fire$Day.Zero..Date.of.Exposure.<- parse_date_time(data_fire$Day.Zero..Date.of.Exposure., fmts)
data_fire$Date.Identified.as.a.Contact<- parse_date_time(data_fire$Date.Identified.as.a.Contact, fmts)
data_fire$ExposureGap<-as.numeric(data_fire$Date.Identified.as.a.Contact-data_fire$Day.Zero..Date.of.Exposure.)/(60*60*24)
##Manually changed dat typos for rows 3, 7, 76 = years had been mis entered.

#data_fire$Date.Identified.as.a.Contact[76]-data_fire$Day.Zero..Date.of.Exposure.[76]
##Count number of LFT's per individual.
levels(data_fire$X.2)<-c("No","No","Yes","Yes")
levels(data_fire$X.4)<-c("No","No","Yes","Yes","Yes")
levels(data_fire$X.6)<-c("No","No","No","No","Yes","Yes")
levels(data_fire$X.8)<-c("No","No","No","No","Yes","Yes","Yes")
levels(data_fire$X.10)<-c("No","No","No","No","Yes")
levels(data_fire$X.12)<-c("No","No","No","No","Yes","Yes")
levels(data_fire$X.14)<-c("No","No","Yes","Yes","No","No","No","Yes")

summary(data_fire$X.2)
summary(data_fire$X.4)
summary(data_fire$X.6)
summary(data_fire$X.8)
summary(data_fire$X.10)
summary(data_fire$X.12)
summary(data_fire$X.14)

##Sort LFT Results
levels(data_fire$X.3)<-c("Negative","Negative",NA,"x")
levels(data_fire$X.5)<-c("Negative","Negative","Negative",NA,NA,"x")
levels(data_fire$X.7)<-c("Negative","Negative",NA,NA,NA,NA,"x")
levels(data_fire$X.9)<-c("Negative","Negative","Negative",NA,NA,NA,NA,"x")
levels(data_fire$X.11)<-c("Negative",NA,NA,NA,NA,"x")
levels(data_fire$X.13)<-c("Negative",NA,NA,NA,NA,"x")
levels(data_fire$X.15)<-c("Negative","Negative",NA,NA,NA,NA,"x","x")

##Count number of LFTs per individual
data_fire$TotalLFT<-rowSums(data_fire[,c("X.2","X.4","X.6","X.8","X.10","X.12","X.14")]=="Yes",na.rm=TRUE)

##Assess prediciton accurary
data_fire$LFTSummary<-"Negative"
data_fire$LFTSummary[data_fire$X.3=="Positive"|data_fire$X.5=="Positive"|data_fire$X.7=="Positive"|data_fire$X.9=="Positive"|
data_fire$X.11=="Positive"|data_fire$X.13=="Positive"|data_fire$X.15=="Positive"]<-"Positive"
data_fire$LFTSummary<-as.factor(data_fire$LFTSummary)

##Sort PCR Result
levels(data_fire$PCR.Result)<-c(NA,"Negative","Positive",NA)
FireRes<-table(data_fire$LFTSummary,data_fire$PCR.Result,useNA="ifany")
write.csv(FireRes,file="FireResultsAccuracy.csv")

##6 missing PCR, 3 withdrew from the scheme mid pilot, 3 did not receive test results.

data_fire$ExpectedVisits<-8-data_fire$ExposureGap
data_fire$ExpminusObs<-data_fire$ExpectedVisits - data_fire$TotalLFT

table(data_fire$ExposureGap,data_fire$TotalLFT)
##Individual 45 has a missing date of identification but starts LFT on day 1 and has 6 LFTs so we shall count as identified on day 1.

subset(data_fire,data_fire$ExposureGap==0&data_fire$TotalLFT==0)

summary(data_fire$Age)
summary(data_fire$X.1)

#########################################################################################################
#########################################################################################################
##Alder Hey
data_AH<-read.csv("data/2021-09-01/AlderHey.csv",header=TRUE,na.strings = c("NULL","NaN",""))
data_AH<-read.csv("AlderHey.csv",header=TRUE,na.strings = c("NULL","NaN",""))
data_AH<-data_AH[2:185,1:43]

##Id 150 was swabbed negative so shouln't be in scheme.
data_AH<-data_AH[-150,]

data_AH$Day.Zero..Date.of.Exposure.<-as.Date(as.character(data_AH$Day.Zero..Date.of.Exposure.),format="%d/%m/%Y")
data_AH$Date.Identified.as.a.Contact<-as.Date(as.character(data_AH$Date.Identified.as.a.Contact),format="%d/%m/%Y")
data_AH$ExposureGap<-as.numeric(data_AH$Date.Identified.as.a.Contact-data_AH$Day.Zero..Date.of.Exposure.)

##May need to exclude id 163 as they were identified more than 7 days after exposure.

##Count number of LFT's per individual.
levels(data_AH$X.1)<-c("No","No","PCR","Yes","Yes")
levels(data_AH$X.3)<-c("No","No","No","PCR","Yes","Yes")
levels(data_AH$X.5)<-c("No","No","No","No","PCR","Yes","Yes")
levels(data_AH$X.7)<-c("No","No","No","PCR","Yes","Yes")
levels(data_AH$X.9)<-c("No","No","No","PCR","Yes","Yes")
levels(data_AH$X.11)<-c("No","No","No","PCR","Yes","Yes")
levels(data_AH$X.13)<-c("No","No","No","No","PCR","Yes","Yes")

summary(data_AH$X.1)
summary(data_AH$X.3)
summary(data_AH$X.5)
summary(data_AH$X.7)
summary(data_AH$X.9)
summary(data_AH$X.11)
summary(data_AH$X.13)

##Sort LFT Results
levels(data_AH$X.2)<-c(NA,"Negative","Negative","Positive",NA)
levels(data_AH$X.4)<-c(NA,"Negative","Negative",NA)
levels(data_AH$X.6)<-c(NA,"Negative","Negative","Positive",NA)
levels(data_AH$X.8)<-c(NA,"Negative","Negative",NA,NA)
levels(data_AH$X.10)<-c(NA,"Negative","Negative",NA)
levels(data_AH$X.12)<-c(NA,NA,"Negative","Negative",NA,"Symptomatic")
levels(data_AH$X.14)<-c(NA,"Negative","Negative",NA,NA,"Symptomatic")

##Sort PCR REsults
levels(data_AH$PCR.Result)<-c(NA,NA,NA,"Negative","Negative",NA,"Positive","Positive",NA)

##Count number of LFTs per individual
data_AH$TotalLFT<-rowSums(data_AH[,c("X.1","X.3","X.5","X.7","X.9","X.11","X.13")]=="Yes",na.rm=TRUE)

##Assess prediciton accurary
data_AH$LFTSummary<-"Negative"
data_AH$LFTSummary[data_AH$X.2=="Positive"|data_AH$X.4=="Positive"|data_AH$X.6=="Positive"|data_AH$X.8=="Positive"|
data_AH$X.10=="Positive"|data_AH$X.12=="Positive"|data_AH$X.14=="Positive"]<-"Positive"
data_AH$LFTSummary<-as.factor(data_AH$LFTSummary)

AHRes<-table(data_AH$LFTSummary,data_AH$PCR.Result,useNA="ifany")
write.csv(AHRes,file="AHResultsAccuracy.csv")

data_AH$ExpectedVisits<-8-data_AH$ExposureGap
data_AH$ExpminusObs<-data_AH$ExpectedVisits - data_AH$TotalLFT

table(data_AH$ExposureGap,data_AH$TotalLFT)

subset(data_AH,data_AH$ExposureGap==0&data_AH$TotalLFT==4)

summary(as.numeric(as.character(data_AH$Age)))
summary(data_AH$Ethnicity)

levels(data_AH$Ethnicity)<-c(NA,"African",NA,NA,"White","White","White","White","Asian","Asian","Asian","White",NA,"Mixed","White","White","Mixed","White","White")                 


#########################################################################################################
#########################################################################################################
##Wings

data_Wings<-read.csv("WingsV3.csv",header=TRUE,na.strings = c("NULL","NaN",""))
data_Wings<-data_Wings[2:10,]

##Count number of LFT's per individual.
levels(data_Wings$X.1)<-c("No","No","Yes","Yes")
levels(data_Wings$X.3)<-c("No","No","Yes")
levels(data_Wings$X.5)<-c("No",,"Yes")
levels(data_Wings$X.7)<-c("No","Yes","Yes")
levels(data_Wings$X.9)<-c("No","Yes")
levels(data_Wings$X.11)<-c("No","Yes")
levels(data_Wings$X.13)<-c("No","Yes")

summary(data_Wings$X.1)
summary(data_Wings$X.3)
summary(data_Wings$X.5)
summary(data_Wings$X.7)
summary(data_Wings$X.9)
summary(data_Wings$X.11)
summary(data_Wings$X.13)

##Sort LFT Results
levels(data_Wings$X.2)<-c("Negative","Negative",NA)
levels(data_Wings$X.4)<-c("Negative","Negative",NA)
levels(data_Wings$X.6)<-c("Negative","Negative","Negative",NA)
levels(data_Wings$X.8)<-c("Negative","Negative","Negative",NA)
levels(data_Wings$X.10)<-c("Negative","Negative",NA)
levels(data_Wings$X.12)<-c("Negative","Negative",NA)
levels(data_Wings$X.14)<-c("Negative","Negative",NA)


##Sort PCR REsults
levels(data_Wings$PCR.Result)<-c("Negative","Negative")

##Count number of LFTs per individual
data_Wings$TotalLFT<-rowSums(data_Wings[,c("X.1","X.3","X.5","X.7","X.9","X.11","X.13")]=="Yes",na.rm=TRUE)

##Assess prediciton accurary
data_Wings$LFTSummary<-"Negative"
data_Wings$LFTSummary[data_Wings$X.2=="Positive"|data_Wings$X.4=="Positive"|data_Wings$X.6=="Positive"|data_Wings$X.8=="Positive"|
data_Wings$X.10=="Positive"|data_Wings$X.12=="Positive"|data_Wings$X.14=="Positive"]<-"Positive"
data_Wings$LFTSummary<-as.factor(data_Wings$LFTSummary)

WingsRes<-table(data_Wings$LFTSummary,data_Wings$PCR.Result,useNA="ifany")
write.csv(FireRes,file="FireResultsAccuracy.csv")


par(mfrow=c(1,3))
tmp1<-count(data_police,ExposureGap)
x<-barplot(tmp1$n,col="steelblue",xlab="Days between exposure and identification",ylab="Number of Individuals",
ylim=c(0,400),names=tmp1$ExposureGap,main="Mersey Police",cex.main=2,cex.axis=1.5,las=1,cex.lab=1.5,cex.names=1.5)

tmp1<-count(data_fire[!is.na(data_fire$ExposureGap),],ExposureGap)
x<-barplot(tmp1$n,col="steelblue",xlab="Days between exposure and identification",ylab="Number of Individuals",
ylim=c(0,30),names=tmp1$ExposureGap,main="Mersey Fire",cex.main=2,cex.axis=1.5,las=1,cex.lab=1.5,cex.names=1.5)

tmp1<-count(data_AH,ExposureGap)
x<-barplot(tmp1$n,col="steelblue",xlab="Days between exposure and identification",ylab="Number of Individuals",
ylim=c(0,40),names=tmp1$ExposureGap,main="Alder Hey Children's Hospital",cex.main=2,cex.axis=1.5,las=1,cex.lab=1.5,cex.names=1.5)

##Plot of timings
s1<-subset(data_police,data_police$PCR.Result=="Positive")
#f1<-function(x){max(x,na.rm=TRUE)}
#s1$Date.evaluatory.PCR.Test..should.be.day..7.[18:31]<-apply(s1[18:31,c("Day..1.post.exposure","Day..2.post.exposure","Day..3.post.exposure","Day..4.post.exposure",
#"Day..5.post.exposure","Day..6.post.exposure","Day..7.post.exposure")],1,f1)
s2<-subset(data_fire,data_fire$PCR.Result=="Positive")
s3<-subset(data_AH,data_AH$PCR.Result=="Positive")

s1<-s1[,c("ExposureGap","X.1","X.3","X.5","X.7","X.9","X.11","X.13","Date.evaluatory.PCR.Test..should.be.day..7.","Day.Zero..Date.of.Exposure.")]
s2<-s2[,c("ExposureGap","X.3","X.5","X.7","X.9","X.11","X.13","X.15","Date.evaluatory.PCR.Test..should.be.day.6.or.day..7.","Day.Zero..Date.of.Exposure.")]
s3<-s3[,c("ExposureGap","X.2","X.4","X.6","X.8","X.10","X.12","X.14","Date.evaluatory.PCR.Test..should.be.day.6.or.day..7.","Day.Zero..Date.of.Exposure.")]

colnames(s2)<-names(s1)
colnames(s3)<-names(s1)
s1<-rbind(s1,s2,s3)
s1$Date.evaluatory.PCR.Test..should.be.day..7.<-as.Date(as.character(s1$Date.evaluatory.PCR.Test..should.be.day..7.),format="%d/%m/%Y")
s1$Date.evaluatory.PCR.Test..should.be.day..7.[1]<-"2020-12-20"

s1$PCR.day<-as.numeric(s1$Date.evaluatory.PCR.Test..should.be.day..7.-s1$Day.Zero..Date.of.Exposure.)
s1$PCR.day[32:34]<-c(7,7,7)

s1$PCR.day[18:31]<-c(6,1,2,6,5,4,1,7,6,3,7,7,6,3)
#tiff("SMARTReleasePositives.tiff",units="in",width=8.268,height=11.693,res=300,compression="lzw")
library(svglite)
svglite("HaltonFigure16.svg",width=8.268,height=11.693)
#library(devEMF)
#emf(file = "HaltonFigure16.emf", ,width=8.268,height=11.693,emfPlus = TRUE)
plot((s1$ExposureGap-0.15),1:nrow(s1),pch=18,col="orange",xlab="Day Post Exposure",xlim=c(0,7.5),las=1,ylab="Individual",cex=1.5,yaxt="n",xaxt="n")
axis(side = 2,1:nrow(s1),las=1)
axis(side = 1,0:7)
points(rep(1,sum(s1$X.1=="Negative",na.rm=TRUE)),which(s1$X.1=="Negative"),pch=16,col="green",cex=1.5)
points(rep(2,sum(s1$X.3=="Negative",na.rm=TRUE)),which(s1$X.3=="Negative"),pch=16,col="green",cex=1.5)
points(rep(3,sum(s1$X.5=="Negative",na.rm=TRUE)),which(s1$X.5=="Negative"),pch=16,col="green",cex=1.5)
points(rep(4,sum(s1$X.7=="Negative",na.rm=TRUE)),which(s1$X.7=="Negative"),pch=16,col="green",cex=1.5)
points(rep(5,sum(s1$X.9=="Negative",na.rm=TRUE)),which(s1$X.9=="Negative"),pch=16,col="green",cex=1.5)
points(rep(6,sum(s1$X.11=="Negative",na.rm=TRUE)),which(s1$X.11=="Negative"),pch=16,col="green",cex=1.5)
points(rep(7,sum(s1$X.13=="Negative",na.rm=TRUE)),which(s1$X.13=="Negative"),pch=16,col="green",cex=1.5)

points(rep(1.15,sum(s1$X.1=="Positive",na.rm=TRUE)),which(s1$X.1=="Positive"),pch=16,col="red",cex=1.5)
points(rep(2.15,sum(s1$X.3=="Positive",na.rm=TRUE)),which(s1$X.3=="Positive"),pch=16,col="red",cex=1.5)
points(rep(3.15,sum(s1$X.5=="Positive",na.rm=TRUE)),which(s1$X.5=="Positive"),pch=16,col="red",cex=1.5)
points(rep(4.15,sum(s1$X.7=="Positive",na.rm=TRUE)),which(s1$X.7=="Positive"),pch=16,col="red",cex=1.5)
points(rep(5.15,sum(s1$X.9=="Positive",na.rm=TRUE)),which(s1$X.9=="Positive"),pch=16,col="red",cex=1.5)
points(rep(6.15,sum(s1$X.11=="Positive",na.rm=TRUE)),which(s1$X.11=="Positive"),pch=16,col="red",cex=1.5)
points(rep(7.15,sum(s1$X.13=="Positive",na.rm=TRUE)),which(s1$X.13=="Positive"),pch=16,col="red",cex=1.5)

points(s1$PCR.day+0.3,1:nrow(s1),pch=15,col="red",cex=1.5)
for(i in 1:(1+nrow(s1))){abline(h=-.5+i,lty=2,col="gray90")}
abline(h=31.5,col="gray19",lty=2)
abline(h=32.5,col="gray19",lty=2)
abline(h=34.5,col="gray19",lty=2)
#text(0.5,32,"Mersey Fire",cex=0.9)
#text(0.5,33,"Alder Hey",cex=0.9)
#text(0.5,12,"Mersey Police",cex=0.9)
legend("top", inset = c(0,-0.05),legend=c("Date Identified as contact","Negative LFT","Positive LFT","Positive PCR"),col=c("orange","green","red","red"),
pch=c(18,16,16,15),xpd=TRUE,ncol=4)
dev.off()


Gap<-c(data_AH$ExposureGap,data_fire$ExposureGap,data_police$ExposureGap,
c(2,1,2,3,1,1,1,1,0,1,4,0),
c(4,2,0,4,0,0,2),
c(3,0),
c(1,1,1,1),
c(1,2)
)

LFTPos<-c(rep(1,sum(s1$X.1=="Positive",na.rm=TRUE)),
rep(2,sum(s1$X.3=="Positive",na.rm=TRUE)),
rep(3,sum(s1$X.5=="Positive",na.rm=TRUE)),
rep(4,sum(s1$X.7=="Positive",na.rm=TRUE)),
rep(5,sum(s1$X.9=="Positive",na.rm=TRUE)),
rep(6,sum(s1$X.11=="Positive",na.rm=TRUE)),
rep(7,sum(s1$X.13=="Positive",na.rm=TRUE)))


s1$DayLFTPos<-NA
s1$DayLFTPos[s1$X.1=="Positive"]<-1
s1$DayLFTPos[s1$X.3=="Positive"]<-2
s1$DayLFTPos[s1$X.5=="Positive"]<-3
s1$DayLFTPos[s1$X.7=="Positive"]<-4
s1$DayLFTPos[s1$X.9=="Positive"]<-5
s1$DayLFTPos[s1$X.11=="Positive"]<-6
s1$DayLFTPos[s1$X.13=="Positive"]<-7

summary(s1$DayLFTPos)

s2<-subset(s1,!is.na(s1$DayLFTPos))
s2$IdentTime<-s2$DayLFTPos-s2$ExposureGap

