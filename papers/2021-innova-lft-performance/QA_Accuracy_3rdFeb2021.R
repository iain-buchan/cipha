##MAST summaries 4th December 2020.
library(dplyr)
library(PropCIs)
library(gplots)

###
#LFD08976709 � This should change from Void to Positive
#LFD08977980 � This should change in the reappraised column from Void to Positive
#check1<-subset(data6,data6$LFD_Barcode %in% c("LFD08976709","LFD08977980"))
###

##QA data
data2<-read.delim("Liverpool_MAST_LFD_QA.txt")
data2<-data2[1:(nrow(data2)-2),]

##Amend two wrong LFD barcode reappraisal results
data2$Study_team_re_appraisal_of_result[data2$LFD_Barcode=="LFD08976709"]<-"Positive"
data2$Study_team_re_appraisal_of_result[data2$LFD_Barcode=="LFD08977980"]<-"Positive"

dim(data2)
## [pasted console output or note, commented out for parsing] length(unique(data2$�..SS))
length(unique(data2$LFD_Barcode))
length(unique(data2$PCR_Barcode))
sum(data2$N_gene==0)
sum(data2$N_gene>0 & data2$N_gene<=40)
sum(data2$S_gene==0)
sum(data2$S_gene>0 & data2$S_gene<=40)
sum(data2$ORF1ab==0)
sum(data2$ORF1ab>0 & data2$ORF1ab<=40)

levels(data2$LFDResultSite)<-c("Negative","Negative","Positive","Void")
levels(data2$PCR_Result)<-c("Negative","Negative","Positive","Void")
levels(data2$LFDResultQA)<-c("Negative","Negative","Negative","Positive","Void","Positive")

##Load MAST data for matching
##data1<-read.delim("data/13-01-21/COVIDPILLAR2_VCam_MastP2.txt",header = TRUE,quote="")
##Last two rows seem to have NAs for basically everything - safe to remove I think
##data1<-data1[1:(nrow(data1)-2),]
##data1$SpecimenProcessedDate<-as.Date(as.character(data1$SpecimenProcessedDate))
load("data/14-12-20/CorrectedMAST13Jan2021.RData")

data1$AppointmentDate<-as.Date(as.character(data1$AppointmentDate))
##order observations by appointment date.
data1<-data1[with(data1,order(PseudoID,AppointmentDate)),]
sum(data1$SpecimenID %in% data2$LFD_Barcode)
sub1<-subset(data1,data1$SpecimenID %in% data2$LFD_Barcode)
sub2<-subset(data2,!(data2$LFD_Barcode %in% data1$SpecimenID))

data2$PseudoID<-NULL
for(i in 1:nrow(data2)){
ifelse(data2$LFD_Barcode[i] %in% data1$SpecimenID,data2$PseudoID[i]<-data1$PseudoID[data1$SpecimenID %in% data2$LFD_Barcode[i]][1],data2$PseudoID[i]<-i)
}
data2<-data2[with(data2,order(PseudoID)),]

IDS1<-unique(data2$PseudoID)
data3<-NULL
Count1<-Count2<-Count3<-0

for(i in 1:length(IDS1)){
t1<-subset(data2,data2$PseudoID %in% IDS1[i])
if(nrow(t1)==1){data3<-rbind(data3,t1)}
if(nrow(t1)==2){
if(t1$LFDResultSite[1]==t1$LFDResultSite[2] & t1$PCR_Result[1]==t1$PCR_Result[2]){data3<-rbind(data3,t1[1,]);Count1<-Count1+1}
if(t1$LFDResultSite[1]=="Void"|t1$LFDResultSite[2]=="Void"){data3<-rbind(data3,t1[t1$LFDResultSite!="Void",]);Count2<-Count2+1}
if(t1$PCR_Result[1]=="Void"|t1$PCR_Result[2]=="Void"){data3<-rbind(data3,t1[t1$PCR_Result!="Void",]);Count3<-Count3+1}
}
if(nrow(t1)==3){data3<-rbind(data3,t1[1,])}  ##Only happens once that there are 3 rows. All negative LFTs, 2 negative PCR and 1 VOID
}

data_Original<-data2
data2<-data3

QAcheck<-table(data2$LFDResultQA,data2$LFDResultSite)
QAcheck
#write.csv(QAcheck,file="QAcheck25Nov2020.csv")

QAres<-table(data2$LFDResultSite,data2$PCR_Result)
QAres
#write.csv(QAres,file="QAres_24Nov2020.csv")

##Sensitivity
QAres[2,2]/(QAres[1,2]+QAres[2,2])
exactci(QAres[2,2],(QAres[1,2]+QAres[2,2]),conf.level=0.95)

##Specificity
QAres[1,1]/(QAres[1,1]+QAres[2,1])
exactci(QAres[1,1],(QAres[1,1]+QAres[2,1]),conf.level=0.95)

##PPV
QAres[2,2]/(QAres[2,1]+QAres[2,2])
exactci(QAres[2,2],(QAres[2,1]+QAres[2,2]),conf.level=0.95)

##NPV
QAres[1,1]/(QAres[1,1]+QAres[1,2])
exactci(QAres[1,1],(QAres[1,1]+QAres[1,2]),conf.level=0.95)

data6<-data2
##Define 0's as NA calculate mean of 3 columns without NA's. Categorise Viral load
data6$N_gene1<-data6$N_gene
data6$S_gene1<-data6$S_gene
data6$ORF1ab1<-data6$ORF1ab
data6$N_gene[data6$N_gene=="NULL"]<-NA
data6$N_gene[data6$N_gene==0]<-NA
data6$S_gene[data6$S_gene=="NULL"]<-NA
data6$S_gene[data6$S_gene==0]<-NA
data6$ORF1ab[data6$ORF1ab=="NULL"]<-NA
data6$ORF1ab[data6$ORF1ab==0]<-NA
data6$N_gene<-as.numeric(as.character(data6$N_gene))
data6$S_gene<-as.numeric(as.character(data6$S_gene))
data6$ORF1ab<-as.numeric(as.character(data6$ORF1ab))

f1<-function(x){mean(x,na.rm=TRUE)}
data6$CTRes<-apply(data6[,c("N_gene","S_gene","ORF1ab")],1,f1)
data6$CTRes[data6$PCR_Result=="Void"]<-NA
data6$CTRes1<-data6$CTRes
data6$CTRes1[data6$CTRes<18.3]<-"<18.3"
data6$CTRes1[data6$CTRes>=18.3&data6$CTRes<24.4]<-"[18.3, 24.4)"
data6$CTRes1[data6$CTRes>=24.4&data6$CTRes<30.5]<-"[24.4, 30.5)"
data6$CTRes1[data6$CTRes>=30.5&data6$CTRes<35]<-"[30.5, 35)"
data6$CTRes1[data6$CTRes>=35]<-">35"
#data6$CTRes1[data6$PCR_Result=="Void"]<-"Void"

data6$CTRes1<-as.factor(data6$CTRes1)

CTRestab<-table(data6$LFDResultSite,data6$CTRes1)
CTRestab

ch1<-which((data6$S_gene1!=0|data6$N_gene1!=0|data6$ORF1ab1!=0)&data6$PCR_Result=="Void")
length(ch1)
s1<-data6[ch1,]
s1$CTRes<-apply(s1[,c("N_gene","S_gene","ORF1ab")],1,f1)

##
test1<-count(subset(data6,data6$LFDResultSite!="Void"), CTRes1)
test2<-count(subset(data6,data6$LFDResultSite=="Positive"),CTRes1)
test1<-test1[c(4,1:3),]
test2<-test2[c(3,1,2,4),]
test2[4,2]<-0

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims
##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC

#library(devEMF)
#emf(file = "CtAccuracy.emf", width=12,height=6.5,emfPlus = FALSE)
tiff("QApaperFigure1.tiff",units="in",width=12,height=6.5,res=300,compression="lzw")
x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of samples",ylim=c(0,40),
main=expression("Proportion of samples with positive LFT according to RT-qPCR cycle threshold (C"[T]*")"), xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=0.8, x=0, y=-3, expression("Mean C"[T]*" score"), xpd=TRUE, srt=0)
text(cex=1.2, x=x, y=-5, c(expression("" <= 100),"(100-10,000]","(10,000-1,000,000]",">1,000,000"), xpd=TRUE, srt=0)
text(cex=0.8, x=0, y=-5, "Approximate Viral Load", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFT Negative","LFT Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of LFT positives",side=4,line=0.5)
dev.off()

##N gene results
data6$CTResN<-data6$N_gene
data6$CTResN[data6$PCR_Result=="Void"]<-NA
data6$CTRes1N<-data6$CTResN
data6$CTRes1N[data6$CTResN<18.3]<-"<18.3"
data6$CTRes1N[data6$CTResN>=18.3&data6$CTResN<24.4]<-"18.3-24.4"
data6$CTRes1N[data6$CTResN>=24.4&data6$CTResN<30.5]<-"24.4-30.5"
data6$CTRes1N[data6$CTResN>=30.5&data6$CTResN<35]<-"30.5-35"
data6$CTRes1N[data6$CTResN>=35]<-">35"
data6$CTRes1N<-as.factor(data6$CTRes1N)
CTRestabN<-table(data6$LFDResultSite,data6$CTRes1N)
CTRestabN
sum(data6$PCR_Result=="Void"&data6$N_gene1>35)
sum(data6$PCR_Result=="Void"&data6$N_gene1>30)
sum(data6$PCR_Result=="Void"&data6$N_gene1==0)
sum(data6$PCR_Result=="Void"&data6$N_gene1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Void"&data6$N_gene1==0&data6$LFDResultSite=="Positive")
sum(data6$PCR_Result=="Positive"&data6$N_gene1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Positive"&data6$N_gene1==0&data6$LFDResultSite=="Positive")

##
test1<-count(subset(data6,data6$LFDResultSite!="Void"), CTRes1N)
test2<-count(subset(data6,data6$LFDResultSite=="Positive"),CTRes1N)

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims

##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC

emf(file = "NgeneAccuracy.emf", width=12,height=6.5,emfPlus = FALSE)
x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of samples",ylim=c(0,40),
main="Proportion of samples with positive LFT according to N gene result", xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=0.8, x=0, y=-3, "N gene Cycle Threshold", xpd=TRUE, srt=0)
text(cex=1.2, x=x, y=-5, c("<100","100-10,000","10,000-1,000,000",">1,000,000"), xpd=TRUE, srt=0)
text(cex=0.8, x=0, y=-5, "Approximate Viral Load", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFT Negative","LFT Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of LFT positives",side=4,line=0.5)
dev.off()

##S gene results
data6$CTResS<-data6$S_gene
data6$CTResS[data6$PCR_Result=="Void"]<-NA
data6$CTRes1S<-data6$CTResS
data6$CTRes1S[data6$CTResS<18.3]<-"<18.3"
data6$CTRes1S[data6$CTResS>=18.3&data6$CTResS<24.4]<-"18.3-24.4"
data6$CTRes1S[data6$CTResS>=24.4&data6$CTResS<30.5]<-"24.4-30.5"
data6$CTRes1S[data6$CTResS>=30.5&data6$CTResS<35]<-"30.5-35"
data6$CTRes1S[data6$CTResS>=35]<-">35"
data6$CTRes1S<-as.factor(data6$CTRes1S)
CTRestabS<-table(data6$LFDResultSite,data6$CTRes1S)
CTRestabS
data6[which(data6$S_gene1==0 & data6$PCR_Result=="Positive"),c("LFDResultSite","PCR_Result","S_gene1")]
sum(data6$PCR_Result=="Void"&data6$S_gene1>35)
sum(data6$PCR_Result=="Void"&data6$S_gene1>30)
sum(data6$PCR_Result=="Void"&data6$S_gene1==0)
sum(data6$PCR_Result=="Void"&data6$S_gene1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Void"&data6$S_gene1==0&data6$LFDResultSite=="Positive")
sum(data6$PCR_Result=="Positive"&data6$S_gene1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Positive"&data6$S_gene1==0&data6$LFDResultSite=="Positive")

##
test1<-count(subset(data6,data6$LFDResultSite!="Void"), CTRes1S)
test2<-count(subset(data6,data6$LFDResultSite=="Positive"),CTRes1S)
test2[4,2]<-0

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims
##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC

x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of Samples",ylim=c(0,40),
main="Proportion of Samples with positive LFT test according to S gene result", xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=0.8, x=0, y=-3, "S gene Cycle Threshold", xpd=TRUE, srt=0)
text(cex=1.2, x=x, y=-5, c("<100","100-10,000","10,000-1,000,000",">1,000,000"), xpd=TRUE, srt=0)
text(cex=0.8, x=0, y=-5, "Approximate Viral Load", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFT Negative","LFT Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of positive LFT tests",side=4,line=0.5)


##ORF1ab gene results
data6$CTResO<-data6$ORF1ab
data6$CTResO[data6$PCR_Result=="Void"]<-NA
data6$CTRes1O<-data6$CTResO
data6$CTRes1O[data6$CTResO<18.3]<-"<18.3"
data6$CTRes1O[data6$CTResO>=18.3&data6$CTResO<24.4]<-"18.3-24.4"
data6$CTRes1O[data6$CTResO>=24.4&data6$CTResO<30.5]<-"24.4-30.5"
data6$CTRes1O[data6$CTResO>=30.5&data6$CTResO<35]<-"30.5-35"
data6$CTRes1O[data6$CTResO>=35]<-">35"
data6$CTRes1O<-as.factor(data6$CTRes1O)
CTRestabO<-table(data6$LFDResultSite,data6$CTRes1O)
CTRestabO
data6[which(data6$ORF1ab1==0 & data6$PCR_Result=="Positive"),c("LFDResultSite","PCR_Result","ORF1ab1")]
sum(data6$PCR_Result=="Void"&data6$ORF1ab1>35)
sum(data6$PCR_Result=="Void"&data6$ORF1ab1>30)
sum(data6$PCR_Result=="Void"&data6$ORF1ab1==0)
sum(data6$PCR_Result=="Void"&data6$ORF1ab1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Void"&data6$ORF1ab1==0&data6$LFDResultSite=="Positive")
sum(data6$PCR_Result=="Positive"&data6$ORF1ab1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Positive"&data6$ORF1ab1==0&data6$LFDResultSite=="Positive")


##
test1<-count(subset(data6,data6$LFDResultSite!="Void"), CTRes1O)
test1<-test1[-2,]
test2<-count(subset(data6,data6$LFDResultSite=="Positive"),CTRes1O)
test2[4,2]<-0

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims

##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC

x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of Samples",ylim=c(0,40),
main="Proportion of Samples with positive LFT test according to ORF1ab gene result", xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=0.8, x=0.1, y=-3, "ORF1ab gene Cycle Threshold", xpd=TRUE, srt=0)
text(cex=1.2, x=x, y=-5, c("<100","100-10,000","10,000-1,000,000",">1,000,000"), xpd=TRUE, srt=0)
text(cex=0.8, x=0, y=-5, "Approximate Viral Load", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFT Negative","LFT Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of positive LFT tests",side=4,line=0.5)

##Link QA data to the MAST data to extract age (distribution), sex, ethnicity, sampling date (when sample taken), sample result reporting date,  
##success in T&T making contact (yes/No). LAst one not possible i think.

Age<-vector(length=nrow(data2))
Sex<-vector(length=nrow(data2))
Ethnicity<-vector(length=nrow(data2))
SampleDate<-vector(length=nrow(data2))
ResultDate<-vector(length=nrow(data2))
LFTResult<-vector(length=nrow(data2))
TestCentre<-vector(length=nrow(data2))
check1<-vector(length=nrow(data2))

for(i in 1:nrow(data2)){
t1<-subset(data1,data1$SpecimenID %in% data2$LFD_Barcode[i])
check1[i]<-nrow(t1)
if(nrow(t1)==0){Age[i]<-Sex[i]<-Ethnicity[i]<-SampleDate[i]<-ResultDate[i]<-LFTResult[i]<-TestCentre[i]<-"NA"}
if(nrow(t1)>0){
Age[i]<-t1$Age[1]
Sex[i]<-as.character(t1$Gender[1])
Ethnicity[i]<-as.character(t1$Ethnicity[1])
SampleDate[i]<-t1$AppointmentDate[1]
ResultDate[i]<-t1$SpecimenProcessedDate[1]
LFTResult[i]<-as.character(t1$TestResultDesc[1])
TestCentre[i]<-as.character(t1$TestCentreID[1])
}
}

save("Age","data_Original","data2","data6","Ethnicity","LFTResult","ResultDate","SampleDate","Sex","TestCentre",file="ModifiedData14jan2021.RData")

##Two of these barcodes are duplicated in the MAST data, but their patient data is identical on each row, so OK to just take first row value.
##[1] LFD08407364 LFD08577555

##Counts of test by day and by result
data2$TestDate<-as.Date(as.character(data2$TestDate))

tmp1<-count(data2,TestDate)
tmp2<-count(subset(data2,data2$LFDResultSite!="Void"),TestDate)
colnames(tmp2)<-c("TestDate","VoidLFT")
tmp3<-count(subset(data2,data2$LFDResultSite=="Positive"),TestDate)
colnames(tmp3)<-c("TestDate","PositiveLFT")
tmp4<-count(subset(data2,data2$PCR_Result!="Void"),TestDate)
colnames(tmp4)<-c("TestDate","VoidPCR")
tmp5<-count(subset(data2,data2$PCR_Result=="Positive"),TestDate)
colnames(tmp5)<-c("TestDate","PositivePCR")
tmp6<-left_join(tmp1,tmp2)
tmp7<-left_join(tmp6,tmp3)
tmp8<-left_join(tmp7,tmp4)
tmp9<-left_join(tmp8,tmp5)
tmp9[is.na(tmp9)]<-0

x<-barplot(tmp9$n, width = 0.5, space = 2,col="grey",xlab="",ylab="Daily Test Counts",ylim=c(0,800),main="", xaxt="n")
barplot(tmp9$VoidLFT, width = 0.5, space = 2,col="blue",xaxt="n",add=T)
barplot(tmp9$PositiveLFT, width = 0.5, space = 2,col="red",xaxt="n",add=T)
barplot(tmp9$n, width = 0.5, space = c(3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),col="darkgrey",xaxt="n",add=T)
barplot(tmp9$VoidPCR, width = 0.5, space = c(3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),col="lightblue",xaxt="n",add=T)
barplot(tmp9$PositivePCR, width = 0.5, space = c(3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),col="rosybrown1",xaxt="n",add=T)
labs <- tmp9$TestDate
text(cex=0.8, x=x, y=-80, labs, xpd=TRUE, srt=90)
legend("topright",legend=c("Void LFT","Negative LFT","Positive LFT","Void PCR","Negative PCR","Positive PCR"),col=c("grey","blue","red","darkgrey","lightblue","rosybrown1"),pch=15)

##Plot positive PCRs over time.
data6$TestDate<-as.Date(as.character(data6$TestDate))
datanew<-subset(data6,data6$PCR_Result=="Positive")
datanew<-subset(data2,data2$PCR_Result=="Positive")
tmp1<-count(datanew,TestDate)
tmp2<-count(subset(datanew,datanew$LFDResultSite!="Void"),TestDate)
colnames(tmp2)<-c("TestDate","VoidLFT")
tmp3<-count(subset(datanew,datanew$LFDResultSite=="Positive"),TestDate)
colnames(tmp3)<-c("TestDate","PositiveLFT")
tmp4<-left_join(tmp1,tmp2)
tmp5<-left_join(tmp4,tmp3)


check1<-aggregate(CTRes~TestDate,median,data=datanew)
names(check1)<-c("TestDate","CTRes")
tmp5<-left_join(tmp5,check1)

x<-barplot(tmp5$n,col="grey",xlab="",ylab="Daily Test Counts",ylim=c(0,20),main="Count of Positive PCR results by LFT Result", xaxt="n")
barplot(tmp5$VoidLFT,col="blue",xaxt="n",add=T)
barplot(tmp5$PositiveLFT,col="red",xaxt="n",add=T)
labs <- tmp5$TestDate
text(cex=0.8, x=x, y=-1.5, labs, xpd=TRUE, srt=90)
legend("topleft",legend=c("Void LFT","Negative LFT","Positive LFT"),col=c("grey","blue","red"),pch=15)
par(new=TRUE)
plot(tmp5$TestDate,tmp5$CTRes,ylab="",xlab="",axes=FALSE,ylim=c(15,35),col="green",type="l",lwd=2)
axis(4,at=seq(15,35,by=5),las=1,cex=0.5,col="darkgreen",line=-1,xpd=TRUE)
mtext(expression("Median C"[T]*" Value"),side=4,line=1)




##Barchart of results by ethnicity
Ethnicity1<-as.factor(as.character(Ethnicity))
summary(as.factor(Ethnicity))
levels(Ethnicity1)<-c("Black, African, Black British, or Carribean","Asian or Asian British","Black, African, Black British, or Carribean",
"Another ethnic group","Another ethnic group","Mixed or multiple ethnic groups","White","Asian or Asian British","Mixed or multiple ethnic groups",
"Asian or Asian British","Asian or Asian British","Mixed or multiple ethnic groups","Mixed or multiple ethnic groups","Black, African, Black British, or Carribean",
"White","Black, African, Black British, or Carribean","Asian or Asian British","Asian or Asian British","White","Mixed or multiple ethnic groups","NA",
"Asian or Asian British","Prefer not to say","White")                                               

temp1<-table(Ethnicity1)
x<-barplot(temp1,xlab="",ylab="Number of individuals",ylim=c(0,5000),main="Individuals Tested by Ethnicity", xaxt="n",col="steelblue")
#labs <- names(temp1)
labs1<-c("Black, African","Asian or","Another","Mixed or", "White","Missing","Prefer")
labs2<-c("Black British","Asian British","ethnic group","multiple","","","not to")
labs3<-c("or Caribbean","","","ethnic groups","","","say")
text(cex=0.7, x=x, y=-150, labs1, xpd=TRUE, srt=0)
text(cex=0.7, x=x, y=-400, labs2, xpd=TRUE, srt=0)
text(cex=0.7, x=x, y=-650, labs3, xpd=TRUE, srt=0)

##CT by age??
##Split by sex
Age1<-as.numeric(Age)
df1<-as.data.frame(cbind(Age1,Sex))
df1$Age1<-as.numeric(as.character(df1$Age1))

df2<-df1[df1$Age1>17 & df1$Age1<100,]
df2<-subset(df2,!is.na(df2$Age1))
par(mfrow=c(1,2))
hist(df2$Age1[df2$Sex=="Male"],main="Males",xlab="Age",ylab="Frequency",ylim=c(0,400),xlim=c(0,100),col="steelblue")
hist(df2$Age1[df2$Sex=="Female"],main="Females",xlab="Age",ylab="Frequency",ylim=c(0,400),xlim=c(0,100),col="steelblue")
##1 patient is apparently 149 and a couple of people are 0.

summary(Age2)
sd(Age2,na.rm=TRUE)

summary(as.factor(Sex))
summary(as.factor(Sex))/sum(Sex=="Male"|Sex=="Female")

library(plotrix)
# create a categorical age variable
agegrp = cut(df2$Age1, breaks=c(18, 20, 30, 40, 50, 60, 70, 80, 90, 100))

# create a vector of percentages in each age range
women = as.vector(100*table(agegrp[df2$Sex=="Female"])/sum(df2$Sex=="Female"))
men = as.vector(100*table(agegrp[df2$Sex=="Male"])/sum(df2$Sex=="Male"))

women = as.vector(table(agegrp[df2$Sex=="Female"]))
men = as.vector(table(agegrp[df2$Sex=="Male"]))


pyramid.plot(men, women, 
   labels=c("(18,20]","(20,30]","(30,40]","(40,50]","(50,60]","(60,70]","(70,80]","(80,90]","(90,100]"), 
   gap=3,unit="%")

pyramid.plot(df2$Age1[df2$Sex=="Male"],df2$Age1[df2$Sex=="Female"],labels=NA,top.labels=c("Male","Age","Female"))

##Inter reviewer agreement 
install.packages("irr")
library(irr)
kappa2(cbind(data6$LFDResultSite,data6$LFDResultQA))

data6$Age<-Age
data7<-subset(data6,data6$CTRes>0&data6$Age!="NA")

plot(as.numeric(data7$Age)[data7$Age!=0],data7$CTRes[data7$Age!=0],pch=16,xlim=c(0,100),xlab="Age",ylab="Mean CT value",main="PCR Cycle Threshold (Ct) by Age",col="blue")
points(as.numeric(data7$Age)[data7$LFDResultSite=="Positive"],data7$CTRes[data7$LFDResultSite=="Positive"],col="red",pch=16)
points(as.numeric(data7$Age)[data7$LFDResultSite=="Void"],data7$CTRes[data7$LFDResultSite=="Void"],col="darkgreen",pch=16)
legend("topright",legend=c("Void LFT","Negative LFT","Positive LFT"),col=c("darkgreen","blue","red"),pch=16)

plot(as.numeric(data7$Age)[data7$Age!=0],data7$CTRes[data7$Age!=0],pch=16,xlim=c(0,100),xlab="Age",ylab="Mean CT value",main="PCR Cycle Threshold (Ct) by Age",col="blue")
points(as.numeric(data7$Age)[data7$LFDResultSite=="Positive"],data7$CTRes[data7$LFDResultSite=="Positive"],col="red",pch=16)
points(as.numeric(data7$Age)[data7$LFDResultSite=="Void"],data7$CTRes[data7$LFDResultSite=="Void"],col="darkgreen",pch=16)
legend("topright",legend=c("Void LFT","Negative LFT","Positive LFT"),col=c("darkgreen","blue","red"),pch=16)

##Barcharts of Test Centres!
tmp2<-count(data2,Site)
par(mar=c(14.1,4.1,4.1,2.1))
x<-barplot(tmp2$n,xlab="",ylab="Number of individuals",ylim=c(0,250),main="Individuals Tested by Test Centre", xaxt="n",col="steelblue")
labs <- tmp2$Site
text(cex=0.7, x=x, y=-80, labs, xpd=TRUE, srt=90)

##assess discordance across sites.
c1<-unique(data2$Site)
Prop1<-vector(length=48)
for(i in 1:48){
s1<-subset(data2,data2$Site==c1[i])
Prop1[i]<-sum(s1$LFDResultSite==s1$LFDResultQA)/nrow(s1)
}

Prop1[order(Prop1,decreasing=TRUE)]
plot(1:48,Prop1[order(Prop1,decreasing=TRUE)],xlab="Test Centre",ylab="Proportion agreement",main="Agreement between Site and QA LFT Readings by Test Site",
pch=16,ylim=c(0,1))

##map of liverpool with test centre numbers. Based on Mark Green's code.
# Load location of sites data (tried matching your codes with my data on locations of sites)
library(leaflet) # For creating interactive maps
library(sf) # For wrangling spatial data

sites <- read.csv("TestSiteCodes_location.csv") 
head(sites)
tmp1<-unique(data2$Site)
##Join Test CEntre to data6
data6$TestCentre<-TestCentre
t5<-count(data6,TestCentre)
#colnames(t5)<-c("Site.ID","n")

# Join on stats to location data
sites <- merge(sites, t5, by.x = "Site.ID", by.y = "TestCentre", all.x = TRUE) 
# Note I cannot find some codes and their sites:
# t5 data frame n = 82 vs sites data frame n = 74
# Need to complete the dataset therefore, but I do not have the full records

sites_sp <- sites %>% 
  st_as_sf(coords = c("Lon", "Lat")) %>% # Define co-ordinates
  st_set_crs(4326) # Define co-ordinate reference system


library(mapview)
m <- leaflet() %>%
  # Plot second circle that adjusts size based on number of tests
  addCircleMarkers(data = sites_sp, # Data for plotting points
                   lng = sites$Lon, # Longitude
                   lat = sites$Lat, # Latitude
                   radius = ~ sqrt(n), # Display circles in relation to size of total LFTs (scaled so not too large for the map)
                   color = "steelblue", # Colour
                   stroke = FALSE, # Gets rid of border for circles
                   fillOpacity = 0.5) %>% # Makes transparent
  addProviderTiles("CartoDB.Positron") %>% # Incldue base map to aid interpretation
  addControl("Counts of QA Tests at Liverpool Test Centres", position = "bottomleft") %>% 
  setView(lng = median(sites$Lon), # Define where to centre the map
          lat = median(sites$Lat),
          zoom = 12)
mapshot(m, file = "QACounts.png",selfcontained = FALSE)


save(Age,data_Original,data2,data6,data7,Ethnicity,LFTResult,ResultDate,SampleDate,Sex,TestCentre,file="QAMergedData.RData")            



##Now to look at the reappraisal results
QAres<-table(data6$Study_team_re_appraisal_of_result,data6$PCR_Result)[2:4,]
##Sensitivity
QAres[2,2]/(QAres[1,2]+QAres[2,2])
exactci(QAres[2,2],(QAres[1,2]+QAres[2,2]),conf.level=0.95)

##Specificity
QAres[1,1]/(QAres[1,1]+QAres[2,1])
exactci(QAres[1,1],(QAres[1,1]+QAres[2,1]),conf.level=0.95)

##PPV
QAres[2,2]/(QAres[2,1]+QAres[2,2])
exactci(QAres[2,2],(QAres[2,1]+QAres[2,2]),conf.level=0.95)

##NPV
QAres[1,1]/(QAres[1,1]+QAres[1,2])
exactci(QAres[1,1],(QAres[1,1]+QAres[1,2]),conf.level=0.95)

CTRestab<-table(data6$Study_team_re_appraisal_of_result,data6$CTRes1)
CTRestab

ch1<-which((data6$S_gene1!=0|data6$N_gene1!=0|data6$ORF1ab1!=0)&data6$PCR_Result=="Void")
length(ch1)
s1<-data6[ch1,]
s1$CTRes<-apply(s1[,c("N_gene","S_gene","ORF1ab")],1,f1)

##
test1<-count(subset(data6,data6$Study_team_re_appraisal_of_result!="Void"), CTRes1)
test2<-count(subset(data6,data6$Study_team_re_appraisal_of_result=="Positive"),CTRes1)
#test1<-test1[c(1,3:5,2),]

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims

##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC
x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of Samples",ylim=c(0,40),
main="Proportion of Samples with positive LFD test according to viral load", xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=1.2, x=(x[2]+.5*(x[3]-x[2])), y=-5, "Mean CT score", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFD Negative","LFD Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of positive LFD tests",side=4,line=0.5)


##N gene results
CTRestabN<-table(data6$Study_team_re_appraisal_of_result,data6$CTRes1N)
CTRestabN
sum(data6$PCR_Result=="Void"&data6$N_gene1>35)
sum(data6$PCR_Result=="Void"&data6$N_gene1>30)
sum(data6$PCR_Result=="Void"&data6$N_gene1==0)
sum(data6$PCR_Result=="Void"&data6$N_gene1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Void"&data6$N_gene1==0&data6$LFDResultSite=="Positive")
sum(data6$PCR_Result=="Positive"&data6$N_gene1==0&data6$LFDResultSite=="Negative")
sum(data6$PCR_Result=="Positive"&data6$N_gene1==0&data6$LFDResultSite=="Positive")

##
test1<-count(subset(data6,data6$Study_team_re_appraisal_of_result!="Void"), CTRes1N)
test2<-count(subset(data6,data6$Study_team_re_appraisal_of_result=="Positive"),CTRes1N)

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims

##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC
x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of Samples",ylim=c(0,40),
main="Proportion of Samples with positive LFD test according to N gene result", xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=1.2, x=(x[2]+.5*(x[3]-x[2])), y=-5, "N gene", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFD Negative","LFD Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of positive LFD tests",side=4,line=0.5)

##S gene results
CTRestabS<-table(data6$Study_team_re_appraisal_of_result,data6$CTRes1S)
CTRestabS
data6[which(data6$S_gene1==0 & data6$PCR_Result=="Positive"),c("LFDResultSite","PCR_Result","S_gene1")]
sum(data6$PCR_Result=="Void"&data6$S_gene1>35)
sum(data6$PCR_Result=="Void"&data6$S_gene1>30)
sum(data6$PCR_Result=="Void"&data6$S_gene1==0)
sum(data6$PCR_Result=="Void"&data6$S_gene1==0&data6$Study_team_re_appraisal_of_result=="Negative")
sum(data6$PCR_Result=="Void"&data6$S_gene1==0&data6$Study_team_re_appraisal_of_result=="Positive")
sum(data6$PCR_Result=="Positive"&data6$S_gene1==0&data6$Study_team_re_appraisal_of_result=="Negative")
sum(data6$PCR_Result=="Positive"&data6$S_gene1==0&data6$Study_team_re_appraisal_of_result=="Positive")

##
test1<-count(subset(data6,data6$Study_team_re_appraisal_of_result!="Void"), CTRes1S)
test2<-count(subset(data6,data6$Study_team_re_appraisal_of_result=="Positive"),CTRes1S)

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims

##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC

x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of Samples",ylim=c(0,40),
main="Proportion of Samples with positive LFD test according to S gene result", xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=1.2, x=(x[2]+.5*(x[3]-x[2])), y=-5, "S gene", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFD Negative","LFD Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of positive LFD tests",side=4,line=0.5)


##ORF1ab gene results
CTRestabO<-table(data6$Study_team_re_appraisal_of_result,data6$CTRes1O)
CTRestabO
data6[which(data6$ORF1ab1==0 & data6$PCR_Result=="Positive"),c("LFDResultSite","PCR_Result","ORF1ab1")]
sum(data6$PCR_Result=="Void"&data6$ORF1ab1>35)
sum(data6$PCR_Result=="Void"&data6$ORF1ab1>30)
sum(data6$PCR_Result=="Void"&data6$ORF1ab1==0)
sum(data6$PCR_Result=="Void"&data6$ORF1ab1==0&data6$Study_team_re_appraisal_of_result=="Negative")
sum(data6$PCR_Result=="Void"&data6$ORF1ab1==0&data6$Study_team_re_appraisal_of_result=="Positive")
sum(data6$PCR_Result=="Positive"&data6$ORF1ab1==0&data6$Study_team_re_appraisal_of_result=="Negative")
sum(data6$PCR_Result=="Positive"&data6$ORF1ab1==0&data6$Study_team_re_appraisal_of_result=="Positive")


##
test1<-count(subset(data6,data6$Study_team_re_appraisal_of_result!="Void"), CTRes1O)
test1<-test1[-2,]
test2<-count(subset(data6,data6$Study_team_re_appraisal_of_result=="Positive"),CTRes1O)
test2[4,2]<-0

Prop<-test2$n[1:4]/test1$n[1:4]
lowerLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[1],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[1],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[1],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[1])

upperLims<-c(exactci(test2$n[1],test1$n[1],conf.level=0.95)$conf.int[2],
exactci(test2$n[2],test1$n[2],conf.level=0.95)$conf.int[2],
exactci(test2$n[3],test1$n[3],conf.level=0.95)$conf.int[2],
exactci(test2$n[4],test1$n[4],conf.level=0.95)$conf.int[2])
Prop
lowerLims
upperLims

##Cumulative Sensitivity
Ncum<-cumsum(test1$n[1:4])
pcum<-cumsum(test2$n[1:4])
Propcum<-pcum/Ncum
lowerLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[1],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[1],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[1],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[1])

upperLimsC<-c(exactci(pcum[1],Ncum[1],conf.level=0.95)$conf.int[2],
exactci(pcum[2],Ncum[2],conf.level=0.95)$conf.int[2],
exactci(pcum[3],Ncum[3],conf.level=0.95)$conf.int[2],
exactci(pcum[4],Ncum[4],conf.level=0.95)$conf.int[2])
Propcum
lowerLimsC
upperLimsC

x<-barplot(test1$n[4:1],col="grey",xlab="",ylab="Number of Samples",ylim=c(0,40),
main="Proportion of Samples with positive LFD test according to ORF1ab gene result", xaxt="n")
barplot(test2$n[4:1],col="lightblue",xaxt="n",add=T)
labs <- test1$CTRes1[4:1]
text(cex=1.2, x=x, y=-3, labs, xpd=TRUE, srt=0)
text(cex=1.2, x=(x[2]+.5*(x[3]-x[2])), y=-5, "ORF1ab gene", xpd=TRUE, srt=0)
legend("topleft",legend=c("LFD Negative","LFD Positive","Cumulative Sensitivity"),col=c("grey","lightblue","darkgreen"),pch=c(15,15,16,16))
par(new=TRUE)
plotCI(c(0.6,1.75,2.9,4.05),Propcum[4:1],ui=upperLimsC[4:1],li=lowerLimsC[4:1], xlab="",ylab="",xlim=c(0,4.5),axes=FALSE,ylim=c(0,1),
gap=0,col="darkgreen",pch=16)
lines(c(0.6,1.75,2.9,4.05),Propcum[4:1],col="darkgreen")
axis(4,at=seq(0,1,by=0.2),las=1,cex=1.5,col="black",line=-1.5,xpd=TRUE)
mtext("Cumulative proportion of positive LFD tests",side=4,line=0.5)


##Sensitivity analyses.

##Best case scenario
IDS1<-unique(data_Original$PseudoID)
data4<-NULL

for(i in 1:length(IDS1)){
t1<-subset(data_Original,data_Original$PseudoID %in% IDS1[i])
if(nrow(t1)==1){data4<-rbind(data4,t1)}
if(nrow(t1)==2){
if(t1$LFDResultSite[1]==t1$LFDResultSite[2] & t1$PCR_Result[1]==t1$PCR_Result[2]){data4<-rbind(data4,t1[1,])}
if(t1$LFDResultSite[1]=="Void" & t1$LFDResultSite[2]=="Negative"){data4<-rbind(data4,t1[2,])}
if(t1$LFDResultSite[2]=="Void" & t1$LFDResultSite[1]=="Negative"){data4<-rbind(data4,t1[1,])}
if(t1$PCR_Result[1]=="Void"&t1$PCR_Result[2]=="Negative"){data4<-rbind(data4,t1[2,])}
if(t1$PCR_Result[2]=="Void"&t1$PCR_Result[1]=="Negative"){data4<-rbind(data4,t1[1,])}
if(t1$PCR_Result[1]=="Void"&t1$PCR_Result[2]=="Positive"){data4<-rbind(data4,t1[1,])}
if(t1$PCR_Result[2]=="Void"&t1$PCR_Result[1]=="Positive"){data4<-rbind(data4,t1[2,])}
}
if(nrow(t1)==3){data4<-rbind(data4,t1[1,])}  ##Only happens once that there are 3 rows. All negative LFTs, 2 negative PCR and 1 VOID
}


QAres<-table(data4$LFDResultSite,data4$PCR_Result)
QAres
##Sensitivity
QAres[2,2]/(QAres[1,2]+QAres[2,2])
exactci(QAres[2,2],(QAres[1,2]+QAres[2,2]),conf.level=0.95)

##Specificity
QAres[1,1]/(QAres[1,1]+QAres[2,1])
exactci(QAres[1,1],(QAres[1,1]+QAres[2,1]),conf.level=0.95)

##PPV
QAres[2,2]/(QAres[2,1]+QAres[2,2])
exactci(QAres[2,2],(QAres[2,1]+QAres[2,2]),conf.level=0.95)

##NPV
QAres[1,1]/(QAres[1,1]+QAres[1,2])
exactci(QAres[1,1],(QAres[1,1]+QAres[1,2]),conf.level=0.95)

##Worst case scenario
IDS1<-unique(data_Original$PseudoID)
data5<-NULL

for(i in 1:length(IDS1)){
t1<-subset(data_Original,data_Original$PseudoID %in% IDS1[i])
if(nrow(t1)==1){data5<-rbind(data5,t1)}
if(nrow(t1)==2){
if(t1$LFDResultSite[1]==t1$LFDResultSite[2] & t1$PCR_Result[1]==t1$PCR_Result[2]){data5<-rbind(data5,t1[1,])}
if(t1$LFDResultSite[1]=="Void" & t1$LFDResultSite[2]=="Negative"){data5<-rbind(data5,t1[1,])}
if(t1$LFDResultSite[2]=="Void" & t1$LFDResultSite[1]=="Negative"){data5<-rbind(data5,t1[2,])}
if(t1$PCR_Result[1]=="Void"&t1$PCR_Result[2]=="Negative"){data5<-rbind(data5,t1[1,])}
if(t1$PCR_Result[2]=="Void"&t1$PCR_Result[1]=="Negative"){data5<-rbind(data5,t1[2,])}
if(t1$PCR_Result[1]=="Void"&t1$PCR_Result[2]=="Positive"){data5<-rbind(data5,t1[2,])}
if(t1$PCR_Result[2]=="Void"&t1$PCR_Result[1]=="Positive"){data5<-rbind(data5,t1[1,])}
}
if(nrow(t1)==3){data5<-rbind(data5,t1[t1$PCR_Result=="Void",])}  ##Only happens once that there are 3 rows. All negative LFTs, 2 negative PCR and 1 VOID
}


QAres<-table(data5$LFDResultSite,data5$PCR_Result)
QAres
##Sensitivity
QAres[2,2]/(QAres[1,2]+QAres[2,2])
exactci(QAres[2,2],(QAres[1,2]+QAres[2,2]),conf.level=0.95)

##Specificity
QAres[1,1]/(QAres[1,1]+QAres[2,1])
exactci(QAres[1,1],(QAres[1,1]+QAres[2,1]),conf.level=0.95)

##PPV
QAres[2,2]/(QAres[2,1]+QAres[2,2])
exactci(QAres[2,2],(QAres[2,1]+QAres[2,2]),conf.level=0.95)

##NPV
QAres[1,1]/(QAres[1,1]+QAres[1,2])
exactci(QAres[1,1],(QAres[1,1]+QAres[1,2]),conf.level=0.95)

##Full data analysis
QAres<-table(data_Original$LFDResultSite,data_Original$PCR_Result)
QAres

##Sensitivity
QAres[2,2]/(QAres[1,2]+QAres[2,2])
exactci(QAres[2,2],(QAres[1,2]+QAres[2,2]),conf.level=0.95)

##Specificity
QAres[1,1]/(QAres[1,1]+QAres[2,1])
exactci(QAres[1,1],(QAres[1,1]+QAres[2,1]),conf.level=0.95)

##PPV
QAres[2,2]/(QAres[2,1]+QAres[2,2])
exactci(QAres[2,2],(QAres[2,1]+QAres[2,2]),conf.level=0.95)

##NPV
QAres[1,1]/(QAres[1,1]+QAres[1,2])
exactci(QAres[1,1],(QAres[1,1]+QAres[1,2]),conf.level=0.95)


### Discordance by site...
c1<-unique(data2$Site)
Prop1<-vector(length=48)
for(i in 1:48){
s1<-subset(data2,data2$Site==c1[i])
Prop1[i]<-sum(s1$LFDResultSite==s1$PCR_Result)/nrow(s1)
}

Prop1[order(Prop1,decreasing=TRUE)]
plot(1:48,Prop1[order(Prop1,decreasing=TRUE)],ylab="Proportion agreement",main="Agreement between LFT and PCR Results by Test Site",
pch=16,ylim=c(0,1),xlab="Anonymised Test Centre",xaxp=c(1,48,(length(c1)-1)),cex.axis=0.6)


