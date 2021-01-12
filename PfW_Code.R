#######################
### Replication Code for Pressure for War
#######################

###Table of Contents:
#1) Loading in Data
#2) Creating All Necessary Variables
#3) Creating Datasets for Analysis
#4) The Logits
#5) The LASSOs
#6) The Permutation
#7) Figures
#8) Outside Survey Data Analysis
#9) Appendix D
#10) Southern Realignment
#11) Changes in Terms
#12) Replicating Main Findings w/ Limited (CSA) South
#13) Predicted Probability Estimates
#14) Extension w/ ICB Data
#15) Hazard Model for H2
#16) Misc Calculations

###1) Loading in Data
library(readr)
library(data.table)
library(tidyverse)
library(stargazer)
library(sandwich)
library(glmnet)
library(miceadds)
library(memisc)
library(sjPlot)
library(sjmisc)
library(survival)
library(survminer)

#setwd("~/Documents/Academic/Dissertation/591")

dcdata<-read.csv("15-03-13-USMIDs4.csv") # This is the replication data from Dafoe & Caughey 2016
mida<-read.csv("MIDA_4.01.csv") # This and the following file are the MIDs data version 4.01
midb<-read.csv("MIDB_4.01.csv")
sdata<-read.csv("IVslim.csv") # This is the original data gathered for this paper

###2) Creating all Necessary Variables
#Outline:
#2a) Get Dataset of All US MIDS
#2b) Code by President
#2c) Merge in Gathered IV data
#2d) Merge in DC Variables (via dispnum3)
#2e) Create (and denote) IV
#2f) Create (and denote) DVs
#2g) Create (and denote) all controls

#2a) Get Dataset of All US MIDS:
mids<-merge(midb,mida,by="DispNum3")
rm(mida,midb)
mids<-subset.data.frame(mids, mids$StAbb=="USA")

#2b) Code by President:
mids$StLeader<-"Nobody"
mids$StTerm<-1
mids$StLeader<-ifelse(mids$StYear.x>=1808, "Madison", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1812 & mids$StYear.x<=1816, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1816, "Monroe", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1820 & mids$StYear.x<=1824, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1824, "Adams", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1828, "Jackson", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1832 & mids$StYear.x<=1836, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1836, "Van Buren", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1840, "Tyler", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1844, "Polk", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1848, "Fillmore", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1852, "Pierce", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1856, "Buchanan", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1860, "Lincoln", mids$StLeader)
mids$StTerm<-ifelse((mids$StYear.x==1865 & mids$StMon.x<=4),2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1865 | (mids$StYear.x==1865 & mids$StMon.x>4) |
                        (mids$StYear.x==1865 & mids$StMon.x==4 & mids$StDay.x>15),
                      "Johnson A", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1868, "Grant", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1872 & mids$StYear.x<=1876, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1876, "Hayes", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1880, "Arthur", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1884, "Cleveland", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1888, "Harrison", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1892, "Cleveland", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1896, "McKinley", mids$StLeader)
mids$StTerm<-ifelse(((mids$StYear.x==1901 & mids$StMon.x<9) |
                       (mids$StYear.x==1901 & mids$StMon.x==9 &
                          mids$StDay.x<14)),2, mids$StTerm)
mids$StLeader<-ifelse((mids$StYear.x>1901 | (mids$StYear.x==1901 & mids$StMon.x>9) |
                         (mids$StYear.x==1901 & mids$StMon.x==9 & mids$StDay.x>14)),
                      "Roosevelt T", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1904 & mids$StYear.x<=1908, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1908, "Taft", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1912, "Wilson", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1916 & mids$StYear.x<=1920, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1920, "Harding", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1924, "Coolidge", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1928, "Hoover", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1932, "Roosevelt F", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1936 & mids$StYear.x<=1940, 2, mids$StTerm)
mids$StTerm<-ifelse(mids$StYear.x>1940 & mids$StYear.x<=1944, 3, mids$StTerm)
mids$StTerm<-ifelse(((mids$StYear.x==1945 & mids$StMon.x<4) |
                       (mids$StYear.x==1945 & mids$StMon.x==4 &
                          mids$StDay.x<12)), 4, mids$StTerm)
mids$StLeader<-ifelse((mids$StYear.x>1945 | (mids$StYear.x==1945 & mids$StMon.x>4) |
                         (mids$StYear.x==1945 & mids$StMon.x==4 & mids$StDay.x>12)),
                      "Wilson", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1948 & mids$StYear.x<=1948, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1952, "Eisenhower", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1956 & mids$StYear.x<=1960, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1960, "Kennedy", mids$StLeader)
mids$StLeader<-ifelse((mids$StYear.x>1963 | (mids$StYear.x==1963 & mids$StMon.x>11) |
                         (mids$StYear.x==1963 & mids$StMon.x==11 & mids$StDay.x>22)),
                      "Johnson L", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1964 & mids$StYear.x<=1964, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1968, "Nixon", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1972 &
                      (mids$StYear.x<1974 |
                         (mids$StYear.x==1974 & mids$StMon.x<8) |
                         (mids$StYear.x==1974 & mids$StMon.x==8 & mids$StDay.x<=9)),
                    2, mids$StTerm)
mids$StLeader<-ifelse((mids$StYear.x>1974 | (mids$StYear.x==1974 & mids$StMon.x>8) |
                         (mids$StYear.x==1974 & mids$StMon.x==8 & mids$StDay.x>9)),
                      "Ford", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1976, "Carter", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1980, "Reagan", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1984 & mids$StYear.x<=1988, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>1988, "Bush Sr", mids$StLeader)
mids$StLeader<-ifelse(mids$StYear.x>1992, "Clinton", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>1996 & mids$StYear.x<=2000, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>2000, "Bush Jr", mids$StLeader)
mids$StTerm<-ifelse(mids$StYear.x>2004 & mids$StYear.x<=2008, 2, mids$StTerm)
mids$StLeader<-ifelse(mids$StYear.x>2008, "Obama", mids$StLeader)

#2c) Merge in Gathered IV data:
sdata$StLeader<-sdata$President
sdata$StTerm<-sdata$Term
sdata$StLdTm<-paste(sdata$StLeader,sdata$StTerm)
sdata<-as.data.frame(sdata)
sdata$S.Pct.EC<-as.numeric(sdata$S.Pct.EC)
sdata$S.Pct.Pop<-as.numeric(sdata$S.Pct.Pop)
sdata$S.Pct.EC.CSA<-as.numeric(sdata$S.Pct.EC.CSA)

mids$StLdTm<-paste(mids$StLeader,mids$StTerm)

fulldata<-merge(sdata,mids, by="StLdTm",all=T)
fulldata$missing<-is.na(fulldata$DispNum3)
fulldata<-subset.data.frame(fulldata,missing==FALSE)

#2d) Merge in DC Variables (via dispnum3)
borrowedvars <- c("dispnum3", "Leader", "outcome", "LngthMIDUS", "Party", "MiltExp", "MiltOcc",
                  "proprec", "Era", "pctvetmi", "unified", "PresName", "GastilSum",
                  "puritan", "tidewater", "quaker", "border", "DaysInTrm1", "US_Init",
                  "US_alone", "opp_alone", "BothAlone", "FishDisp", "Party1Factor",
                  "TermStartDate1", "StartDateUS", "EndDateUS","lnprevdead", "elitevet")
borroweddata <- dcdata[borrowedvars]
names(borroweddata)[1] <- "DispNum3"

fulldata<-merge.data.frame(fulldata, borroweddata,by="DispNum3",all=F)

#2e) Create (and denote) IV:
#Main IV (Pct Electoral College Votes from the South) is "S.Pct.Ec"; to test:
fulldata$S.Pct.EC<-as.numeric(fulldata$S.Pct.EC)
fulldata$S.Pct.EC.CSA<-as.numeric(fulldata$S.Pct.EC.CSA)
#Secondary IV (Presidential Southernness) is "Southern"; to test:
fulldata$Southern
#Secondary IV (Pct Popular Vote from the South) is "S.Pct.Pop"; to test:
fulldata$S.Pct.Pop<-as.numeric(fulldata$S.Pct.Pop)
#Main IV, Alternative Specification:
fulldata$ToWin<-0
fulldata$ToWin<-ifelse(fulldata$Year==1808, 89, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1812|
                         fulldata$Year==1816, 109, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1820, 116, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1824|
                         fulldata$Year==1828, 131, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1832, 144, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1836|
                         fulldata$Year==1840, 148, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1844, 138, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1848, 146, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1852|
                         fulldata$Year==1856, 149, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1860, 152, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1864, 118, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1868, 148, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1872, 177, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1876|
                         fulldata$Year==1880, 185, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1884|
                         fulldata$Year==1888, 201, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1892, 223, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1896|
                         fulldata$Year==1900, 224, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1904, 239, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1908, 242, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year>=1912&
                         fulldata$Year<=1928, 266, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1932, 267, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year>=1936&
                         fulldata$Year<=1956, 266, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year==1960, 269, fulldata$ToWin)
fulldata$ToWin<-ifelse(fulldata$Year>=1964, 270, fulldata$ToWin)
fulldata$SpecA<-ifelse(fulldata$Total.EC-fulldata$S.EC<fulldata$ToWin,
                       fulldata$S.Pct.EC,0)

#2f) Create (and denote) DVs
#DV1: Use of Force; "UseOfForce";
fulldata$UseOfForce<-ifelse(fulldata$HostLev.x>=4,1,0)
#DV2: Length of Dispute: "DurationUS";
fulldata$DurationUS<-fulldata$DaysInTrm1
#AltDV: American Victory; "USWins";
fulldata<-fulldata %>% mutate(USWins = case_when((SideA==1 & Outcome%in%c(1,4))~1,
                                                 SideA==0 & Outcome%in%c(2,3)~1,
                                                 TRUE~0))
#AltDV: Unilateral American Victory; "USUni";
fulldata<-fulldata %>% mutate(USUni = case_when((SideA==1 & Outcome%in%c(1))~1,
                                                 SideA==0 & Outcome%in%c(2)~1,
                                                 TRUE~0))

#2g) Create (and denote) all controls:
#Time, measured as year of presidential election; "Year"
fulldata$Year<-as.numeric(fulldata$Year)
#Term, measured as whether the MID was in the 1st or 2nd+ term of the pres; "term"
fulldata$term<-ifelse(fulldata$Term==1,1,0)
#Inherited, whether the President had succeeded to the Presidency from VP; "Inherited"
fulldata$Inherited<-as.numeric(fulldata$Inherited)
#Power, two binaries for whether the US is a great power or a super power; "greatpower","superpower"
fulldata$greatpower<-ifelse(fulldata$Era!="pre-1897",1,0)
fulldata$superpower<-ifelse(fulldata$Era=="post-1945",1,0)
#Fishing Dispute, binary for whether the MID is a fishing dispute "FishDisp"
fulldata$FishDisp<-as.numeric(fulldata$FishDisp)
#table(fulldata$FishDisp) ; So 20 of these 372 disputes (5.3%) are fishing disputes
#Recent MIDS, the numbers of MIDS in the past 5, 10 years; "past5", "past10"
fulldata$prev5<-rep(NA,nrow(fulldata))
for (i in 1:nrow(fulldata)){
  term.start <- fulldata$Year[fulldata$Leader==fulldata$Leader[i]][1]
  fulldata$prev5[i] <-
    sum(fulldata$StYear.x>(term.start-5)
        & fulldata$EndYear.x<=term.start&fulldata$FishDisp==0, na.rm = TRUE)
}
fulldata$prev10<-rep(NA,nrow(fulldata))
for (i in 1:nrow(fulldata)){
  term.start <- fulldata$Year[fulldata$Leader==fulldata$Leader[i]][1]
  fulldata$prev10[i] <-
    sum(fulldata$StYear.x>(term.start-10)
        & fulldata$EndYear.x<=term.start&fulldata$FishDisp==0, na.rm = TRUE)
}
#Log Number of Fatalities in the most recent war; "logdead"
fulldata$dead<-round(exp(fulldata$lnprevdead), 0)
fulldata$dead[fulldata$Leader=="HarrisonWH"]<-2260
fulldata$dead[fulldata$Leader == "Polk"] <- 2260
fulldata$dead[fulldata$Leader == "Taylor"] <- 13283
fulldata$dead[fulldata$Leader == "Lincoln"] <- 13283
fulldata$dead[as.numeric(fulldata$Leader) >= 17 & as.numeric(fulldata$Leader) <= 24] <-
  620000  ## Includes Confederate dead in Civil War
fulldata$dead[fulldata$Leader == "JohnsonLB"] <- 36574
fulldata$dead[fulldata$Leader == "Clinton" | fulldata$Leader=="BushGW"] <- 382
fulldata$dead[fulldata$Leader == "Obama"] <- 4222
fulldata$logdead <- log(fulldata$dead)
#Vets in Power, the proportion of political elites who served; "elitevet"
fulldata$elitevet<- ifelse(fulldata$Leader %in% c("Clinton", "BushGW", "Obama"),40,fulldata$elitevet)
#Binary indicators for party; "whig", "rep"
fulldata$whig<-ifelse(fulldata$Party=="Whig",1,0)
fulldata$rep<-ifelse(fulldata$Party=="Republican",1,0)
#Presidential Military Experience, is the president a vet; "presvet"
fulldata$presvet<-ifelse(fulldata$MiltExp=="yes",1,0)
#Linked to other disputes; "Linked";
fulldata$linked=as.numeric(fulldata$Link1!=0)

###3) Creating Datasets for Analysis
#Outline:
#3a) Produce dataset: Summary (for figures)
#3b) Produce dataset: Logit
#3c) Produce dataset(s): LASSO
#3d) Produce dataset: Permutation (following this, clear all other data)

#3a) Produce dataset: Summary (for figures)
#First dataset to be used for the Southern-ness v Southern EC Support Difference Figure:
#For this task, use "sdata"
#Creating a factor for "Southern":
sdata$S.Text<-ifelse(sdata$Southern==1,"Southern","Non-Southern")

#3b) Produce dataset: Logit
#For the logits, "fulldata" can be used, as the variables will be specified individually.

#3c) Produce dataset(s): LASSO
#The set of DVs:
LassoDVs<-c("S.Pct.EC", "S.Pct.Pop", "Southern",
            "Year", "term", "Inherited", "greatpower", "superpower", "FishDisp",
            "prev5", "prev10", "logdead", "elitevet", "whig", "rep", "presvet", "Orig")
LassoX<-fulldata[LassoDVs]
LassoX$NAs<-ifelse(rowSums(is.na(LassoX)) > 0,1,0)
X_wnas<-NULL
X_wnas<-as.integer(which(LassoX$NAs==1))
LassoX<-as.matrix(LassoX[-c(X_wnas),])

#The set of IVs:
LassoY1<-(fulldata[-c(X_wnas),])
LassoY1<-as.matrix(LassoY1$UseOfForce)
LassoY2<-fulldata[-c(X_wnas),]
LassoY2<-as.matrix(LassoY2$DurationUS)


#3d) Produce dataset: Permutation (following this, clear all other data)
#Four sets, w/ each IV and DV combo:
perm1<-fulldata[c("S.Pct.EC","UseOfForce")]
perm2<-fulldata[c("S.Pct.EC","DurationUS")]
perm3<-fulldata[c("Southern","UseOfForce")]
perm4<-fulldata[c("Southern","DurationUS")]

###4) The Logits
#Outline:
#4a) The Logits themselves
#4b) Paper & Full Result presentation

#4a) The Logits themselves
#H1 Just EC (in-text):
logith1a<-glm(UseOfForce~S.Pct.EC,data=fulldata,family="binomial")
#H1 Just Southern:
logith1b<-glm(UseOfForce~Southern,data=fulldata,family="binomial")
#H1 EC & South (in-text):
logith1c<-glm(UseOfForce~S.Pct.EC+Southern,data=fulldata,family="binomial")
#H1 Presidency-Specific:
logith1d<-glm(UseOfForce~S.Pct.EC+Southern+Year+term+Inherited+whig+rep+presvet,
              data=fulldata,family="binomial")
#H1 Conflict-Specific:
logith1e<-glm(UseOfForce~S.Pct.EC+FishDisp+Orig,data=fulldata,family="binomial")
#H1 National Conditions:
logith1f<-glm(UseOfForce~S.Pct.EC+greatpower+superpower+prev5+prev10+logdead+elitevet,
              data=fulldata,family="binomial")
#H1 Full (in-text):
logith1g<-glm(UseOfForce~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
              prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata,family="binomial")
#H1 EC & South (No Cold War):
logith1h<-glm(UseOfForce~S.Pct.EC+Southern,
              data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,],family="binomial")
#H1 Full (No Cold War):
logith1i<-glm(UseOfForce~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
              data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,],family="binomial")
#H1 EC & South (Spec A):
logith1j<-glm(UseOfForce~SpecA+Southern,data=fulldata,family="binomial")
#H1 Full (Spec A)
logith1k<-glm(UseOfForce~SpecA+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata,family="binomial")
#H1 EC & South (1945-Present):
logith1l<-glm(UseOfForce~S.Pct.EC+Southern,
              data=fulldata[fulldata$StYear.x>1945,],family="binomial")
#H1 Full (1945-Present):
logith1m<-glm(UseOfForce~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
              data=fulldata[fulldata$StYear.x>1945,],family="binomial")
#H1 EC & South (No World Wars):
logith1n<-glm(UseOfForce~S.Pct.EC+Southern,
              data=fulldata[!fulldata$DispNum3%in%c(257,258,1774),],family="binomial")
#H1 Full (No World Wars):
logith1o<-glm(UseOfForce~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
              data=fulldata[!fulldata$DispNum3%in%c(257,258,1774),],family="binomial")
#H1 LASSO-Selected IVs:
logith1p<-glm.cluster(UseOfForce~S.Pct.EC+Inherited+greatpower+superpower+
                prev10+elitevet+whig+rep+presvet+Orig,
              data=fulldata,family="binomial",cluster="President")
#H2 Just EC (in-text):
logith2a<-lm(DurationUS~S.Pct.EC,data=fulldata)
#H2 Just Southern:
logith2b<-lm(DurationUS~Southern,data=fulldata)
#H2 EC & South (in-text):
logith2c<-lm(DurationUS~S.Pct.EC+Southern,data=fulldata)
#H2 Presidency-Specific:
logith2d<-lm(DurationUS~S.Pct.EC+Southern+Year+term+Inherited+whig+rep+presvet,
              data=fulldata)
#H2 Conflict-Specific:
logith2e<-lm(DurationUS~S.Pct.EC+FishDisp+Orig,data=fulldata)
#H2 National Conditions:
logith2f<-lm(DurationUS~S.Pct.EC+greatpower+superpower+prev5+prev10+logdead+elitevet,
              data=fulldata)
#H2 Full (in-text):
logith2g<-lm(DurationUS~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata)
#H2 EC & South (No Cold War):
logith2h<-lm(DurationUS~S.Pct.EC+Southern,
             data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,])
#H2 Full (No Cold War):
logith2i<-lm(DurationUS~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
             data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,])
#H2 EC & South (Spec A):
logith2j<-lm(DurationUS~SpecA+Southern,data=fulldata)
#H2 Full (Spec A):
logith2k<-lm(DurationUS~SpecA+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata)
#H2 EC & South (1945-Present):
logith2l<-lm(DurationUS~S.Pct.EC+Southern,
             data=fulldata[fulldata$StYear.x>1945,])
#H2 Full (1945-Present):
logith2m<-lm(DurationUS~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
             data=fulldata[fulldata$StYear.x>1945,])
#H2 EC & South (No World Wars):
logith2n<-lm(DurationUS~S.Pct.EC+Southern,
             data=fulldata[!fulldata$DispNum3%in%c(257,258,1774),])
#H2 Full (No World Wars):
logith2o<-lm(DurationUS~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
             data=fulldata[!fulldata$DispNum3%in%c(257,258,1774),])
#H1 Full Robust SE's:
logith1g_cov <- vcovHC(logith1g, type = "HC")
logith1g_robustse <- sqrt(diag(logith1g_cov))
#H2 Full Robust SE's:
logith2g_cov <- vcovHC(logith2g, type = "HC")
logith2g_robustse <- sqrt(diag(logith2g_cov))
#H1 Clustered (by pres-term) SE's:
logith1g_cse<-glm.cluster(data=fulldata, formula=UseOfForce~S.Pct.EC+Southern+
                            Year+term+Inherited+greatpower+superpower+FishDisp+
                            prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
                          cluster="President",family="binomial")
#H2 Clustered (by pres-term) SE's:
logith2g_cse<-lm.cluster(data=fulldata, formula=DurationUS~S.Pct.EC+Southern+
                           Year+term+Inherited+greatpower+superpower+FishDisp+
                           prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
                         cluster="President")
#H3 Just EC
logith3a<-glm.cluster(USWins~S.Pct.EC,data=fulldata,family="binomial",cluster="StLdTm")
#H3 Full
logith3b<-glm.cluster(USWins~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata,family="binomial",
                cluster="StLdTm")
#H3  EC & Southern
logith3c<-glm.cluster(USWins~S.Pct.EC+Southern,data=fulldata,family="binomial",cluster="StLdTm")
#H4 Just EC & Interaction
logith4a<-glm.cluster(data=fulldata, formula=UseOfForce~S.Pct.EC*rep,
                          cluster="President",family="binomial")
#H4 Full
logith4b<-glm.cluster(data=fulldata, formula=UseOfForce~S.Pct.EC*rep+Southern+
                            Year+term+Inherited+greatpower+superpower+FishDisp+
                            prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
                          cluster="President",family="binomial")

#4b) Paper & Full Result presentation
stargazer(logith1a,logith1c,logith1g,title="Logistic Regressions Testing Hypothesis 1",
          align=TRUE,dep.var.labels=c("Use of Force"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                                             "First Term","Inherited","Great Power","Super Power",
                                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                                             "Vet President","US Initiated"),
          no.space=TRUE,style="apsr")
stargazer(logith1b,logith1d,logith1e,logith1f,title="Additional Logit Combinations for Hypothesis 1",
          align=TRUE,dep.var.labels=c("Use of Force"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                          "First Term","Inherited","Whig","Republican","Vet President",
                          "Fishing Dispute","US Initiated","Great Power","Super Power",
                          "Past 5 Yrs","Past 10 Yrs","Log War Deaths","Pct Vet Elites"),
          no.space=TRUE,style="apsr")
stargazer(logith2a,logith2c,logith2g,title="OLS Regressions Testing Hypothesis 2",
          align=TRUE,dep.var.labels=c("MID Duration"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                          "First Term","Inherited","Great Power","Super Power",
                          "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                          "Log War Deaths","Pct Vet Elites","Whig","Republican",
                          "Vet President","US Initiated"),
          no.space=TRUE,style="apsr")
stargazer(logith2b,logith2d,logith2e,logith2f,title="Additional OLS Combinations for Hypothesis 2",
          align=TRUE,dep.var.labels=c("MID Duration"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                             "First Term","Inherited","Whig","Republican","Vet President",
                             "Fishing Dispute","US Initiated","Great Power","Super Power",
                             "Past 5 Yrs","Past 10 Yrs","Log War Deaths","Pct Vet Elites"),
          no.space=TRUE,style="apsr")
#Full Models w/ Robust SE's:
stargazer(logith1g, logith1g, se=list(NULL, logith1g_robustse),
          title="Hypothesis 1 Full Logit, Robust Standard Errors",
          column.labels=c("default","robust"), align=TRUE,dep.var.labels=c("MID Duration"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                             "First Term","Inherited","Great Power","Super Power",
                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                             "Vet President","US Initiated"),style="apsr")
#Clustered SE's added manually.
stargazer(logith2g, logith2g, se=list(NULL, logith2g_robustse),
          title="Hypothesis 2 Full OLS, Robust Standard Errors",
          column.labels=c("default","robust"), align=TRUE,dep.var.labels=c("MID Duration"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                             "First Term","Inherited","Great Power","Super Power",
                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                             "Vet President","US Initiated"),style="apsr")
#Full Models, No Cold War:
stargazer(logith1h,logith1i,logith2h,logith2i,title="Regressions Testing Hypotheses 1, 2 (No Cold War)",
          align=TRUE,dep.var.labels=c("Use of Force", "MID Duration"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                             "First Term","Inherited","Great Power","Super Power",
                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                             "Vet President","US Initiated"),
          no.space=TRUE,style="apsr")
#Full Models, 1945-Present:
stargazer(logith1l,logith1m,logith2l,logith2m,title="Regressions Testing Hypotheses 1, 2 (1945-Present)",
          align=TRUE,dep.var.labels=c("Use of Force", "MID Duration"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                             "First Term","Inherited","Great Power","Super Power",
                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                             "Vet President","US Initiated"),
          no.space=TRUE,style="apsr")
#Full Models, No World Wars:
stargazer(logith1n,logith1o,logith2n,logith2o,title="Regressions Testing Hypotheses 1, 2 (No World Wars)",
          align=TRUE,dep.var.labels=c("Use of Force", "MID Duration"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                             "First Term","Inherited","Great Power","Super Power",
                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                             "Vet President","US Initiated"),
          no.space=TRUE,style="apsr")
#Alternative Specifications:
stargazer(logith1j,logith1k,logith2j,logith2k,title="Regressions Testing Hypotheses 1, 2 (Alt Spec)",
          align=TRUE,dep.var.labels=c("Use of Force", "MID Duration"),
          covariate.labels=c("Pct S EC (Alt Spec)","Southern","Election Year",
                             "First Term","Inherited","Great Power","Super Power",
                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                             "Vet President","US Initiated"),
          no.space=TRUE,style="apsr")
#LASSO-determined model:
stargazer(logith1p,title="Regression Testing Hypotheses 1, LASSO-Determined",
          align=TRUE,dep.var.labels=c("Use of Force"),
          covariate.labels=c("Pct Southern EC","Inherited","Great Power","Super Power",
                             "Past 10 Yrs","Pct Vet Elites","Whig","Republican",
                             "US Initiated"),
          no.space=TRUE,style="apsr")

###5) The LASSOs
#Outline:
#5a) Run and print Betas for the H1 LASSO
#5b) Run and print Betas for the H2 LASSO

#5a) Run and print Betas for the H1 LASSO
h1_cvLasso<-cv.glmnet(x=LassoX,y=LassoY1,family="binomial",alpha=1,nfolds=5)
h1_Lasso<-glmnet(x=LassoX,y=LassoY1,family="binomial",alpha=1,lambda=h1_cvLasso$lambda.min)
h1_nzbetas<-h1_Lasso$beta[which(h1_Lasso$beta!=0),]
print(h1_nzbetas)

#5b) Run and print Betas for the H2 LASSO
h2_cvLasso<-cv.glmnet(x=LassoX,y=LassoY2,family="gaussian",alpha=1,nfolds=5)
h2_Lasso<-glmnet(x=LassoX,y=LassoY2,family="gaussian",alpha=1,lambda=h2_cvLasso$lambda.min)
h2_nzbetas<-h2_Lasso$beta[which(h2_Lasso$beta!=0),]
print(h2_nzbetas)

###6) The Permutation
#Outline:
#6a) Permutation for H1
#6b) Permutation for H2

#6a) Permutation for H1
#Permuting EC support
i<-NULL
work<-NULL
SSE<-NULL
for(i in 1:500){
  work<-rnorm(372,mean(fulldata$S.Pct.EC),sd(fulldata$S.Pct.EC))
  work<-as.data.frame(work)
  work$Imagined_Force<-rnorm(372,mean(fulldata$UseOfForce),sd(fulldata$UseOfForce))
  work$Imagined_Force<-ifelse(work$Imagined_Force>.5,1,0)
  glm_temp<-glm(Imagined_Force~work,data=work,family="binomial")
  work$Pred_Force<-predict(glm_temp,data.frame(work=work$work))
  work$Pred_Force<-ifelse(work$Pred_Force>0,1,0)
  SSE$SSE[i]<-sum((work$Imagined_Force-work$Pred_Force)^2)
}
SSE<-as.data.frame(SSE)
H1_SSE<-NULL
H1_SSE<-predict(logith1a,data.frame(S.Pct.EC=fulldata$S.Pct.EC))
H1_SSE<-as.data.frame(H1_SSE)
H1_SSE$pred<-ifelse(H1_SSE$H1_SSE>0,1,0)
H1_SSE<-sum((fulldata$UseOfForce-H1_SSE$pred)^2)
sum(SSE$SSE>H1_SSE)/length(SSE$SSE)
#Permuting Southern
i<-NULL
work<-NULL
SSE<-NULL
for(i in 1:500){
  work<-rnorm(372,mean(fulldata$Southern),sd(fulldata$Southern))
  work<-as.data.frame(work)
  work$work<-ifelse(work$work>.5,1,0)
  work$Imagined_Force<-rnorm(372,mean(fulldata$UseOfForce),sd(fulldata$UseOfForce))
  work$Imagined_Force<-ifelse(work$Imagined_Force>.5,1,0)
  glm_temp<-glm(Imagined_Force~work,data=work,family="binomial")
  work$Pred_Force<-predict(glm_temp,data.frame(work=work$work))
  work$Pred_Force<-ifelse(work$Pred_Force>0,1,0)
  SSE$SSE[i]<-sum((work$Imagined_Force-work$Pred_Force)^2)
}
SSE<-as.data.frame(SSE)
H1_SSE<-NULL
H1_SSE<-predict(logith1b,data.frame(Southern=fulldata$Southern))
H1_SSE<-as.data.frame(H1_SSE)
H1_SSE$pred<-ifelse(H1_SSE$H1_SSE>0,1,0)
H1_SSE<-sum((fulldata$UseOfForce-H1_SSE$pred)^2)
sum(SSE$SSE>H1_SSE)/length(SSE$SSE)

#6b) Permutation for H2
#Permuting EC support
i<-NULL
work<-NULL
SSE<-NULL
for(i in 1:500){
  work<-rnorm(372,mean(fulldata$S.Pct.EC),sd(fulldata$S.Pct.EC))
  work<-as.data.frame(work)
  work$Imagined_Length<-rnorm(372,mean(fulldata$DurationUS),sd(fulldata$DurationUS))
  lm_temp<-lm(Imagined_Length~work,data=work)
  work$Pred_Length<-predict(lm_temp,data.frame(work=work$work))
  SSE$SSE[i]<-sum((work$Imagined_Length-work$Pred_Length)^2)
}
SSE<-as.data.frame(SSE)
H2_SSE<-NULL
H2_SSE<-predict(logith2a,data.frame(S.Pct.EC=fulldata$S.Pct.EC))
H2_SSE<-as.data.frame(H2_SSE)
H2_SSE<-sum((fulldata$DurationUS-H2_SSE$H2_SSE)^2)
sum(SSE$SSE>H2_SSE)/length(SSE$SSE)
#Permuting Southern
i<-NULL
work<-NULL
SSE<-NULL
for(i in 1:500){
  work<-rnorm(372,mean(fulldata$Southern),sd(fulldata$Southern))
  work<-as.data.frame(work)
  work$work<-ifelse(work$work>.5,1,0)
  work$Imagined_Length<-rnorm(372,mean(fulldata$DurationUS),sd(fulldata$DurationUS))
  lm_temp<-lm(Imagined_Length~work,data=work)
  work$Pred_Length<-predict(lm_temp,data.frame(work=work$work))
  SSE$SSE[i]<-sum((work$Imagined_Length-work$Pred_Length)^2)
}
SSE<-as.data.frame(SSE)
H2_SSE<-NULL
H2_SSE<-predict(logith2b,data.frame(Southern=fulldata$Southern))
H2_SSE<-as.data.frame(H2_SSE)
H2_SSE<-sum((fulldata$DurationUS-H2_SSE$H2_SSE)^2)
sum(SSE$SSE>H2_SSE)/length(SSE$SSE)

###7) Figures
#Outline:
#7a) Diff Between Southern-ness and Electoral Dependence
#7b) Figure of Presidents
#7c) Figure of MIDs Over Time
#7d) Marginal Effects of Southern Support

#7a) Diff Between Southern-ness and Electoral Dependence:
ggplot(data=sdata, aes(x=as.character(Southern), y=S.Pct.EC)) +
  geom_point()+
  labs(x="Presidential Southern-ness", y="% of E.C. Vote from South")+
  scale_x_discrete(labels=c("Non-Southern","Southern"))+
  coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("ecdepend.png", width = 8, height = 3)

#7b) Figure of Presidents:
ggplot(data=sdata, aes(x=S.Pct.EC, y=Year,shape=S.Text)) +
  geom_point()+
  xlim(0,.8)+
  labs(x="% of E.C. Vote from South", y="Year Elected")+
  geom_text(aes(label=StLdTm),hjust=0, vjust=0)+
  scale_shape_discrete(name="Southern-ness")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("presidents.png",width=6,height=8)

#7c) MIDs Over Time:
ggplot(fulldata, aes(StYear.y)) +
  geom_density(adjust = 1)+
  labs(x="Start Year")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("mids.png",width=6,height=4)

#7d) Marginal Effects of Southern Support
me.data <- fulldata %>% select(UseOfForce,S.Pct.EC,Southern)
me.data$Southern = ifelse(me.data$Southern==1,"Southern","Non\nSouthern")

me.logit<-glm(UseOfForce~S.Pct.EC+Southern,data=me.data,family="binomial")

sj1<-sjPlot::plot_model(
  model = me.logit,
  type="pred",
  terms = c("S.Pct.EC [all]"),
  show.legend = T
) +sjPlot::theme_sjplot()+
  labs(title="",
       y="Prob. of Using Force",
       x="% Southern Support")
sj2<-sjPlot::plot_model(
  model = me.logit,
  type="pred",
  terms = c("S.Pct.EC [all]","Southern"),
  show.legend = T
) +sjPlot::theme_sjplot()+
  labs(title="",
       y="Prob. of Using Force",
       x="% Southern Support",
       color="Presidential\nHeritage")
ggsave("MEplot.png", gridExtra::arrangeGrob(sj1,sj2,nrow = 1,ncol=2),width=10,height=4)



ggsave("JT_SJ1.png",sj1,width=6,height=4)

###8) Outside Survey Analysis; All individual survey datasets available from the Roper iPoll databank.
#Outline:
#8a) CCES Data
#8b) TISS Data
#8c) Yank 1995 Data
#8d) Yank 1989 Data
#8e) Yank 1993 Data

#8a) CCES:
#The following analyzes the 2016 CCES Data:
load("CCES16_Common_OUTPUT_Jul2017_VV.RData")
cces<-x
rm(x)
#Creating the Southern var:
cces$Southern<-0
cces$Southern<-ifelse(cces$inputstate=="Alabama",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Arizona",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Arkansas",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Florida",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Georgia",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Kentucky",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Louisiana",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Mississippi",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="New Mexico",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="North Carolina",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Oklahoma",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="South Carolina",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Tennessee",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Texas",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="Virginia",1,cces$Southern)
cces$Southern<-ifelse(cces$inputstate=="West Virginia",1,cces$Southern)
cces$SWM<-ifelse((cces$Southern==1 & cces$gender=="Male" & cces$race=="White"),1,0)
#Now testing for differences b/w Southerners and non/Southerners:
#Importance of defense spending:
cces$CC16_301f_num<-ifelse(cces$CC16_301f=="Very High Importance",1,0)
t.test(cces$SWM,cces$CC16_301f_num)
t.test(cces$SWM[cces$Southern==1],cces$CC16_301f_num[cces$Southern==1])
t.test(cces$SWM[cces$gender=="Male"],cces$CC16_301f_num[cces$gender=="Male"])
t.test(cces$SWM[cces$race=="White"],cces$CC16_301f_num[cces$race=="White"])

table(cces$CC16_301f)
table(cces$CC16_301f[cces$Southern==1])
table(cces$CC16_301f[cces$SWM==1])
#Importance of nat'l security:
cces$CC16_301k_num<-ifelse(cces$CC16_301k=="Very High Importance",1,0)
t.test(cces$SWM,cces$CC16_301k_num)
t.test(cces$SWM[cces$Southern==1],cces$CC16_301k_num[cces$Southern==1])
t.test(cces$SWM[cces$gender=="Male"],cces$CC16_301k_num[cces$gender=="Male"])
t.test(cces$SWM[cces$race=="White"],cces$CC16_301k_num[cces$race=="White"])

table(cces$CC16_301k)
table(cces$CC16_301k[cces$Southern==1])
table(cces$CC16_301k[cces$SWM==1])
#Sending lots of troops to fight ISIS:
cces$CC16_312_7_num<-ifelse(cces$CC16_312_7=="Yes",1,0)
t.test(cces$SWM,cces$CC16_312_7_num)
t.test(cces$SWM[cces$Southern==1],cces$CC16_312_7_num[cces$Southern==1])
t.test(cces$SWM[cces$gender=="Male"],cces$CC16_312_7_num[cces$gender=="Male"])
t.test(cces$SWM[cces$race=="White"],cces$CC16_312_7_num[cces$race=="White"])

table(cces$CC16_312_7[cces$Southern==0])
table(cces$CC16_312_7[cces$Southern==1])
table(cces$CC16_312_7[cces$SWM==1])
#Go to war to protect allies:
cces$CC16_414_5_num<-ifelse(cces$CC16_414_7=="Yes",1,0)
t.test(cces$SWM,cces$CC16_414_5_num)
t.test(cces$SWM[cces$Southern==1],cces$CC16_414_5_num[cces$Southern==1])
t.test(cces$SWM[cces$gender=="Male"],cces$CC16_414_5_num[cces$gender=="Male"])
t.test(cces$SWM[cces$race=="White"],cces$CC16_414_5_num[cces$race=="White"])

table(cces$CC16_414_5[cces$Southern==0])
table(cces$CC16_414_5[cces$Southern==1])
table(cces$CC16_414_5[cces$SWM==1])

table(cces$CC16_414_5[cces$pid3=="Democrat"])/sum(cces$pid3=="Democrat")
table(cces$CC16_414_5[cces$pid3=="Republican"])/sum(cces$pid3=="Republican")

#8b) TISS
#The following analyzes the TISS's Survey on the Military data:
load("depository_tiss_spss.RData")
tiss<-x
rm(x)
#Creating the SWM var:
tiss$SWM<-ifelse((tiss$Q79=="SOUTH" & tiss$Q63=="MALE" & tiss$Q80=="WHITE"),1,0)
#Now testing the importance of maintaining US military superiority:
tiss$Q01J_num<-ifelse(tiss$Q01J=="VERY IMPORT",1,0)
t.test(tiss$SWM,tiss$Q01J_num)
t.test(tiss$SWM[tiss$Q79=="SOUTH"],tiss$Q01J_num[tiss$Q79=="SOUTH"])
t.test(tiss$SWM[tiss$Q63=="MALE"],tiss$Q01J_num[tiss$Q63=="MALE"])
t.test(tiss$SWM[tiss$Q80=="WHITE"],tiss$Q01J_num[tiss$Q80=="WHITE"])

table(tiss$Q01J)
table(tiss$Q01J[tiss$Q79=="SOUTH"])
table(tiss$Q01J[tiss$SWM==1])
#Support missile strikes in response to terror:
tiss$Q06_num<-ifelse(tiss$Q06=="AGREE STRONGLY",1,0)
t.test(tiss$SWM,tiss$Q06_num)
t.test(tiss$SWM[tiss$Q79=="SOUTH"],tiss$Q06_num[tiss$Q79=="SOUTH"])
t.test(tiss$SWM[tiss$Q63=="MALE"],tiss$Q06_num[tiss$Q63=="MALE"])
t.test(tiss$SWM[tiss$Q80=="WHITE"],tiss$Q06_num[tiss$Q80=="WHITE"])

table(tiss$Q06[tiss$Q79!="SOUTH"])/sum(is.na(tiss$Q06[tiss$Q79!="SOUTH"])==F)
table(tiss$Q06[tiss$Q79=="SOUTH"])
table(tiss$Q06[tiss$SWM==1])

table(tiss$Q06[tiss$Q76=="DEMOCRAT"])/sum(is.na(tiss$Q06[tiss$Q76=="DEMOCRAT"])==F)
table(tiss$Q06[tiss$Q76=="REPUBLICAN"])/sum(is.na(tiss$Q06[tiss$Q76=="REPUBLICAN"])==F)

#8c) Yank 1995
#Bringing in the data:
yank1<-as.data.set(spss.portable.file('USYANK.por'))
yank1<-as.data.frame(yank1)
#Creating the Southern var:
yank1$Southern<-0
yank1$Southern<-ifelse(yank1$state=="Alabama",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Arizona",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Arkansas",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Florida",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Georgia",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Kentucky",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Louisiana",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Mississippi",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="New Mexico",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="North Carolina",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Oklahoma",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="South Carolina",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Tennessee",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Texas",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="Virginia",1,yank1$Southern)
yank1$Southern<-ifelse(yank1$state=="West Virginia",1,yank1$Southern)
yank1$SWM<-ifelse((yank1$Southern==1 & yank1$sex=="Male" & yank1$race=="White"),1,0)
#Should the US retaliate against the Serbs?
yank1$q2b_num<-ifelse(yank1$q2b=="Should attack",1,0)

t.test(yank1$SWM[yank1$q2b!="Not sure"],yank1$q2b_num[yank1$q2b!="Not sure"])
t.test(yank1$SWM[yank1$Southern==1&yank1$q2b!="Not sure"],
       yank1$q2b_num[yank1$Southern==1&yank1$q2b!="Not sure"])
t.test(yank1$SWM[yank1$sex=="Male"&yank1$q2b!="Not sure"],
       yank1$q2b_num[yank1$sex=="Male"&yank1$q2b!="Not sure"])
t.test(yank1$SWM[yank1$race=="White"&yank1$q2b!="Not sure"],
       yank1$Q2b_num[yank1$race=="White"&yank1$q2b!="Not sure"])

table(yank1$q2b[yank1$Southern==0])/sum(yank1$Southern==0)
table(yank1$q2b[yank1$Southern==1])/sum(yank1$Southern==1)
table(yank1$q2b[yank1$SWM==1])/sum(yank1$SWM==1)

table(yank1$q2b[yank1$partyid=="Democrat"])/sum(yank1$partyid=="Democrat")
table(yank1$q2b[yank1$partyid=="Republican"])/sum(yank1$partyid=="Republican")

#8d) Yank 1989
#Bringing in the data:
yank2<-as.data.set(spss.portable.file('USYANK2.por'))
yank2<-as.data.frame(yank2)
#Creating the Southern var:
yank2$Southern<-0
yank2$Southern<-ifelse(yank2$state=="Alabama",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Arizona",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Arkansas",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Florida",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Georgia",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Kentucky",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Louisiana",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Mississippi",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="New Mexico",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="North Carolina",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Oklahoma",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="South Carolina",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Tennessee",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Texas",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="Virginia",1,yank2$Southern)
yank2$Southern<-ifelse(yank2$state=="West Virginia",1,yank2$Southern)
yank2$SWM<-ifelse((yank2$Southern==1 & yank2$sex=="Male" & yank2$race=="White"),1,0)
#Should the US retaliate for the Pan Am bombing?
yank2$q11_num<-ifelse(yank2$q11=="Yes",1,0)

t.test(yank2$SWM[yank2$q11!="Not sure"],yank2$q11_num[yank2$q11!="Not sure"])
t.test(yank2$SWM[yank2$Southern==1&yank2$q11!="Not sure"],
       yank2$q11_num[yank2$Southern==1&yank2$q11!="Not sure"])
t.test(yank2$SWM[yank2$sex=="Male"&yank2$q11!="Not sure"],
       yank2$q11_num[yank2$sex=="Male"&yank2$q11!="Not sure"])
t.test(yank2$SWM[yank2$race=="White"&yank2$q11!="Not sure"],
       yank2$Q11_num[yank2$race=="White"&yank2$q11!="Not sure"])

table(yank2$q11[yank2$Southern==0])/sum(yank2$Southern==0)
table(yank2$q11[yank2$Southern==1])/sum(yank2$Southern==1)
table(yank2$q11[yank2$SWM==1])/sum(yank2$SWM==1)

table(yank2$q11[yank2$partyid=="Democrat"])/sum(is.na(yank2$q11[yank2$partyid=="Democrat"])==F)
table(yank2$q11[yank2$partyid=="Republican"])/
  sum(is.na(yank2$q11[yank2$partyid=="Republican"])==F)

#Will retaliation work?
yank2$q12_num<-ifelse(yank2$q12=="Less likley",1,0)

t.test(yank2$SWM[yank2$q12!="Not sure"],yank2$q12_num[yank2$q12!="Not sure"])
t.test(yank2$SWM[yank2$Southern==1&yank2$q12!="Not sure"],
       yank2$q12_num[yank2$Southern==1&yank2$q12!="Not sure"])
t.test(yank2$SWM[yank2$sex=="Male"&yank2$q12!="Not sure"],
       yank2$q12_num[yank2$sex=="Male"&yank2$q12!="Not sure"])
t.test(yank2$SWM[yank2$race=="White"&yank2$q12!="Not sure"],
       yank2$Q12_num[yank2$race=="White"&yank2$q12!="Not sure"])

table(yank2$q12[yank2$Southern==0])/sum(yank2$Southern==0)
table(yank2$q12[yank2$Southern==1])/sum(yank2$Southern==1)
table(yank2$q12[yank2$SWM==1])/sum(yank2$SWM==1)

table(yank2$q12[yank2$partyid=="Democrat"])/sum(is.na(yank2$q12[yank2$partyid=="Democrat"])==F)
table(yank2$q12[yank2$partyid=="Republican"])/
  sum(is.na(yank2$q12[yank2$partyid=="Republican"])==F)


#8e) Yank 1993
#Bringing in the data:
yank3<-as.data.set(spss.portable.file('USYANK3.por'))
yank3<-as.data.frame(yank3)
#Creating the Southern var:
yank3$Southern<-0
yank3$Southern<-ifelse(yank3$state=="Alabama",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Arizona",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Arkansas",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Florida",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Georgia",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Kentucky",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Louisiana",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Mississippi",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="New Mexico",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="North Carolina",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Oklahoma",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="South Carolina",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Tennessee",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Texas",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="Virginia",1,yank3$Southern)
yank3$Southern<-ifelse(yank3$state=="West Virginia",1,yank3$Southern)
yank3$SWM<-ifelse((yank3$Southern==1 & yank3$sex=="Male" & yank3$race=="White"),1,0)
#Should maintaining respect be an important goal?
yank3$q15e_num<-ifelse(yank3$q15e=="Should be",1,0)

t.test(yank3$SWM[yank3$q15e!="Not sure"],yank3$q15e_num[yank3$q15e!="Not sure"])
t.test(yank3$SWM[yank3$Southern==1&yank3$q15e!="Not sure"],
       yank3$q15e_num[yank3$Southern==1&yank3$q15e!="Not sure"])
t.test(yank3$SWM[yank3$sex=="Male"&yank3$q15e!="Not sure"],
       yank3$q15e_num[yank3$sex=="Male"&yank3$q15e!="Not sure"])
t.test(yank3$SWM[yank3$race=="White"&yank3$q15e!="Not sure"],
       yank3$Q15e_num[yank3$race=="White"&yank3$q15e!="Not sure"])

table(yank3$q15e[yank3$Southern==0])/sum(yank3$Southern==0)
table(yank3$q15e[yank3$Southern==1])/sum(yank3$Southern==1)
table(yank3$q15e[yank3$SWM==1])/sum(yank3$SWM==1)

table(yank3$q15e[yank3$partyid=="Democrat"])/sum(yank3$partyid=="Democrat")
table(yank3$q15e[yank3$partyid=="Republican"])/sum(yank3$partyid=="Republican")


###9) Appendix D: Alternative Research Design
#Outline:
#9a) Creating the buckets
#9b) Running the t-tests

#9a) Creating the buckets:
#See Appendix D for meaningul interpretation of letters
fulldata$bucket<-NA
fulldata$bucket<-ifelse(fulldata$Southern==1 & fulldata$S.Pct.EC>mean(fulldata$S.Pct.EC),
                     "A",fulldata$bucket)
fulldata$bucket<-ifelse(fulldata$Southern==0 & fulldata$S.Pct.EC>mean(fulldata$S.Pct.EC),
                     "B",fulldata$bucket)
fulldata$bucket<-ifelse(fulldata$Southern==1 & fulldata$S.Pct.EC<mean(fulldata$S.Pct.EC),
                     "C",fulldata$bucket)
fulldata$bucket<-ifelse(fulldata$Southern==0 & fulldata$S.Pct.EC<mean(fulldata$S.Pct.EC),
                     "D",fulldata$bucket)
#9b) Running the t-tests & populating the tables
#For Use of Force
t.test(fulldata$UseOfForce[fulldata$bucket=="A"|fulldata$bucket=="B"]~
         fulldata$bucket[fulldata$bucket=="A"|fulldata$bucket=="B"])
t.test(fulldata$UseOfForce[fulldata$bucket=="C"|fulldata$bucket=="D"]~
         fulldata$bucket[fulldata$bucket=="C"|fulldata$bucket=="D"])
t.test(fulldata$UseOfForce[fulldata$bucket=="A"|fulldata$bucket=="C"]~
         fulldata$bucket[fulldata$bucket=="A"|fulldata$bucket=="C"])
t.test(fulldata$UseOfForce[fulldata$bucket=="B"|fulldata$bucket=="D"]~
         fulldata$bucket[fulldata$bucket=="B"|fulldata$bucket=="D"])
sqrt(var(fulldata$UseOfForce[fulldata$bucket=="A"])/
       length(fulldata$UseOfForce[fulldata$bucket=="A"]))
sqrt(var(fulldata$UseOfForce[fulldata$bucket=="B"])/
       length(fulldata$UseOfForce[fulldata$bucket=="B"]))
sqrt(var(fulldata$UseOfForce[fulldata$bucket=="C"])/
       length(fulldata$UseOfForce[fulldata$bucket=="C"]))
sqrt(var(fulldata$UseOfForce[fulldata$bucket=="D"])/
       length(fulldata$UseOfForce[fulldata$bucket=="D"]))
#And Conflict Duration:
t.test(fulldata$DurationUS[fulldata$bucket=="A"|fulldata$bucket=="B"]~
         fulldata$bucket[fulldata$bucket=="A"|fulldata$bucket=="B"])
t.test(fulldata$DurationUS[fulldata$bucket=="C"|fulldata$bucket=="D"]~
         fulldata$bucket[fulldata$bucket=="C"|fulldata$bucket=="D"])
t.test(fulldata$DurationUS[fulldata$bucket=="A"|fulldata$bucket=="C"]~
         fulldata$bucket[fulldata$bucket=="A"|fulldata$bucket=="C"])
t.test(fulldata$DurationUS[fulldata$bucket=="B"|fulldata$bucket=="D"]~
         fulldata$bucket[fulldata$bucket=="B"|fulldata$bucket=="D"])
sqrt(var(fulldata$DurationUS[fulldata$bucket=="A"])/
       length(fulldata$DurationUS[fulldata$bucket=="A"]))
sqrt(var(fulldata$DurationUS[fulldata$bucket=="B"])/
       length(fulldata$DurationUS[fulldata$bucket=="B"]))
sqrt(var(fulldata$DurationUS[fulldata$bucket=="C"])/
       length(fulldata$DurationUS[fulldata$bucket=="C"]))
sqrt(var(fulldata$DurationUS[fulldata$bucket=="D"])/
       length(fulldata$DurationUS[fulldata$bucket=="D"]))



###10) Southern Realignment Design
#Outline:
#10a) Creating the buckets
#10b) Running the t-tests

#10a) Creating the buckets:
#See Paper for meaningul interpretation of letters
fulldata$SoRe<-NA
fulldata$SoRe<-ifelse(fulldata$President %in% c("Roosevelt F", "Truman","Kennedy")|
                        fulldata$StLdTm=="Johnson L 1", "a",fulldata$SoRe)
fulldata$SoRe<-ifelse(fulldata$President %in% c("Harding", "Coolidge", "Hoover",
                                                "Eisenhower"),"b",fulldata$SoRe)
fulldata$SoRe<-ifelse(fulldata$President %in% c("Carter","Clinton")|
                        fulldata$StLdTm=="Johnson L 2", "c", fulldata$SoRe)
fulldata$SoRe<-ifelse(fulldata$President %in% c("Nixon", "Ford", "Reagan","Bush Sr",
                                                "Bush Jr"), "d", fulldata$SoRe)
#10b) Running the t-tests & populating the tables
#For Use of Force
t.test(fulldata$UseOfForce[fulldata$SoRe=="a"|fulldata$SoRe=="b"]~
         fulldata$SoRe[fulldata$SoRe=="a"|fulldata$SoRe=="b"])
t.test(fulldata$UseOfForce[fulldata$SoRe=="c"|fulldata$SoRe=="d"]~
         fulldata$SoRe[fulldata$SoRe=="c"|fulldata$SoRe=="d"])
t.test(fulldata$UseOfForce[fulldata$SoRe=="a"|fulldata$SoRe=="c"]~
         fulldata$SoRe[fulldata$SoRe=="a"|fulldata$SoRe=="c"])
t.test(fulldata$UseOfForce[fulldata$SoRe=="b"|fulldata$SoRe=="d"]~
         fulldata$SoRe[fulldata$SoRe=="b"|fulldata$SoRe=="d"])
sqrt(var(fulldata$UseOfForce[fulldata$SoRe=="a"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="a"))
sqrt(var(fulldata$UseOfForce[fulldata$SoRe=="b"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="b"))
sqrt(var(fulldata$UseOfForce[fulldata$SoRe=="c"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="c"))
sqrt(var(fulldata$UseOfForce[fulldata$SoRe=="d"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="d"))

#And Conflict Duration:
t.test(fulldata$DurationUS[fulldata$SoRe=="a"|fulldata$SoRe=="b"]~
         fulldata$SoRe[fulldata$SoRe=="a"|fulldata$SoRe=="b"])
t.test(fulldata$DurationUS[fulldata$SoRe=="c"|fulldata$SoRe=="d"]~
         fulldata$SoRe[fulldata$SoRe=="c"|fulldata$SoRe=="d"])
t.test(fulldata$DurationUS[fulldata$SoRe=="a"|fulldata$SoRe=="c"]~
         fulldata$SoRe[fulldata$SoRe=="a"|fulldata$SoRe=="c"])
t.test(fulldata$DurationUS[fulldata$SoRe=="b"|fulldata$SoRe=="d"]~
         fulldata$SoRe[fulldata$SoRe=="b"|fulldata$SoRe=="d"])
sqrt(var(fulldata$DurationUS[fulldata$SoRe=="a"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="a"))
sqrt(var(fulldata$DurationUS[fulldata$SoRe=="b"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="b"))
sqrt(var(fulldata$DurationUS[fulldata$SoRe=="c"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="c"))
sqrt(var(fulldata$DurationUS[fulldata$SoRe=="d"&is.na(fulldata$SoRe)==F])/
       sum(fulldata$SoRe[is.na(fulldata$SoRe)==F]=="d"))

#Figure for propensity to use force:
SoReFig<-fulldata[fulldata$SoRe%in%c("a","b","c","d"),]
SoReFig<-SoReFig %>% group_by(StLdTm) %>% summarize(meanUoF=mean(UseOfForce),
                                                    year=first(Year),
                                                    party=first(Party))
ggplot(data=SoReFig,aes(x=year,y=meanUoF))+
  geom_col(aes(fill=party))+
  scale_fill_manual(values=c("blue", "red"))+
  geom_vline(xintercept = 1964)+
  labs(x="Year / Term", y="Propensity to Use Force",
       title="Presidents' Propensity to Use Force")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("SoReFig.png", width = 8, height = 4)



###11) Looking at Changes b/w Terms
#Outline:
#11a) Creating the dataset
#11b) Plotting the Changes
#11c) Getting Mean Changes

#11a) Creating the dataset
#Making a new dataset to use here:
pairs <- c(1,2,3,4,6,7,14,15,17,18,21,23,24,25,26,27,29,30,34,35,38,39,40,41,43,44,
           45,46,49,50,52,53,54,55)
deltadata <- unique(sdata$StLdTm)[pairs]
deltadata<-as.data.frame(deltadata)
names(deltadata)[1]<-"StLdTm"
temp <- fulldata %>% group_by(StLdTm) %>% summarize(meanUoF = mean(UseOfForce),
                                                    meanLoC = mean(DurationUS),
                                                    S.Pct.EC = first(S.Pct.EC),
                                                    Southern = first(Southern))
library(plyr)
deltadata <- join(deltadata,temp,by="StLdTm",type="left")
detach("package:plyr", unload=TRUE)
rm(pairs,temp)
no_na <- c(3,4,9,10,15,16,17,18,19,20,23,24,27,28,29,30,31,32,33,34)
deltadata <- deltadata[no_na,]
t1<-c(1,3,5,7,9,11,13,15,17,19)
t2<-c(2,4,6,8,10,12,14,16,18,20)
changedata<-NULL
changedata$StLdTm<-c("Monroe","Grant","Roosevelt T","Wilson","Roosevelt F","Eisenhower",
                     "Nixon","Reagan","Clinton","Bush Jr")
changedata<-as.data.frame(changedata)
changedata$deltaUoF<-(deltadata$meanUoF[t2]-deltadata$meanUoF[t1])
changedata$deltaLoC<-(deltadata$meanLoC[t2]-deltadata$meanLoC[t1])
changedata$deltaSEC<-(deltadata$S.Pct.EC[t2]-deltadata$S.Pct.EC[t1])
changedata$deltaSou<-(deltadata$Southern[t2])
rm(deltadata,no_na,t1,t2)

#11b) Plotting the Changes
ggplot(data=changedata, aes(x=deltaSEC,y=deltaUoF))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Change in S. Dep.", y="Change in Prop. to Use Force",
       title="Changes in Support, Use of Force")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave("deltas.png", width = 4, height = 6)

#11c) Getting Mean Changes
mean(changedata$deltaUoF[changedata$deltaSEC>=0])
mean(changedata$deltaUoF[changedata$deltaSEC<=0])


###12) Replicating Main Findings w/ Limited (CSA) South
#Outline:
#12a) Running the Main Regressions

#12a) Running the Main Regressions
#H1 Just EC (in-text):
csa.logith1a<-glm(UseOfForce~S.Pct.EC.CSA,data=fulldata,family="binomial")
#H1 EC & South (in-text):
csa.logith1c<-glm(UseOfForce~S.Pct.EC.CSA+Southern,data=fulldata,family="binomial")
#H1 Full (in-text):
csa.logith1g<-glm(UseOfForce~S.Pct.EC.CSA+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata,family="binomial")
#H1 Full (No Cold War):
csa.logith1i<-glm(UseOfForce~S.Pct.EC.CSA+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
              data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,],family="binomial")
#H2 Just EC (in-text?):
csa.logith2a<-lm(DurationUS~S.Pct.EC.CSA,data=fulldata)
#H2 EC & South (in-text):
csa.logith2c<-lm(DurationUS~S.Pct.EC.CSA+Southern,data=fulldata)
#H2 Full (in-text):
csa.logith2g<-lm(DurationUS~S.Pct.EC.CSA+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata)
#H2 Full (No Cold War):
csa.logith2i<-lm(DurationUS~S.Pct.EC.CSA+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
             data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,])
#Clustered (by pres-term) SE's:
csa.logith1g_cse<-glm.cluster(data=fulldata, formula=UseOfForce~S.Pct.EC.CSA+Southern+
                            Year+term+Inherited+greatpower+superpower+FishDisp+
                            prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
                          cluster="President",family="binomial")
csa.logith2g_cse<-lm.cluster(data=fulldata, formula=DurationUS~S.Pct.EC.CSA+Southern+
                           Year+term+Inherited+greatpower+superpower+FishDisp+
                           prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
                         cluster="President")
#H1 Displayed for Appendix
stargazer(csa.logith1a,csa.logith1c,csa.logith1g,title="Logistic Regressions Testing Hypothesis 1 (Limited Definition of South)",
          align=TRUE,dep.var.labels=c("Use of Force"),
          covariate.labels=c("Pct Southern EC","Southern","Election Year",
                             "First Term","Inherited","Great Power","Super Power",
                             "Fishing Dispute","Past 5 Yrs","Past 10 Yrs",
                             "Log War Deaths","Pct Vet Elites","Whig","Republican",
                             "Vet President","US Initiated"),
          no.space=TRUE,style="apsr")



###13) Predicted Probability Estimates
#Outline:
#13a) Various Predictions

#13a) Various Predictions:
#First, the super-limited model:
pred.data.1<-data.frame("S.Pct.EC"=c(quantile(sdata$S.Pct.EC)[c(2,4)]))
pred.1<-as.data.frame(predict(logith1a,pred.data.1,type="response"))
#And the model w/ Southern-ness:
pred.data.2<-data.frame("S.Pct.EC"=rep(c(quantile(sdata$S.Pct.EC)[c(2,4)]),2),
                        "Southern"=c(0,0,1,1))
pred.2<-as.data.frame(predict(logith1c,pred.data.2,type="response"))


###14) Extension Using ICB Data


#14a) Load, clean ICB Data:
#Loading in the ICB Data:
icbdata <- read.delim("ICBdata.csv", header=T)
#Trim to just conflicts where USA was responsive actor:
icbdata<-filter(icbdata,actor=="USA")

#14b) Create necessary variables & merge in Pres data:
#Creating a variable in the ICB data for which Pres (and term) was in charge when dispute was triggered:
icbdata$StLeader<-"Nobody"
icbdata$StTerm<-1
icbdata$StLeader<-ifelse(icbdata$systrgyr>1932, "Roosevelt F", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1936 & icbdata$systrgyr<=1940, 2, icbdata$StTerm)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1940 & icbdata$systrgyr<=1944, 3, icbdata$StTerm)
icbdata$StTerm<-ifelse(((icbdata$systrgyr==1945 & icbdata$systrgmo<4) |
                       (icbdata$systrgyr==1945 & icbdata$systrgmo==4 &
                          icbdata$StDay.x<12)), 4, icbdata$StTerm)
icbdata$StLeader<-ifelse((icbdata$systrgyr>1945 | (icbdata$systrgyr==1945 & icbdata$systrgmo>4) |
                         (icbdata$systrgyr==1945 & icbdata$systrgmo==4 & icbdata$StDay.x>12)),
                      "Wilson", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1948 & icbdata$systrgyr<=1948, 2, icbdata$StTerm)
icbdata$StLeader<-ifelse(icbdata$systrgyr>1952, "Eisenhower", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1956 & icbdata$systrgyr<=1960, 2, icbdata$StTerm)
icbdata$StLeader<-ifelse(icbdata$systrgyr>1960, "Kennedy", icbdata$StLeader)
icbdata$StLeader<-ifelse((icbdata$systrgyr>1963 | (icbdata$systrgyr==1963 & icbdata$systrgmo>11) |
                         (icbdata$systrgyr==1963 & icbdata$systrgmo==11 & icbdata$StDay.x>22)),
                      "Johnson L", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1964 & icbdata$systrgyr<=1964, 2, icbdata$StTerm)
icbdata$StLeader<-ifelse(icbdata$systrgyr>1968, "Nixon", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1972 &
                      (icbdata$systrgyr<1974 |
                         (icbdata$systrgyr==1974 & icbdata$systrgmo<8) |
                         (icbdata$systrgyr==1974 & icbdata$systrgmo==8 & icbdata$StDay.x<=9)),
                    2, icbdata$StTerm)
icbdata$StLeader<-ifelse((icbdata$systrgyr>1974 | (icbdata$systrgyr==1974 & icbdata$systrgmo>8) |
                         (icbdata$systrgyr==1974 & icbdata$systrgmo==8 & icbdata$StDay.x>9)),
                      "Ford", icbdata$StLeader)
icbdata$StLeader<-ifelse(icbdata$systrgyr>1976, "Carter", icbdata$StLeader)
icbdata$StLeader<-ifelse(icbdata$systrgyr>1980, "Reagan", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1984 & icbdata$systrgyr<=1988, 2, icbdata$StTerm)
icbdata$StLeader<-ifelse(icbdata$systrgyr>1988, "Bush Sr", icbdata$StLeader)
icbdata$StLeader<-ifelse(icbdata$systrgyr>1992, "Clinton", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>1996 & icbdata$systrgyr<=2000, 2, icbdata$StTerm)
icbdata$StLeader<-ifelse(icbdata$systrgyr>2000, "Bush Jr", icbdata$StLeader)
icbdata$StTerm<-ifelse(icbdata$systrgyr>2004 & icbdata$systrgyr<=2008, 2, icbdata$StTerm)
icbdata$StLeader<-ifelse(icbdata$systrgyr>2008, "Obama", icbdata$StLeader)
#Merging in the president-level data:
icbdata$StLdTm<-paste(icbdata$StLeader,icbdata$StTerm)
icbdata<-merge(sdata,icbdata, by="StLdTm",all=F)

#14c) Creating the outcome and explanatory variables
# Main outcome variable: Did America Respond w/ Force?
icbdata<-icbdata %>% dplyr::mutate(RespForce=dplyr::recode(majres,
                                                            "2"=0,"3"=0,"4"=0,"5"=0,"6"=0,"7"=0,
                                                            "8"=1,"9"=1))
# Important control: Was the trigger non-violent?
icbdata<-icbdata %>% dplyr::mutate(triggerNV=dplyr::recode(triggr,
                                                           "1"=1,"2"=1,"3"=1,"4"=1,"5"=1,"7"=1,
                                                           "8"=0,"9"=0))
# Outcome measuring duration: "trgterra"
# Variable measuring whether the US had been involved in a conflict with the other nation w/in the last 10 years: "repeat."
# Outcome measuring whether US won:
icbdata<-icbdata %>% dplyr::mutate(victory=dplyr::recode(outcom,"1"=1,"2"=0,"3"=0,"4"=0))

#14d) Running regressions to answer each of the four hypotheses:
summary(glm.cluster(data=icbdata, formula=RespForce~S.Pct.EC+systrgyr+as.factor(President)+
                      triggerNV,
            cluster="President",family="binomial"))
summary(glm.cluster(data=icbdata, formula=I(log(trgterra))~S.Pct.EC+systrgyr+Southern+as.factor(President)+
                      triggerNV,
                    cluster="President",family="gaussian"))
summary(coxph(Surv(trgterra, victory)~S.Pct.EC+systrgyr+Southern+as.factor(President)+
                triggerNV, data = icbdata))
summary(glm.cluster(data=icbdata, formula=victory~S.Pct.EC+systrgyr+as.factor(President)+
                      triggerNV,
                    cluster="President",family="binomial"))
summary(glm.cluster(data=icbdata, formula=RespForce~S.Pct.EC*repeat.+systrgyr+as.factor(President)+
                      triggerNV,
                    cluster="President",family="binomial"))



### 15) Hazard Model for H2

#Creating the status variable, where 1 = censor and 2 = US withdrawal / stalemate / compromise
fulldata<-fulldata %>% mutate(status = case_when((SideA==1 & Outcome%in%c(3,5,6))~2,
                                                SideA==0 & Outcome%in%c(4,5,6)~2,
                                                TRUE~1))

#Univariate cox regression for Pres. heritage:
cox1 <- coxph(Surv(DurationUS, status) ~ Southern, data = fulldata)
summary(cox1)

#Univariate cox regression for Pres. dependence:
cox2 <- coxph(Surv(DurationUS, status) ~ S.Pct.EC, data = fulldata)
summary(cox2)

#Bringing in both variables:
cox3 <- coxph(Surv(DurationUS, status) ~ S.Pct.EC+Southern, data = fulldata)
summary(cox3)

#And with the full set of covariates:
cox4 <- coxph(Surv(DurationUS, status)~S.Pct.EC+Southern+Year+Term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig, data = fulldata)
summary(cox4)

#H2 Just Southern:
summary(coxph(Surv(DurationUS, status) ~Southern,data=fulldata))
#H2 Presidency-Specific:
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern+Year+term+Inherited+whig+rep+presvet,
             data=fulldata))
#H2 Conflict-Specific:
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+FishDisp+Orig,data=fulldata))
#H2 National Conditions:
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+greatpower+superpower+prev5+prev10+logdead+elitevet,
             data=fulldata))
#H2 Full
#And with the full set of covariates:
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern+Year+Term+Inherited+greatpower+superpower+FishDisp+
                prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig, 
              data = fulldata,cluster="StLdTm",na.action=na.omit))
#H2 EC & South (No Cold War):
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern,
             data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,]))
#H2 Full (No Cold War):
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
             data=fulldata[fulldata$StYear.x<1947|fulldata$StYear.x>1991,]))
#H2 EC & South (Spec A):
summary(coxph(Surv(DurationUS, status)~SpecA+Southern,data=fulldata))
#H2 Full (Spec A):
summary(coxph(Surv(DurationUS, status)~SpecA+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,data=fulldata))
#H2 EC & South (1945-Present):
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern,
             data=fulldata[fulldata$StYear.x>1945,]))
#H2 Full (1945-Present):
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
             data=fulldata[fulldata$StYear.x>1945,]))
#H2 EC & South (No World Wars):
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern,
             data=fulldata[!fulldata$DispNum3%in%c(257,258,1774),]))
#H2 Full (No World Wars):
summary(coxph(Surv(DurationUS, status)~S.Pct.EC+Southern+Year+term+Inherited+greatpower+superpower+FishDisp+
               prev5+prev10+logdead+elitevet+whig+rep+presvet+Orig,
             data=fulldata[!fulldata$DispNum3%in%c(257,258,1774),]))

### 16) Misc Calculations

### Do Southern-Dependent Presidents see America provoked less?
numbers<-fulldata %>% group_by(StLdTm) %>% filter(US_Init==0) %>%  tally
numbers<-merge(numbers,sdata,by="StLdTm",all=F)
cor(numbers$n,numbers$S.Pct.EC)
cor(numbers$n,numbers$Southern)
