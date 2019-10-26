setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Data Wrangling and EDA with R\\Module 1")

#cr<-read.csv("Credit.csv")
#colSums(is.na(cr))

cr<-read.csv("Credit.csv",na.strings=c("",NA)) #here nothing is replaced by NA
View(cr)
colSums(is.na(cr))

library(dplyr)
options(scipen=999)                                           
##Data Exploration##
#This is a credit card dataset

#Sanity check
#Identify outliers, replace them
#Approaches to impute missing values
#Bin the data-> Quantile function, ntile() for binning
#Partitioning data: test and training samples
str(cr)
names(cr)

#Duplicate columns present, remove them
cr<-cr[,-c(1,12)]

#Sanity check
summary(cr)

#Missing values
index<-which(is.na(cr$Good_Bad)) #here in row of Good_Bad only all NA's are present so you just 
# have to remove those rows to remove all NA's
cr[index,]

cr<-cr[-index,]

summary(cr)

#Look at individual summaries

summary(cr$RevolvingUtilizationOfUnsecuredLines) #Ratio variable

cr%>%filter(RevolvingUtilizationOfUnsecuredLines==0)%>%nrow()

cr%>%filter(RevolvingUtilizationOfUnsecuredLines>=0.99)%>%nrow()

#Percentile breakup

quantile(cr$RevolvingUtilizationOfUnsecuredLines,p=c(1:100)/100)



#Discuss with client, 2 is the limit on the number, replace

cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)%>%nrow()

cr%>%filter(RevolvingUtilizationOfUnsecuredLines<=2)->cr


summary(cr$age)

cr%>%filter(age==0)%>%nrow()

quantile(cr$age,p=(1:100)/100)

cr%>%filter(age!=0)->cr



summary(cr$Gender)

summary(cr$Region)





sum(is.na(cr$MonthlyIncome))

summary(cr$MonthlyIncome)

cr%>%filter(MonthlyIncome==0)%>%nrow()

#quantile(cr$MonthlyIncome,p=c(1:100)/100) #this wont work here

quantile(cr$MonthlyIncome,p=c(1:100)/100,na.rm=TRUE)

cr%>%filter(MonthlyIncome>25000)%>%nrow()




quantile(cr$MonthlyIncome,p=c(990:1000)/1000,na.rm=TRUE) #to get decimal places as percentages
# ex: 99.1,99.2,..... In above example it is 90,91,92,93,.......

#We find after discussions that '0' here means a missing value


cr$MonthlyIncome<-ifelse(cr$MonthlyIncome==0,NA,cr$MonthlyIncome)
#sum(is.na(cr$MonthlyIncome))

summary(cr$Rented_OwnHouse)

summary(cr$Occupation)

summary(cr$Education)

summary(cr$NumberOfTime30.59DaysPastDueNotWorse)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(1:100)/100)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(990:1000)/1000, na.rm=TRUE)

# We find on discussions with stakeholders that large numbers are missing values

cr$NumberOfTime30.59DaysPastDueNotWorse<-ifelse(cr$NumberOfTime30.59DaysPastDueNotWorse==98,NA,
                                                cr$NumberOfTime30.59DaysPastDueNotWorse)

quantile(cr$NumberOfTime30.59DaysPastDueNotWorse,p=c(990:1000)/1000, na.rm=TRUE)

cr$NumberOfTime30.59DaysPastDueNotWorse<-ifelse(cr$NumberOfTime30.59DaysPastDueNotWorse==96,NA,
                                                cr$NumberOfTime30.59DaysPastDueNotWorse)

#here we want to remove extreme values from the dataset so we are replacing 98 by NA so what happens is 
#remaining values are kept as same and only 98 is removed. Again see the quantile and again remove 96
#as 96 is also extreme value

summary(cr$DebtRatio)

cr%>%filter(DebtRatio==0)%>%nrow()

quantile(cr$DebtRatio,p=c(1:100)/100)

##Cap at 2## (After discussions with stakeholders)

cr$DebtRatio<-ifelse(cr$DebtRatio>2,2,cr$DebtRatio)

quantile(cr$DebtRatio,p=c(1:100)/100)

summary(cr$NumberOfOpenCreditLinesAndLoans)

quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)

#Higher magnitude numbers represent missing value

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==58,NA,
                                           cr$NumberOfOpenCreditLinesAndLoans)
quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==57,NA,
                                           cr$NumberOfOpenCreditLinesAndLoans)
quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==56,NA,
                                           cr$NumberOfOpenCreditLinesAndLoans)
quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans==54,NA,
                                           cr$NumberOfOpenCreditLinesAndLoans)
quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfOpenCreditLinesAndLoans<-ifelse(cr$NumberOfOpenCreditLinesAndLoans>24,NA,
                                           cr$NumberOfOpenCreditLinesAndLoans)
quantile(cr$NumberOfOpenCreditLinesAndLoans,p=c(1:100)/100,na.rm=TRUE)

summary(cr$NumberOfTimes90DaysLate)

quantile(cr$NumberOfTimes90DaysLate,p=c(1:100)/100)

quantile(cr$NumberOfTimes90DaysLate,p=c(990:1000)/1000,na.rm=TRUE)

#Higher numbers represent missing value

cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==98,NA,cr$NumberOfTimes90DaysLate)
quantile(cr$NumberOfTimes90DaysLate,p=c(990:1000)/1000,na.rm=TRUE)

cr$NumberOfTimes90DaysLate<-ifelse(cr$NumberOfTimes90DaysLate==96,NA,cr$NumberOfTimes90DaysLate)
quantile(cr$NumberOfTimes90DaysLate,p=c(990:1000)/1000,na.rm=TRUE)

summary(cr$NumberRealEstateLoansOrLines)

quantile(cr$NumberRealEstateLoansOrLines,p=c(1:100)/100)

quantile(cr$NumberRealEstateLoansOrLines,p=c(990:1000)/1000,na.rm=TRUE)

cr%>%filter(NumberRealEstateLoansOrLines==54)%>%nrow()

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines==54,NA,
                                        cr$NumberRealEstateLoansOrLines)
quantile(cr$NumberRealEstateLoansOrLines,p=c(990:1000)/1000,na.rm=TRUE)

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines==32,NA,
                                        cr$NumberRealEstateLoansOrLines)
quantile(cr$NumberRealEstateLoansOrLines,p=c(990:1000)/1000,na.rm=TRUE)

cr$NumberRealEstateLoansOrLines<-ifelse(cr$NumberRealEstateLoansOrLines>9,NA,
                                        cr$NumberRealEstateLoansOrLines)
quantile(cr$NumberRealEstateLoansOrLines,p=c(990:1000)/1000,na.rm=TRUE)

summary(cr$NumberOfTime60.89DaysPastDueNotWorse)

quantile(cr$NumberOfTime60.89DaysPastDueNotWorse,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfTime60.89DaysPastDueNotWorse<-ifelse(cr$NumberOfTime60.89DaysPastDueNotWorse==98,NA,
                                                cr$NumberOfTime60.89DaysPastDueNotWorse)
quantile(cr$NumberOfTime60.89DaysPastDueNotWorse,p=c(1:100)/100,na.rm=TRUE)

cr$NumberOfTime60.89DaysPastDueNotWorse<-ifelse(cr$NumberOfTime60.89DaysPastDueNotWorse==96,NA,
                                                cr$NumberOfTime60.89DaysPastDueNotWorse)
quantile(cr$NumberOfTime60.89DaysPastDueNotWorse,p=c(1:100)/100,na.rm=TRUE)


summary(cr$NumberOfDependents)

unique(cr$NumberOfDependents)

cr$NumberOfDependents<-as.character(cr$NumberOfDependents)
