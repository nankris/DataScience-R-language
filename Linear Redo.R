setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Predictive analysis with R\\Module 1")
data<-read.csv("DirectMarketing-2.csv")
library(dplyr)
library(ggplot2)
library(car)

head(data)

##Do exploratory analysis##
plot(data$Age,data$AmountSpent,col="red")

str(data$Age) #this is factor

#Combine the Middle and Old levels together
data$Age1<-ifelse(data$Age!="Young","Middle-Old",as.character(data$Age))
str(data$Age1) #the type is character now
data$Age1<-as.factor(data$Age1)
summary(data$Age1)

plot(data$Age1,data$AmountSpent)

#Gender
plot(data$Gender,data$AmountSpent,col="red")

#Own house
summary(data$OwnHome)
plot(data$OwnHome,data$AmountSpent,col="red")

#Married
summary(data$Married)
plot(data$Married,data$AmountSpent,col="red")

#Location
summary(data$Location)
plot(data$Location,data$AmountSpent,col="red")

#Salary
summary(data$Salary)


plot(data$Salary,data$AmountSpent)#Might be heteroescadasticity

#Children
summary(data$Children)

data$Children<-as.factor(data$Children)

plot(data$Children,data$AmountSpent,col="red") #why more children less spending? are they saving?
#probably i should investigate on this point. Why more children less spending


data$Children1<-ifelse(data$Children==3|data$Children==2,"3-2",as.character(data$Children))
data$Children1<-as.factor(data$Children1)


#data$Children1<-ifelse(data$Children==3|data$Children==2,"3-2",data$Children) #above and this give same
#str(data$Children1)

summary(data$Children1)

plot(data$Children1,data$AmountSpent,col="red")

#History
summary(data$History)
#Impute Missing values # here we are just summarizing data with amountspent across history
tapply(data$AmountSpent,data$History,mean)
#####it  automatically removes NAs while calculating

#tapply(data$History, data$AmountSpent, mean) #This is giving NA and unnecessary values

#data%>%group_by(History)%>%summarise(means=mean(AmountSpent)) #this is giving me warning message
names(data)
colSums(is.na(data))#only in history there are NA values

summary(data$History)
ind<-which(is.na(data$History))
mean(data[ind,"AmountSpent"]) #mean of amountspent where History is NA

data%>%filter(History=="Medium")%>%select(AmountSpent)->Amt_M
head(Amt_M)

p<-ggplot(data[ind,],aes(x=AmountSpent))
q<-ggplot(Amt_M,aes(x=AmountSpent))


p+geom_histogram()
q+geom_histogram()

#Create a category called missing
data$History1<-ifelse(is.na(data$History),"Missing",as.factor(data$History))
data$History1<-as.factor(data$History1)

summary(data$History1)

data$History1<-factor(data$History1,labels=c("High","Low","Medium","Missing"))
summary(data$History1) # 1,2,3,missing are replaced by high, low, medium, missing

#Catalogues
summary(data$Catalogs)

names(data)

which(colnames(data)=="Age")
which(colnames(data)=="Children")
which(colnames(data)=="History")



data1<-data[,-c(1,7,8)] #because we added Age1, Children1, History1 already so removing original
str(data1)

mod1<-lm(AmountSpent~.,data=data1) #AmountSpent is dependent variable

summary(mod1)
names(data1)

library(dummies)
data2=dummyVars("~." ,data =data1,fullRank =T)
data3 <-data.frame(predict(data2,newdata= data1))
names(data3)
head(data3)

mod2<-lm(AmountSpent~.,data=data3)
summary(mod2)


'''
data_dummies<-dummy.data.frame(data1)
data_dummies1<-dummy.data.frame(data1, all=FALSE)
names(data_dummies)
names(data_dummies1)
str(data_dummies)

install.packages("dummyVars")
library(caret)
data_dummies<-dummyVars("~." ,data =data1,fullRank =T)
names(data_dummies)
head(data_dummies)


?dummyVars'''


names(data1)
names(dummy.data.frame(data1))

mod2<-lm(formula = AmountSpent ~ Gender + Location + Salary + Catalogs + 
           Children1 + History1, data = data1)
summary(mod2)

hist(data1$Salary)

#summary(mod2)

summary(data1)
#Remove insignificant variabes
#HistoryMissing
#GenderMale

names(data1)

#Create dummy variables
data1$Male_d<-ifelse(data1$Gender=="Male",1,0)
data1$Female_d<-ifelse(data1$Gender=="Female",1,0)

data1$Missing_d<-ifelse(data1$History1=="Missing",1,0)
data1$Low_d<-ifelse(data1$History1=="Low",1,0)
data1$Med_d<-ifelse(data1$History1=="Medium",1,0)
data1$High_d<-ifelse(data1$History1=="High",1,0)

names(data1)

mod3<-lm(formula = AmountSpent ~ Male_d + Location + Salary + Catalogs + 
           Children1+Med_d+Low_d , data = data1)

summary(mod3)

mod4<-lm(formula = AmountSpent ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)

summary(mod4)

#Signs
tapply(data$AmountSpent,data$History,mean)
data1%>%filter(History1!="Medium",History1!="Low")%>%summarize(Mean=mean(AmountSpent)) #inline

tapply(data1$AmountSpent,data1$Location,mean) #inline

#Assumption checks

hist(mod4$residuals)
qqPlot(mod4$residuals)
#Non normal behaviour observed

#Multicollinearity Check

vif(mod4)

names(mod4)

#Constant  variance check
plot(mod4$fitted.values,mod4$residuals) #Funnel shape

#Remidies: Apply log transform to y variable

#vif means variance inflation factor

mod5<-lm(formula = log(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)

summary(mod5)

qqPlot(mod5$residuals)#qqplot looks okay
plot(mod5$fitted.values,mod5$residuals)# Still funnel
qqPlot(mod5$residuals)

summary(mod5)
#Apply square root transform

mod6<-lm(formula = sqrt(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod6)
qqPlot(mod6$residuals) 
plot(mod6$fitted.values,mod6$residuals)#Seems okay

vif(mod6) #vif should be less than 10 then no multicollenearity

predicted<-mod6$fitted.values
actual<-sqrt(data1$AmountSpent)

dat<-data.frame(predicted,actual)

row(dat)[,2]
row(dat)

p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))
#row(dat)
#row(dat)[,2]
p+geom_line(colour="red")+geom_line(data=dat,aes(y=actual),colour="green")

