library(gains)
library(dplyr)
library(irr) #to calculate accuracy metrics
library(caret) # to calculate accuracy metrics
setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Predictive analysis with R\\Module 2")
dm<-read.csv("C:/Users/Poori/Desktop/jigsaw/Predictive analysis with R/Module 2/dm-1.csv")
# Direct Marketer who wants to come up with a process to identify good customers, identify customer id's who are considered good according to his definition

View(dm)
head(dm)
str(dm)
colSums(is.na(dm)) #History have 303 NA values

quantile(dm$AmountSpent, p=seq(1,100,10)/100)
mean(dm$AmountSpent)
dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm%>%select(-AmountSpent)->dm #here we are removing because we already captured this data in Target

#dm%>%mutate(Target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
#dm%>%select(-AmountSpent)->dm
dm$AmountSpent
summary(dm)

#Minimal Data Prep

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)

#sum(is.na(dm$History))
#dm$History1<-ifelse(is.na(dm$History),"missing",as.character(dm$History))
#sum(is.na(dm$History1))
#dm$History1<-as.factor(dm$History1)

summary(dm$History1)

dm$Children<-as.factor(dm$Children)
dm$Catalogs<-as.factor(dm$Catalogs)

#names(dm)

dm<-dm[,-8] #8th column is History

#Splitting into test and training samples
set.seed(200)
index<-sample(nrow(dm),0.70*nrow(dm),replace=F)
#in sample first parameter is dataset, next is number to be sampled
#after that whether replacement sampling is true or false
train<-dm[index,]
test<-dm[-index,]

#Build the first model using all the variables 

# 9 column in train is Cust_Id
# Target is a column name in Train not any parameter of glm
mod<-glm(Target~.,data=train[,-9],family="binomial") #as target is 1,0 types we are using binomial, if it is more than one level then
#we use multinomial
summary(mod)

step(mod,direction="both") #gives the best model that we can have with the column names. It gives formula

mod1<-glm(formula = Target ~ Age + Location + Salary + Children + Catalogs +  History1, family = "binomial", data = train)
#here we need not to use train[,-9] because we are explicitly giving
# which variable to use as independent

summary(mod1)

#Creating dummies

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)

train$Hist.Mid_d<-ifelse(train$History1=="Medium",1,0)

train$Children2_d<-ifelse(train$Children=="2",1,0)

train$Children3_d<-ifelse(train$Children=="3",1,0)

test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)

test$Hist.Mid_d<-ifelse(test$History1=="Medium",1,0)

test$Children2_d<-ifelse(test$Children=="2",1,0)

test$Children3_d<-ifelse(test$Children=="3",1,0)


mod2<-glm(Target~AgeYoung_d+Location+Salary+Children3_d+Children2_d+Catalogs+Hist.Mid_d,data=train,family="binomial")

summary(mod2)

#in class upto hear only they teached
#next part is in next class
#names(test)
pred<-predict(mod2,type="response",newdata=test) #viva question, why type ="response". Because we get probabilities as output

head(pred)

#names(dm)
#colnames(dm)
#rownames(dm)

table(dm$Target)/nrow(dm) #to predict good or bad customer
pred<-ifelse(pred>=0.399,1,0)

pred<-as.factor(pred)

?kappa2
####read about kappa metric
kappa2(data.frame(test$Target,pred))

#A kappa of 1 indicates perfect agreement, whereas a kappa of 0 indicates agree- ment equivalent to chance.

#confusion matrix required varaibles to be factors
pred<-as.factor(pred)
test$Target<-as.factor(test$Target)
confusionMatrix(pred,test$Target,positive="1")

#in gains first argument must be numeric
test$Target<-as.numeric(test$Target)
#nrow(train)
gains(test$Target,predict(mod2,type="response",newdata=test),groups = 15)

test$prob<-predict(mod2,type="response",newdata=test)

quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

targeted<-test[test$prob>0.732602471&test$prob<=0.999747759,"Cust_Id"]
head(targeted) #so these customer ids are important for our business


library("arules")
ad<-AdultUCI
names(ad)
