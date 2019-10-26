#Equation of Logistic Models
p(y)=1/(1+exp(-a-bx))

1-p/p=exp(-a-bx)

ln((1-p)/p)=-a-bx

ln(p/(1-p))=a+bx

#-------Importing the data---------
setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Predictive analysis with R\\Module 2")
goodbad<-read.csv("GOODBAD.csv")
str(goodbad)
#1=good, 0=bad
table(goodbad$Good.Bad)

plot(goodbad$Good.Bad)
dim(goodbad)
#Checking for missing values
colSums(is.na(goodbad))

sampling<-sort(sample(nrow(goodbad), nrow(goodbad)*.7))

length(sampling)

#Select training sample
train<-goodbad[sampling,]
test<-goodbad[-sampling,]
nrow(train)
nrow(test)

#Table of y for the train dataset
#table(train$Good.Bad)
#table(test$Good.Bad)
table(train$Good.Bad)/700
table(test$Good.Bad)/300

table(train$Good.Bad,train$Check_Account_Status)

#Logistic Regression
names(train)
myresult<-glm(data=train,Good.Bad ~ Check_Account_Status+CreditHistory,
              family=binomial)

'''myresult<-glm(formula=Good.Bad ~ Check_Account_Status+CreditHistory,
              family=binomial, data=train)'''

myresult<-glm(formula=Good.Bad ~ Check_Account_Status+CreditHistory,
              family="binomial", data=train)

summary(myresult)

#odds=exp(0.4800)
#p/1-p=exp(0.4800)
#p=odds/1+odds
exp(0.681595)
1.616074

1.616074/(1+1.616074)
exp(0.681595)/(1+exp(0.681595)) 
# it gives the chances of a person being good when there is a unit change in A12.


#p=1.75/(1+1.75)

#Iteration 2 : Adding Duration
myresult<-glm(data=train,Good.Bad ~ Check_Account_Status+CreditHistory+Duration,
              family=binomial)
summary(myresult)


confint(myresult) # Computes confidence intervals for one or more parameters in a fitted model.

#Finding Predicted Values
predicted <- myresult$fitted.values
head(predicted)

head(predict(myresult, type="response", data=train))

head(train$Good.Bad)

#predict(myresult,data=train,type="response")
table(train$Good.Bad)/700

#Confusion Matrix
predbkt<-ifelse(predicted>0.5,'G','B') #we can keep it 0 and 1 also

predbkt_num<-ifelse(predicted>0.5, 1,0)

table(predbkt_num,train$Good.Bad)

table(train$Good.Bad)
#Plotting ROC Curve
#install.packages("ROCR")
library(ROCR)
?performance
# The prediction function of the ROCR library basically creates a structure to validate 
#our predictions qirh actual values

pred<-prediction(predicted,train$Good.Bad) #prediction is from ROCR library
# prediction(predicted, actual) same as confusionMatrix(predicted, actual, positive="1")
head(pred)
pred

# "tpr" and "fpr" are arguments of the "performance" function indicating that the plot is 
#between the true positive rate and the false positive rate.
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
?performance
perf

plot(perf,col="red")
abline(0,1, col = "grey")
abline(0,1, lty = 8, col="blue")

predbkt_num<-as.factor(predbkt_num)
train$Good.Bad<-as.factor(train$Good.Bad)
confusionMatrix(predbkt_num,train$Good.Bad, positive="1")

True Positives : 338/489 #how they came?
False Positives : 58/211

#How to choose cutoff's?
#use @ to access the slots
cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
head(cutoffs)
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
head(cutoffs)

perf@alpha.values[[1]]

auc<-performance(pred,"auc")
auc
auc<-unlist(slot(auc,"y.values"))
auc

#Gives best fitted model
#To choose a good model
?step
reduced<-step(myresult,direction="backward") #the mode of stepwise search, can be one of "both", "backward", or "forward", 
#with a default of "both". If the scope argument is missing the default for direction is "backward". Values can be abbreviated.
# based on the above code,CreditHistory had low AIC, so run a model with only tht variable

###tried above step function for forward, backward and both direction and got same output

myresult<-glm(data=train,Good.Bad ~ CreditHistory,
              family=binomial)
summary(myresult)
