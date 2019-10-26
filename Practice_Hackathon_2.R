#install.packages("mice")
install.packages("Metrics")
library(mice)
library(car)
library(Metrics)
library(dplyr)
library(caret)
library(lubridate)
getwd()
setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Predictive analysis with R\\Module 1")

data1 <- read.csv("DSA_Hackathon_Dataset.csv")
summary(data1)
str(data1)


colSums(is.na(data1))
which(is.na(data1$waterfront))
which(is.na(data1$grade))
data2 <- data1[-c(19976:19992),]
colSums(is.na(data2))

#missing values
md.pattern(data2)
imputed_Data <- mice(data2,m=3,maxit = 2,method='cart')
summary(imputed_Data)
data3 <- complete(imputed_Data,1)
colSums(is.na(data3))


list=colnames(data3)
list1 bro onemB<- list[-c(8,9,15)]

#outliers detection
par(mfrow=c(4,4))
for(i in 1:length(list1))
{
  hist(data3[,list1[i]],col="dodgerblue4",main=list1[i])
}


par(mfrow=c(4,4))
for(i in 1:length(list1))
{
  boxplot(data3[,list1[i]],col="dodgerblue4",main=list1[i])
}

par(mfrow=c(4,4))
for(i in 1:length(list1))
{
  boxplot(data3[list1[i]],data3$price,xlab=list1[i],ylab="price",col="dodgerblue4",main=list1[i])
}
dev.off()
#outlier imputation
boxplot(data3$price)

index1<- which(data3$price > 60000000)
data3[index1,"price"] <- 60000000

dotplot(data3$bedrooms)
index2<- which(data3$bedrooms > 15)
data3[index2,"bedrooms"] <- 15
data3[15871,"bedrooms"]

summary(data3$sqft_lot)
dotplot(data3$sqft_lot)
boxplot(data3$sqft_lot)
hist(log(data3$sqft_lot))

index3<- which(data3$sqft_lot > 1100000)
data3[index3,"sqft_lot"] <- 1100000
data3[1720,"sqft_lot"]

boxplot(data3$sqft_basement)
summary(data3$sqft_basement)
index4<- which(data3$sqft_basement > 3500)
data3[index4,"sqft_basement"] <- 3500
data3[8093,"sqft_basement"]

par(mfrow=c(4,4))
for(i in 1:length(list1))
{
  qqPlot(data3[,list1[i]],distribution="norm",main=list1[i])
}

qqnorm(data3$bathrooms)
qqline(data3$bathrooms,col="red",main="bathrooms")
qqPlot(data3$bathrooms,distribution="norm",main="bathrooms")


cor(data3[,3:16])  #ignore Id and price columns

#Training and Testing dataset
set.seed(123)
train <- sort(sample(nrow(data3),nrow(data3)*.7))
test <- -train
trainingData <- data3[train,]
testingData <- data3[test,]
testingPrice <- data3$price[test]
str(data3)
colSums(is.na(data3))
#model  - complete model
model1 <- lm(price ~.,data=trainingData)
summary(model1)

#residuals vs regressor
residualPlots(model1)

#deletion diagnostics
influenceIndexPlot(model1)
#new model with omited observations   #model 2
data4 <- data3[-c(12778),]
nrow(data4)
data4 <- data4[,-c(1)]   #remove column ID
colnames(data4)

model2 <- lm(price~.,data = data4)
summary(model2)

#model 3
par(mfrow=c(2,2))
hist(data3$price)
hist(sqrt(data3$price))
hist(log(data3$price))

model3 <- lm(log(price)~.,data = data4)
summary(model3)   #mape is high

#model 4
model4 <- lm(sqrt(price)~.,data = data4)
summary(model4)  #mape is high


#add necessary log transformations to independent variables
model5 <- lm(price ~ bedrooms+bathrooms+sqft_living+log(sqft_lot)+floors+waterfront+view+condition+grade+log(sqft_above)+sqft_basement+yr_built+yr_renovated+zipcode,data=data4)
summary(model5)

#predict
new_predict <- predict.lm(model5,testingData,type="response")
head(new_predict)

#Error estimation
rmse(testingPrice,new_predict)
mape(testingPrice,new_predict)

vif(model5)
#model6 
model6 <- lm(price ~ bedrooms+bathrooms+log(sqft_lot)+floors+waterfront+view+condition+grade+sqft_basement+yr_built+yr_renovated+zipcode,data=data4)
summary(model6)
dat1 <- data.frame(act=data4$price,pred=model6$fitted.values)

plot(model5)
dat <- data.frame(act=data4$price,pred=model5$fitted.values)

#validation
valData <- read.csv("C:\\Users\\Poori\\Desktop\\jigsaw\\Predictive analysis with R\\Module 1\\DSA_Hackathon_Validation_Dataset.csv")
valPrice <- valData$price
head(valPrice)
new_predict <- predict.lm(model5,valData,type="response")
head(new_predict)

rmse(valPrice,new_predict)
mape(valPrice,new_predict)

vif(model5) #vif should be less than 5

#New changes - after review
colnames(data4)
data5 <- data4[,-c(10,11,12)]
colnames(data5)
data6 <- data5 %>% count(zipcode) %>% ungroup()%>%arrange(desc(n))

for (i in 1:length(data5$yr_renovated))
{
  if(data5$yr_renovated[i]==0){
    data5$age[i] =year(Sys.Date())-data5$yr_built[i]
  }else{
    data5$age[i]=year(Sys.Date())-data5$yr_renovated[i]
  }
  
}
head(data5$age)
data5 <- data5[,-c(10,11)]
is.factor(data5$zipcode)
data5$zipcode <- as.factor(data5$zipcode)

data7=dummyVars("~." ,data =data5,fullRank =T)
data7d <-data.frame(predict(data7,newdata= data5))
colnames(data5)
colnames(data7d)


#Training and Testing dataset
set.seed(123)
train <- sort(sample(nrow(data7d),nrow(data7d)*.7))
test <- -train
trainingData <- data7d[train,]
testingData <- data7d[test,]
testingPrice <- data7d$price[test]
#model  - complete model
model9 <- lm(price ~.,data=trainingData)
summary(model9)

#predict
new_predict1 <- predict.lm(model9,testingData,type="response")
head(new_predict)

#Error estimation
rmse(testingPrice,new_predict1)
mape(testingPrice,new_predict1)
vif(model9)

#removing extra dummies
#data6 -count
#data6$zipcode <- ifelse(data6$zipcode<=350,"others",data6$zipcode)
