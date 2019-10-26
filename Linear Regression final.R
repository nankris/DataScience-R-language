
#--------------------------------Linear Regression -------------------------------------
setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Predictive analysis with R\\Module 1")
###Linear Regression Sample

##Loading Data
mmix<-read.csv("MMix.csv",header=TRUE,stringsAsFactors=FALSE)

##Checking Data Characteristics
dim(mmix)
str(mmix)
head(mmix)
tail(mmix)
names(mmix)

#DV=1/0 variable :presence /absence of Sales

#Dv = Sales

dim(mmix)
#summary statistics
summary(mmix)
summary(mmix$NewVolSales)


#checking outliers
x<-boxplot(mmix$NewVolSales)
out<-x$out
out

#To get list of outliers
#Outlier treatment
x$out
index<-mmix$NewVolSales %in% x$out

#which(mmix$NewVolSales==23889)


index
sum(index)
length(index)
dim(mmix[-index,])
non_outlier<-mmix[-index,]
dim(non_outlier)

y<-boxplot(non_outlier$NewVolSales)
y$out

#checking missing values
colSums(is.na(mmix))
summary(mmix)

#Treating missing values
mmix$NewVolSales[is.na(mmix$NewVolSales)]<-mean(mmix$NewVolSales,na.rm=TRUE)
summary(mmix$Base.Price)

#--------------------------------Exploratory Analysis -------------------------------------
dev.off()
library(ggplot2)

##Univariate Analysis
#Viz
qplot(mmix$NewVolSales)
hist(mmix$NewVolSales)
hist(mmix$Base.Price)


##Bivariate analysis 

#Viz
with(mmix,qplot(NewVolSales,Base.Price))

#qplot(mmix$NewVolSales, mmix$Base.Price) #same as above

with(mmix,qplot(NewVolSales,InStore))
qplot(mmix$NewVolSales,mmix$Radio)

#Correlations
cor(mmix$NewVolSales,mmix$Base.Price)

#with(mmix,cor(NewVolSales, Base.Price)) same as above

with(mmix,cor(NewVolSales,Radio))
with(mmix,cor(NewVolSales,InStore))


##What is the use of log variables?
##To make a variable in scale with the other variable
with(mmix,qplot(log(NewVolSales),InStore))

##Creating Indicator Variables
##Why indicator or dummy variables? : Because a model does not accept chaaracter values.
##Only numbers of factors.

unique(mmix$Website.Campaign)
table(mmix$Website.Campaign)
mmix$FB<-ifelse(mmix$Website.Campaign=="Facebook",1,0)


##Creating New Variables

#Data TRansformations
mmix$LnSales<-log(mmix$NewVolSales)
mmix$LnPrice<-log(mmix$Base.Price)
mmix$OfflineSpend<-mmix$Radio+mmix$TV+mmix$InStore

quantile(mmix$Base.Price, p=seq(0,100,10)/100)
quantile(mmix$Base.Price, p=c(0,10,20,30,40,50,60,70,80,90,100)/100)
quantile(mmix$Base.Price, p=c(1:100)/100) #below buckets are made like around 22, 44, 66% separations

#Create price buckets - converting numeric variable into categorical
mmix$Price_Bkt[mmix$Base.Price < 15.03]<-"Low"  #quantile below 30 percent data
mmix$Price_Bkt[mmix$Base.Price >= 15.03 & mmix$Base.Price < 15.33]<-"Avg" #quantile from 30 to 50%
mmix$Price_Bkt[mmix$Base.Price >= 15.33 & mmix$Base.Price < 15.64]<-"High" #quantile from 50 to 70%
mmix$Price_Bkt[mmix$Base.Price >= 15.64]<-"V.High" #quantile from 70 to 100%


#--------------------------------Building models -------------------------------------

##Building SimpLe Linear Regression Model
?lm
attach(mmix) #now we don't need to use mmix$ symbol to access variables inside mmix. We can directly write them
Reg<-lm(NewVolSales~Base.Price,data=mmix)

#lm(NewVolSales~Base.Price,data=mmix)

Reg
#y=a+bx
#sales=a+b(base.price)
#Newvolsales=53487-2176(base.price)

#Checking summary of the regression object "Reg"
Reg
summary(Reg)

#Metrics to assess a model:
#Rsquare
#Coefficients
#P values : Significance levels of the IDV's
#Residuals distribution

#Factor variables as IDV's
#Remember one of the factor type becomes a baseline.The estimates of the other
#types of factor are only given by the model.

Reg<-lm(NewVolSales~as.factor(Price_Bkt),data=mmix)
summary(Reg)
#Creating dummy for low bucket since it is significant
mmix$PrizeBktLow<-ifelse(mmix$Price_Bkt=="Low",1,0)


attach(mmix)
Reg<-lm(NewVolSales~PrizeBktLow,data=mmix)
summary(Reg)

#Getting the formula
formula(Reg)

##Multivariate Regression Model
#Iteration 1
Mulreg<-lm(NewVolSales~Base.Price+InStore+as.factor(Website.Campaign),data=mmix)
Mulreg
summary(Mulreg)

#Iteration 2
Mulreg<-lm(NewVolSales~Base.Price+InStore+Radio+TV+as.factor(Website.Campaign),data=mmix)
Mulreg
summary(Mulreg)

#Iteration 3
Mulreg<-lm(NewVolSales~LnPrice+InStore+as.factor(Website.Campaign),data=mmix)
Mulreg
summary(Mulreg)

#Choose the model with the highest R square 
#Getting the formula
formula(Mulreg)

#--------------------------------Testing models -------------------------------------
train
##Getting predicted values
PredSales<-predict(Mulreg,data=train)
PredSales


##Finding Residuals
ResSales<-resid(Mulreg)
ResSales

plot(ResSales)

##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity - exists if there is a pattern between predicted values and error

plot(PredSales,ResSales,abline(0,0)) #try (1000,0) inside abline
plot(NewVolSales,ResSales,abline(0,0))


##Plotting actual vs predicted values
plot(mmix$NewVolSales,col="blue",type="l")
lines(PredSales,col="red",type="l")

#plot(PredSales, col="red", type="l") this will overwrite the before plotted one.

##Try on a different validation data
sampling<-sort(sample(nrow(mmix), nrow(mmix)*.7))
head(sampling)
length(sampling)

#--------------------------------Training and Test Splits -------------------------------------

#Select training sample
train<-mmix[sampling,]
test<-mmix[-sampling,]
nrow(train)
nrow(test)

predict(Mulreg,data=test)

#--------------------------------checking for multicollinearity within variables ------------------

# What is multicollinearity , a great article!
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis

#The function is vif .It belongs to car package
library(car)
vif(Mulreg)

#vif= variation inflation factor
##If the variables in the model have a vif>=10, then you can exclude them from the model.

#What is vif?:
#http://support.minitab.com/en-us/minitab/17/topic-library/modeling-statistics/regression-and-correlation/
#model-assumptions/what-is-a-variance-inflation-factor-vif/