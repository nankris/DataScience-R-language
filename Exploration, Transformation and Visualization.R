# To access the quality of data
# EDA - it helps in identifying right modeliing technique for underlying data

# Good Quality data - ?
# Without missing values 
# Without outlier 
# Without Incomplete value ( Data should be complete)
# Without Incorrect Value ( Data should be correct)
# Without Invalid Values
# Without Duplicate Values 
# Cover entire population - check for data sufficiency
# Compare summary statistics with Data Dictionary 
# Marketing_Camp_Type -> DD says 5 value - Email, Call, Direct, Instore, Text_Based 
# During EDA, you find out there are only 4 values - Email, Call, Direct, Instore
# During EDA, you find out there are 6      values - Email, Call, Direct, Instore, Text_Based, Inperson

# What is Invalid Value & Incorrect 
# Age - 215 years ( Invalid value)
# My age is 25 but in system 35 - Incorrect value

# What is outlier ?
# Time spent by different people in the shopping mall - 20, 18, 130, 12, 15 
# Age of the person                                   - 32, 35,  35, 28, 21 
# WithKids/WithoutKids                                -  N,  N,   Y, N,  N 

# Missing Values ? "NA"
#  Treatment of missing values ? - "AFTER TREATMENT YOU SHOULD HAVE SIZEABLE AMOUNT OF DATA FOR MODELLING"
#                                - "DEFAULT IS IGNORE, If problem - Impute 
#  1) Impute - 
#  2) Ignore - (delete the row having missing value)
#  3) Delete the col having missing values - 

#   I have 10000 records & 20% of records are missing - Impute
#   I have    1   million records & 20% is missing    - Ignore 

# How to check the effectiveness of treatment 
# When to check mean, median and mode ?
# If data is not skewed (not having extreme values) - Mean 
# else check median 
# MODE is only for categories 

# Task in EDA
# Transformation of variable 
# Separate numeric & non-numeric features 
# Univariate Analysis , Bivariate analysis, Multivariate Analysis 
# Derived variables 
# Dummy variables 
# Cross tabulation 
# Correlation 

setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Class Codes")

# Base plotting
# Read data
ir<-iris
str(ir)

# Univariate Analysis


#Using histograms
hist(ir$Sepal.Width)
hist(ir$Sepal.Width,freq=FALSE)

?hist
hist(ir$Sepal.Width,col="orange",labels=TRUE)


#Box plots

boxplot(ir$Sepal.Width)


summary(ir$Sepal.Width) #Mean<Median, negatively skewed


x<- boxplot(ir$Sepal.Width)
summary(ir$Sepal.Width) #Mean>Median, positively skewed

#Improving the asethetics of boxplot
boxplot(ir$Petal.Length,col="red",main="Distribution of Petal length")

# Boxplot distribution by categorical variable
#Studying a continous variable across groups
#Distribution of Sepal width across different species

boxplot(ir$Sepal.Width~ ir$Species,xlab="Species",
        main="Sepal Length across sepcies",col="red")

#plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",
#    main="Sepal Length across sepcies",col="red")


#Adding multiple plots in single plotting window
par(mfrow=c(1,2))

#mfrow >> no. of multiple figures row wise
#mfcol >> no. of multiple figures col wise
#par>> graphical parameters
boxplot(ir$Sepal.Width ~ ir$Species,xlab="Species",
        main="Sepal width across sepcies",col="red")

boxplot(ir$Sepal.Length ~ ir$Species,xlab="Species",
        main="Sepal Length across sepcies",col="red")

# dev.off is used to remove the mfrow function 

# Using plot() to study to continous variables

# Syntax
# plot(x=variable to be displayed on x axis, y = variable to be displayed on y axis)
par(mfrow=c(1,1))

plot(x=ir$Petal.Width,y=ir$Petal.Length)


# Adding xlabels, ylables and title

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"))
# plot(x=ir$Sepal.Length,y=ir$Sepal.Width,col=ir$Species,main="sepalL vs sepalW", xlab = "sepal length", ylab = "sepal width")
# above code also works

# Adding colors, plotting symbols

#Adding colors
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red")


#Adding different plotting symbol
# pch - plotting character - 0:25
?pch
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=5)


plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=3)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=5)

#Adding  more options (type of plotting char and line width)


plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4,
     type="l",lwd=3)

#plot types
# "p": Points.
# "l": Lines.
# "b": Both.
# "c": The lines part alone of "b"
# "o": Both "overplotted"
# "h": Histogram like (or high-density) vertical lines.
# "n": No plotting.

# Making a conditional bivariate plot

# Seeing relationship across different species 
# (adding a 3rd dimesion to visulaization)

#color by species
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species)

#change plotting char by species
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species),
     col=ir$Species)

#Changing size of plotting char by its value
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species))

#Size and color
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species),
     col=ir$Species)


#Adding a legend

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species))

unique(ir$Species)
legend(1.3,3,c("Setosa","Versicolor","Verginica"),pch=c(2,1,4))
?legend # to know more about legend

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species,
     pch=as.numeric(ir$Species),xlim = c(0,5)) # just the x axis will be in between 0 and 5

legend(0.2,7,c("Setosa","Versicolor","Verginica"),pch=1:3,col=1:3)

# Another way to implement box plots

par(mfrow=c(1,2))
plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",
     main="Sepal Width across sepcies",col="red")
plot(x=ir$Species,y=ir$Sepal.Length,xlab="Species",
     main="Sepal Length across sepcies",col="red")

#reset graph parameters to default
dev.off()


install.packages("ggplot2")
library(ggplot2)

# ggplots library
dat1<-read.csv("C://Users//Poori//Desktop//jigsaw//R for data science//Class Codes//audit.csv")


##Grammar elements##
# ggplot >> grammar of graphics
#Aesthetic maps
#Geoms

#Stats and geoms
# Creating histograms of income by Gender

p<-ggplot(dat1,aes(x=Income))
p+geom_histogram()
# to avoid exponential values in income use options(scipen=999)
p+geom_histogram() + facet_grid(.~Gender)

# Creating boxplots
p<-ggplot(dat1,aes(y=Income))
p+geom_boxplot() 
p+geom_boxplot() + facet_grid(.~Gender)


# Bivariate analysis
p<-ggplot(dat1,aes(x=Age,y=Income))
#Creating a scatter plot 
p+geom_point()
  
# Adding a 3rd dimension to it
p+geom_point(aes(color= Gender))

#What if I want to fix the value of color aesthetic rather than mapping it?

p+geom_point(aes(color="blue")) # we won't get blue color as output but orange color.

p+geom_point(color="blue")#Mapping Vs Fixing a value # Here we get blue color

#Adjusting scales
p<-ggplot(dat1,aes(x=Age,y=Income))
p+geom_point(aes(color=Gender))+scale_x_continuous(breaks = seq(0,80,10))

#p+geom_point(aes(color=Gender))+scale_x_continuous(breaks=seq(0,100,10))


#Other meanings of scale
p+geom_point(aes(size=Gender,color=Income))#Adjust the size of points

#ggplot(dat1,aes(x=Age,y=Income,size=Gender,color=Income)) is not working

p+geom_point(aes(size=Gender,color=Income))+ scale_size_discrete(range = c(1,2))

p+geom_point(aes(size=Gender,color=Income))+ scale_size_discrete(range = c(1,3))

#p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(range=c(1,3))+facet_grid(Gender~.)

p+geom_point(aes(size=Gender,color=Income))+ scale_size_discrete(range = c(1,3))+
  scale_color_continuous(low="blue",high="red")

#p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(range=c(1,3))+scale_color_continuous(low="blue",high="red")+facet_grid(Gender~.)

#Giving lables
p+geom_point(aes(size=Gender,color=Income))+ scale_size_discrete(labels=c("F","M"),range = c(1,3))+
  scale_color_continuous(low="blue",high="red")

#p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range=c(1,3))+scale_color_continuous(low="blue",high="red")+facet_grid(Gender~.)

#Smoothening
p+geom_point(aes(size=Gender,color=Income))+ scale_size_discrete(labels=c("F","M"),range = c(1,3))+
  scale_color_continuous(low="blue",high="red")+ geom_smooth()+facet_grid(.~Gender)

#p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range=c(1,3))+scale_color_continuous(low="blue",high="red")+geom_smooth()+facet_grid(Gender~.)

p+geom_point(aes(size=Gender,color=Income))+ scale_size_discrete(labels=c("F","M"),range = c(1,3))+
  scale_color_continuous(low="blue",high="red")+ geom_smooth()+facet_grid(Gender~Employment)



###These are some of the things ggplot2() can do, 
# make sure you understand what grammar of graphics is, 
# how it works within ggplot2()#


# Case study - 
# A reatiler does various marketing investments and campaigns
# (offline and online) to boost sales of its products along with adjusting prices
# As a marketing budgeting manager for the retailer, the client would like
# to understand 
# a.) Is price an important factor to drive sales? 
# b.) How spends and other campaigns are performing? 
# c.) Where should the budget be diverted if a particular channel is not performing

## Install important libraries

library(dplyr)  #data manipulation
library(ggplot2)  #data visualization

setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Class Codes")


##Loading Data
mmix<-read.csv("C:/Users/Poori/Desktop/jigsaw/R for data science/Class Codes/MMix.csv",header=TRUE,stringsAsFactors=FALSE, na.strings = "")

#use na.strings

##mmix <- MMix
##Checking Data Characteristics
#dimensions
dim(mmix)
#structure of data
str(mmix)
#top and bottom records of data
head(mmix)
tail(mmix)

#Dv = Sales
##attach mmix
#The database is attached to the R search path. This means that
#the database is searched by R when evaluating a variable, so objects
#in the database can be accessed by simply giving their names

attach(mmix) #this is used because in future code we don't need to reference variables within mmix data frame with $ sign


#cr<-read.csv("Credit.csv",na.strings=c("",NA))
#attach(cr)

#------ Univariate analysis----------
#summary statistics
summary(NewVolSales)

#summary(NPA.Status)
# rm(cr)

summary(mmix)

# #checking missing values
colSums(is.na(mmix))

# #Treating missing values
# NewVolSales[is.na(NewVolSales)]<-mean(NewVolSales,na.rm=TRUE)
# summary(NewVolSales)

#checking sales distribution graphically
hist(NewVolSales)

#Example
# match_scores <- c(50, 199, 201, 236, 269,271,278,283,291, 301, 303,341)
# msbox<- boxplot(match_scores)
# 
# #Get outlier
# msbox$out
# 
# match_scores1x


#checking outliers
x<-boxplot(NewVolSales1)
boxplot(mmix$NewVolSales1)

# Outlier treatment - Way 1
#To get list of outliers
list_out<- x$out
list_out

#gives the positions in the data where outliers are present
index<-which(mmix$NewVolSales %in% list_out) # this is working here

index<-which(mmix$NewVolSales==x$out) # here this is not working (in code.R reverse scenario is happening)

#---Shortlist the outliers from the dataset and replace
mmix$NewVolSales[index]

#na.rm=TRUE --> making sure missing values are removed before calculating mean
mean_sales<-mean(mmix$NewVolSales,na.rm=TRUE)
mmix$NewVolSales[index]<-mean_sales

# Outlier treatment - way 2
qn = quantile(mmix$NewVolSales, c(0.05, 0.95), na.rm = TRUE)
y<- IQR(NewVolSales)
q1 <- quantile(NewVolSales, 0.25)
q3 <- quantile(NewVolSales, 0.75)

mmix<- within(mmix, { NewVolSales1 = ifelse(NewVolSales < (q1-1.2*y), qn[1], NewVolSales)
        NewVolSales1 = ifelse(NewVolSales > (q3+1.2*y), qn[2], NewVolSales)})

#--------------------------------Exploratory Analysis -------------------------------------


##Univariate Analysis
#Viz
hist(NewVolSales)
hist(Base.Price)

#?qplot
#hist(sqrt(islands), breaks = 12, col = "lightblue", border = "pink")

# Quickplot
# qplot(x, y, ..., data, facets = NULL, margins = FALSE, geom = "auto",
#       xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL,
#       xlab = NULL, ylab = NULL, asp = NA, stat = NULL,
#       position = NULL)

#qplot(mpg, wt, data = mtcars)
#qplot(mpg, wt, data = mtcars, colour = cyl)
#qplot(mpg, wt, data = mtcars, size = cyl)
#qplot(mpg, wt, data = mtcars, facets = vs ~ am)

#names(mtcars)

##Bivariate analysis 

#Correlations
cor(NewVolSales,Base.Price)
cor(NewVolSales,Radio)
cor(NewVolSales,TV)
cor(NewVolSales,InStore)

#Viz for continuous variables
qplot(Base.Price, NewVolSales)
qplot(InStore, NewVolSales)
qplot(Radio, NewVolSales)
plot(TV, NewVolSales)

#Viz for categorical variables
qplot(NewVolSales, Facebook)  #this does not give any good interpretation


#Facebook campaign
boxplot(NewVolSales~ Facebook, col = "red")

#Twitter campaign
boxplot(NewVolSales~ Twitter, col = "red")

#Website campaign
boxplot(NewVolSales~ WebCamp, col = "red")

#Online campaign
boxplot(NewVolSales~ Online, col = "red")

#Inserts
boxplot(NewVolSales~ Inserts, col = "red")


##What is the use of log variables?
##To make a variable in scale with the other variable

qplot(InStore, NewVolSales)
qplot(InStore, log(NewVolSales))


qplot(Radio, log(NewVolSales))

qplot(log(Radio), log(NewVolSales))


##Creating New Variables

#Data TRansformations
mmix$LnSales<-log(NewVolSales)
mmix$LnPrice<-log(Base.Price)
mmix$OfflineSpend<-Radio+TV+InStore

summary(mmix$OfflineSpend)
summary(mmix$NewVolSales)

# Creating price buckets
summary(Base.Price)
#Create price buckets - converting numeric variable into categorical
mmix$Price_Bkt[mmix$Base.Price < 15.03]<-"Low"
mmix$Price_Bkt[mmix$Base.Price >= 15.03 & mmix$Base.Price < 15.33]<-"Avg"
mmix$Price_Bkt[mmix$Base.Price >= 15.33 & mmix$Base.Price < 15.64]<-"High"
mmix$Price_Bkt[mmix$Base.Price >= 15.64]<-"V.High"

# table(mmix$Price_Bkt, mmix$Facebook)

#Viz for categorical variables
#qplot(NewVolSales, Price_Bkt)
boxplot(NewVolSales~ mmix$Price_Bkt)

#Create price buckets - converting numeric variable into categorical
mmix$Price_Bkt[mmix$Base.Price < 15.03]<-"Low"
mmix$Price_Bkt[mmix$Base.Price >= 15.03 & mmix$Base.Price < 15.64]<-"Avg"
mmix$Price_Bkt[mmix$Base.Price >= 15.64]<-"V.High"

mmix$Spend_Bkt[mmix$OfflineSpend < 391]<-"LowSpend"
mmix$Spend_Bkt[mmix$OfflineSpend >= 391 & mmix$OfflineSpend < 510]<-"AvgSpend"
mmix$Spend_Bkt[mmix$OfflineSpend >= 510]<-"HighSpend"

mmix$Sales_Bkt[mmix$NewVolSales < 19049]<-"LowSales"
mmix$Sales_Bkt[mmix$NewVolSales >= 19049 & mmix$NewVolSales < 20943]<-"AvgSales"
mmix$Sales_Bkt[mmix$NewVolSales >= 20943]<-"HighSales"

table(mmix$Spend_Bkt, mmix$Price_Bkt)
table(mmix$Spend_Bkt, mmix$Sales_Bkt)
table(mmix$Price_Bkt, mmix$Sales_Bkt)


 plot(mmix$OfflineSpend, mmix$Base.Price)



#Viz for categorical variables
#qplot(NewVolSales, Price_Bkt)
boxplot(NewVolSales~ mmix$Price_Bkt)

#Create a category called "No campaign"
mmix$Website.Campaign_upd1<-ifelse(is.na(mmix$Website.Campaign),"No campaign",mmix$Website.Campaign)
mmix$Website.Campaign_upd1<-as.factor(mmix$Website.Campaign_upd1)

summary(mmix$Website.Campaign_upd)

summary (mmix$Website.Campaign_upd1)

#----------------

library(ggplot2)
ggplot(mmix,aes(x=NewVolSales)) + geom_histogram()
ggplot(mmix,aes(x=NewVolSales)) + geom_histogram()+ facet_grid(mmix$Facebook)

#box plots : 
ggplot(mmix,aes(y=NewVolSales,x=Price_Bkt))+ geom_boxplot(col = "red")
ggplot(mmix,aes(y=NewVolSales,x=Price_Bkt))+ geom_boxplot() +facet_grid(mmix$Twitter)

#scatter plots

p<-ggplot(mmix,aes(x=NewVolSales,y=Base.Price,color=as.factor(mmix$Price_Bkt)))
p+geom_point()
p+geom_point()+facet_grid(Facebook)
p+geom_point()+facet_grid(mmix$Spend_Bkt~Facebook)

#Missing value treatment using random forest
# install.packages("randomForest")
# library("randomForest")                   
# #Using R package for dealing missing values
# insurance$bmi<-na.roughfix(insurance$bmi) 
# #Quite useful while working with large datasets##median values

library(leaflet)

schools = read.csv("https://s3.us-east-2.amazonaws.com/datafaculty/chicago/schools.csv")

leaflet(schools)%>%addTiles()%>%addCircles(lng = ~long, lat =  ~lat,  popup = ~schoolname)

leaflet(schools)%>%addTiles()%>%addCircles(lng = ~long, lat = ~lat , popup =  ~schoolname)


# shape file 
library(rgdal)

shape1 = readOGR(dsn = "Subway")
shape2 = readOGR(dsn = "nyha_15a")
shape1@data
# pairs of northings and eastings 
shape1@coords
## most library will expect geographical information in lat / long 

shape1_ll = spTransform(shape1,CRS("+init=epsg:4326"))

leaflet(shape1_ll)%>%addTiles()%>%addCircles()

leaflet(shape1_ll)%>%addTiles()%>%addCircles(popup = ~NAME)

shape2@data%>%head()
shape2@polygons[1]%>%head()

shape2_ll = spTransform(shape2, CRS("+init=epsg:4326"))
shape2@data$HealthArea%>%summary()
pal = colorBin("Greens", bins = 4, domain = c(100,9300))

leaflet(shape2_ll)%>%addTiles()%>%addPolygons(fillColor =  ~pal(HealthArea), stroke = F, fillOpacity = 1)
leaflet(shape2_ll)%>%addTiles()%>%addPolygons()

hour = read.csv("hour.csv")
day = read.csv("day.csv")
library(dplyr)
data_hr <- hour%>%group_by(hr)%>%summarize(Mean_Count = mean(cnt))
library(ggplot2)
p<- ggplot(data_hr , aes(x= hr, y = Mean_Count))
p+geom_point()   

p+geom_line()
p+geom_line()+scale_x_continuous(breaks= seq(0,24,1))+scale_y_continuous(breaks = seq(0,500,20))

m <- ggplot(day, aes(x= cnt))
m+ geom_histogram() + facet_grid(weekday~.)

m <- ggplot(day, aes(x= as.factor(weekday), y= cnt))
m+geom_boxplot()+facet_grid(weathersit~.)


