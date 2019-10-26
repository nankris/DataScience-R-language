setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Module 3")














# Base plotting

# Using plot() to study to continous variables

ir<-iris
str(ir)












# Syntax
# plot(x=variable to be displayed on x axis, y = variable to be displayed on y axis)

plot(x=ir$Petal.Width,y=ir$Petal.Length)










# Adding xlabels, ylables and title
#plot(x=ir$Petal.Length,y=ir$Petal.Width, xlab="P L", ylab="P W", main = "PL vs PW") also works
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"))











# Adding colors, plotting symbols







#Adding colors
plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red")











#Adding different plotting symbol

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=2)








plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=3)

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4)










#Adding  more options


plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col="red",pch=4,type="p",lwd=2)

#plot(x=ir$Sepal.Length, y=ir$Sepal.Width, main="sepal l vs sepal W", xlab="sepal Length", ylab="sepal width", col=as.numeric(ir$Species), pch=as.numeric(ir$Species), lwd=3)








# Making a conditional bivariate plot
 
# Seeing relationship across different species

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species)














plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species),col=ir$Species)










plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species))
#cex is used to change the size of points in the plot based on the species in above code






plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),cex=as.numeric(ir$Species),col=ir$Species)







#Adding a legend

plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),pch=as.numeric(ir$Species),cex=as.numeric(ir$Species),col=ir$Species)
unique(ir$Species)
legend(0.2,7,c("Setosa","Versicolor","Verginica"),pch=1:3)







plot(x=ir$Petal.Width,y=ir$Petal.Length,main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal Length"),col=ir$Species,pch=as.numeric(ir$Species))

legend(0.2,7,c("Setosa","Versicolor","Verginica"),pch=1:3,col=1:3)

#Studying a continous variable across groups
#Distribution of Sepal lengths across different species



# Univariate Analysis

#Box plots

boxplot(ir$Petal.Length)










summary(ir$Petal.Length) #Mean<Median, negatively skewed (remember the long tail graph to easily remember +ve and -ve skewed)







boxplot(ir$Sepal.Width)
summary(ir$Sepal.Width) #Mean>Median, positively skewed

#Improving the asethetics of boxplot
boxplot(ir$Petal.Length,col="red",main="Distribution of Petal length")







plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",main="Sepal Length across sepcies",
     col="red")












#Using histograms
hist(ir$Sepal.Width,col="orange")





hist(ir$Sepal.Width,col="orange",labels=TRUE)

hist(ir$Sepal.Width,col="orange",freq=FALSE)
hist(ir$Sepal.Width,col="orange",labels=TRUE,freq=FALSE)
lines(density(ir$Sepal.Width))


#Adding multiple plots in single plotting window
par(mfrow=c(1,2))


plot(x=ir$Species,y=ir$Sepal.Width,xlab="Species",main="Sepal Width across sepcies",
     col="red")
plot(x=ir$Species,y=ir$Sepal.Length,xlab="Species",main="Sepal Length across sepcies",
     col="red")
dev.off()


















##Direct Marketing data
mk<-read.csv("C:/Users/Poori/Desktop/jigsaw/R for data science/Module 3/Data Visualiazaion in R - Pre class Videos/DirectMarketing.csv")













library(ggplot2)

#Understand the relationship between Salary and AmountSpent
p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_point()

#aes is aesthetic mapping 
#geom_point is points in the map



# this also works the same
p<-ggplot(mk,aes(x=Salary,y=AmountSpent,colour=Gender))
p+geom_point()





#Understanding the conditional relationship based on Gender
p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
q<-p+geom_point(aes(colour=Gender))
#  p+geom_point(aes(col=Gender)) also works the same
#geometrical object is point so geom_point
q
q+xlab("Salary in $")+ylab("Expenditure in $")









#Making a trellis plot for both the genders and fitting a tred line
p+geom_point(aes(colour=Gender))+geom_smooth(aes(colour=Gender))

#Creating a trellis plot
p+geom_point(aes(colour=Gender))+geom_smooth(aes(colour=Gender))+facet_grid(Gender~.)


# try
p+geom_point(aes(colour=Gender))+geom_smooth(aes(colour=Gender))+facet_grid(.~Gender)








#Understanding Univaraites
p<-ggplot(mk,aes(x=AmountSpent))
p+geom_histogram()






# also try this code
p<-ggplot(mk,aes(x=AmountSpent, colour=Gender))
p+geom_histogram()



#Understanding Gender wise distribution
p+geom_histogram(aes(fill=Gender))








#Modifying the position
p+geom_histogram(aes(fill=Gender,colour=Gender),position="stack",alpha=0.3)
#p+geom_histogram(aes(fill=Gender,colour=Gender),position="stack",alpha=0.3, binwidth = 50)









#Alternative, draw a trellis plot
p+geom_histogram(aes(fill=Gender))+facet_grid(Gender~.)

#p+geom_histogram(aes(fill=Gender), position = "dodge")
#








#Polishing the graph
p+geom_histogram(aes(fill=Gender,colour=Gender),alpha=0.3)+facet_grid(Gender~.)











#Boxplots
p<-ggplot(mk,aes(y=AmountSpent,x=Gender,fill=Gender))
p+geom_boxplot()









#Density plots
options(scipen=999)
#This is to prevent scientific notation as output, i.e., e+09 etc notations are not seen 
# don't know how to overwrite options() settings
p<-ggplot(mk,aes(x=AmountSpent))
#p+geom_density(aes(fill=Gender,colour=Gender),alpha=0.3, position="stack")
#p+geom_density(aes(fill=Gender,colour=Gender),alpha=0.3, position="identity")
#p+geom_density(aes())
#p+geom_density(aes(colour=Gender),alpha=0.3)
#p+geom_density(aes(fill=Gender),alpha=0.3)
p+geom_density(aes(fill=Gender,colour=Gender),alpha=0.3)















#Conditional density plot
p+geom_density(aes(fill=Gender))










#Improving the plot
p+geom_density(aes(fill=Gender,colour=Gender),alpha=0.4)














#2d counts (bivariate heat maps) 

p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_point(aes)
p+geom_bin2d()












#Changing ticks on a continous scale
p<-ggplot(mk,aes(x=Salary,y=AmountSpent))
p+geom_point()

p+geom_point()+scale_x_continuous(breaks=seq(0,150000,10000))+scale_y_continuous(breaks=seq(0,6000,500))


#Changing fill behaviour
p+geom_bin2d()+scale_fill_gradientn(colours=c("blue","white","red"))+scale_x_continuous(breaks=seq(0,150000,10000))+scale_y_continuous(breaks=seq(0,6000,500))
#p+geom_bin2d()+scale_fill_gradientn(colours=c("blue","white","red","green"))



#Geospatial visualization
library(ggmap)
library(ggplot2)
library(dplyr)
library(rgdal)
#rgdal to read shape information


map<-get_map(location = "bangalore",maptype="hybrid")
ggmap(map)

#Overlaying data on the map from a text file
sh<-read.csv("schools.csv")

head(sh)

ggmap(map)+geom_point(data=sh,aes(x=long,y=lat),colour="red")


#https://data.cityofnewyork.us/Health/Health-Areas/5p78-k3zm
shape1<-readOGR(dsn="nyha_15a","nyha")

class(shape1)

#shape1 data,ploygons,plotOrder,bbox,proj4string

head(shape1@data)

#Look at the locational information
shape1@bbox
#Long-lat needs to be converted to proper format
shape1<-spTransform(shape1,CRS("+init=epsg:4326"))


#extract long-lat data 
shape1.f<-fortify(shape1)
#fortify is used to convert it to data frame
# see head(shape1,1) and head(shape1.f,1)
head(shape1.f)
head(shape1@data)

dim(shape1.f)
dim(shape1@data)

shape1@data$id<-unique(shape1.f$id)

#Merging the data
shapeM<-merge(shape1.f,shape1@data,by.x="id",by.y="id")

map<-get_map("New York City",maptype="terrain")
ggmap(map)+geom_polygon(data=shapeM,aes(x=long,y=lat,group=id,fill=Shape_Area),alpha=0.4,colour="black")+scale_fill_gradientn(colours = c("red","white","blue"))

options(scipen=999)

## SpatialPointsDataFrame Newyork Subway Entrance Data
shape2<-readOGR(dsn="Subway","DOITT_SUBWAY_ENTRANCE_01_13SEPT2010")

class(shape2)



#Components of shape2 :data, coordinate.nrs, coords,bbox,proj4string
shape2@

dim(shape2@data)

dim(shape2@coords)

#Contents of shape2@coords
head(shape2@coords)

#Data in Northings and Eastings: Convert it into Long-Lat
shape2<-spTransform(shape2,CRS("+init=epsg:4326"))

#Combine data and long latt info as a dataframe
dataC<-data.frame(shape2@data,shape2@coords)
head(dataC)
map<-get_map("New York City",maptype="satellite")
ggmap(map)+geom_point(data=dataC,aes(x=coords.x1,y=coords.x2),colour="red",alpha=0.3)



#Probability and Statistics Class 16 th march
#SPSS is not popularly used, GUI based, created by IBM, we don't do any code in SPSS
#SQL, they won't cover SQL in depth, in data engineering side SQL will be used alot,
#data analytics people won't use SQL alot
#Inferential statistics: we talk about sample, population. Drawing inferences about sample, about
#the population and for that we use probality
#most part we deal inferential statistics, and less descriptive statistics
#Central Tendancies are mean median and mode (are descriptive statistics)
# Chebyshev's inequality and quartile
# Categorical data - Frequency distribution
# we will do predictive modeling
# Percentile output: shows overall how our data is distributed
# qualitative and quantitative
# qualitative is discrete/categorical, Descriptive
# quantitative is numeric/continuous,
# for categorical variables we can plot histogram, Histogram: frequency distribution of that
#columns in continuos data bin will be continuous range and for discrete data bin will be
# unique value
# histogram can be used for both continuous and discrete

# different data types include numeric, integer, character, date, time
# qualitative and quantitative we have
# qualitative data is discrete and categorical, continuous qualitative
# quantitative data is numeric or continuous, discrete quantitative
# for descriptive statistics we need continuous data (not necessarily as we need discrete data for 
# mode and for median also it works on discrete data)
# Histogram frequency distribution of that column when we have discrete data
# how many unique values are occuring for that particular column
# like number of policies we are getting
# you can use continuous data for discriptive statistics
# we can also use pivot table instead of histogram and each have their own advantages
# pivot table also calculates fequency data if we properly kept variables in rows and columns
# chebyshev's inequality

# cntrl+shft+c for multiline columns
# shift down arrow for multiple line selections

# mean + two sd and mean-two sd

#inferential statistics 


# probability theory
# statistics type 
# population sample
# descriptive: provides a visual, tabular or numeric summary of large amounts of data to explain 
#               its key characteristics
#               identify patterns in large amounts of data 
#               data set is assumed as stand alone
# Inferential: uses a sample of data to make inferences about the general population
#             assumes that sample is representative of a large population
#             draws conclusions about population based on a smaller sample 
# 
# statistical inference
# we can estimate unknown population parameters based on properties of a sample
# we can test hypothesis about a population based on sample parameters 
# 
# why do we need to seperate the two?
# population tends to be very large and making it difficult
# to collect and analyze data on the population 
# 
# it is easier to take a subset of the population analyzxe the subset and 
# then make inferences about the population 
# 
# The second point depends on a fundamental assumption - what is that? 

# different samplings are stratified random sampling and simple random sampling

# a good sample is without bias, full coverage, nonresponse inclusive 

# simple random sample and stratified random sampling
 
# probability the basics 
# probability of event A happening = p(A)= number of outcomes where event A occurs/total no. of 
# possible outcomes 
# 
# Random sampling 
# here each element in the population have equal chance of selection
# we have to see whether sample is representative of the population
# this is inferential statistics: where we infer about population based on sample data 
# 
# Random Variable: a variable whose outcome is not a 100% certain
# ex: tossing a coin three times and getting atleast two heads 
# The total no. of sales of a retail store is a random variable 
# 
# average is 10000, and 1000 will be unlikely and 9500, 10500 are likely events to happen for that 
# happen 
# 
# if there is no uncertainity then it is not a random variable  
# when we deal with inferential statistics then it associates with randomness 
# expected value of a certain outcome will be present but not certainly we can't say 
# 
# in statistics we try to find how much uncertainity we are dealing with
# 
# predictive analysis only possible if variable we are dealing with is a random variable
# 
# The basis of inferential statistics is we always deal with random variable
# also we calculate probability of a particular outcome
# 
# probability we are dealing with is random chance probability
# 
# coin toss is a random variable and it is descrete random variable
# depending upon nature of random variable it will follow a probability distribution function
# 
# 
# probability distribution function: if we plot all the outcomes and plot the probability of that 
# outcome then we will get the probability density funciton
# probability density function gives all the possible outcomes u can get
# all those possible outcomes are plotted against probablity in probability density function
# 
# coin toss follows binomial distribution function
# binom.dist() is used in excel for calculating binomial distribution


# sum of pobability density functoin is 1, i.e area that is covered under plot graph in pdf 
# function is 1
# the value we are getting is expected value
# if coin is fair we see maximum probability in center tosses (like out of 10 tosses 5 heads have
# high probability) 
# the distribution is normal in coin toss

# when we toss a coin once: bernoulli trial
# if two outcomes then binomial distribution
# pdf p(x) formula for binomial distribution you can use
 
# cumulative is if we add previous values
# like probability of heads less that or equal 5 in 10 throws then we need cumulative probabilty
 
# pdf (exactly 5 heads in 10 throws)
# cdf (less than or equal to 5 heads out of 10 throws)
 
# binomial distribution examples given in the class, look at excel sheet
 
# possion distribution:
# random variable can be discrete and can be continuous
# discrete random variable: binomial pdf, poisson distributionb
# continuous random variable: normal pdf, t-test
 
# poisson: used to model number of events occuring in a time frame
# ex: number of insurance claims in a month
# disease spread in a day
# number of telephone calls in an hour
# number of patients needing emergency services in a day

# The following conditions apply to correctly use a poisson distribution
# 1. events have to be counted as whole numbers
# 2. events are independent: so if one event occurs, it does not impact the chances of 
# second event occuring
# 3. avg frequency of occurence for the given time period is known
# 4. number of events that have already occured can be counted



# lambda is the mean number of occurences in a given interval of time
# there is no n in the distribution function

# solve a question on poisson distribution as given in the class

# difference between binomial or poisson distribution
# see the slide of class ppt





# data visualization in R class 17th march
# visualization is pre step for modeling
# another use is simply to understand the data
# for building model we need to understand data, and visualization will do that
# data manipulation use: very basic and very useful
library(dplyr)
library(ggplot2)
# grammer of plotting elements, Aesthetic maps, Geoms, Statistical Transformation, Scales
# Once we practice enough, charts in r are 20 times more powerful than charts in excel. Easier and
# powerful also
# Grammar of graphics because, it is building blocks of graphs: aesthetic maps, geoms, statistical 
# transformatin, scales
# aesthetic maps: defining x axis and y axis
# Geoms: they are deciding elements to plot line, point or histogram
# scales: decides measuring unit, range, size of bubble, size of line etc.
# statistical transformation: if creating a chart and showing something which is not available in 
# data then we are doing statistical transformation
# example of statistical transformation: to create hist and how many males and females are to be 
# shown. But in raw data it won't have count of male and female. This is called statistical
# transformation
# if age vs income then no statistical transformation. Because those data is already available in
# raw data average income by age band is not available in raw data and we calculate it. 
# This is also statistical transformation
# statistical transforamtion is when you are directly plotting in chart that is not available in 
# the raw data
# Geoms: line chart or point chart etc.
# scales: color, size etc in the plots
library(ggplot2)
# In ggplot2 aesthetics  always works on input variable of a data
# whenever we are using aesthetics we must use variable
p+geom_point(aes(color="blue")) # this won't to what it supposed to do because blue is not a 
                                # variable
# to get color we need to get rid of aesthetics
p+geom_point(color="blue")



p<-ggplot(dat1,aes(x=Gender))
# y axis didn't decided
p+geom_bar() #why?
# bar chart is categorical variable in data and doing frequency distribution 
# difference between histogram and bar chart
# histogram with continuous variable and bar chart works with discrete variables (categorical
# variable)
p<-ggplot(mtcars,aes(x=factor(cyl)))
p+geom_bar()
# cartesian and polar coordinates
p+geom_bar()+coord_polar()
# bar chart and pie chart are same thing but only coordinates are different

#Adjusting scales
# scale_x_continuous() scale we are changing and x axis scale and continuous variable we are 
# changing so we use that command
# scale_size_discrete is used because we control scale, size is element which is one of the 
# aesthetic, and the variable is discrete
# scale, aesthetic, type of the aesthetic
# sclae_color_continuous: color is assigned to continuous so we are using, color is the aesthetic
# scale_color_continous(low="blue", high="red"). blue to low income and red to high income
# once we practice with good amount of time spending we will be able to do it easily
# ggplot2 we are learning is to create insights about it once we learned how to create good charts
# sclae_size_discrete(label=c("F","M"),range=c(1,1))
# + guides(size=F,color=F) then legends will be dissapeared
# facet_grid used for cross tabulation
# to see the same chart by different variables
# tilda is a sperator. On left we can write something, on right we can write something.
# filter only selects one of the values of variable
# facet grid will break the chart on the basis of variable that we give as input
# currently we are breaking it with Gender .~Gender
# facet_grid is only useful for categorical variable
# what is ~, Gender~. what it does, then it will give horizontal break. If .~Gender then vertical break
# if Gender~Marital then male and female as horizontal cut and marital status on vertical cut
# Gender~Marital+Employment. Gender cut horizontally and marital+employment vertically. It is 
# complecated because we are using 2 variables on y axis
# aesthetic can control x, y axis, size and color
# scale works on top of aesthetic, scale_x, scale_y, scale_size, scale_color are 4 possible combinations


# why part of ggplot2
# try and get insights of the data
# business context

# we joined insurance company and task is deciding risk in premium
# insurance company has two models
# if customer is claiming the insurance, how much claim should he get

# univariate and bivariate analysis
# univariate analysis is when you are analysing one variable at a time
# bivariate is anaysing when two variables
# unless we understood individual distribution we won't appreciate bi variate
# variables are categorical and continuous
# categorical vs continuous, categorical vs categorical, continous vs continous are three types of
# bi variates we can have
# who are good with r, good with syntax, good with modeling. But few people who are good with 
# getting insights with data
# differentiator is having common sense and business accument to derive pattern
# coding is higien, if we don't know coding we won't be able to derive insights

# outlier is a rear and extreme value
# no missing value in age
# BMI there are two missing values
# below 18 age they are not accepting the insurance claims
# summary, table we analyse. But try to analyse the data.
# best insights: percentage of smokers are highest
# best insights: percentage of smokers are high in male
# insight: derivable from data, hypothesis: assumption depending on data, and needs to be proved 
# separately
# box plot is used for understanding continuous variable
# 90 and 95 are outliers
# we are sure about outliers after seeing boxplot
# most customers are in the range of 18 to 65
# some non smokers also have high charges
# dodge: to not club the bars in histogram.

# data engineering: syntax we see mostly
# business analyst: what does the chart says
# pairs we use for seeing correlation matrix among variables in a data frame\
# 7-8 hours on this data set, only on ggplot2 then we can open up the data. Then we will be 
# very good at getting insights


# March 23 class
# summary
# enough practice syntax won't be tough and it will looks
# aesthetics use fill color size x y 
# aesthetics works always on variable
# scale can control anything the aesthetic would define
# scale_type_continous or discrete
# type is whatever aesthetic will define
# if aesthetic is mapped to continuous then continuous
# before defining point or line chart we can use aesthetic or after point or line chart definition
# guides(size=F) those aesthetics will go away (this is to remove legend)


ir<-iris
hist(ir$Sepal.Width)
?hist
hist(ir$Sepal.Width,col="orange",labels=TRUE)

# Box plots
boxplot(ir$Sepal.Width)
summary(ir$Sepal.Width)
# rectangle boundaries are 1st and 3rd quartiles,
x<-boxplot(ir$Sepal.Width)
x
x$out
names(x)

boxplot(ir$Petal.Length,col="red",main="Distribution of Petal Length")

# par command doing pre processsing


# pch is plotting caracter, takes values from 1 to 25
# type variable, type="p", type="l" p point, l line chart
# type is like geom_point, geom_line 
# lwd is line width
# plot types: n No plotting, h Histogram o Both overplotted
# aes(color=species) to do col=ir$Species
# pattern is only visible using plots
# cex changes size, character extension or expansion

# xlim=c(0,3) it is to control the x axis


# MMix data set i have work on
# Base.Price is averaeg price index of that week
# using data to see marketing campaign is useful or not
# colSums(is.na(mmix))
# hist(NewVolSales)

# index<-which(mmix$NewVolSales %in% list_out)
# %in% is to compare in list_out (list_out is x$out)
# mmix$NewVolSales[index]
# mean_sales<-mean(mmix$NewVolSales,na.rm=TRUE)
# mmix$NewVolSales[index]<-mean_sales
# more purest way is to not considering outlier while calculating mean

# replacing outlers with boundaries will make more sense
# this is another way of doing other than replacing with mean
# qn<-quantile(mmix$NewVolSales,c(0.05,0.95),na.rm=TRUE)
# y<-IQR(NewVolSales)
# q1<-quantile(NewVolSales,0.25)
# q3<-quantile(NewVolSales,0.75)
# IQR is inter quartile range, q3-q1 is inter quartile range



# mmix<-within(mmix,{NewVolSales1=ifelse(NewVolSales<(q1-1.5*y),NewVolSales) NewVolSales1=ifelse(NewVolSales>(q3+1.5*y),qn[2],NewVolSales)})
# if else command, first value of qn is fifth quantile
# second element of qn is 95th percentile




?within
# data preparation we didn't covered
# within is don't want to use column names, within is similar to attach
# definitin of outlier in boxplot is different from the custom definition we use
# Still we will get outlier even if we replaced by above commands


# exploratory analysis

# correlations

# cor(NewVolSales,Base.Price)
# -1 to 1 is correlation range



# qplot(Base.Price, NewVolSales)
# qplot is command of scatter plot
# correlation is assessing trend between two continuous variables, it will point to a direction 
# and magnitude
# qplot is relatively clear than basic plots

# capability of deciding what plot to use is an important skill which comes by practice
# qplot(InStore, log(NewVolSales))

# sometimes by taking log we get plot more clearer
# to reduce range of a variable then we use log function

# we can create price continuous variable into discrete variables then it will be clear that 
# it will become discrete
# continuous vs continuous is not possible in box plot


# ggmap
# not frequently used, but if we work with geospatial data then it is very important

# instead of using ggmpa package we will use leaflet package
# until three months back ggmap is free, now google has made it paid
# leaflet package is new package and is free 

# use of these packages: taregt credit card customers for opening a bank account
# your nearest branch is this and this, and you can map these things using these packages and 
# allocate sales allocation
# city planning is another use case: where are do i need to increase crime control at night
# for opening new ATMs is another use case
# lot of demand of ATMs are calculated and used this information to keep new ATMs
# if we are not in that particular domain of working then we won't use
# 99% data analytics don't require. It is a niche concept
# 


install.packages(leaflet)
library(leaflet)
schools<-read.csv("")
leaflet(schools)%>%addTitles() # displays world map
leaflet(schools)%>%addTitles()%>%addCircles(lng=~long,lat=~lat)

# in leaflet: whenever we want to reference a variable then we use ~ in front of it
# so ~long we use for longitudinal data

# to differentiate by school type
leaflet(schools)%>%addTiles()%>%addCircles(lng=~long,lat=~lat,popup=~schoolname)

# if we click on the point then it will show which school it is

# multiple data labels also we can do

# lot of times data is not available in csv and it is available in shapefiles


library(rgdal)
shape1=readOGR(dsn="Subway")
shape2=readOGR(dsn="nyha_15a")
shape1@data
# data element is like school name, address, id of school etc
# coordinate element is coordinates of the school
# data elemets are some name some link and some LINE
# shape1@data%>%head()
# shape1@coords%>%head()
# the data are northings and eastings
# we have to convert it into longitude andlatitude
shape1_ll=spTransform(shape1,CRS("+init=epsg:4326"))
leaflet(shape1_ll)%>%addTiles()%>%addCircles()
leaflet(shape1_ll)%>%addTiles()%>%addCircles(popup=~NAME)



# in shape1 data name column is NAME


# second shapefile is polygon file

# shape1 file has name lat long
# every column has certain data

# shape2 have various rectangle in it

# shape2 city lat long
# in lat, long we need multiple points as polygons require more data
# shape2 is in different shape type

  



# March 24 class (Excel introduction) (have to work on file that is been sent and also on once 
# again see the class video)



# March 31 Class (Hypothesis Testing)
# by ankit sir (data visualization also same sir) (He is very good) (visualization and data 
# manipulation is very well explained earlier)
'
Descriptive statistics (it is done)(mean, median, mode, skewness, kurtosis all are covered earlier)
We need more data set to practive all those things.But better practice is
on the same data set repeat the concept multiple times then it will be best
Try out minor variations multiple times. Once comfortable you can move on to new data sets
Instead of new data sets directly do it on same data set multiple times
Keep doing assignments and everything will be fine
Class summary
-Intro to descriptive statistics (5 mins)
-difference between descriptive and inferential
-random variables
-distributions
-binomial distribution
-poisson distribution
-normal distribution
-background to why we use probability and all 
-central limit theorem
-Hypothesis testing overview
-all probability calculations in that we must be expert
-if we dont do probability properly then we wont be able to 
do good in hypothesis testing and if hypothesis testing is not 
good then it is basic to future data science

Actual business examples on binomial, continuous distribution
and why all this is relevant for computation of hypothesis testing

Descriptive statistics also known as summary statistics
Why Descriptive statistics done? What is the use
In india three models of analytics 
One people are working in consultancies like fractal
musigma where we work for external client (Everytime we work on new data)
Second offshoring centres such as banks (Everytime we work on new data)
Third is we are working for indian company (Inhouse like flipkart etc)
fractal
musigma 


We must know what is within the data. To do that
open data and go to each column. Sometimes it is not possible
If you have less data then we can go through the data directly
If we have huge data then it is not easy to go through all the data points toframe a mental picture
To do this job we use summary statistics (If we have large data)

What is the first component of descriptive statistics
-Central Tendency
Mean, median mode are ways to measure central tendency

If we have 10 lakh customers then for income what is the best measure of central tendency
Ans: then mean will be good measure

Use case of computing median we have better understanding of data (like 50 % below this particular value and all)
Mode is not that good measure for continuous variable
Mode is useful for discrete variables
Knowing formulas of mean, median and mode but more important is understanding of metric(When 
to use and all)

Standard deviation: deviation from the mean, how much it is deviated, how data distributed from 
mean
Standard deviation is second aspect of central tendency (Disperson)
If we have 10 customers and all of them have income same then there will be no dispersion

if mumbai sd is 20, and average 80 then people are getting 60 to 100. Mean +- sd covers majority of 
population
if bangalore sd is 15 and average 80 then people are getting 65 to 95
Use of standard deviation is to convey something about the data

Value is in concepts and not in formulas

Third measure in central tendensy is shape 
Positively skewed, negatively skewed and 0 skewed
Computing skewness is of no use but understanding it is important
Income distribution of india is positively skewed or negatively skewed?
Ans: Positively skewed (X axis is number of people, y axis is income)
Because there are more people in lower side than higher side
What is the use of skewness? 
Ways of conveying something to others

Mean=median then 0 skewed and will be a bell shaped curve (Normal distribution)

Kurtosis: it is 4th pillar of descriptive statistics (4th aspect of central tendensy)
This talks about how peaky is the data
Whereever the peak is there
What is the x axis corresponding to the peak? (In kurtosis)
Ans: Mode
at peak max observations are present
peak with highest height is the mode (If we have multiple peaks)


Inferential statistics:
What is inferential statistics?
about infering about population using the sample
We use sample to make those inferences (we make inferences on populations)
While doing this thing we will end up doing future predictions and business related decisions

Population: 
Sample: By definition it is not necessary to represent the population. Sample by definition 
should be subset of population. If it represents the population it is a good sample. We need sample
which is representative of data. Sample must be subset of population.
Sample is not representative is still a sample. 

can i make inferences about population with any sample?
Ans: No

What is a representative sample? When can one say that?
When it has similar characteristics of population, can say same story as population, must be 
collected without any bias


Discriptive statistics of sample is similar to population then sample is representative 
of population

If we were given a choice on working on sample and population what will u choose?
Computation will be higher on population 
Analysing population is time consuming
If we dont have time on processing data then sample we will work on

Sometimes access to population is not possible
Even if u have population computing will be expensive
also Expensive to collect population

So we most likely use sample for the population

What are the ways to collecting a representative sample?
By randomly selecting the data is one way
There is no reason that it will have bias if picked up randomly

Random sampling
Stratified sampling

If we pick a sample of size 200 and size 1000, which one will be better?
1000 will be better. Bigger sample is more representative to the population

Choose as randomly as possible and choose as large sample as possible

Random should be representative most of the times

Stratified sampling: Here we will know distribution of population in different age groups (Lets say)
Now as we already know the distribution then we will chose the same distribution in the smaple as well
Like 20% low 50% medium and 30% high age customers in population we also chose same percent in 
sample as well

Stratified sampling is generally done if variables of critical importance are there then we will 
ensure
that in sampling that critical variable is represeted properly in sample 

Stratified sampling will arise from business need

Cluster sampling is different concept (Different use case and different nuances)

90% - 95% times simple random sampling will be useful in real life business 

-Case Study

Airline sales example

In sales department we are working
For every flight there are 100 seats
Sometimes we can sell 100 tickets, sometimes people wont show up
So how many seats you have to oversell to compensate to those people who wont showup

Different companies have different strategies to sell number of extra tickets
But why are we not sure how many tickets to sell tomorrow even if we have past data?
The reason is future is not predictable and cant exactly say how many people will cancel
It is a random variable so we wont be able to predict how many people cancel the next day

Most of the business scenarios are driven by random variables
Even if it is random variable we will know two things
1) what is the range of possible outcomes
2) probability is also sometimes known from past data


Number of heads in 10 coin tosses is a random variable
Range of outcomes is 0 to 10
Which outcome is most probable? (5 has highest probable)
Probability of 3 and 7 are equal
Probability of 0 and 10 are equal
3 has higher probability than 0 heads
between 5 and 3 5 is more probable
5 will have maximum possibility and from 6 it goes down

binom.dist() function is used excel 
binom.dist(5,10,0.5,FALSE) FALSE for point probability, TRUE for cumulative probability

There are two types of random variables.
1) Continuous RV
2) Discrete RV

Discrete RV: RV will be discrete

Number of cars which are going to be parked today
Number of studets that are going to join a class

Continuous RV:
Income going to generate in next quarter
Amount of sales

Various Types of discrete distributions:
1) Binomial Discrete RV, 2) Poisson Discrete RV

1) Binomial Discrete RV:
A discrete variable which can take only two inputs
Ex: Will it rain tomorrow or not

Number of heads in a ten coin tosses is a binomial random variable as the output is only two choices

Probability that exactly 4 heads
binom.dist(4,10,0.5,FALSE) is the excel formula

We are keeping false as we need point probability
If the question is less than equal to 4 heads then cumulative will be true

Probability that less than equal to 3 heads will come
binom.dist(3,10,0.5,TRUE)

Probability that less than 7 heads will come
binom.dist(6,10,0.5,TRUE)

Probability that we get greater than equal to 5 heads will come
1-binom.dist(4,10,0.5,TRUE)

Probability of getting greter than 7 heads
1-binom.dist(7,10,0.5,TRUE)

Probabiliyt of getting 7 heads
1-binom.dist(7,10,0.5,FALSE)

Probability of getting <= 3 heads
binom.dist(3,10,0.5,TRUE)

Probability of getting >= 6 heads
1-binom.dist(5,10,0.5,TRUE)

Poisson Distribution (How poisson is different from binomial)
Poisson is a discrete random variable

What is poisson distribution?
A RV can take more than two values
For poisson concept of time frame is involved
Ex:-How many calls will i get in next hour
It can only take discrete values
For solving this we need past average number
Lambda is average hourly number (If question says hourly details)
Lambda is average dialy number (If question says dialy details)

If 38 people enrolled in this class what is the probability that 28 people joins the class
It is a binomial question

Better to understand which is binomial and which is poisson

Binomial distribution is always bounded
In the question the range of number of joining students is 0-38 (so bounded)

There is a open webinar happening of everybody in india
In that webinar first 15 min entry is allowed and average number of entries is 200 people from india joining
How many people will join the session in first 15 minutes from india?
Now this is a poisson (It is not bounded because number of people that could join is not bounded)
(Number of people joining could be 200 or 1000 or 10,000)

35 enrolled, exactly 28 people joining and prob for every student is 70%
binom.dist(28,35,0.7,FALSE) 

calls per hour = 50 in call center, what is the prob that in next 1 hour <=55 calls will come
=poisson.dist(55,50,TRUE)

What is the prob that >66 calls will come?
=1-poisson.dist(66,50,TRUE)
# every one in class is wrong and if i am correct i will be very happy
(Only kanya, prateek and another is given)
Not happy(Have to be very attentive for the questions)(My logic is correct only i kept 65 
instead of 66)

What is normal distribution?
Symmetric about the (Single) mean
Mean=Median=Mode
The two tails extend indefinitely and never touch the axis

Normal distribution is not a single distribution, it is a family of distributions
For different mean different curve will be there
For difining normal distribution we need only two parameters (Mean and standard deviation are we 
needed)

We cant difine nearest to some point in x axis, like nearest to 12 could be 11.999999 or 12.0001 
or some other value

For continuous point probability is 0
so <12 and <=12 are same

But for discrete it matters <3 = <=2 heads

But for continuous it doesnt matter

Delivery time from zomato is a normal distribution with mean 25 mins and sd of 3 mins

What is the prob that food will get delivered in <=30 mins

=norm.dist(30,25,2,TRUE) #95% chance

What is the prob that food will get delivered in >24 mins?

=1-norm.dist(24,25,2,TRUE)

---Business Example---

e-commerce website where we have 4 resources
suppose each resource can handle 101 orders
Website can handle upto 450 orders
1 Order = 100rs revenue
1 resource = 500 rupees daily cost

406 orders are coming in any given day
Then how many will our resources can handle?
Resource capacity is only 404
Websites will log those extra 2 orders and resources cant handle those two orders
2 orders are not fulfulled
Revenue loss is 200 rs
suppose we are getting 420 orders then 1600 revenue loss
cost of a resource is 500 so we will increase the resource by 1 if 420 orders
So we cant predict it before the day
We cant hire or fire every day based on the orders on dialy basis
Q) Should i hire another resource for future?

orders per day - daily average is 398

>=410 orders?
It is a random variable, it is poisson because it is not bounded and time frame concept is there
1-poisson.dist(409,398,true) # 28 percent

It requires logical analysis
if 420 orders # 20%

Number of orders                
405-450
after 450 website wont take the order as it is maximum capacity
=poisson.dist(405,398,FALSE)
do for 406,407 till 450
Also compute revenue loss (406-404)*100
expected loss = revenue loss * probability
total expected loss is sum of all the expected loss
then total expected loss is 507
This means we are saving 7 rupees more for 405 to 450
So we should hire the resource by this analysis (507)

Why are we multiplying probability with revenue?
because probability of each losses is not same
so we multiply with probability

By using poisson distribution we are solving business problem


if 5% response rate
if target 500 people what is prob that 40 or more people will respond

Binomial formula
1-binom.dist(39,500,0.05,TRUE)


# 7 th April class Hypothesis testing
Hypothesis testing : assumptions made based on historical data
About making inferences, using knowledge available in data
Underlying things is: we always talk about some variable
whenever we talk hypothesis testing. 
Random chance variation: variation because of just random chance
Or the variation could be due to some external factor
This is the idea of hypothesis testing
Sales: day1 5000, day2 6000, day3 5000. Flucutation could be due to 
random chance variation. But also due to some external event
This we have to prove. External factors such as holiday.

How do we quantify or how do we know that changes in sample
By taking each dependent variablt and establish relation ship with outcome
Lets take avg number of no shows as output

What is the probability of an event to happen due to random chance to happen

Q) avg no. of no shows. Population says on an avg 5% problty of no shows
and when we pick sample of last 10 days and avg no. of no shows coming is 3.5%
lets say prob of getting 3.5% of no shows is 40% when we pluck out a sample

i.e., seing 3.5% or less is high probable

Lets say prob of getting 3.5% or less of no shows is 15%
The likelihood decreased. It is 15% now so we can say that 
external factor involved. (dont know why)

Hypothesis testing classic example
weight produced expected to be 2.5 lbs
standard deviation is 0.12 lbs
Random sample of 45 units is taken
and mean weight is 2.68 lbs
Is there an issue?
let us say normally distributed

There is always quality check at end.
As a manufacturing unit, can i do quality check of all
the products? because it is not feasible, high cost

So we take a sample out of total production and check 
the quality.

We have to take sample as large as possible to be representative
We have to take sample as large as possible according to the resources are available

Is there an issue with the batch the is being produced
as the weight is 2.68 lbs and is greater than genera average
of 2.5 lbs

Is it because random chance variation
or the machine is creating large pistons

take proability of getting >-2.68 pounds using normal 
distribution
The lower u go much strickter is the evaluation
5% we take regularly and is accepted industrially and academically

Hypothesis testing framework

Null Hypothesis:
No difference in sample and population, due to random chance variation
sample mean and population mean are different because of random chance variation
no issues at all, all is good

Alternate Hypothesis (Ha):
Some issue with sample drawn from the population
There are som efactor driving the difference between
sample and population
Negative of Null Hypothesis

Identify the correct distribution we are gonna use
we know weight is continuous variable and it is falling on
normal distribution, so we take normal distribution

Significance level (alpha: ) Criterion used for rejecting the NULL
hypothesis

P-Value: The probability of outcomes more extreme than the observed outcome

Test Distribution: The right test distribution to be used 
to calculate probability of outcome being driven by random chance variation


y-probability values
x-variable
As long as there is outcome there must be associated prbability

Observing >=2.68 is gonna be all the associated probabilities
The area under the curve above 2.68 x value is the probability

prob(<2.68)=1-prob(>=2.68)

cutoff value alpha: alpha is probability value
0.05 is the probability we want and we have to find the associated outcome

All the outcomes towards right side of the alpha = 0.05 will be less than 0.05

H0 Sample mean weight is same as popul mean weight (production process fine)
H1 Sample mean weight>popul mean weight (Production process is not fine)

Distribution: Normal Distribution
Alpha=0.05 (Significance level) (Cutoff decided to reject null hypothesis)

probability of observing mean weight of 2.68 or more in a sample,
when population mean is 2.5 and standard deviation is 0.12

1-normdist(2.68,2.5,0.12,TRUE) #Ans

We got 0.06681 so this is greater than 5% so we fail to reject null hypothesis 

There is 7% chance of getting mean weight of 2.68 or more
purely because of random chance variation

##Central limit theorem##
sample1 10 units
sample2 100 units
and batch of 1000 units

s2 suits because much more representative

mean weight = 2.5
if we pick one piston then we wont get mean 2.5
as we increase the sample size then the mean will come closer
to 2.5

if we chose more samples then mean of that sample will be closer to 
population mean

if we keep on collecting samples of size n and total 1000 samples
each sample will have its own mean
means we have 1000 means i mean if we select 1000 samples

The distribution of sample means is going to follow the normal distribution

Population will have any kind of distribution 
But sample distribution is going to b normal distribution

N(Mean(Population)),sd = sd(population)/sqrt(sample size)

population deviatio is more
if we take samples then the deviation will be less
so we divide the populatino deviation with sqrt(sample size)

variation in mean when we took 10 units only
if we have large size of samples then variation in mean will be less

Irrespective of what population distribution
when we try to pull out multiple samples, mean of that 
samples tend to be closer to normal distribution.

=NORMDIST(2.55,2.5,0.12/SQRT(45),TRUE) #close to 0.9999 we willg et

0.0025 prob of random chance variation

< 5% chance of random chance variation then we reject null hypthesis

# so earlier we did wrong. By considering central limit theorem
we are getting correct output. SO we always divide with sqrt(sample size)


Sample Problem:
In the recent years, the mean age of all college students in a city has been 23
A random sample of 50 students revealed that their mean age is 24.1
Assuming age is normally distributed with SD of 2.4
Can we infer at significance level of 0.05 that the mean age has changed for the city?


We are talking about two tail tests

Population mean is 23
Sample mean age is 24.1
Standard deviation from populatin is 2.4
Sample size is 50
Distribution is normal

H0 No change is age of sample and population: mean(sample age)=mean(population age)
Aleternate hypothesis: There is change in age due to external factor
means mean(Sample age)!=mean(population age)

alpha=0.05
we need to do two tail test so we will make stricter
so 0.05/2 = 0.0256 for 2 tail test we use this alpha = 0.025

Most of the businesses do 1 tail test

what is the probability of observing an outcome as extreme as 24.1 or more
in a population mean of 23 and standard deviation of 2.4

Extreme: towards right side or left side of graph


(1-normdist(24.1,23,2.4/sqrt(50), TRUE))

0.0006 is answer, so <0.025 so reject null hypothesis

age has changed is answer


#New example
1. average promotion participation rate of customers for a retailer is 10%
2. On a new promotion sent to 20000 customers, 2300 customers signed up for the promo event
3. Is this likely due to random chance variation? Or is it really that the promo campaign is actually a big hit?

Solution:

H0: new campaign performance is same as historical campaign performances
(Only random chance variation)
It is one tail: because we expect the campaign to perform better. (we dont want it to perform bad)

H1: New promotion campaign is actually a big hit

Distribution is a Binomial
alpha: 0.05
Population average: 10%
Sample average probability: 2300/20000: 11.5%
Prbability of seeing an outcome of 11.5% purely because of RCV, given population average is 10%

Probability of seeing an outcome of >=2300 success in 20000 trails
purely because of RCV, given population average is 10%
=1-BINOMDIST(2299,20000,0.1,TRUE)

Q) when is t test used
used for small sized samples
an extension of normal distribution
If sample size is very low (<30 in general), the variable is gonna follow t distribution
so when sample size is low we use t test



Q) supposing we want to test if college students sleep a lot
less than the general population
AVerge sleep hours for the population is (Supposed) to be 8 hours.
We take a random sample of 10 college students and you ask them how many hours they sleep.

1 7
2 6.8
3 6
4 7
5 5.5
6 6.6
7 5.5
8 7.5
9 9
10 5.5

average: 6.64
sd: 1.1

H0 Students sleep same number of hours as population sleep number of hours
HA Students sleep less than general population

Distribution t Distrbution as data is very small
alpha 0.05

t statistics: (x-mu)/sigma/sqrt(n)
x for sample average: 6.64
mu for population average: 8
sigma for sample sd: 1.1

n = 10

we got t stat as -3.909: t stat

=TDIST(abs(-3.909),9,1) #we cant give negative value as first argument
degree of freedom is number of samples-1
last argument is tails, wheather 1 tail or 2 tail


2 sample t test: when we compare two stores
Test and control 
test store where we run promotion
control store where we dont run promotio

paired sample test

excel has data type which allows you to do t test
Q) situation: Company A has been facing a lot of customer complaints about how long calls
take to be resolved when clients contact the customer center.
The VP, call center has been asked to figure out ways
of reducing issue resolution time when customers call

This is a classic Pre-Post test
Average call time              average call time 
before implementation         after implementation
8.5                         6.5
9.5                         5.5 
10                         6.5
7                          3
12.5                       5
9                          11
4                          8.5
9                          9.5
9                          8.5
9                          9


8.75   7.3 are verage of above data

H0 No reduction in call timing post implementation
Post mean=pre mean

HA call timing is reduced post implementation  Post mean< pre mean


alpha = 0.05


t-test for equal variances: in excel
# output range where we want output

We will get a table: we need mean value
We need P(T<=t) one tail: 0.087
so we will fail to reject null hypothesis
as 0.087>0.05

Paired sample test: See class video or content u get
post class



########### 14 th april class #############
y=mx+c linear equation 
y is dependent or response variable
x is predictor or independent variable

case study

Use R to create linear regression equation

market mix/ master mix model is widely used in 
linear regression 

hoardings, fb, instagram, radio, 

Above the line marketing: for Mass marketing. Like
FB, insta etc.

If personal selling then below the line marketing.

radio, tv, hoarding etc above the line

In store promotions: Samples given to taste etc. are also 

Which one will you invest our money in?
depends on resources we have

How should we choose how much proportion of
promotions in different resources are used?

Which ever is giving good amount of sales
then we will use more allocation.

long term?
TV is long term, social media long term.
instore is shortterm, discount is shortterm.

which one is giving how much sales cant be said
which medium of advertisement is giving
good results cant be said properly

independent is controllable
dependant is not controllable

sales=b1+b1*TV+b2*Radio+b3*social media+...

more the b coefficient more important is the 
medium
price, season of sale, etc are also important

we should include as many factors as we can.
again it may not be possible, as 1000s of factors
are influencing the dependent variable

Data:
radio gross rating points
TV trp
percentage of stores went outof stock in that week
instore promotions: what percentage of stores have
instore promotions are taken in columns
Discount percentage that offered in that week also given

15% of 4000 stores are taken
for all the pharmacy stores in US are taken in the data

what is the impact of marketing activities in
sales is the problem we are solving

We can find return on investment 

We are going to do linear regression 

Some relation ships are linear and some are non
linear relationships

those which are linear turn out to be significant
those which are non linear turn out to be non significant
in this particular problem

when price is increasing saled decreasing


library(corrgram)
cormmix<-corrgram(mmix)
cormmix
write.csv(cormmix,"cormix.csv")

Three things with missing values:
ignore
delete
impute

We should not do ignore (Unless we have lots of data)
when to remove and when to impute

replace with mean (If normally distributed)
replace with median (If not distributed normally)

Outliers if we have:
ignore
delete
impute

Residuals: difference between actual and predicted

Residuals should be normally distributed around mean

there is no multicollinearity
Independent variables should not be correlation
vif() we use to do that
variance inflation factor is the full form

vif values must be <4 if more than 4 then we drop
that variable

mean absolute percentage error (MMAPE)

MAPE=2.33 means 2% away from mean
MAPE<5% is good'


#### pre class videos - R module 3 leaflet video 1 ####
library(dplyr)
library(leaflet)
library(rgdal)
schols<-read.csv("C://Users//Poori//Desktop//jigsaw//R for data science//Module 3//Leaflet files//Data Visualiazaion in R - Pre class Videos//schools.csv")
names(schols)

leaflet()%>%addTiles()

leaflet(schols)%>%addTiles()

leaflet(schols)%>%addTiles()%>%addCircles(lng = ~long,lat = ~lat)

leaflet(schols)%>%addTiles()%>%addCircles(lng = ~long, lat = ~lat, popup = ~category)

# R module 3 leaflet video 2) #



getwd()
setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Module 3\\Leaflet files\\Data Visualiazaion in R - Pre class Videos")
shp3<-readOGR(dsn = "nyha_15a")
# readOGR is present in rgdal library

shp3@data%>%head()
shp3@polygons[1]

# converting them into long and lat
shp4<-spTransform(shp3, CRS("+init=epsg:4326"))
shp4@polygons[1]

leaflet(shp4)%>%addTiles()%>%addPolygons()

# leaflet(shp4)%>%addTiles()%>%addPolygons()

leaflet(shp4)%>%addTiles()%>%addPolygons(weight = 0.9)

shp4@data%>%head()

# health area is numbers but we need characters for popups

leaflet(shp4)%>%addTiles()%>%addPolygons(weight=0.9,popup = ~as.character(HealthArea))

shp4@data$HealthArea%>%summary() # to get min and max
pal<-colorBin("Reds",bins = 4,domain = c(100,9300))

leaflet(shp4)%>%addTiles()%>%addPolygons(weight=0.9,popup = ~as.character(HealthArea),fillColor = ~pal(HealthArea))

leaflet(shp4)%>%addTiles()%>%addPolygons(weight=0.9,popup = ~as.character(HealthArea),fillColor = ~pal(HealthArea),fillOpacity = 1)

pal<-colorBin("Reds",bins=4,domain = c(10,9300))
leaflet(shp4)%>%addTiles()%>%addPolygons(weight=0.9,popup = ~as.character(HealthArea),fillColor = ~pal(HealthArea), fillOpacity = 1)%>%addLegend(pal=~pal,values = ~HealthArea)





pal<-colorBin("Reds",bins = 4, domain = c(100,9300))
leaflet(shp4)%>%addTiles()%>%addPolygons(weight=0.9,popup = ~as.character(HealthArea), fillColor=~pal(HealthArea),fillOpacity = 1)

leaflet(shp4)%>%addTiles()%>%addPolygons(weight=0.9,popup=~as.character(HealthArea),fillColor=~pal(HealthArea),fillOpacity=1)%>%addLegend(pal = pal, values = ~HealthArea)

# data science visualization case study #
library(ggplot2)
HD<-read.csv("C://Users//Poori//Desktop//jigsaw//R for data science//Module 3//Leaflet files//Data Visualiazaion in R - Pre class Videos//case_study_heart_disease_data_set.csv")
names(HD)

ggplot(HD,aes(x=trestbps))+geom_histogram(aes(fill=as.factor(DV)),position="dodge")+facet_grid(Sex+thal~cp+exang)

ggplot(HD,aes(x=chol))+geom_histogram(aes(fill=as.factor(DV),position="dodge"))+facet_grid(Sex+thal~cp+exang)

ggplot(HD,aes(x=thalach))+geom_histogram(aes(fill=as.factor(DV),position="dodge"))+facet_grid(Sex+thal~cp+exang)


ggplot(HD,aes(x=oldpeak))+geom_histogram(aes(fill=as.factor(DV),position="dodge"))+facet_grid(Sex+thal~cp+exang)

ggplot(HD,aes(y=trestbps,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)


# DV heart condition is there or not

# 0 refers to males and 1 refers to females

ggplot(HD,aes(y=oldpeak,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)

# case study part 2 video code and practice

# for thalach variable
ggplot(HD,aes(y=thalach,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)

# for cholestrol
ggplot(HD,aes(y=chol,x=as.factor(DV),fill=as.factor(DV)))+geom_boxplot()+facet_grid(Sex~cp)



p<-ggplot(HD,aes(x=Age,y=thalach,color=DV))
p+geom_point()+facet_grid(.~DV)

# an inverse trend in above plot is seen

p+geom_point()+facet_grid(thal~Sex+fbs)

p1<-ggplot(HD,aes(x=Age,y=chol, color=DV))
p1+geom_point()+facet_grid(.~DV)

p1+geom_point()+facet_grid(thal~Sex+fbs)

p2<-ggplot(HD,aes(x=trestbps,y=thalach, color=DV))
p2+geom_point()+facet_grid(.~DV)
p2+geom_point()+facet_grid(thal~Sex+fbs)

df<-read.csv("C://Users//Poori//Desktop//jigsaw//R for data science//Module 3//Leaflet files//dataF.csv")

names(df)

df1<-df

names(df1)

names(df1)[1]<-"knar"

names(df1)

ir<-data(iris)
names(ir)
names(iris)
ir<-iris
names(ir)

names(ir)[5]<-seiceps

names(ir)[5]<-"seiceps"

names(ir)

names(ir)[2]<-"B"
names(ir)

X<-c(1,2,3)
Y<-c("A","B","C")
Z<-c("1A","2B","3C")
datf<-data.frame(X,Y,Z)
datf

# below code is from below website:
# https://ggplot2.tidyverse.org/reference/ggtheme.html

str(mtcars$vs)

epticars<-within(mtcars,{vs<-factor(vs, labels = c("V-shaped", "Straight"))
                          am<-factor(am,labels = c("Automatic","Manual"))
                          cyl<-factor(cyl)
                          gear<-factor(gear)})
epticars
names(epticars)
library(ggplot2)
p1<-ggplot(epticars)+geom_point(aes(x=wt,y=mpg,colour=gear))+
  labs(title = "fuel economy declines as weight increases",
       subtitle = "(1973-74)", 
       caption = "data from the 1974 motor trend us magazine",
       tag="Figure 1", x="Weight (1000lbs)", y = "Fuel economy (MPG)",
       colours="Gears")

p1+theme_gray()


p2<-ggplot(epticars)+geom_point(aes(x=wt,y=mpg,colour=gear))+labs(title = "fuel economy
                                      declines as weight inreases", subtitle = "(1973-73)",
                                      caption = "data from the 1974 motor trend us magazine",
                                      tag = "Figure 1", x="Weight (1000lbs)", y="Fuel Economy(MPG)",
                                      colours = "Gears")
p2+theme_bw()


p3<-ggplot(epticars)+ geom_point(aes(x=wt,y=mpg,colour=gear))+labs(x="weight",y="mpg economy", title = "Fuel
                                                       economy declines as weight increases",
                                                       subtitle = "1973-74",caption = "data from 
                                                       the 1974 motor trends",colours="Gears",
                                                       tag="figure 1")
p3+theme_linedraw()


p4<-ggplot(epticars)+geom_point(aes(x=wt,y=mpg,colour=gear))+labs(x="Weight", y="fuel economy",
                                                                  title="economy declines as weight increases",
                                                                  subtitle = "1974-93", tag = "Figure 1",
                                                                  caption = "data from the 1974 motor trends")
p4+theme_light()

p4+theme_dark()

p4+theme_minimal()

p4+theme_classic()

p4+theme_void()

names(epticars)

p5<-p4+facet_grid(vs~am)
p5+theme_bw()
p5+theme_dark()
p5+theme_classic()
p5+theme_void()
p5+theme_linedraw()
p5+theme_minimal()
p5+theme_light()



# legends (ggplot2) from below website:
# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/

library(ggplot2)
names(PlantGrowth)
pg<-PlantGrowth
bp<-ggplot(pg,aes(x=group,y=weight,fill=group))+geom_boxplot()
bp

# removing the legend
bp+guides(fill=FALSE)
# or can be done by
bp
bp+scale_fill_discrete(guide=FALSE)

# below removes all legends
bp+theme(legend.position = "none")
bp


bp+scale_fill_discrete(breaks=c("trt1","ctr1","trt2"))

bp+guides(fill=guide_legend(reverse = TRUE))
bp

bp+scale_fill_discrete(guide = guide_legend(reverse = TRUE))

bp+scale_fill_discrete(breaks=rev(levels(PlantGrowth$group)))


bp+guides(fill=guide_legend(title = NULL))

bp+scale_fill_discrete(guide = guide_legend(title = NULL))

bp+theme(legend.title = element_blank())


bp+scale_fill_discrete(name="experimental\nconditions")

bp+scale_fill_discrete(name="experimental\nconditions",breaks=c("ctrl","trt1","trt2"),labels=c("control",
                                                                                               "treatment 1",
                                                                                               "treatment 2")
 
                                                                                                                                                                                             "treatment 2"))
df1 <- data.frame(
  sex = factor(c("Female","Female","Male","Male")),
  time = factor(c("Lunch","Dinner","Lunch","Dinner"), levels=c("Lunch","Dinner")),
  total_bill = c(13.53, 16.81, 16.24, 17.42)
)

names(df1)
str(df1)

lp<-ggplot(data = df1, aes(x=time, y=total_bill, group = sex, shape = sex))+geom_line()+geom_point()
lp + theme_dark()
lp

lp<-ggplot(data = df1, aes(x=time, y=total_bill))+geom_line()+geom_point()
lp

lp<-ggplot(data = df1, aes(x=time, y=total_bill, group = sex))+geom_line()+geom_point()
lp

lp<-ggplot(data = df1, aes(x=time, y=total_bill, shape = sex))+geom_line()+geom_point()
lp


lp<-ggplot(data = df1, aes(x=time, y=total_bill, shape = sex, group = sex))+geom_line()
lp

lp<-ggplot(data = df1, aes(x=time, y=total_bill, shape = sex, group = sex))+geom_point()
lp

lp+scale_fill_discrete(name="payer", breaks=c("Female","Male"), labels=c("Woman","Man"))

lp+scale_shape_discrete(name="payer", breaks=c("Female","Male"), labels=c("Woman","Man"))
# If you use a line graph, you will probably need to use scale_colour_xxx and/or scale_shape_xxx 
# instead of scale_fill_xxx. colour maps to the colors of lines and points, 
# while fill maps to the color of area fills. shape maps to the shapes of points.



names(df1)
lp<-ggplot(data=df1,aes(x=time,y=total_bill,group=sex,shape=sex,colour=sex))+geom_line()+geom_point()
lp


lp+scale_shape_discrete(name="Payer",breaks=c("Female","Male"), labels = c("Woman", "Man"))

lp+scale_colour_discrete(name="Payer",breaks=c("Female","Male"), labels = c("Woman", "Man"))+
  scale_shape_discrete(name="Payer",breaks=c("Female","Male"), labels = c("Woman", "Man"))

'There are many kinds of scales. They take the form 
scale_xxx_yyy. 
Here are some commonly-used values of xxx and yyy:

xxx	Description
colour:	Color of lines and points
fill:	Color of area fills (e.g. bar graph)
linetype:	Solid/dashed/dotted lines
shape:	Shape of points
size:	Size of points
alpha:	Opacity/transparency

yyy	Description
hue:	Equally-spaced colors from the color wheel
manual:	Manually-specified values (e.g., colors, point shapes, line types)
gradient:	Color gradient
grey:	Shades of grey
discrete:	Discrete values (e.g., colors, point shapes, line types, point sizes)
continuous:	Continuous values (e.g., alpha, colors, point sizes)'


# another way to change label names is directly changing data frame itself

pg<-PlantGrowth
str(pg)
str(pg$group)
unique(pg$group)

levels(pg$group)[levels(pg$group)=="ctrl"]<-"Control"
levels(pg$group)[levels(pg$group)=="trt1"] <- "Treatment 1"
levels(pg$group)[levels(pg$group)=="trt2"] <- "Treatment 2"

names(pg)
names(pg)[names(pg)=="group"]<-"ExperimentalCondition"
head(pg)
ggplot(data=pg,aes(x=ExperimentalCondition,y=weight, fill = ExperimentalCondition))+geom_boxplot()


pg<-PlantGrowth
levels(pg$group)[levels(pg$group)=="ctrl"]<-"Control"
levels(pg$group)[levels(pg$group)=="trt1"] <- "Treatment 1"
levels(pg$group)[levels(pg$group)=="trt2"] <- "Treatment 2"
names(pg)[names(pg)=="group"]<-"Experimental Condition"

ggplot(data=pg,aes(x=`Experimental Condition`,y=weight, fill = `Experimental Condition`))+
  geom_boxplot()

# back ticks must be used to refer a variable with space



names(bp)

bp
# before we used element_blank to remove the title name
bp+theme(legend.title = element_text(colour = "blue",size=16,face="bold"))


bp+theme(legend.text=element_text(colour="blue",size=16,face="bold"))

bp+theme(legend.background = element_rect())

bp+theme(legend.background = element_rect(fill="blue",size=0.5,linetype = "dottede"))
?linetype
# 0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash


bp+theme(legend.background = element_rect(fill="gray",size=0.5,linetype = "dotted"))
?fill

# red, blue, gray, gray90 etc.

bp+theme(legend.position = "top")
# left/right/top/bottom

# legend inside the plotting area
bp + theme(legend.position=c(.5, .5))

# geom_text() adds text directly to the plot. geom_label() draws a rectangle behind the text, 
# making it easier to read.

# below code is from below website:
# https://ggplot2.tidyverse.org/reference/geom_text.html
names(mtcars)
  
p<-ggplot(mtcars,aes(x=wt,y=mpg,label=rownames(mtcars)))
p+geom_text()
dim(mtcars)

p+geom_text(check_overlap = TRUE)

p+geom_label()

p+geom_text(size=10)

# par not working for ggplot (Have to see workaround)
par(mfrow=c(1,2))
p+geom_point()+geom_text(hjust=0,nudge_x = 0)
p+geom_point()+geom_text(hjust=0,nudge_x = 0.05)
p+geom_point()+geom_text(hjust=0.5,nudge_x = 0.05)
dev.off()

p+geom_point()+geom_text(vjust=0,nudge_y=0.05)
p+geom_point()+geom_text(vjust=1.5,nudge_y=0.05)

p+geom_point()+geom_text(angle=45)

p+geom_point()+geom_text(hjust=0,nudge_x=0.05,aes(colour=factor(cyl)))

p+geom_point()+geom_text(hjust=0,nudge_x=0.05,aes(colour=factor(cyl)))+scale_colour_discrete( l=40)


p+geom_point()+geom_label(hjust=-0.1,x_nudge=0.4,aes(colour=factor(cyl)))

p+geom_point()+geom_label(hjust=-0.1,x_nudge=0.4,aes(fill=factor(cyl)))

#below code not working properly don't know why
p+geom_point()+geom_label(hjust=-0.1,x_nudge=0.4,aes(fill=factor(cyl), colour="white"))


p+geom_text(aes(size=wt))


p+geom_text(aes(size=wt))+scale_radius(range = c(3,6))


p+geom_text(aes(label=paste(wt,"^","(",cyl,")", sep = "")),parse = TRUE)

p<-ggplot(mtcars,aes(x=wt,y=mpg,label=rownames(mtcars)))
p+geom_text()+annotate("text",label="plot mpg vs. wt",x=2,y=15,size=8,colour="red")

rownames(mtcars)
str(mtcars)
head(mtcars)

p+geom_text()+annotate("text",label="plot mpg vs. wt",x=2,y=15,size=8,colour="red")


df <- data.frame(
  x = factor(c(1, 1, 2, 2)),
  y = c(1, 3, 2, 1),
  grp = c("a", "b", "a", "b")
)

ggplot(data=df,aes(x,y,group=grp))+geom_col(aes(fill=grp),position = "dodge")+
         geom_text(aes(label=y),position = "dodge")

ggplot(data=df,aes(x=x,y=y,group=grp))+geom_col(aes(fill=grp),position="dodge")+
  geom_text(aes(label=y),position=position_dodge(0.9))

# Use you can't nudge and dodge text, so instead adjust the y position



ggplot(df,aes(x=x,y=y,group=grp))+geom_col(aes(fill=grp),position="dodge")+
  geom_text(aes(label=y),position=position_dodge(0.9),vjust=-0.4)

ggplot(df,aes(x=x,y=y,group=grp))+geom_col(aes(fill=grp),position="dodge")+
  geom_text(aes(label=y, y=y+0.05),position=position_dodge(0.9))

ggplot(df,aes(x=x,y=y,group=grp))+geom_col(aes(fill=grp),position="dodge")+
  geom_text(aes(label=y, y=y+0.05),position=position_dodge(0.9), vjust=0)



df <- data.frame(
  x = c(1, 1, 2, 2, 1.5),
  y = c(1, 2, 1, 2, 1.5),
  text = c("bottom-left", "bottom-right", "top-left", "top-right", "center")
)

ggplot(df, aes(x, y)) + geom_text(aes(label = text))

ggplot(df,aes(x,y))+geom_text(aes(label=text),hjust="inward",vjust="inward")
  
# Assignmnet of module 3 non graded data visualization

df<-read.csv("C://Users//Poori//Desktop//jigsaw//R for data science//Module 3//Leaflet files//dataF.csv")
head(df)
unique(df$Brand)
names(df)

head(df,11)
  
p<-ggplot(df,aes(x=Company.Advertising,y=Brand.Revenue, label=rownames(df$Brand)))

p+geom_text()

# geom_label(x=c("Company Advertising in Billions of $"), y=c("Brand Revenue in Billions of $"),
# title = c("Technology"))


library(dplyr)  
p<-df%>%filter(Industry=="Technology")

'
?label
??label

rownames(mtcars)
rownames(p)
names(p$Brand)

rownames(p$Brand)

Brand
p$Brand'

q<-ggplot(p,aes(x=Company.Advertising,y=Brand.Revenue,label=p$Brand, group = Brand.Value))
q+ geom_point(aes(size=Brand.Value, colour=factor(Brand)))+geom_text(aes(colour=factor(Brand), 
  size = Brand.Value+30, vjust=1))+ labs(x=c("Company Advertising in Billions of $"), 
  y=c("Brand Revenue in Billions of $"), title=c("Technology"))


q+ geom_point(aes(size=Brand.Value, colour=factor(Brand)))+geom_text(aes(colour=factor(Brand), 
   size = Brand.Value+30, vjust=1))+ labs(x=c("Company Advertising in Billions of $"), 
   y=c("Brand Revenue in Billions of $"), title=c("Technology"))+
  scale_shape_discrete(name="Brand.Value",breaks=c(30,60,90))+
  guides(colour=FALSE)+scale_size_continuous(name="Brand Value $ (Billions)", breaks = c(30,60,100))



names(p)
names(df)

head(unique(df$Industry),15)


lux<-df%>%filter(Industry=="Luxury")
head(lux)
dim(lux)

q1<-ggplot(lux,aes(x=Company.Advertising,y=Brand.Revenue,label=Brand))
q1+geom_point(aes(size=Brand.Value, colour=Brand))+geom_text(aes(hjust=0.5,vjust= 1.3, 
   colour=Brand, size = Brand.Value+30))+guides(colour=FALSE)+
  scale_size_continuous(name="Brand Value $ (Billions)", breaks = c(10.0, 28.1))+
  theme_light()+theme(legend.key = element_rect(fill="light blue", color="black"))+
  scale_x_continuous(breaks=seq(0,6,0.1))


# These websites i hve to practice again

# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software

# http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/


# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html



















