##----------------------Introduction to R--------------------------------#
getwd()
setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Module 1")

#USe # for writing a comment

#Assignment of values to a variable "<-"
x1<- 5
x2 = 7
x3 <- 155

#Can do arithmetic operations
p<-5
p
q<-p+2
q

a<- 5+5
a

pi
sqrt(25)
2^2+5


# Items in R are stored as objects


#Over writing a variable
x<- 5
x<-"Jigsaw"

#deleting the variables
rm(x1)
x1

#Programatically figuring out objects
ls()
data()
data("iris")
class(iris)
class(AirPassengers)
# ts is time series object
?iris
#typeof(AirPassengers)

# How to create a vector?
# It consists of either numbers,strings, or logical values but not all of them together.
# It contains only 1 type of element
# Type of variables : Class of the variables (Integer,character,logical)

# c is the combining operator

# Vector : Most Simplest structure in R .Only one data type


#Integer, L forces the numbers to be integer
z<-c(3,2,4)
class(z)
z<- c(3L,4L,35L)
class(z)


#Numeric  
x<-5 #it is a numeric vector of 1 element
x<-c(1,2,-5,6) # numeric vector of 4 elements
x
class(x)

a<-3
a1<-c(a,a*4,-7*a)
a1

A<-c(1,2,3,NA)
A
class(A)


x<-c(1,2,3,4)
class(x)
x1<-c(1.2,2.4,3.5,4.5)
class(x1)


#Character
string<-c("1","2","2","3","4")
class(string)

B<-c('a','b','c',NA)
B
class(B)

#Logical
sp<-c(TRUE,FALSE,TRUE)  
class(sp)
    

#Vectors shouldn't have mixed type of data
p<-c(1,2,"g")
class(p)

p<-c(TRUE,FALSE,"G")
class(p)

p<-c(TRUE,FALSE,3)
class(p)
class(c(TRUE,FALSE,1))
class(c(1,2,"G"))
class(c("A","B",4))
class(c("A","B",FALSE))


#Vector,working with vectors

num<-c("a","b","c","d","e")
num
num[1]
num[4]
num[-1]
num[1:3]
num

num1<- num

#assgining names 
names(num)<-c("x1","x2","x3","x4","x5")
names(num)[4]
num["x4"]
num[c("x1","x2")]


length(num)

#vector addition
a1<-c(1,2,3)
a2<-c(4,5,6)
a1+a2

 x<-c("qsdfds","rfsdfs","rsfds")
 nchar(x)


#factors
#stores categorical variables.

gender<- c(1,2,1,2,1,2,1,2)
gender <- factor(gender, levels = c(1,2), 
                 labels = c("Male","Female"))
table(gender)
class(gender)

x <- c("yes","no","no","yes","no")
x
class(x)
y <- as.factor(x)
class(y)
table(y)



#dataframes
#Each column can be a different Data types. 
#Consider the following vectors : 

product=c("Bag","shoes","belt","belt")

total_price=c(500,1000,150,10000)

color=c("Blue","red","red","Blue")

quantity=c(5,NA,3,4)


product_details <- data.frame(product,total_price,color,quantity,stringsAsFactors=T)
product_details


class(product_details)

str(product_details)

product_details1 <- data.frame(product,total_price,color,quantity)
str(product_details1)

product_details[,2]
product_details[,"total_price"]

product_details[1:2,]
product_details[2,2]

product_details[2,c("color","quantity")]
product_details[2,-2]

data()
data("iris")
head(iris,10)
tail(product_details,2)


#Lists : Recursive vectors. Can handle different data types
my.list <- list( name = c("Robert", "Emma"), age = c (65, 54,43),retired = c (TRUE, FALSE),product_details)

my.list

my.list$age
my.list["age"]

class(my.list$age)
class(my.list)

class(my.list["age"])

my.list[["age"]][2]

my.list[3]
my.list [[3]][2]

my.list$name


#Data.frames

id<- c(1:100)
id
classes<- c(rep("1st-Standard",50),rep("2nd-Standard",50))
classes
score<- c(rep(75,50),rep(80,50))
score

mydata<- data.frame(Roll_num =id,
                     Standard = classes,
                     Score =score )

mydata


head(mydata)
head(mydata,10)
tail(mydata)
dim(mydata)
str(mydata)
summary(mydata)


#Matrix

mat<- matrix(c(2:10),3,3)
mat

mat<-matrix(1:9,c(3,3))
mat
mat<-matrix(c(1:9),3,3,byrow = TRUE)
mat

mat1<-  matrix (c(1:15), nrow = 5, ncol = 3)
mat1

mat<-matrix(c(1:15),5,3)
mat

#array
ary<- array(c(1:15),dim = c(3,5,1,2)) #we can use 1,2 or 2,2 or 2,1 etc as it represents 11,12,21,22 in 2*2 
#matrix so just do the same according to the dimention of matrix you have to input the data
ary

##Importing tabular data##

#Checks needed to ensure that data is imported correctly


import1<-read.table('sample2.csv',sep=",",header = T)
import1
import2<-read.csv("sample2.csv",sep=",",header=TRUE)
import2



summary(import1)
summary(import2)
str(import1) #Are column data types correct

import2<-read.table("sample1.txt")#Why error

import2<-read.table("sample1.txt",sep="\t")

import2#What is wrong?

import2<-read.table("sample1.txt",sep="\t",header=TRUE)
import2
summary(import2)
str(import2)

import2<-read.table("sample1.txt",sep="\t",header=TRUE,na.strings = c(NA,"Missing",""))
import2
summary(import2) # it will show NA as 1 in each column that has NA because we just replaced all of NA, Missing, "" with NA


str(import2)

class(import1)
class(import2)

##Data import is considered successfull: 1.Delimiters are identified correctly, 2.Missing values in the data are recognized as missing (NA)


import3<-read.csv('sample2.csv',header=F)
import3

## Xlsx file

#install.packages("readxl")
library(readxl)

import4 <- read_excel("sample3.xlsx", sheet = 1)
import5 <- read_excel("sample3.xlsx", sheet = "Data2")
import4
import5

#install.packages("jsonlite")
library(jsonlite)
web1<-fromJSON("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=47699&t.k=g9GdVHlQ1eM&action=employers&q=pharmaceuticals&userip=192.168.43.42&useragent=Mozilla/%2F4.0")

class(web1)

str(web1)


#Age of Death of Successive Kings of England
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
#see nlines argument in scan function, also see flush argument
kings

write.csv(import5, "C:\\Users\\User10\\Desktop\\Jigsaw\\export5.txt")





###################
 #Doubts Session
###################
#finding mean and standard deviation in iris dataset
library(dplyr)
ir<-iris
unique(ir$Species)
ir%>%group_by(Species)%>%summarise(Avg=mean(Sepal.Length), sd= sd(Sepal.Length))
ir%>%group_by(Species)%>%summarise(Avg=mean(Sepal.Length), sd= sd(Sepal.Length), avg1=mean(Petal.Width))
ir%>%group_by(Species)%>%summarise(Avg=mean(Sepal.Length), sd= sd(Sepal.Length), N=n(), Max=max(Sepal.Length))
ir%>%group_by(Species)%>%summarise(Avg=mean(Sepal.Length), sd= sd(Sepal.Length), N=n(), Max=max(Sepal.Length))%>%arrange(-Avg)
#- is used to sort it in descending order

ir%>%group_by(Species)%>%summarise(Avg=mean(Sepal.Length), sd= sd(Sepal.Length), N=n(), Max=max(Sepal.Length))%>%arrange(-Avg)%>%as.data.frame()
#converting it into data frame so that we will won't get error
#mode = frequently occured data set

#if we have NA in our data set, we have to remove it from our set by using na.rm
ir%>%group_by(Species)%>%summarise(Avg=mean(Sepal.Length, na.rm = TRUE), sd= sd(Sepal.Length), N=n(), Max=max(Sepal.Length))%>%arrange(-Avg)%>%as.data.frame()

#we are giving ir as data frame

names(ir)
#logical vector : TRUE or FALSE and will return logical output

log=c(TRUE, FALSE)
class(log)
#logical class above one

#working of sapply and lapply
app=apply(iris[,1:4],2,mean)
#to find mean column wise we are using above code, applying mean column wise
#apply returns vector or list or array
class(app)
#output will be numeric vector, 1 for rows, 2 for columns, find mean column wise (2)
tapply=tapply(iris$Petal.Width, iris$Species, mean)
tapply
#to find mean group wise, output is array, and in apply output is numeric
class(tapply)
#iris[1:10,]
list=list(a=c(5:10),b=c(15:20))
#above is list with two vectors
#list has only length, list is heterogeneous and not homogenious so we can't find dimention of the list
length(list)
lapply=lapply(list,mean)
#when input is list then we use lapply function
length(lapply)
class(lapply)
sapply=sapply(list,mean)
class(sapply)
#sapply, important function when working with large data sets, it gives numeric vector as output
#we can use sample function for random sampling
#lapply returs list and sapply returs a matrix

#date questions

library(lubridate)
date="20180519"
class(date)
#date=as.character(date)
ymd(date)
class(ymd(date))


date1<-"10032019"
date1_date<-dmy(date1)
#to convert two types of dates in same column then how to use it
#there is a function to do it
#parse_date_time function we can do that
data$date_1<-parse_date_time((data$date_1), orders=c("%d-%b","%b-%d"))

#if date is numeric format then we have to convert it into character format and then to date format

#dplyr functionality
iris%>%filter(Sepal.Length>5)%>%mutate(sepal_table=Sepal.Length+1)%>%summarise(Avg=mean(Sepal.Length),avg1=mean(sepal_table))
#to combine more than one functionality
#mutate is to create new variable


iris%>%filter(Sepal.Length>3 & Sepal.Length<=7)%>%mutate(sepal_table=Sepal.Length+1)%>%summarise(Avg=mean(Sepal.Length),avg1=mean(sepal_table))


irdf<-iris%>%filter(Sepal.Length>3 & Sepal.Length<=7)%>%mutate(sepal_table=Sepal.Length+1)
head(irdf)
dim(irdf)
#it only had 138 rows, but iris have 150 so filtered values are also considered


#dates are in two formats "01-Jan" and "Jan-01"
library(lubridate)
data$date_1
data$date_1=as.character(data$date_1)
# so in c() we are using "%d-%b", "%b-%d" because two different formats

# %d date and %b month

#data$Month=month(data$date_1,label=TRUE)
#month will extract month from the date format
#label = TRUE is used to create column name

#difference between posixct and posixlt

date=as.POSIXct(strptime("2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"))
unclass(date)
class(date)
#1301189400 duration between the date and 1970 1 Jan
#1970 1 Jan is the reference date
#based on unix refernce they used this

now<-as.POSIXlt(Sys.time())
class(now)
unclass(now)
#unclass is in list format when we unclass in posixlt
#in posixct it is in number of seconds


#aggregate function explaination

aggregate(iris[,-5],by=list(iris$Species),mean)
#we are removing species column as it is character and then we are grouping remaining based on mean


#xtabs working, paste0 working, ~operator working

#readHTMLTable working
#importing HTML file
library(XML)
data=readHTMLList(file.choose())
#asks to select the file from above code then you can select HTML file
str(data)
data=as.data.frame(data$the_list)
#the_list is got after analysing str(data)
#have to convert it to the data.frame
head(data)


library(RCurl)

#working of ~ operator
#if any function needs input as function then we need tilda

#better to learn sql for data science

#types of joins
# see cheat sheet 
#practice codes as much as possible



#working of xtabs function
head(mtcars)
xtabs(disp~vs+am,data=mtcars)
#considering 3 variables, displacement, vs, am
#to get table out of it, we are passing input as formula (sum of two variables vs and am)
#whenever we try to pass input as a formula we use tilda ~
#target towards disp based on categoris vs and am so we used ~ in between

#if we want to create a table using more than two variable then we must use xtabs
#if we have two variables only then we can use table

table(mtcars$vs,mtcars$am) #12 times 0 occured in both vs and am, see the table to get hold of it

#working of melt and dcast
students=data.frame(sid=c(1,1,2,2),eterm=c(1,2,1,2),math=c(50,65,75,69),English=c(40,45,55,59),Physics=c(70,65,80,60))
#melt is used to convert wide format data
students
library(dplyr)
library(reshape2)
melt=melt(students,id=c("sid","eterm"))
names(melt)=c("id","term","Subject","Marks")

#dcast is used to convert long format data in to wide format

dcast=dcast(melt,id+term~Subject,value.var = "Marks")
#dcast=dcast(profiled,cluster~variable,value.var = "z_score")
#value.var from which column we should look

#paste0 working
#to concatenate

nth<-paste0(1:12,c("st","nd","rd",rep("th",9)))
nth

#sqldf package
library(sqldf)
DF<-read.csv.sql("iris.csv",sql="select * from file order by random limit by 50")

#imputing missing values
air<-airquality
head(air)
air$Solar.R<-mean(air$Solar.R)
#our output will be NA in all the observations under Solar.R

air$Solar.R<-mean(air$Solar.R, na.rm=TRUE)
#air$Solar.R<-mean(air$Solar.R, rm.na=TRUE)
head(air)

#below is correct code
air<-airquality
air$Solar.R[is.na(air$Solar.R)]<-mean(air$Solar.R, na.rm = TRUE)
head(air)

#how to find quantiles
quantile(iris$Sepal.Length, p=c(1:100)/100)
# 50% less we have 5.6, from above code we get all the quantiles
summary(iris$Sepal.Length)

#mean > median or mode then positively skewed
#mean< median or mode then negatively skewed

quantile(iris$Sepal.Length)

#kurtosis helps as understanding peakness
#from that value if we move away then we get drastic variation, in positive kurtosis, i.e sudden fall down
#positive kurtosis means more peak
#negative kurtosis means less peak
#normal distribution





#good resources to learn R coding?
#good books he can recomment?
#paste0 working
#
#in the Jigsaw forum we have book references
# look for rblogger, for each and every topic you will find exercises and solutions















































