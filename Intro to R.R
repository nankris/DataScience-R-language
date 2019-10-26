                    ####Introduction to R####
##Part1-> R atomic data types and datastructures, saving workspace and scripts
##Part2-> Importing tabular data (text files, SQL servers)
##Part3-> Working with web data (Grabbing tables,JSON)

                   
          ##Part 1-> Data types and datastructures##

#Most relevant data types and datastructures

#Data import using most import functions results in the creation of a dataframe
          
setwd("F:\\Ebooks\\Programming and Statistical Packages\\R\\Data")                            
data1<-read.csv("cpu.csv")
                   
class(data1)                  

#Working with data frames

#Selecting a specific row/column or both: Subsetting/Slicing

data1[2,2]

data1[,2]

data1$MYCT

data1[,"MYCT"]

data1[1,]

data1[0,0] #Indices in R start from 1 not 0

#Subsetting/Slicing multiple rows and columns

data1[1,2,3,4,5] #Need to assign vector of indices

data1[c(1,2,3,4,5),]

data1[c(1,2,3,4,5),c(1,5)]

data1[c(1,2,3,4,5),c("Vendor","CACH")]

data1[c(1,2,3,4,5),-c(1,5)]

data1[c(1,2,3,4,5),-c("Vendor","CACH")] #Character vectors

data1[1:5,c("Vendor","CACH")] # ?

data1[1:5,c(1,5)] # ?

is.vector(c(1,2,3,4,5))
is.vector(1:5) #Sequence functions in R will generate vectors, so they can be useful in slicing/indexing

#Some import functions also generate lists: Mostly functions that work with web objects like JSON/XML

library(jsonlite)
data2<-fromJSON("F:\\Work\\Jigsaw Academy\\Blogs\\September 15\\Visa.txt")
class(data2)#Not every data import will result in creation of dataframes

str(data2)

data2$fields
class(data2$fields)

head(data2$data)
class(data2$data)

data2$fields[,1]
data2$data[1,2]

#Lists are not only encountered while importing data but also when we work with machine learning algorithms

model1<-lm(data1$MYCT~data1$MMIN)

mode(model1)

model1$coefficients
model1$residuals #etc

save.image(file="F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\Workspace.RData")

rm(data1,data2,model1)

load("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\Workspace.RData")

#Saving workspace will save only objects created in a working session, it will not save your R code!!!!!

##Importing tabular data##

#Checks needed to ensure that data is imported correctly
#1. Delimiter in the file
#2. How missing values are populated in the data


import1<-read.table("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\sample2.csv",sep=",",header = TRUE)

summary(import1) #Focus on the missing values
str(import1) #Are column data types correct

import2<-read.table("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\sample1.txt")#Why error

import2<-read.table("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\sample1.txt",sep="\t")

import2#What is wrong?

import2<-read.table("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\sample1.txt",sep="\t",header=TRUE)

summary(import2)
str(import2)

import2<-read.table("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\sample1.txt",sep="\t",header=TRUE,na.strings = c(NA,"Missing",""))

summary(import2)

str(import2)

class(import1)
class(import2)

##Data import is considered successfull: 1.Delimiters are identified correctly, 2.Missing values in the data are recognized as missing (NA)

#Working with SQL servers#

#Identify which SQL data base you are working with
#MySql and Postgresql are most popular

#If mysql is the sql engine to be connected then RMySql can be used to connect R to sql

library(RMySQL)

con<-dbConnect(MySQL(),dbname="world",username="root",password="root")

dbListTables(con)

import.sql1<-dbGetQuery(con,"SELECT * FROM city")

class(import.sql1)

dbDisconnect(con)

#One way to connect to SQL is using RODBC, this will work with other SQL distribution such as Postgresql, SQL Server and not just MySQL

library(RODBC)
connection<-odbcConnect("mysql",uid="root",pwd="root") #dsn has to be specified, this will be linked to only one database in sql server. dsn can be changed by accessing control panel, admnistrative tools, odbc data sources
sqlTables(connection)

import.sql2<-sqlFetch(connection,"host_summary")

close(connection)

class(import.sql2)

###Working with web data###

#Working with web objects JSON and XML#
#Sometimes data is not available in tabular formats: csv, sql servers#
#Particuarly when working with API's

#Glassdoor API, https://www.glassdoor.co.in/developer/index.htm
library(jsonlite)
web1<-fromJSON("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=47699&t.k=g9GdVHlQ1eM&action=employers&q=pharmaceuticals&userip=192.168.43.42&useragent=Mozilla/%2F4.0")

class(web1)

str(web1)

#Getting tables from web pages
#Many web pages contain tabular data as comma separated or tab separated
#UCI Abalone data set
web2<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",header=FALSE)

head(web2)# Column names are missing

#Use the documentation on the page to figure out the column names

name<-c("Sex","Length","Diameter","Height","Whole.Weight","Shuclked.Weight","Viscera.Weight","Shell.Weight","Rings")

names(web2)<-name
head(web2)
str(web2)

#Fetching HTML tables from a webpage
library(XML)
web3<-readHTMLTable("http://www.inflationdata.com/Inflation/Consumer_Price_Index/HistoricalCPI.aspx?reloaded=true")

class(web3)

#One can save the web page locally and still read in the tables

web4<-readHTMLTable("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\Historical Consumer Price Index (CPI).html")

class(web4)

#If analysis on this data has to be done then it should be converted to a dataframe

web4<-as.data.frame(web4)
class(web4)
head(web4)#What is wrong?
write.csv(web4,"F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\cpi.csv",row.names = F)

web5<-read.csv("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\cpi.csv")

head(web5)#Need to skip the first line

web5<-read.csv("F:\\Work\\Jigsaw Academy\\Data Scientist Course\\Data Science Redo\\Live Classes\\Intro to R\\cpi.csv",skip=1)
head(web5)
