setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Module 2")
oj <- read.csv("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Module 2\\oj.csv")

str(oj)
nrow(oj)
ncol(oj)
dim(oj)
names(oj)

#dataframe[rows,columns]
oj[3,3]



oj[c(1,2,8,456),c(1,3,6)]





oj[c(1:5),"brand"]



#Logical Subseting

#Selecting only those rows where brand bought is tropicana
dat<-oj[oj$brand=='tropicana',]

dat1<-oj[oj$brand=="tropicana" | oj$brand == "dominics",]
dat1

dat_2<-oj[oj$brand=="tropicana" & oj$feat==0,]
dat_2

# all are same
oj[,"store"], oj$store, oj[,1]







#Using Or condition, brand bought is tropicana or dominicks
dat1<-oj[oj$brand=='tropicana'|oj$brand=='dominicks',]
head(dat1)












#Using And condition, brand bought is tropicana and no feature advertisement is run
dat2<-oj[oj$brand=='tropicana' & oj$feat==0,]
head(dat2)











#Subsetting using which() operator
index<-which(oj$brand=="dominicks")
head(index)
dat3<-oj[index,]
dat3










#Selecting Columns
dat4<-oj[,c("week","brand")]
head(dat4)










#Seleecting+Subsetting
dat5<-oj[oj$brand=='tropicana' & oj$feat==0,c("week","store")]
head(dat5)










dim(oj)
#Adding new columns
oj$logInc<-log(oj$INCOME)
dim(oj)



#remove columns
oj<-oj[,-c(18)]
oj<-oj[-18] # this also does the same
dim(oj)
View(oj)






#Revenue Column
head(oj$logmove)
head(exp(oj$logmove))
oj$revenue<-exp(oj$logmove)*oj$price
View(oj$revenue)
head(oj$revenue,10)


str(oj[18])







#Sorting data
numbers<-c(10,100,5,8)
order(numbers)
order(-numbers)


#how to get proper output while sorting
indx<-order(dat4)
dat4[indx]

#or
dat4[order(dat4)]





dat6<-oj[order(oj$week),]
head(dat6)
View(dat6)
min(oj$week)










dat7<-oj[order(-oj$week),]
head(dat7)
dat7$week%>%head() # but load dplyr library
max(oj$week)



##Group by summaries


class(oj$brand)
unique(oj$brand)








#Mean price of juice across brands







#Summarize-Price
#Summarize by-Brand (factor)
#Summarize how-Mean

#Syntax aggregate(variable to be summarized, by=list(variable by which grouping is to be done),function)



aggregate(oj$price,by=list(oj$brand),mean)
aggregate(oj$price,list(oj$brand),mean) # works the same
class(aggregate(oj$price,by=list(oj$brand),mean))
# aggregate returns data frame



# difference between aggregate and tapply: https://stackoverflow.com/questions/25966052/what-is-the-difference-of-tapply-and-aggregate-in-r
# aggregate is designed to work on multiple columns with one function and returns a dataframe for one row for each category
# tapply is designed to work on single vector

# didn't understand this code
# found this code in example(tapply)
ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
# means b=1 is 0 and b is not 1, a=1,2 is 1 so a is both 1 and 2
tapply(1:3,ind)
tapply(1:3, ind, sum)


# sample function, #letters is default data set in r
cat=sample(letters[21:24], 15,rep=TRUE)


tapply(oj$price,oj$brand,mean)


class(tapply(oj$price,oj$brand,mean))
# array is the output class of tapply











#Mean income of people by brand
#Summarize-Income
#Summarize by-Brand
#Summarize how-Mean
aggregate(oj$INCOME,by=list(oj$brand),mean)
class(aggregate(oj$INCOME,by=list(oj$brand),mean))
tapply(oj$INCOME,oj$brand,mean)
class(tapply(oj$INCOME,oj$brand,mean))


# mean, sum, range, quantile, length







#Cross tabulations
# Units of different brands sold based on if feature advertisement was run or not
table(oj$brand,oj$feat)

#table(oj$price,oj$brand)

class(table(oj$brand,oj$feat))

# returns table








# xtabs is used for more than one variable relationships while table is used for single variable relationships
xtabs(oj$INCOME~oj$brand+oj$feat)









#dplyr
library(dplyr)
dat8<-filter(oj,brand=="tropicana")
dim(filter(oj,brand=="tropicana"))



dat9<-filter(oj,brand=="tropicana"|brand=="dominicks")
dim(filter(oj,brand=="tropicana"|brand=="dominicks"))



#Selecting Columns
dat10<-select(oj,brand,INCOME,feat)
names(dat10)







dat11<-select(oj,-brand,-INCOME,-feat)





# in base R creating new column does create in the same data frame, but in dplyr u can assign it to new different data frame 

#Creating a new column
dat12<-mutate(oj,logIncome=log(INCOME))

oj$logIncome<-log(oj$INCOME)
dim(oj)
names(oj)

dat12<-log(oj$INCOME)
dim(dat12)
dat12
class(dat12)

# in base R to assign new column to new data then it is difficult
dat12<-mutate(oj,loginc=log(INCOME))
names(dat12)
names(oj)

#Arranging data #sorting data
dat13<-arrange(oj,INCOME)
dat13$week%>%head()

# in base R we use order to do this, obviously syntax is different there and here








#both does the same function

dat14<-arrange(oj,desc(INCOME)
dat14<-arrange(oj,-INCOME)





#Group Wise summaries
gr_brand<-group_by(oj,brand)
summarize(gr_brand,mean(INCOME),sd(INCOME))

summarize(gr_brand,meanincome=mean(INCOME),sdincome=sd(INCOME)) # does same but with column names

oj%>%group_by(brand)%>%summarize(mean(INCOME), sd(INCOME))
oj%>%group_by(brand)%>%summarize(meanincome=mean(INCOME), sdincome=sd(INCOME))

# read about ungroup()




#Pipelines
#Base R code

mean(oj[oj$INCOME>=10.5,"price"])

#dplyr code for doing above piece of code
summarize(filter(oj,INCOME>=10.5),mean(price))


# pipeline %>% means the argument on left of pipeline goes as first argument of pipeline right code

oj%>%filter(INCOME>=10.5)%>%summarize(mean(price))



oj%>%filter(price>=2.5)%>%mutate(logIncome=log(INCOME))%>%summarize(mean(logIncome),median(logIncome),sd(logIncome))



##Date
fd<-read.csv("C:\\Users\\Poori\\Desktop\\jigsaw\\R for data science\\Class Codes\\Fd.csv")
str(fd)







fd$FlightDate<-as.Date(fd$FlightDate,"%d-%b-%y")

str(fd)


head(months(fd$FlightDate))
length(fd$FlightDate)
unique(months(fd$FlightDate))


head(weekdays(fd$FlightDate))
unique(weekdays(fd$FlightDate))




#Finding time interval

fd$FlightDate[60]-fd$FlightDate[900]

difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "weeks")

difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "days")
#'arg' should be one of "auto", "secs", "mins", "hours", "days", "weeks"

difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "hours")

difftime(fd$FlightDate[3000],fd$FlightDate[90])

#Subsetting data based on time information
library(dplyr)

#Subset the data for day=Sunday
dim(fd)
fd_s<-fd%>%filter(weekdays(FlightDate)=="Sunday")
dim(fd_s)
#Find the number of flights on Sundays for destination Atlanta (in DestCityName Atlanta is stored as Atlanta, GA)
fd%>%filter(weekdays(FlightDate)=="Sunday",DestCityName=="Atlanta, GA")%>%nrow()


#Find the number of flights on Sundays for all cities
fd%>%filter(weekdays(FlightDate)=="Sunday")%>%group_by(DestCityName)%>%summarize(n())




#POSIXct and POSIXlt
date1<-Sys.time()
date1
class(date1)
weekdays(date1)
months(date1)
date2<-as.POSIXlt(date1)
date2
str(date2)
unclass(date2)
names(unclass(date2)) # to know the names of date2 in posixlt class
#can see below categories when you unclass the date2 
date2$wday
date2$zone
date2$hour


#Merging data
##Joins using Merge
df1 = data.frame(CustomerId=c(1:6),Product=c(rep("Toaster",3),rep("Radio",3)))
df2 = data.frame(CustomerId=c(2,4,6),State=c(rep("Alabama",2),rep("Ohio",1)))

merge(x=df1,y=df2,by="CustomerId")#Inner Join/Intersection of both tables

merge(x = df1, y = df2, by = "CustomerId", all = TRUE)#Outer join: # it is like union

merge(x = df1, y = df2, by = "CustomerId", all.x=TRUE)#Left join # it's like all of left df is present

merge(x = df1, y = df2, by = "CustomerId", all.y=TRUE)#Right join #its like all of right df is present




#Missing values
a<-c(1,2,3,4,5,6,NA,NA,NA,7,8,9)
is.na(a)
sum(is.na(a))

#data(airquality)

air<-airquality
head(air)




sum(is.na(air$Ozone))
sum(is.na(air$Solar.R))



summary(air)


#Imputing Missing values

air$Ozone[is.na(air$Ozone)]<-45

summary(air)





air$Solar.R[is.na(air$Solar.R)]<-mean(air$Solar.R,na.rm=TRUE)
summary(air)






#reshape2() # just to change from wide format to long format and vice versa
library(reshape2)
person<-c("Sankar","Aiyar","Singh")
age<-c(26,24,25)
weight<-c(70,60,65)
wide<-data.frame(person,age,weight)
wide










melt(wide,id.vars="person")



melt(wide,id.vars="person",value.name ="Demo_Value" )

melt(wide,id.vars="person",variable.name ="age" ) # just column name changes

melt(wide,id.vars="person",value.name ="Demo_Value" , variable.name = "age_value")

melt(wide,id.vars="person",value.name ="Demo_Value" , variable.name = "age_value", person.name="person_name")
#above code for person is not working

melted<-melt(wide,id.vars="person",value.name ="Demo_Value" )



#help("~")


# have to do it again carefully

dcast(melted,person~variable,value.var = "Demo_Value")







#String manipulation
a<-"Batman"
substr(a,start=2,stop=6)

nchar(a)
tolower(a)
toupper(a)

b<-"Bat-Man"
strsplit(b,split="-")
c<-"Bat/Man"
strsplit(c,split="/")




#paste(b,c) # "Bat-Man Bat/Man"
paste(b,split=c)
paste(b,c) # works same as above
# "Bat-Man "Bat/Man"
c(b,c)

grep("-",c(b,c))


c(b,c)
grepl("/",c(b,c)) # outputs in which index it is present and index starts from 1

c(c,b)
grepl("/",c(c,b)) # logical and outputs TRUE or FALSE


b
sub("-","/",b) #substitutes / in place of -





d<-"Bat-Ma-n"


sub("-","/",d)


gsub("-","/",d) # substitutes all the - by / while sub only does for the first displayed value






#sqldf
library(sqldf)
#Using SELECT statement
oj_s<-sqldf("select brand, income, feat from oj ")
dim(oj_s)
names(oj_s)

#Subseting using where statement
oj_s<-sqldf("select brand, income, feat from oj where price<3.8 and income<10")
dim(oj_S)

#Order by statement # ascending # descending orders
sqldf("select store,brand,week,logmove,feat,price, income from oj order by income asc")

sqldf("select store,brand,week,logmove,feat,price, income from oj order by income desc")


#distinct
sqldf("select distinct brand from oj")

#Demo sql functions
sqldf("select avg(income) from oj")

sqldf("select min(price) from oj")


###############################
#code for graded assignments
###############################

MM<-read.csv("C:/Users/Poori/Desktop/jigsaw/R for data science/Module 3/MMwoes.csv", sep=",", header = TRUE, skip = 1)
loan_ordered<-order(MM$loan_amnt)
loan_ordered<-MM$loan_amnt[order(MM$loan_amnt)]
length(loan_ordered)
loan_ordered<-loan_ordered[1:124]
median(loan_ordered)



cntrb<-read.csv("C://Users//Poori//Desktop//jigsaw/R for data science//Module 3//contributions.csv", sep=",",header = TRUE)
cntrb%>%filter(cntrb$party=="R")%>%summarize(mean())
cntrb%>%filter(cntrb$party=="R")%>%summarize(mean(amount))
cntrb%>%filter(cntrb$party=="D")%>%summarize(max(amount))
cntrb%>%filter(cntrb$party=="D")%>%summarize(min(amount))
cntrb%>%filter(contrb$party=="D")%>%group_by(state)%>%summarize(mean(amount))
cntrb%>%filter(cntrb$party=="D")%>%group_by(state)%>%summarize(mean(amount))
#8
cntrb%>%filter(cntrb$party=="D")%>%group_by(state)%>%summarize(mean(amount))%>%sort()

