                            ##dplyr##


library(dplyr)

dat1<-read.csv("C:\\Users\\admin\\Documents\\Jigsaw\\Data manipulation and visualization\\Topic 6 - Data manipulation and Visualization\\audit.csv")
head(dat1)

#Get all rows where gender is Male and employment is private#
dat2<-filter(dat1, Employment=="Private")

dat2<-filter(dat1, Marital=="Married" & Gender=="Male")

dat3 <- select(dat1,ID,Age,Employment)




dat3<-filter(select(dat1,Employment,Gender,ID),Employment=="Private" & Gender=="Male")


chk <- select(dat1,Employment,Gender,ID)







dat2<-select(filter(dat1,Employment=="Private" & Gender=="Male"),ID,Marital,Gender)

#Get all observations for columns Age and Marital status where gender is male and employment is private
#Filter+Selection process

dat3<-select(dat2,Age,Marital)

#Is there a better way to do this 2 step process? Can we write a single line of code?

dat4<-select(filter(dat1,Employment=="Private",Gender=="Male"),Age,Marital)

#Or use pipes
dat1%>%filter(Employment=="Private",Gender=="Male") -> temp1

dat1%>%select(ID, Age, Gender, Employment)%>%filter(Employment=="Private",Gender=="Male")-> temp1




dat1%>%filter(Employment=="Private",Gender=="Male")%>%select(Age,Marital)%>%filter(Age>=35)%>%select(Age)->dat7






#Groupby aggregations

#Find average income  of Males and Females across marital status
dat1%>%group_by(Gender,Marital,Employment)%>%summarise(MeanIncome=mean(Income))->summ2

dat1%>%group_by(Gender , Marital)%>%summarise(MeanIncome=mean(Income) ,  cnt_people = n())->summ2










dat1%>%summarise(MeanIncome=mean(Income))

table(dat1$Marital)





#Find out the total number of Males and Females by marital status
dat1%>%group_by(Marital,Gender)%>%summarise(counted=n(), suminc= sum(Income), mininc = min(Income))->summ3

#Or
dat1%>%count(Gender,Marital)

#count is a wrapper for group_by and summarise(n())

#Piping works only if the data types returned by functions and accepted by functions is of compatible type
dat1%>%group_by(Gender,Marital)%>%summarise(MeanIncome=mean(Income))

dat1%>%group_by(Gender,Marital)%>%summarise(MeanIncome=mean(Income),suminc = sum(Income))



class(dat1)



dat1%>%group_by(Gender,Marital)%>%summarise(MeanIncome=mean(Income))%>%group_by()%>%summarise(inc_total = sum(MeanIncome))
  
  

dat1%>%group_by(Gender,Marital)%>%summarise(MeanIncome=mean(Income))%>%nrow()

dat1%>%group_by(Gender,Marital)%>%summarise(MeanIncome=mean(Income))%>%summary()
dat1%>%summary()
#Keep in mind the compatibility of the classes returned and accepted by various functions

                         ##Window functions in dplyr()##
#group_by and summarise would usually produce a single aggregation per group, group mean, sum, count etc

                     #Window family: ranking functions, finding top 10, top 5% in                                           each group

#Top two income  numbers per group of gender



dat1%>%select(ID,Gender,Income)%>%group_by(Gender)%>%filter(min_rank(desc(Income))<=14)%>%arrange(desc(Income))-> tmp7#notice how arrange() works here




#Top 1% by income in each groudat1%>%mutate(GRP=ntile(Income,10))%>%select(GRP,Income)%>%group_by(GRP)%>%summarise(mininc= min(Income),maxinc= max(Income),count = n())

dat1%>%select(ID,Gender,Marital,Income)%>%group_by(Gender)%>%filter(cume_dist(desc(Income))<=0.01)%>%arrange(desc(Income))


dat1%>%mutate(income_2 = Income/100)->test


dat1%>%mutate(Group=ntile(desc(Income),9))%>%group_by(Group)%>%summarise(Maximum=max(Income),Minimum=min(Income),Count=n())




#Dividing Income into 10 equal parts
head(dat2)

dat2%>%group_by(GRP)%>%summarise(Maximum=max(Income),Minimum=min(Income))

#If we have to create groups in descending order??

                     #Window Family: aggregate functions
#cumsum
#cummax
#cummin
#cumany
#cumall

x<-1:10

cumsum(x)
cummax(x)
cummin(x)
cumany(x<=9)#Can be used for filtering
cumall(x<=9)

cumany(x>=9)#??
cumall(x>=9)#?? 


#(Are all the elements in the subset >=9?)

#Using aggregate window functions
dat1%>%mutate(Group=ntile(desc(Income),10))%>%group_by(Group)%>%summarise(Maximum=max(Income),Minimum=min(Income),Count=n())%>%mutate(CumulativeCount=cumsum(Count))

#Filtering rows where cumulative count is >= 800

#First way
dat1%>%mutate(Group=ntile(desc(Income),10))%>%group_by(Group)%>%summarise(Maximum=max(Income),Minimum=min(Income),Count=n())%>%mutate(CumulativeCount=cumsum(Count))%>%filter(CumulativeCount>=800)

#Second way
dat1%>%mutate(Group=ntile(desc(Income),10))%>%group_by(Group)%>%summarise(Maximum=max(Income),Minimum=min(Income),Count=n())%>%mutate(CumulativeCount=cumsum(Count))%>%filter(cumany(CumulativeCount>=800))


#Where can this be useful?
library(Lahman)
data(Batting)#Data on batting averages
?Batting
head(Batting)

Batting%>%group_by(playerID)%>%filter(cumany(G>163))%>%select(playerID,G,yearID)%>%arrange(desc(G))%>%data.frame()

##Working with character data
#Will use mostly in cleaning the data
dat5<-read.csv("C:\\Users\\admin\\Documents\\Jigsaw\\Data manipulation and visualization\\Topic 6 - Data manipulation and Visualization\\Strings.csv")
str(dat5)
head(dat5)#is there something wrong? 
mean(dat5$Income_M)#Why will this happen

#Need to clean the data


dat5$Income_M<-gsub("Rs","",dat5$Income_M)
head(dat5)

dat5$Income_M<-gsub("/-","",dat5$Income_M)
head(dat5)
mean(dat5$Income_M)#Now why an error?

str(dat5)

dat5$Income_M<-as.numeric(dat5$Income_M)
mean(dat5$Income_M)

#Sometimes you might need to use Regexes to work with character data you can refer to this link http://www.zytrax.com/tech/web/regex.htm

x<-paste("$",seq(1,100,10))
x
#How to remove $?
x<-gsub("$","",x)
x#Why?? Need to use regex
x<-gsub("[$]","",x)
x

#Selecting odd column names
library(arules)
data("AdultUCI")

names(AdultUCI)

AdultUCI%>%select(capital-gain)%>%dim()#Why this error?


AdultUCI%>%select(`capital-gain`)%>%dim()#Notice the column name specification


#Need to create a column named "log of age"

AdultUCI%>%mutate(Log of age = log(age))%>%dim()#What can I do?


AdultUCI%>%mutate(`Log of age` = log(age))%>%dim()

                        ###Data Visualization###
library(dplyr)
library(ggplot2)
dat1<-read.csv("C:\\Users\\admin\\Documents\\Jigsaw\\Data manipulation and visualization\\Topic 6 - Data manipulation and Visualization\\audit.csv")


##Grammar elements##
#Aesthetic maps
#Geoms
#Statistical Transformation
#Scales
library(ggplot2)
p<-ggplot(dat1,aes(x=Age,y=Income))
p+geom_point()



p+geom_point(aes(color= Gender))


p+geom_point(aes(color=Gender))

#What if I want to fix the value of color aesthetic rather than mapping it?

p+geom_point(aes(color="blue"))

p+geom_point(color="blue")#Mapping Vs Fixing a value

#Stats and geoms

p<-ggplot(dat1,aes(x=Gender))
p+geom_bar()#Why?




p<-ggplot(dat1,aes(x=Income))
p+geom_histogram()





#Coordinate systems

p<-ggplot(mtcars,aes(x=factor(cyl)))

p+geom_bar()


p+geom_bar()+coord_polar()


#Adjusting scales
p<-ggplot(dat1,aes(x=Age,y=Income))
p+geom_point(aes(color=Gender))+scale_x_continuous(breaks = seq(0,80,10))

p+geom_point(aes(color=Gender))+scale_x_continuous(breaks=seq(0,80,10))


#Other meanings of scale
p+geom_point(aes(size=Gender,color=Income))#Adjust the size of points

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(range = c(1,2))

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(range = c(1,3))


p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(range = c(1,3)) +scale_color_continuous(low="blue",high="red")

#Adjust the gradient of the color
p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,1))+scale_color_continuous(low="blue",high="red")

#What if I don't want to see the legend corresponding to size?
p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,1))+scale_color_continuous(low="blue",high="red")+guides(size=F)
p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+scale_color_continuous(low="blue",high="red")+guides(size=F,color=F)

#Can we improve this further?

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,1))+scale_color_continuous(low="blue",high="red")+guides(size=F)+facet_grid(Marital~Gender)#Anything else we can do?

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+scale_color_continuous(low="blue",high="red")+guides(size=F)+facet_grid(Gender~.)#Anything else we can do?


p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+scale_color_continuous(low="blue",high="red")+guides(size=F)+facet_grid(Employment~.)#Anything else we can do?

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+scale_color_continuous(low="blue",high="red")+guides(size=F)+facet_grid(Gender~Marital+Employment)#Anything else we can do?


#Add conditional mean estimate
p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+scale_color_continuous(low="blue",high="red")+guides(size=F)+geom_smooth()+facet_grid(Gender~.)

p+geom_point(aes(size=Gender,color=Income))+scale_size_discrete(labels=c("F","M"),range = c(1,3))+scale_color_continuous(low="blue",high="red")+guides(size=F)+geom_smooth()+facet_grid(Gender~Employment)


##ggplot2() make sure what is the datatype of the object you are plotting
VADeaths
p<-ggplot(VADeaths,aes(x=`Rural Male`,y=`Rural Female`))#Why this error?
class(VADeaths)
gp<-data.frame(VADeaths)
gp

p<-ggplot(gp,aes(x=Rural.Male,y=Rural.Female))
p+geom_point()

##Class Excercise##

head(economics)
head(presidential)
presidential<-presidential[-c(1:3),]

q<-ggplot(economics,aes(date,unemploy))
q+geom_line()
unemp=q+geom_line()+xlab("")+ylab( "No. unemployed (1000s)")

yrng <- range(economics$unemploy)
xrng <- range(economics$date)

unemp+geom_rect(data=presidential,aes(NULL,NULL,xmin=start,xmax=end,fill=party),ymin=yrng[1],ymax=yrng[2])





unemp+geom_rect(data=presidential,aes(NULL,NULL,xmin=start,xmax=end,fill=party),ymin=yrng[1],ymax=yrng[2],alpha=0.2)



###These are some of the things ggplot2() can do, make sure you understand what grammar of graphics is, how it works within ggplot2()###







