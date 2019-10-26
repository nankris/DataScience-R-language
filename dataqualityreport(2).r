library("dplyr")
setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project")
getwd()
dat<-read.csv("C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\sampletelecomfinal.csv", stringsAsFactors = FALSE)
str(dat)
colSums(is.na(dat))
nrow(dat)
length(which(dat$car_buy %in% "New"))
length(which(dat$car_buy %in% "UNKNOWN"))
#Dropping NA columns
indx<-which(names(dat) %in% c("mailordr","occu1","numbcars","retdays","wrkwoman","solflag","proptype","mailresp","cartype","car_buy","children","div_type"))
dat2 <- dat[,-indx]
names(dat2)

dat <- dat2
str(dat)

DataType<-list()
NoOfRecords<-list()
UniqueRecords<-list()
DataAvailable<-list()
AvailablePercent<-list()
Missing<-list()
MissingPercent<-list()
Minimum<-list()
Maximum<-list()
Mean<-list()
`5th Percentile` <-list()
`10th Percentile`<-list()
`25th Percentile`<-list()
`50th Percentile`<-list()
`75th Percentile`<-list()
`90th Percentile`<-list()
`95th Percentile`<-list()


j<-1
#class(dat[,"mou_Mean"])
for (i in names(dat)){
  #DataType1[j]<-class(dat[,i])
  DataType[j]<-class(dat[[i]])    # dat$i, "dat$i" dat[[i]] dat[,i]
  NoOfRecords[j]<-length(dat[[i]])
  UniqueRecords[j]<-length(unique(dat[[i]]))
  DataAvailable[j]<-(length(dat[[i]])-sum(is.na(dat[[i]])))
  AvailablePercent[j]<-(as.numeric(DataAvailable[j])/as.numeric(NoOfRecords[j]))
  Missing[j]<-sum(is.na(dat[[i]]))
  MissingPercent[j]<-(as.numeric(Missing[j])/as.numeric(NoOfRecords[j]))
  if (class(dat[[i]])=="character"){
    Minimum[j]<-0
    Maximum[j]<-0
    Mean[j]<-0
    
    `5th Percentile`[j]<-0
    `10th Percentile`[j]<-0
    `25th Percentile`[j]<-0
    `50th Percentile`[j]<-0
    `75th Percentile`[j]<-0
    `90th Percentile`[j]<-0
    `95th Percentile`[j]<-0
    
    
  } else {
    Minimum[j]<-min(dat[[i]], na.rm = TRUE)
    Maximum[j]<-max(dat[[i]],na.rm = TRUE)
    Mean[j]<- mean(dat[[i]],na.rm = TRUE)

    `5th Percentile`[j]<-quantile(dat[[i]],c(0.05),na.rm = TRUE)
    `10th Percentile`[j]<- quantile(dat[[i]],c(0.1),na.rm = TRUE)
    `25th Percentile`[j]<- quantile(dat[[i]],c(0.25),na.rm = TRUE)
    `50th Percentile`[j]<-quantile(dat[[i]],c(0.5),na.rm = TRUE)
    `75th Percentile`[j] <- quantile(dat[[i]],c(0.75),na.rm = TRUE)
    `90th Percentile`[j]<-quantile(dat[[i]],c(0.90),na.rm = TRUE)
    `95th Percentile`[j]<-quantile(dat[[i]],c(0.95),na.rm = TRUE)
  }

  j<-j+1
}



DataType<-unlist(DataType)
NoOfRecords<-unlist(NoOfRecords)
UniqueRecords<- unlist(UniqueRecords)
DataAvailable<-unlist(DataAvailable)
AvailablePercent<-unlist(AvailablePercent)
Missing<-unlist(Missing)
MissingPercent<-unlist(MissingPercent)
Minimum<-unlist(Minimum)
Maximum<-unlist(Maximum)
Mean<-unlist(Mean)
`5th Percentile` <-unlist(`5th Percentile`)
`10th Percentile`<-unlist(`10th Percentile`)
`25th Percentile`<-unlist(`25th Percentile`)
`50th Percentile`<-unlist(`50th Percentile`)
`75th Percentile`<-unlist(`75th Percentile`)
`90th Percentile`<-unlist(`90th Percentile`)
`95th Percentile`<-unlist(`95th Percentile`)

cb <-cbind(DataType, NoOfRecords,UniqueRecords,DataAvailable,AvailablePercent,Missing,MissingPercent,Minimum,Maximum,Mean,`5th Percentile`,`10th Percentile`,`25th Percentile`,`50th Percentile`,`75th Percentile`,`90th Percentile`,`95th Percentile`)
#class(as.data.frame(cbind(DataType, NoOfRecords)))

QualityReport <- as.data.frame(cb)
rownames(QualityReport)<-colnames(dat)
head(QualityReport)

#write.csv(QualityReport,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\SampleQuality_Report (final).csv")

sort(table(dat$ethnic),decreasing=TRUE)
?sort


# Continuous Variable Profiling
str(dat2)
names(dat2)
#models,actvsub,uniqsubs,dwlltype,mtrcycle,truck,churn  --- yet to convert to categorical variables
index <- which(names(dat2) %in% c("income","crclscod","asl_flag","prizm_social_one","area","refurb_new","hnd_webcap","marital","ethnic","age1","age2","models","hnd_price","actvsubs","uniqsubs","forgntvl","dwlltype","dwllsize","mtrcycle","truck","churn","csa"))
dat2_cont <- dat2[,-index]
names(dat2_cont)
dat2_cat <- dat2[,index]
names(dat2_cat)

#Adults,car_buy,cartype,children,churn,creditcd,crtcount,div_type,dualband,educ1,hhstatin,infobase,(kid0_2,kid3_5,kid6_10,kid11_15,kid16_17),last_swap,lor,mailflag,mailordr,mailresp,new_cell,numbcars,occu1,ownrent,pcowner,phones,pre_hnd_price,proptype,ref_qty,rv,solflag,tot_acpt,tot_ret,wrkwoman

dat2_cont$churn <- dat2_cat$churn
unique(dat2_cont$churn)


#names(dat2)
#xy<-dat2$mou_Mean
#dat2%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
#class(dat45)
#dat45
#dat45$N<-unclass(dat2%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
#dat45$churn_perc<-dat45$n/dat45$N
#dat45$GreaterThan<-unclass(dat2%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
#dat45$LessThan<-unclass(dat2%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
#dat45$varname<-rep(avgrev,nrow(dat45))
#dat45

#write.csv(dat45,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\churnpercentage.csv")

names(dat2_cont)

dat2_cont%>%mutate(dec=ntile(completed_perc,n=10))%>%count(churn,dec)%>%filter(churn==1)->dat45
dat45$N<-unclass(dat2%>%mutate(dec=ntile(completed_perc,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(dat2%>%mutate(dec=ntile(completed_perc,n=10))%>%group_by(dec)%>%summarise(min(completed_perc)))[[2]]
dat45$LessThan<-unclass(dat2%>%mutate(dec=ntile(completed_perc,n=10))%>%group_by(dec)%>%summarise(max(completed_perc)))[[2]]
dat45$varname<-rep('completed_perc',nrow(dat45))
dat45

hist(dat2$completed_perc)
boxplot(dat2$completed_perc)
unique(dat2$completed_perc)
qqplot(dat2$mou_Mean,dat2$churn)
hist(dat2$avgmou)

dat2$completed_perc = dat2$comp_vce_Mean/dat2$plcd_vce_Mean
names(dat2)
names(dat2_cont)
cor(dat2_cont[,1:44])
#Added Completed percentage column as completed percentage = (completed voice calls)/(placed voice calls).
#avgmou*months = totcalls, as we don't want redundant variable we are removing avgmou
#avgqty*months=totrev, as avgqty is redundant information as the variation is already captured in totrev variable we are removing avgqty
#avg3mou,avg3qty information is captured in avg6mou,avg6qty. So we are removing both of them
index <- which(names(dat2) %in% c("mou_Mean","totmrc_Mean","mou_Range","change_mou","drop_blk_Mean","drop_vce_Range","owylis_vce_Range","mou_opkv_Range","months","eqpdays","totcalls","iwylis_vce_Mean","rev_Mean","completed_perc","avg6mou","avg6qty","avg3mou","avg3qty","avgmou","avgqty","avgrev","rev_Mean","drop_dat_Mean","drop_vce_Mean","mou_pead_Mean","opk_dat_Mean","datovr_Mean","ovrmou_Mean","blck_dat_Mean","totrev"))                                                                                                                         
length(index)
dat2_cont_req <- dat2[,index]
names(dat2_cont_req)


cor(dat2_cont_req[,1:18])


#categorical variable profiling
dat2_cat%>%count(churn,levels=age2)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_cat%>%filter(age2%in%datC1$levels)%>%count(age2))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("age2",nrow(datC1))
datC1


index <- which(names(dat2) %in% c("income","crclscod","prizm_social_one","area","marital","ethnic","age1","age2","models","hnd_price","actvsubs","asl_flag","uniqsubs","dwllsize","csa","churn","Customer_ID"))
length(index)
dat2_cat_req <- dat2[,index]

names(dat2_cat_req)

#coverting class of categorical variables from character to factors
dat2_cat_req_factor<-list()
for (i in names(dat2_cat_req)){
  dat2_cat_req_factor[[i]]<-as.factor(dat2_cat_req[[i]])
}
class(dat2_cat_req_factor)
dat2_cat_req_factor<-as.data.frame(dat2_cat_req_factor)
class(dat2_cat_req_factor)
str(dat2_cat_req_factor)

colSums(is.na(dat2_cat_req_factor))

#Use decision tree to prepare
#library(rpart)
#classification: method will be class
#Regression: method will be anova
#mod<-rpart(churn~area,data=dat2_cat_req_factor,method="class")
#unique(mod$where)
#dat2_cat_req_factor$Nodes1<-mod$where
#head(dat2_cat_req_factor,30)

#unique(dat2_cat_req_factor$Nodes)

head(dat2_cont_req)
colSums(is.na(dat2_cont_req))

#write.csv(dat2_cont_req,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat2_cont_req.csv")

hist(dat2_cont_req$mou_Mean)
summary(dat2_cont_req$mou_Mean)
quantile(dat2_cont_req$mou_Mean,p=c(1:100)/100, na.rm=TRUE)


colSums(is.na(dat2_cat_req_factor))

dat2_req_final<-cbind(dat2_cont_req,dat2_cat_req_factor)
length(names(dat2_req_final))

names(dat2_req_final)

#dat2_req_final<-dat2_req_final[,-34]
#names(dat2_req_final)

colSums(is.na(dat2_req_final))

indx<-which(is.na(dat2_req_final$mou_Mean))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$avg6mou))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$marital))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$area))
dat2_req_final<-dat2_req_final[-indx,]

indx<-which(is.na(dat2_req_final$hnd_price))
dat2_req_final<-dat2_req_final[-indx,]

colSums(is.na(dat2_req_final))

str(dat2_req_final)

#income is 30 and dwllsize is 43

dat2_req_final<-dat2_req_final[,-c(30,43)]
names(dat2_req_final)

#as NAs churn percentage is clost to level "U" churn percentage we imputed NA values with "U" (from decile analysis)
indx<-which(is.na(dat2_req_final$prizm_social_one))
dat2_req_final[indx,"prizm_social_one"]<-"U" 

str(dat2_req_final)

names(dat2_req_final)
dat2_req_final <-dat2_req_final[,-c(43)] #removing csa
summary(dat2_req_final)
str(dat2_req_final)

#Completed_perc - NA's
summary(dat2_req_final$completed_perc)
indx<-which(is.na(dat2_req_final$completed_perc))
dat2_req_final[indx,"completed_perc"]<- mean(dat2_req_final$completed_perc,na.rm = TRUE)

colSums(is.na(dat2_req_final))

#Change_mou -NA's
summary(dat2_req_final$change_mou)
indx<-which(is.na(dat2_req_final$change_mou))
dat2_req_final[indx,"change_mou"]<-  -9.927 
# Here -9.927 is taken because at later point we removed outliers and imputed mean of the finalized change_mou column

#write.csv(dat2_req_final,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat2_req_final.csv")

dat<-dat2_req_final %>% count(crclscod) %>% ungroup() %>% arrange(desc(n))
names(dat2_req_final)

#Outliers in continuous variables
bx<-boxplot(dat2_req_final$mou_Mean)
quantile(dat2_req_final$mou_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out


indx<-which(dat2_req_final[,"mou_Mean"]>4000)
dat2_req_final[indx,"mou_Mean"]<- 4000 #above 4000 because after 99.9 percentile all values are around 4000


bx<-boxplot(dat2_req_final$totmrc_Mean)
bx
quantile(dat2_req_final$totmrc_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$totmrc_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$totmrc_Mean)


indx<-which(dat2_req_final[,"totmrc_Mean"]>300)
dat2_req_final[indx,"totmrc_Mean"]<- 233 #above 300 because 3rd largest value is 233 so imputing 399 and 309 with 233



bx<-boxplot(dat2_req_final$mou_Range)
bx
quantile(dat2_req_final$mou_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$mou_Range)


indx<-which(dat2_req_final[,"mou_Range"]>4000)
dat2_req_final[indx,"mou_Range"]<- 3545.47 #before imputing 99.9 percentile is 3545.47

names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_blk_Mean)
bx
quantile(dat2_req_final$drop_blk_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_blk_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_blk_Mean)

indx<-which(dat2_req_final[,"drop_blk_Mean"]>200)
dat2_req_final[indx,"drop_blk_Mean"]<- 157.98533 #before imputing 99.9 percentile is 157.98533


names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_vce_Range)
bx
quantile(dat2_req_final$drop_vce_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_vce_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_vce_Range)

indx<-which(dat2_req_final[,"drop_vce_Range"]>103)
dat2_req_final[indx,"drop_vce_Range"]<- 82.99 #before imputing 99.9 percentile is 82.99


names(dat2_req_final)
bx<-boxplot(dat2_req_final$owylis_vce_Range)
bx
quantile(dat2_req_final$owylis_vce_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$owylis_vce_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$owylis_vce_Range)

indx<-which(dat2_req_final[,"owylis_vce_Range"]>250)
dat2_req_final[indx,"owylis_vce_Range"]<- 246.49 #before imputing 99.9 percentile is 246.49



names(dat2_req_final)
bx<-boxplot(dat2_req_final$mou_opkv_Range)
bx
quantile(dat2_req_final$mou_opkv_Range,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_opkv_Range,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$mou_opkv_Range)

indx<-which(dat2_req_final[,"mou_opkv_Range"]>1800)
dat2_req_final[indx,"mou_opkv_Range"]<- 1637.56 #before imputing 99.9 percentile is 1637.56


names(dat2_req_final)
bx<-boxplot(dat2_req_final$totcalls)
bx
quantile(dat2_req_final$totcalls,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$totcalls,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$totcalls)

indx<-which(dat2_req_final[,"totcalls"]>55000)
dat2_req_final[indx,"totcalls"]<- 46307.92 #before imputing 99.9 percentile is 46307.92


names(dat2_req_final)
bx<-boxplot(dat2_req_final$eqpdays)
bx
quantile(dat2_req_final$eqpdays,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$eqpdays,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$eqpdays)

indx<-which(dat2_req_final[,"eqpdays"]>1500)
dat2_req_final[indx,"eqpdays"]<- 1500 #before imputing 99.9 percentile is 1500


names(dat2_req_final)
bx<-boxplot(dat2_req_final$iwylis_vce_Mean)
bx
quantile(dat2_req_final$iwylis_vce_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$iwylis_vce_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$iwylis_vce_Mean)
indx<-which(dat2_req_final[,"iwylis_vce_Mean"]>150)
dat2_req_final[indx,"iwylis_vce_Mean"]<- 150.32 #before imputing 99.9 percentile is 150.32


names(dat2_req_final)
bx<-boxplot(dat2_req_final$rev_Mean)
bx
quantile(dat2_req_final$rev_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$rev_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$rev_Mean)
indx<-which(dat2_req_final[,"rev_Mean"]>3000)
dat2_req_final[indx,"rev_Mean"]<- 428.29 #before imputing 99.9 percentile is 428.29


names(dat2_req_final)
bx<-boxplot(dat2_req_final$completed_perc)
bx
quantile(dat2_req_final$completed_perc,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$completed_perc,p=c(0:100)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$completed_perc)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg6mou)
bx
quantile(dat2_req_final$avg6mou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg6mou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg6mou)

indx<-which(dat2_req_final[,"avg6mou"]>3000)
dat2_req_final[indx,"avg6mou"]<- 2846.95 #before imputing 99.9 percentile is 2846.95


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg6qty)
bx
quantile(dat2_req_final$avg6qty,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg6qty,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg6qty)

indx<-which(dat2_req_final[,"avg6qty"]>2000)
dat2_req_final[indx,"avg6qty"]<- 1541.44 #before imputing 99.9 percentile is 1541.44


names(dat2_req_final)
bx<-boxplot(dat2_req_final$totrev)
bx
quantile(dat2_req_final$totrev,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$totrev,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$totrev)

indx<-which(dat2_req_final[,"totrev"]>8000)
dat2_req_final[indx,"totrev"]<- 7765.58 #before imputing 99.9 percentile is 7765.58


names(dat2_req_final)
bx<-boxplot(dat2_req_final$change_mou)
bx
quantile(dat2_req_final$change_mou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$change_mou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$change_mou)

indx<-which(dat2_req_final[,"change_mou"]>5000)
dat2_req_final[indx,"change_mou"]<- 1612.63 #before imputing 99.9 percentile is 1612.63

summary(dat2_req_final$change_mou)
str(dat2_req_final)
summary(dat2_req_final)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg3mou)
bx
quantile(dat2_req_final$avg3mou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg3mou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg3mou)

indx<-which(dat2_req_final[,"avg3mou"]>4000)
dat2_req_final[indx,"avg3mou"]<- 3673 #before imputing 99.9 percentile is 3673

summary(dat2_req_final$avg3mou)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avg3qty)
bx
quantile(dat2_req_final$avg3qty,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avg3qty,p=c(9990:10000)/10000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avg3qty)

indx<-which(dat2_req_final[,"avg3qty"]>2100)
dat2_req_final[indx,"avg3qty"]<- 1547.95 #before imputing 99.9 percentile is 1547.95

summary(dat2_req_final$avg3qty)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avgmou)
bx
quantile(dat2_req_final$avgmou,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avgmou,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avgmou)

indx<-which(dat2_req_final[,"avgmou"]>4000)
dat2_req_final[indx,"avgmou"]<- 2972.29 #before imputing 99.9 percentile is 2972.29

summary(dat2_req_final$avgmou)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avgqty)
bx
quantile(dat2_req_final$avgqty,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avgqty,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avgqty)

indx<-which(dat2_req_final[,"avgqty"]>1500)
dat2_req_final[indx,"avgqty"]<- 1427.27 #before imputing 99.9 percentile is 1427.27

summary(dat2_req_final$avgqty)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$avgrev)
bx
quantile(dat2_req_final$avgrev,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$avgrev,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$avgrev)

indx<-which(dat2_req_final[,"avgrev"]>1500)
dat2_req_final[indx,"avgrev"]<- 1427.27 #before imputing 99.9 percentile is 1427.27

summary(dat2_req_final$avgqty)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$rev_Mean)
bx
quantile(dat2_req_final$rev_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$rev_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$rev_Mean)

indx<-which(dat2_req_final[,"rev_Mean"]>1500)
dat2_req_final[indx,"rev_Mean"]<- 1427.27 #before imputing 99.9 percentile is 1427.27

summary(dat2_req_final$rev_Mean)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_dat_Mean)
bx
quantile(dat2_req_final$drop_dat_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_dat_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_dat_Mean)

indx<-which(dat2_req_final[,"drop_dat_Mean"]>50)
dat2_req_final[indx,"drop_dat_Mean"]<- 5.49 #before imputing 99.9 percentile is 5.49

summary(dat2_req_final$drop_dat_Mean)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$drop_vce_Mean)
bx
quantile(dat2_req_final$drop_vce_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$drop_vce_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$drop_vce_Mean)

indx<-which(dat2_req_final[,"drop_vce_Mean"]>100)
dat2_req_final[indx,"drop_vce_Mean"]<- 79.32 #before imputing 99.9 percentile is 79.32

summary(dat2_req_final$drop_vce_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$mou_pead_Mean)
bx
quantile(dat2_req_final$mou_pead_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$mou_pead_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$mou_pead_Mean)

indx<-which(dat2_req_final[,"mou_pead_Mean"]>200)
dat2_req_final[indx,"mou_pead_Mean"]<- 122.95 #before imputing 99.9 percentile is 122.95

summary(dat2_req_final$mou_pead_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$opk_dat_Mean)
bx
quantile(dat2_req_final$opk_dat_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$opk_dat_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$opk_dat_Mean)

indx<-which(dat2_req_final[,"opk_dat_Mean"]>70)
dat2_req_final[indx,"opk_dat_Mean"]<- 58.82 #before imputing 99.9 percentile is 58.82

summary(dat2_req_final$opk_dat_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$datovr_Mean)
bx
quantile(dat2_req_final$datovr_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$datovr_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$datovr_Mean)

indx<-which(dat2_req_final[,"datovr_Mean"]>30)
dat2_req_final[indx,"datovr_Mean"]<- 24.46 #before imputing 99.9 percentile is 24.46

summary(dat2_req_final$datovr_Mean)



names(dat2_req_final)
bx<-boxplot(dat2_req_final$ovrmou_Mean)
bx
quantile(dat2_req_final$ovrmou_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$ovrmou_Mean,p=c(9990:10000)/10000, na.rm=TRUE)
bx$out
hist(dat2_req_final$ovrmou_Mean)

indx<-which(dat2_req_final[,"ovrmou_Mean"]>1000)
dat2_req_final[indx,"ovrmou_Mean"]<- 958.15 #before imputing 99.9 percentile is 958.15

summary(dat2_req_final$ovrmou_Mean)


names(dat2_req_final)
bx<-boxplot(dat2_req_final$blck_dat_Mean)
bx
quantile(dat2_req_final$blck_dat_Mean,p=c(1:100)/100, na.rm=TRUE)
quantile(dat2_req_final$blck_dat_Mean,p=c(990:1000)/1000, na.rm=TRUE)
bx$out
hist(dat2_req_final$blck_dat_Mean)

indx<-which(dat2_req_final[,"blck_dat_Mean"]>20)
dat2_req_final[indx,"blck_dat_Mean"]<- 15.58 #before imputing 99.9 percentile is 15.58

summary(dat2_req_final$blck_dat_Mean)


#classification: method will be class
#Regression: method will be anova
set.seed(123)
indx<-sample(nrow(dat2_req_final),0.7*nrow(dat2_req_final))
train<-dat2_req_final[indx,]
test<-dat2_req_final[-indx,]

#library(rpart)
#mod<-rpart(churn~.,data=train,method="class")
#unique(mod$where)
#data$Nodes<-mod$where
#head(data,30)
#x<-predict(mod,test,type="class")
#error<-mean(x!=test$churn)
#error
#1-error
names(dat2_req_final)
dat2_req_finalll<-dat2_req_final
dat2_req_final<-dat2_req_finalll[,-c(24,25)] #here 24,25 are age1 and age2. We removed it because age 0 has lot of cases and which is not required
names(dat2_req_finalll)


dat2_req_final%>%count(churn,levels=crclscod)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_req_final%>%filter(crclscod%in%datC1$levels)%>%count(crclscod))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("crclscod",nrow(datC1))
datC1
datC1_DF<-as.data.frame(datC1)

#write.csv(datC1_DF,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\datC1_DF.csv")

hist(datC1_DF$ChurnPerc)

#0-0.2(level 1), 0.2-0.4(level 2), 0.4 (level 3) above: total 3 groups for crclscod

#C2   CY    D  D5   E   E4  EA    GA   I   U    U1   W   Z4    ZY

#unique(dat2_req_final$crclscod)

j<-1
for (i in dat2_req_final$crclscod){
  #print(i)
  if (i=="C2" || i=="CY" || i=="D" || i=="D5" || i=="E" || i=="E4" || i=="EA" || i=="GA" || i=="I" || i == "U" || i=="U1" || i=="W" || i=="Z4" || i=="ZY"){
    dat2_req_final[j,"crclscod_Level"]<-1
  } else if(i=="A3" || i=="B2" || i=="EM" || i=="J" || i=="TP" ){
    dat2_req_final[j,"crclscod_Level"]<-3
  } else {
    dat2_req_final[j,"crclscod_Level"]<-2
  }
  j=j+1
}
unique(dat2_req_final$crclscod_Level)
head(dat2_req_final,100)
colSums(is.na(dat2_req_final))



dat2_req_final%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_req_final%>%filter(ethnic%in%datC1$levels)%>%count(ethnic))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("ethnic",nrow(datC1))
datC1
datC1_DF<-as.data.frame(datC1)


#write.csv(datC1_DF,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\datC1_DF.csv")
hist(datC1_DF$ChurnPerc)


#X  P   Z   M   C   Level 1
#G R D O Level 3
j<-1
for (i in dat2_req_final$ethnic){
  #print(i)
  if (i=="X" || i=="P" || i=="Z" || i=="M" || i=="C"){
    dat2_req_final[j,"ethnic_Level"]<-1
  } else if(i=="G" || i=="R" || i=="D" || i=="O"){
    dat2_req_final[j,"ethnic_Level"]<-3
  } else {
    dat2_req_final[j,"ethnic_Level"]<-2
  }
  j=j+1
}
unique(dat2_req_final$ethnic_Level)
head(dat2_req_final)
colSums(is.na(dat2_req_final))



dat2_req_final%>%count(churn,levels=hnd_price)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_req_final%>%filter(hnd_price%in%datC1$levels)%>%count(hnd_price))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("hnd_price",nrow(datC1))
datC1
datC1_DF<-as.data.frame(datC1)

#write.csv(datC1_DF,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\datC1_DF.csv")


hist(datC1_DF$ChurnPerc)

#249.9899902     499.9899902      199.9899902       399.9899902      179.9899902      Level 1
#9.989997864      39.98999023      29.98999023       239.9899902     Level 3
j<-1
for (i in dat2_req_final$hnd_price){
  #print(i)
  if (i=="249.9899902" || i=="499.9899902" || i=="199.9899902" || i=="399.9899902" || i=="179.9899902"){
    dat2_req_final[j,"hnd_price_Level"]<-1
  } else if(i=="9.989997864" || i=="39.98999023" || i=="29.98999023" || i=="239.9899902"){
    dat2_req_final[j,"hnd_price_Level"]<-3
  } else {
    dat2_req_final[j,"hnd_price_Level"]<-2
  }
  j=j+1
}
unique(dat2_req_final$hnd_price_Level)
head(dat2_req_final)
colSums(is.na(dat2_req_final))

Customer_ID_col <- dat2_req_final$Customer_ID
dat2_req_finall<-dat2_req_final

names(dat2_req_final)
dat2_req_final<-dat2_req_final[,-c(18,23,25,29)]  #here 19 is crclscod, 24 is ethnic, 25 is hnd_price
#write.csv(dat2_req_finallll,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat2_req_finallll.csv")
names(dat2_req_final)

dat2_req_final$crclscod_Level<-as.factor(dat2_req_final$crclscod_Level)
dat2_req_final$ethnic_Level<-as.factor(dat2_req_final$ethnic_Level)
dat2_req_final$hnd_price_Level<-as.factor(dat2_req_final$hnd_price_Level)

library(caret)
dummy <- dummyVars("~.",data = dat2_req_final,fullRank = T)
dat2_req_final_dummy <- data.frame(predict(dummy,newdata=dat2_req_final))
names(dat2_req_final_dummy)


set.seed(123)
indx<-sample(nrow(dat2_req_final_dummy),0.7*nrow(dat2_req_final))
train<-dat2_req_final_dummy[indx,]
test<-dat2_req_final_dummy[-indx,]
str(train)

set.seed(123)
index1<-sample(nrow(dat2_req_finall),0.7*nrow(dat2_req_finall))
train_cus<-dat2_req_finall[index1,]
test_cus<-dat2_req_finall[-index1,]
str(train_cus)
length(rownames(dat2_req_finall))

names(test)
names(test_cus)
head(test$mou_Mean)
head(test_cus$mou_Mean)

#churn name changed by dummyVars function to churn.1

myresult<-glm(data=train,churn.1 ~., family=binomial)
summary(myresult)

#STEP FUNCTION
#reduced<-step(myresult,direction="both") 
summary(reduced)

trans_sigmodel<-glm(formula = churn.1 ~ mou_Mean + totmrc_Mean + mou_Range + 
                drop_vce_Range + owylis_vce_Range + mou_opkv_Range + months + 
                totcalls + eqpdays + rev_Mean + avg6mou + avg6qty + completed_perc + 
                asl_flag.Y + prizm_social_one.S + prizm_social_one.U + area.CALIFORNIA.NORTH.AREA + 
                area.CHICAGO.AREA + area.DALLAS.AREA + area.DC.MARYLAND.VIRGINIA.AREA + 
                area.MIDWEST.AREA + area.NEW.YORK.CITY.AREA + area.NORTH.FLORIDA.AREA + 
                area.NORTHWEST.ROCKY.MOUNTAIN.AREA + area.PHILADELPHIA.AREA + 
                area.SOUTH.FLORIDA.AREA + area.TENNESSEE.AREA + marital.M + 
                marital.S + models.2 + models.3 + models.4 + uniqsubs.2 + 
                uniqsubs.3 + uniqsubs.4 + uniqsubs.5 + uniqsubs.7 + uniqsubs.12 + 
                crclscod_Level.2 + crclscod_Level.3 + ethnic_Level.2 + ethnic_Level.3 + 
                hnd_price_Level.2 + hnd_price_Level.3 + actvsubs.1, family = binomial, 
              data = train)

summary(trans_sigmodel)

table(dat2_req_final$churn)/nrow(dat2_req_final)

library(ROCR)
head(predicted)
predicted <- trans_sigmodel$fitted.values
str(predicted) 
pred1<-prediction(predicted,train$churn)
auc<-performance(pred1,"auc")     # train -> accuracy - 63.69 #new accuracy - 0.6323882
auc

pred<-predict(trans_sigmodel,newdata=test,type="response")
pred2<-prediction(pred,test$churn)
table(test$churn.1,pred>0.3) #for confusion matrix of test result
auc<-performance(pred2,"auc")      # test -> accuracy - 60.97 #new accuracy - 0.6108719
auc


predf<-ifelse(pred>0.3,1,0)
str(predf)

#for creating targeted customers
library(dplyr)
test%>%mutate(dec=ntile(totrev,n=10))%>%count(churn.1,dec)%>%filter(churn.1==1)->dat45
dat45$N<-unclass(test%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
dat45$churn_perc<-dat45$n/dat45$N
dat45$GreaterThan<-unclass(test%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dat45$LessThan<-unclass(test%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dat45

test1<-test
names(test1)

j<-1
for (i in test1$totrev){
  #print(i)
  if (i>=0 && i<590){
    test1[j,"totrevBuckets"]<-"low"
  } else if(i>=590 && i<1190){
    test1[j,"totrevBuckets"]<-"medium"
  } else {
    test1[j,"totrevBuckets"]<-"high"
  }
  j=j+1
}
str(test1$totrev)
str(test1$totrevBuckets)



j<-1
for (i in pred){
  #print(i)
  if (i>=0 && i<0.2){
    test1[j,"churnpercent"]<-"low"
  } else if(i>=0.2 && i<0.4){
    test1[j,"churnpercent"]<-"medium"
  } else {
    test1[j,"churnpercent"]<-"high"
  }
  j=j+1
}
names(test1)

table(test1$totrevBuckets,test1$churnpercent)
table(test1$churnpercent)

#high high, high medium, medium high are targets


j<-1
for (i in c(1:nrow(test1))){
  #print(i)
  if (test1[i,"churnpercent"]=="high" && test1[i,"totrevBuckets"]=="high"){
    test1[j,"TargetCust"]<-1
  } else if(test1[i,"churnpercent"]=="high" && test1[i,"totrevBuckets"]=="medium"){
    test1[j,"TargetCust"]<-1
  } else if(test1[i,"churnpercent"]=="medium" && test1[i,"totrevBuckets"]=="high"){
    test1[j,"TargetCust"]<-1
  } else {
    test1[j,"TargetCust"]<-0
  }
  j=j+1
}
test1$TargetCust
table(test1$TargetCust) #748

#59+640+49. Getting the same output


names(test1)
test1$Customer_ID <- test_cus$Customer_ID
Targeted_Customers <- test1%>%filter(TargetCust==1)%>%select(Customer_ID,TargetCust)
class(Targeted_Customers)
#write.csv(Targeted_Customers, "C:\\Users\\Sai\\Downloads\\Jigsaw\\Capstone\\New\\New folder\\Targeted_Customers.csv")









#################################
# tryinig different variations in model #
names(train)
trans_sigmodel1<-glm(formula = churn.1 ~ mou_Mean + totmrc_Mean + mou_Range + 
                       drop_vce_Range + owylis_vce_Range + mou_opkv_Range + months + 
                       totcalls + eqpdays + rev_Mean + avg6mou + avg6qty + completed_perc + 
                       asl_flag.Y + prizm_social_one.S + prizm_social_one.U + area.CALIFORNIA.NORTH.AREA + 
                       area.CHICAGO.AREA + area.DALLAS.AREA + area.DC.MARYLAND.VIRGINIA.AREA + 
                       area.MIDWEST.AREA + area.NEW.YORK.CITY.AREA + area.NORTH.FLORIDA.AREA + 
                       area.NORTHWEST.ROCKY.MOUNTAIN.AREA + area.PHILADELPHIA.AREA + 
                       area.SOUTH.FLORIDA.AREA + area.TENNESSEE.AREA + marital.M + 
                       marital.S + models.2 + models.3 + models.4 + uniqsubs.2 + 
                       uniqsubs.3 + uniqsubs.4 + uniqsubs.5 + uniqsubs.7 + 
                       crclscod_Level.2 + crclscod_Level.3 + ethnic_Level.2 + ethnic_Level.3 + 
                       hnd_price_Level.2 + hnd_price_Level.3 + actvsubs.1, family = binomial, 
                    data = train)

summary(trans_sigmodel1)
names(trans_sigmodel1)
class(trans_sigmodel1)
#code for first problem
sort(trans_sigmodel1$coefficients,decreasing = TRUE)

table(dat2_req_final$churn)/nrow(dat2_req_final)

library(ROCR)
head(predicted)
predicted <- trans_sigmodel1$fitted.values
str(predicted) 
pred1<-prediction(predicted,train$churn)
auctrain<-performance(pred1,"auc")     # train -> accuracy - 63.69 #new accuracy - 0.6323882
auctrain

pred<-predict(trans_sigmodel1,newdata=test,type="response")
pred2<-prediction(pred,test$churn)
table(test$churn.1,pred>0.3) #for confusion matrix of test result
auctest<-performance(pred2,"auc")      # test -> accuracy - 60.97 #new accuracy - 0.6108719
auctest

names(train)


'''
1. highest beta coefficient variables

2. cost and billing
ADJMOU	Billing adjusted total minutes of use over the life of the customer: Ignored may be due to correlation
ADJQTY	Billing adjusted total number of calls over the life of the customer: Ignored
ADJREV	Billing adjusted total revenue over the life of the customer: Ignored
AVGREV	Average monthly revenue over the life of the customer
REV_MEAN	Mean monthly revenue (charge amount): Ignored because lot of 0 values in decile analysis
REV_RANGE	Range of revenue (charge amount): Ignored
TOTMRC_MEAN	Mean total monthly recurring charge


  network and service quality
IWYLIS_VCE_MEAN	Mean number of inbound wireless to wireless voice calls
BLCK_DAT_MEAN	Mean number of blocked (failed) data calls: Ignored because lot of 0 values in decile analysis
DROP_BLK_MEAN	Mean number of dropped or blocked calls
DROP_DAT_MEAN	Mean number of dropped (failed) data calls: Ignored because lot of 0 values in decile analysis
DROP_VCE_MEAN	Mean number of dropped (failed) voice calls: Ignored because lot of 0 values in decile analysis
DROP_VCE_RANGE	Range of number of dropped (failed) voice calls

 data usage connectivity issues: 
DROP_DAT_MEAN	Mean number of dropped (failed) data calls: Ignored because lot of 0 values in decile analysis
BLCK_DAT_MEAN	Mean number of blocked (failed) data calls: Ignored because lot of 0 values in decile analysis
MOU_PEAD_MEAN	Mean unrounded minutes of use of peak data calls: Ignored because lot of 0 values in decile analysis
OPK_DAT_MEAN	Mean number of off-peak data calls: Ignored because lot of 0 values in decile analysis



3. plan migration: overage is important parameter to anayze:
DATOVR_MEAN Overage represents calls or minutes of use over the number of minutes allowed by that customers calling plan.
DATOVR_RANGE	Range of revenue of data overage: Ignored because lot of 0 values in decile analysis
OVRREV_MEAN DATOVR_MEAN + VCEOVR_MEAN: Ignored because lot of 0 values in decile analysis
OVRMOU_MEAN	Mean overage minutes of use: Ignored because lot of 0 values in decile analysis 

are parameters related to overage, but not considered because they are having lot of zero values in decile analysis
'''







