
library("dplyr")

setwd("C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project")
getwd()
dat<-read.csv("sampletelecomfinal.csv")
str(dat)
length(names(dat))
names(dat)
summary(dat)

typeof(colSums(is.na(dat)))
class(colSums(is.na(dat)))


k<-names(dat[,order(-colSums(is.na(dat)))])

colSums(is.na(dat[,k]))


colSums(is.na(dat))


index<-order(-colSums(is.na(dat)))

'''
solflag          retdays         wrkwoman         div_type            occu1         proptype          cartype         children         mailordr         mailresp         numbcars         dwllsize 
12999            12825            11590            10782             9693             9432             9010             8704             8426             8235             6492             4974 
dwlltype        income       hnd_webcap prizm_social_one          avg6mou          avg6qty          marital           ethnic             age1             age2         forgntvl         mtrcycle 
4099             3254             1213              941              402              402              214              214              214              214              214              214 
truck          car_buy        hnd_price       change_mou         mou_Mean      totmrc_Mean        rev_Range        mou_Range      ovrrev_Mean         rev_Mean      ovrmou_Mean        roam_Mean 
214              214              130               71               26               26               26               26               26               26               26               26 
da_Mean         da_Range      datovr_Mean     datovr_Range             area              csa    drop_blk_Mean   drop_vce_Range owylis_vce_Range   mou_opkv_Range           months         totcalls 
26               26               26               26                4                4                0                0                0                0                0                0 
eqpdays    custcare_Mean    callwait_Mean  iwylis_vce_Mean   callwait_Range   ccrndmou_Range           adjqty    comp_vce_Mean    plcd_vce_Mean          avg3mou           avgmou          avg3qty 
0                0                0                0                0                0                0                0                0                0                0                0 
avgqty         crclscod         asl_flag       refurb_new           models         actvsubs         uniqsubs     opk_dat_Mean    recv_sms_Mean    blck_dat_Mean    mou_pead_Mean            churn 
0                0                0                0                0                0                0                0                0                0                0                0 
drop_dat_Mean    drop_vce_Mean           adjmou           totrev           adjrev           avgrev      Customer_ID 
0                0                0                0                0                0                0

'''

length(names(dat))
dat1<-dat[,-c(solflag, retdays, wrkwomen,div_type,occu1,proptype,cartype,children,mailordr,mailresp,numbcars,dwlltype,income)]

dat1<-subset(dat, select = -c("solflag", "retdays", "wrkwomen","div_type","occu1","proptype","cartype","children","mailordr","mailresp","numbcars","dwlltype","income") )

length(names(dat1))
names(dat)

colSums(is.na(dat))


order(-colSums(is.na(dat)))

which(colnames(dat)=="car_buy") #65
which(colnames(dat)=="hnd_webcap") #36
which(colnames(dat)=="prizm_social_one") #33

dat2<-dat[,-c(61,53,55,72,49,62,64,66,48,63,52,47,46,12,65,36,33)]
length(names(dat2))
str(dat2)


#write.csv(dat2,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat2.csv")

indx<-which(is.na(dat2$mou_Mean))
dat4<-dat2[-indx,]
#write.csv(dat4,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat4.csv")
colSums(is.na(dat4))


indx<-which(is.na(dat4$marital))
dat5<-dat4[-indx,]
#write.csv(dat5,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat5.csv")
(colSums(is.na(dat5))>0)

indx<-which(colSums(is.na(dat5))>0)
dat6<-dat5[,indx]
colnames(dat6)
summary(dat5)

summary(dat6$change_mou)
x<-boxplot(dat6$change_mou)
x$out
quantile(dat6$change_mou,p=c(1:100)/100, na.rm=TRUE)
#dat6[12127,"change_mou"]<-314

indx<-which(dat6[,"change_mou"]>2000)
dat6[indx,"change_mou"]<- 742.1875 #99 percentile is 742.1875
indx<-which(dat6[,"change_mou"]< -1500)
indx 
dat6[indx,"change_mou"]<- -834.6875

dat6[indx,"change_mou"]<- 742.1875
indx<-which(is.na(dat6$change_mou))
dat6[indx,"change_mou"]<- -4.50 #here we imputed with median 

#indx<-which(dat6[,"change_mou"]< -1000)

summary(dat6)
summary(dat6$avg6mou)
x<-boxplot(dat6$avg6mou)
x$out
quantile(dat6$avg6mou,p=c(1:100)/100, na.rm=TRUE)
#write.csv(dat6,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat6.csv")
indx<-which(dat6[,"avg6mou"]>5000)
indx 
dat6[indx,"avg6mou"]<- 2359.16 #imputing with 99th percentile
#hist(dat6$avg6mou)
indx<-which(is.na(dat6$avg6mou))
dat6[indx,"avg6mou"]<- 519.9 #imputed with mean 
summary(dat6)


summary(dat6)
summary(dat6$avg6qty)
x<-boxplot(dat6$avg6qty)
x$out
quantile(dat6$avg6qty,p=c(1:100)/100, na.rm=TRUE)
#write.csv(dat6,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat6.csv")
indx<-which(dat6[,"avg6qty"]>2000)
indx 
dat6[indx,"avg6qty"]<- 883.72 #imputing with 99th percentile
#hist(dat6$avg6mou)
indx<-which(is.na(dat6$avg6qty))
dat6[indx,"avg6qty"]<- 182 #imputed with mean 
summary(dat6)



summary(dat6)
summary(dat6$hnd_price)
x<-boxplot(dat6$hnd_price)
x$out
quantile(dat6$hnd_price,p=c(1:100)/100, na.rm=TRUE)
#write.csv(dat6,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat6.csv")
indx<-which(dat6[,"hnd_price"]>498)
indx 

#dat6[indx,"hnd_price"]<- 883.72 #not changing here
#hist(dat6$avg6mou)
indx<-which(is.na(dat6$hnd_price))
dat6[indx,"hnd_price"]<- 104.89 #imputed with mean 
summary(dat6)



k1<-colnames(dat6)
k1

#dat5 is data frame with only 5 columns where we have NAs and remaining columsn are cleaned accordingly (final)
dat5copy<-dat5
for (i in k1){
  dat5copy[,i]<-dat6[,i]
}

summary(dat5copy)

indx<-which(is.na(dat5copy$area))
indx
dat5copy<-dat5copy[-indx,]
summary(dat5copy)
dat5final<-dat5copy #just fo backup

which(colnames(dat5copy)=="Customer_ID")
dat5copy<-dat5copy[,-62]

summary(dat5copy)
#write.csv(dat5copy,"C:\\Users\\Poori\\Desktop\\jigsaw\\Capstone Project\\dat5copy.csv")

names(dat5copy)

dat5copy%>%mutate(dec=ntile(totrev,n =10))%>%count(churn,dec)%>%filter(churn==1)->dat45

dat45$N<-unclass(dat5copy%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
names(dat45)
dat45$churnperc<-dat45$n/dat45$N
dat45

#a<-unclass(dat5copy%>%mutate(dec=ntile(totrev,n=10))%>%count(dec))
#a$n

dat5copy%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev))

#dat%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev))

#nrow(dat5copy)
names(dat2)
`-+]+987--`
dat2%>%mutate(dec=ntile(totrev,n =10))%>%count(churn,dec)%>%filter(churn==1)->dat45

dat45$N<-unclass(dat2%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
names(dat45)
dat45$churnperc<-dat45$n/dat45$N
dat45



#a<-unclass(dat5copy%>%mutate(dec=ntile(totrev,n=10))%>%count(dec))
#a$n

unclass(dat2%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))
#you have to build mice funciton on dat5 which have 5 columns with NA


'''
#which(is.na(dat4$mou_Mean))

k1<-names(dat2[,order(-colSums(is.na(dat2)))])

sum(colSums(is.na(dat2[,k1]))>0)

indx<-which(colSums(is.na(dat2[]))>0)

dat3<-dat2[,indx]
str(dat3)

summary(dat3$mou_Mean)
table(dat3$truck)
summary(dat3$truck)

table(dat3$forgntvl)
table(dat3$mtrcycle)
table(dat3$truck)
#table(dat3$roam_Mean)
'''
  


#hist(dat3$truck)

#length(colnames(dat3))


#View(dat2)
#str(dat2)

'''
library("mice")
imputed_Data <- mice(dat2[,indx],m=2,maxit = 1,method='cart')
summary(imputed_Data)
str(imputed_Data)
dat3 <- complete(imputed_Data,1)
summary(dat3)
colSums(is.na(dat3))
#this funciton is taking too long too get the output. Its like 10 minutes and only few variables are processed so dropped the idea of 
#solving this question using this function

order(-colSums(is.na(dat2)))

#dat4<-dat2[,clnms]
#length(colnames(dat4))

k1<-colnames(dat3)
k1

dat2copy<-dat2
for (i in k1){
  dat2copy[,i]<-dat3[,i]
}

colSums(is.na(dat2copy))
length(colnames(dat2copy))
'''































