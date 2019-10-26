
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


order(-colSums(is.na(dat)))

which(colnames(dat)=="car_buy") #65
which(colnames(dat)=="hnd_webcap") #36
which(colnames(dat)=="prizm_social_one") #33

dat2<-dat[,-c(61,53,55,72,49,62,64,66,48,63,52,47,46,12,65,36,33)]
names(dat2)

k1<-names(dat2[,order(-colSums(is.na(dat2)))])

sum(colSums(is.na(dat2[,k1]))>0)
indx<-which(colSums(is.na(dat2[]))>0)

View(dat2)
str(dat2)

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
































