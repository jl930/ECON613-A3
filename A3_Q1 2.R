
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plm)
library(tidyverse)
library(moments)
library(fixest)
dpop<-read.csv("/Users/al/Downloads/population.csv")
dcrime<-read.csv("/Users/al/Downloads/crime_long.csv")


dcrime <- dcrime %>% 
  group_by(crime_month, district, crime_type) %>%
  summarise(crimes=sum(crimes)) %>% tibble()

dcrime$crime_month <- as.Date(dcrime$crime_month)


dcrime %>%
  select(crime_month, crimes) %>%
  group_by(by=crime_month) %>% summarise(crimes.sum=sum(crimes)) %>%
  mutate(month=by) %>%
  ggplot(aes(month, crimes.sum)) +
  geom_line() +
  xlab("time") +
  ylab("crimes")


  
dcrime$month1<-month(dcrime$crime_month)
dcrime$year<-year(dcrime$crime_month)
dcrime$ym<-paste(dcrime$year,dcrime$month1,sep="/")

dcrime<-rename(dcrime,month=crime_month)
crime_pop1<-merge(dcrime,dpop,by=c("month","district"))

crime_pop1 <-crime_pop1 %>%
  mutate(crime.res.type=crimes/tot_pop) %>%
  mutate(violent.res= ifelse(crime_type=="violent",crime.res.type,0)) %>%
  mutate(property.res= ifelse(crime_type=="property",crime.res.type,0))

crime_pop2 <-crime_pop1 %>%  
  group_by(ym,district) %>% mutate(crimes.res=sum(crime.res.type))
  
crime_pop2 <-crime_pop2 %>% 
  mutate(black.share=tot_black/tot_pop)%>%
  mutate(hisp.share=tot_hisp/tot_pop)%>%
  mutate(white.share=tot_white/tot_pop)

population.crime <- crime_pop2 %>% 
  select(-tot_pop, -tot_white, -tot_black, -tot_hisp) %>%
  select(district, everything()) %>% arrange(district, month)

population.crime2 <- crime_pop2 %>% 
  group_by(ym,district) %>% mutate(tot.crime=sum(crime.res.type)) %>% 
  filter(crime_type=="drug") %>% 
  select(-crime_type,-violent.res,-property.res)

#Q3

doff<-read.csv("/Users/al/Downloads/officers.csv")
doff$month<-as.Date(doff$month)
doff<-rename(doff,district=unit)
office.crime<-merge(doff,population.crime2,by=c("month","district"))
head(office.crime)



ols1<-lm(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime-1,office.crime)
summary(ols1)



#Q4 Panel data:more controls

ols2<-lm(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime+factor(district)+factor(year)+factor(month1)-1,office.crime)
summary(ols2)





#Q5 individual fixed effect


office.crime$id<-office.crime %>% group_by(district,NUID) %>% group_indices(district,NUID)

o1<-office.crime %>%
  group_by(id) %>% summarise(aa=mean(arrest),at=mean(tenure),ai=mean(p50_inc),aw=mean(white.share),ah=mean(hisp.share),ab=mean(black.share),ac=mean(tot.crime)) 

bewteen<-lm(aa~at+ai+aw+ah+ab+ac-1,o1)
summary(bewteen)

o2<-office.crime %>%
  group_by(id) %>% 
  mutate(aa=mean(arrest),at=mean(tenure),ai=mean(p50_inc),aw=mean(white.share),ah=mean(hisp.share),ab=mean(black.share),ac=mean(tot.crime)) %>%
  mutate(da=arrest-aa,dt=tenure-at,di=p50_inc-ai,dw=white.share-aw,dh=hisp.share-ah,db=black.share-ab,dc=tot.crime-ac)

within<-lm(da~dt+di+dw+dh+db+dc-1,o2)
summary(within)

office.crime$ym1<-zoo::as.yearmon(office.crime$month)
o3<-office.crime %>% arrange(NUID,ym1) %>%
  group_by(NUID) %>% 
  mutate(la = arrest-dplyr::lag(arrest, n = 1, default = NA),
         lt=tenure-dplyr::lag(tenure, n = 1, default = NA),
         li=p50_inc-dplyr::lag(p50_inc, n = 1, default = NA),
         lw=white.share-dplyr::lag(white.share, n = 1, default = NA),
         lh=hisp.share-dplyr::lag(hisp.share, n = 1, default = NA),
         lb=black.share-dplyr::lag(black.share, n = 1, default = NA),
         lc=tot.crime-dplyr::lag(tot.crime, n = 1, default = NA)
  ) 
fd<-lm(la~lt+li+lw+lh+lb+lc-1,data=o3)
summary(fd)
summary(fd2)

o4<-office.crime %>% arrange(NUID,ym1) %>%
  group_by(id) %>% 
  mutate(la = arrest-lag(arrest, n = 1, default = NA),
         lt=tenure-lag(tenure, n = 1, default = NA),
         li=p50_inc-lag(p50_inc, n = 1, default = NA),
         lw=white.share-lag(white.share, n = 1, default = NA),
         lh=hisp.share-lag(hisp.share, n = 1, default = NA),
         lb=black.share-lag(black.share, n = 1, default = NA),
         lc=tot.crime-lag(tot.crime, n = 1, default = NA)
  ) 
fd1<-lm(la~lt+li+lw+lh+lb+lc-1,data=o4)
summary(fd1)

fd2=plm(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime-1, data=office.crime, model="fd",index=c("id","month"))
wi=plm(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime-1, data=office.crime, model="within",index=c("id","month"))
bew1=plm(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime-1, data=office.crime, model="between",index=c("id","month"))



FE <- coef(bewteen)
FE <- as.data.frame(rbind(FE, c(coef(within)) , c(coef(fd2))))
row.names(FE) <- c("Between", "Within", "First time difference")
names(FE)<-c("tenure","income","white.share","hispanic.share","black.share","crime")
FE
#part 2: using GMM method

gmm <- feols(arrest~tenure+p50_inc+white.share+hisp.share+black.share+tot.crime | month + district + NUID, office.crime, panel.id = c("NUID", "month"))
etable(gmm)
gmmcoef <- fixef(gmm)
summary(gmmcoef)

#method of moments with simulations 
mm_sim=function(par,emp_mom)
{
  nsim=50
  sim_mom=mat.or.vec(9,nsim)
  for(js in 1:nsim){
    #use moment condition
    xbeta=par[1]+par[2]*x1+par[3]*x2+par[4]*x3+par[5]*x4
    sim_mom[,js]=all.moments(xbeta,order.max = 9)[-1]
  }
  sim_mom=apply(sim_mom, 1, mean)
  like=sum(sim_mom-emp_mom)^2)
return(like);
}


