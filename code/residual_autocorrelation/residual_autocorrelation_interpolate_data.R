library(pacman)
library(tidyverse)
library(randomForest)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(splines)
library(mgcv)
library(randomForestSRC)
library(gbm)
library(ggpubr)
library(SuperLearner)
library(ranger)
library(xgboost)
library(tensorflow)
library(keras)
library(tidymodels)
library(recipes)
library("BART")

#prepare data
load("data/all_data_5_cities_interpolated_by_monitor.RData")

all_data<-data_no2_by_monitor

data<-all_data%>%
  filter(!year==2012)


DC_2020_2<-data%>%
  filter(year(Time)==2020,State.Name=="district of columbia",monitor==2)%>%
  group_by(Time)%>%
  summarise(NO2=mean(NO2,na.rm = T),CO=mean(CO,na.rm = T),PM25=mean(PM25,na.rm = T))%>%
  left_join(data%>%
              filter(year(Time)==2020,State.Name=="district of columbia",monitor==2)%>%
              select(-c(NO2,PM25,CO))%>%
              unique())
data<-data%>%
  filter(!(year(Time)==2020 & State.Name=="district of columbia" & monitor==2))%>%
  bind_rows(DC_2020_2)

nweeks<-8
states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia")
start_date<-as.POSIXct("2020-01-01 00:00:00",tz="")
cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"),
               as.Date("2020-03-07"),as.Date("2020-03-06"),
               as.Date("2020-03-11"))
end_date<-cutoff_date+7*nweeks
dates<-data.frame(start_date,cutoff_date,end_date,weeks=8,State.Name=states)

settings<-data%>%
  left_join(dates)%>%
  filter(Date_2020<end_date)%>%
  group_by(State.Name,year,monitor,start_date,cutoff_date,end_date)%>%
  count()%>%
  filter(n>3000*.75)%>%
  select(-n)%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"),
         !(year==2018 & State.Name=="maryland" & monitor==2),
         !(year==2017 & State.Name=="pennsylvania" & monitor==3))


#interpolate
all_ts_interpolated<-data.frame()
i<-1
for(i in 1:nrow(settings)){
  ts<-data%>%
    filter(State.Name==settings$State.Name[i],
           year==settings$year[i],
           monitor==settings$monitor[i],
           Date_2020>=settings$start_date[i],
           Date_2020<settings$end_date[i])%>%
    mutate(post_cutoff=ifelse(Date_2020>=settings$cutoff_date[i],T,F))%>%
    rename(Y=NO2)%>%
    select(day_index,Y,Time,RH,Temp,wind,WS,pressure,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,post_cutoff)
  if(settings$State.Name[i]=="pennsylvania"){
    ts<-ts%>%select(-c(wind,pressure))
  }else if(settings$State.Name[i]=="district of columbia"){
    ts<-ts%>%select(-c(WS,pressure))
  }else{
    ts<-ts%>%select(-WS)
  }
  
  t1<-min(ts$Time,na.rm = T)
  t0<-as.POSIXct(paste0(year(t1),"-01-01 00:00:00"))
  tn<-max(ts$Time,na.rm = T)
  tall<-seq(t0,tn,by="hour")
  ts_interpolated<-data.frame(Time=tall,t=1:length(tall))%>%
    left_join(ts)%>%
    filter(Time>=t1)
  for(k in 3:ncol(ts_interpolated)){
    # print(k)
    ts_interpolated[,k]<-approx(ts_interpolated$t[!is.na(ts_interpolated[,k])],
                                (ts_interpolated[,k])[!is.na(ts_interpolated[,k])],
                                xout=ts_interpolated$t)$y
  }
  all_ts_interpolated<-bind_rows(all_ts_interpolated,ts_interpolated%>%
                                   mutate(State.Name=settings$State.Name[i]))
}
all_ts_interpolated<-all_ts_interpolated%>%
  mutate(post_cutoff=case_when(
    post_cutoff==0~0,
    post_cutoff==1~1,
    (!post_cutoff%in%c(0,1)) & hour(Time)>=12~0,
    (!post_cutoff%in%c(0,1)) & hour(Time)<12~1
  ))

save(all_ts_interpolated,file = "results/interpolated_data.RData")
