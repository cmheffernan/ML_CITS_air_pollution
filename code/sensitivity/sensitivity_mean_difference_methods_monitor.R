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
library(dtw)

settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                      year=seq(2013,2020),
                      method=c("dppc","dib","dd"),
                      LOO_monitor=1:4)%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))%>%
  filter(!(LOO_monitor==1&State.Name=="pennsylvania"))%>%
  filter(!(LOO_monitor==4&State.Name%in%c("maryland","massachusetts")))

load("data/all_data_5_cities_interpolated_by_monitor.RData")

nweeks<-8
states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia","pennsylvania_single")
start_date<-as.POSIXct("2020-01-01 00:00:00",tz="")
cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"),
               as.Date("2020-03-07"),as.Date("2020-03-06"),
               as.Date("2020-03-11"),as.Date("2020-03-06"))
end_date<-cutoff_date+7*nweeks
dates<-data.frame(start_date,cutoff_date,end_date,weeks=8,State.Name=states)

settings$ATT<-NA
settings$pvalue<-NA
i<-1
for(i in seq(1,nrow(settings))){
  print(i)
  
  method<-settings$method[i]
  year_num<-settings$year[i]
  state<-settings$State.Name[i]
  pollutant<-settings$pollutant[i]
  LOO_monitor<-settings$LOO_monitor[i]
  
  data<-data_no2_by_monitor%>%
    filter(!year==2012)%>%
    filter(!monitor==LOO_monitor)%>%
    group_by(State.Name,Date_2020,year,Time)%>%
    summarize(NO2=mean(NO2,na.rm=T))%>%
    ungroup()%>%
    group_by(State.Name,Date_2020,year)%>%
    summarize(NO2=mean(NO2,na.rm=T))
  
    
  if(method=="dppc"){
    data_filtered<-data%>%
      filter(Date_2020>=dates$start_date[dates$State.Name==state],
             Date_2020<dates$end_date[dates$State.Name==state])%>%
      filter(State.Name==state,year==year_num)%>%
      mutate(postcutoff=ifelse(Date_2020>=dates$cutoff_date[dates$State.Name==state],T,F))
    df_att<-data_filtered%>%
      group_by(postcutoff)%>%
      summarise(mean=mean(NO2,na.rm=T))%>%
      pivot_wider(names_from = postcutoff,values_from = mean)%>%
      mutate(ATT=`TRUE`-`FALSE`)
    settings$ATT[i]<-df_att$ATT
    
    fit<-lm(NO2~postcutoff,data=data_filtered)
    settings$pvalue[i]<-summary(fit)$coefficients["postcutoffTRUE","Pr(>|t|)"]
  }else if(method=="dib"){
    data_filtered<-data%>%
      filter(Date_2020>=dates$start_date[dates$State.Name==state],
             Date_2020<dates$end_date[dates$State.Name==state])%>%
      filter(State.Name==state)%>%
      mutate(postcutoff=ifelse(Date_2020>=dates$cutoff_date[dates$State.Name==state],T,F))%>%
      filter(postcutoff==T)%>%
      mutate(year_ind=ifelse(year==year_num,T,F))
    if(!year_num==2020){
      data_filtered<-filter(data_filtered,!year==2020)
    }
    df_att<-data_filtered%>%
      group_by(year_ind)%>%
      summarise(mean=mean(NO2,na.rm=T))%>%
      pivot_wider(names_from = year_ind,values_from = mean)%>%
      mutate(ATT=`TRUE`-`FALSE`)
    settings$ATT[i]<-df_att$ATT
    
    fit<-lm(NO2~year_ind,data=data_filtered)
    settings$pvalue[i]<-summary(fit)$coefficients["year_indTRUE","Pr(>|t|)"]
  }else if(method=="dd"){
    data_filtered<-data%>%
      filter(Date_2020>=dates$start_date[dates$State.Name==state],
             Date_2020<dates$end_date[dates$State.Name==state])%>%
      filter(State.Name==state)%>%
      mutate(postcutoff=ifelse(Date_2020>=dates$cutoff_date[dates$State.Name==state],T,F))%>%
      mutate(year_ind=ifelse(year==year_num,T,F))
    if(!year_num==2020){
      data_filtered<-filter(data_filtered,!year==2020)
    }
    df_att<-data_filtered%>%
      group_by(year_ind,postcutoff)%>%
      summarise(mean=mean(NO2,na.rm=T))%>%
      pivot_wider(names_from = year_ind,values_from = mean)%>%
      mutate(ATT=`TRUE`-`FALSE`)%>%
      select(postcutoff,ATT)%>%
      pivot_wider(names_from = postcutoff,values_from = ATT)%>%
      mutate(ATT=`TRUE`-`FALSE`)
    settings$ATT[i]<-df_att$ATT
    cits<-lm(NO2~postcutoff+year_ind+postcutoff*year_ind,data=data_filtered)
    pvalue<-summary(cits)$coefficients["postcutoffTRUE:year_indTRUE","Pr(>|t|)"]
    settings$pvalue[i]<-pvalue
  }
}

settings$significant<-ifelse(settings$pvalue<0.05,"significant","not significant")

save(settings,file = "results/sensitivity/mean_difference_significance_levels_monitor.RData")
