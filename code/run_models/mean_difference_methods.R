library(pacman)
library(tidyverse)
library(randomForest)
library(lubridate)
library(gridExtra)
library(ggplot2)

#settings matrix
settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                      year=seq(2013,2020),
                      method=c("dppc","dib","dd"))%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))

#create hourly and daily data frames
load("data/all_data_5_cities_interpolated_by_monitor.RData")
data_hourly<-data_no2_by_monitor%>%
  filter(!year==2012)%>%
  group_by(State.Name,Date_2020,year,Time)%>%
  summarize(NO2=mean(NO2,na.rm=T))%>%
  ungroup()
data_daily<-data_hourly%>%
  group_by(State.Name,Date_2020,year)%>%
  summarize(NO2=mean(NO2,na.rm=T))%>%
  ungroup()

#data frame of dates for each state
nweeks<-8
states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia","pennsylvania_single")
start_date<-as.POSIXct("2020-01-01 00:00:00",tz="")
cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"),
               as.Date("2020-03-07"),as.Date("2020-03-06"),
               as.Date("2020-03-11"),as.Date("2020-03-06"))
end_date<-cutoff_date+7*nweeks
dates<-data.frame(start_date,cutoff_date,end_date,weeks=8,State.Name=states)

settings$ATT<-NA
settings$pvalue_model<-NA
i<-1
for(i in seq(1,nrow(settings))){
  print(i)
  method<-settings$method[i]
  year_num<-settings$year[i]
  state<-settings$State.Name[i]
  pollutant<-settings$pollutant[i]
  
  if(method=="dppc"){
    #difference between pre and post cutoff
    data_filtered<-data_hourly%>%
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
    
    #model based p-value
    data_filtered_daily<-data_daily%>%
      filter(Date_2020>=dates$start_date[dates$State.Name==state],
             Date_2020<dates$end_date[dates$State.Name==state])%>%
      filter(State.Name==state,year==year_num)%>%
      mutate(postcutoff=ifelse(Date_2020>=dates$cutoff_date[dates$State.Name==state],T,F))
    fit<-lm(NO2~postcutoff,data=data_filtered_daily)
    settings$pvalue_model[i]<-summary(fit)$coefficients["postcutoffTRUE","Pr(>|t|)"]
  }else if(method=="dib"){
    #difference between intervention year and baseline years
    data_filtered<-data_hourly%>%
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
    
    #model based p-value
    data_filtered_daily<-data_daily%>%
      filter(Date_2020>=dates$start_date[dates$State.Name==state],
             Date_2020<dates$end_date[dates$State.Name==state])%>%
      filter(State.Name==state)%>%
      mutate(postcutoff=ifelse(Date_2020>=dates$cutoff_date[dates$State.Name==state],T,F))%>%
      filter(postcutoff==T)%>%
      mutate(year_ind=ifelse(year==year_num,T,F))
    fit<-lm(NO2~year_ind,data=data_filtered_daily)
    settings$pvalue_model[i]<-summary(fit)$coefficients["year_indTRUE","Pr(>|t|)"]
    
  }else if(method=="dd"){
    #difference in differences 
    data_filtered<-data_hourly%>%
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
    
    #model based p-value
    data_filtered_daily<-data_daily%>%
      filter(Date_2020>=dates$start_date[dates$State.Name==state],
             Date_2020<dates$end_date[dates$State.Name==state])%>%
      filter(State.Name==state)%>%
      mutate(postcutoff=ifelse(Date_2020>=dates$cutoff_date[dates$State.Name==state],T,F))%>%
      mutate(year_ind=ifelse(year==year_num,T,F))
    cits<-lm(NO2~postcutoff+year_ind+postcutoff*year_ind,data=data_filtered_daily)
    pvalue<-summary(cits)$coefficients["postcutoffTRUE:year_indTRUE","Pr(>|t|)"]
    settings$pvalue_model[i]<-pvalue
    
  }
}

save(settings,file = "results/mean_difference_significance_levels.RData")
