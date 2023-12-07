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

linear_settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                             method=c("linear","linear_no_int"),
                             func=c("counterfactuals"),
                             pollutant="NO2",
                             LOO_monitor=1:4)%>%
  filter(!(method=="linear_no_int" & func=="predict"))

#function: counterfactual plots ########################
its_counterfactuals<-function(index,data,dates,pollutant,method){
  print(index)
  
  state<-dates$State.Name[index]
  data<-filter(data,State.Name==state)
  data$State.Name<-as.factor(data$State.Name)
  
  data<-filter(data,Date_2020>=dates$start_date[index],Date_2020<dates$end_date[index])
  
  data$post_cutoff<-ifelse(data$Date_2020>=dates$cutoff_date[index],T,F)
  
  #dependent variable
  data<-rename(data,Y=NO2)
  
  year<-dates$year[index]
  if(!year%in%c("20","2020")){
    data<-filter(data,!year%in%c("20","2020") | Time<dates$cutoff_date[index])
  }
  data$year<-ifelse(data$year==year,T,F)
  
  data$interaction<-data$post_cutoff * data$year 
  
  #select covariates
  if(state=="pennsylvania"){
    iris<-data%>%
      ungroup%>%
      select(day_index,year,post_cutoff,Y,Time,RH,Temp,WS,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }else if(state=="district of columbia"){
    iris<-data%>%
      ungroup%>%
      select(day_index,year,post_cutoff,Y,Time,RH,Temp,wind,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }else{
    iris<-data%>%
      ungroup%>%
      select(day_index,year,post_cutoff,Y,Time,RH,Temp,wind,pressure,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }
  iris<-iris%>%
    ungroup()%>%
    na.omit()%>%
    mutate(day_index=as.numeric(day_index),
           daylight=ifelse(daylight,1,0),
           weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
    as.data.frame()
  
  #set up training and validation dataframes
  train<-iris%>%
    select(-Time)
  validation1<-iris%>%
    filter(year==T,post_cutoff==1)
  validation2<-iris%>%
    filter(year==T,post_cutoff==0)
  
  #fit models and predict on validation sets
  if(method=="linear"){
    knots <- quantile(data$day_index, p = seq(0.1,1,by=0.1),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+bs(day_index,knots=knots)*interaction+.,data=train)
    
    validation1$interaction<-1
    validation1$Y1 <- predict.lm(slope_model,validation1)
    validation1$interaction<-0
    validation1$Y0 <- predict.lm(slope_model,validation1)
    
    validation2$interaction<-0
    validation2$Y1 <- predict.lm(slope_model,validation2)
    validation2$Y0 <- NA
    
    reduced_slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+.,data=select(train,-interaction))
    pvalue<-anova(reduced_slope_model,slope_model)[2,"Pr(>F)"]
  }else if(method=="linear_no_int"){
    
    knots <- quantile(data$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+.,data=train)
    
    validation1$interaction<-1
    validation1$Y1 <- predict.lm(slope_model,validation1)
    validation1$interaction<-0
    validation1$Y0 <- predict.lm(slope_model,validation1)
    
    validation2$interaction<-1
    validation2$Y1 <- predict.lm(slope_model,validation2)
    validation2$Y0 <- NA
    
    pvalue<-summary(slope_model)$coefficients["interaction","Pr(>|t|)"]
  }
  
  year<-as.numeric(as.character(year))
  if(year<2000){
    year<-year+2000
  }
  
  validation<-validation1%>%
    rbind(validation2)
  
  list(validation,pvalue)
}
#

#running functions ######################
for(i in seq(1,nrow(linear_settings))){
  linear_method<-linear_settings$method[i]
  linear_function<-linear_settings$func[i]
  linear_state<-linear_settings$State.Name[i]
  linear_pollutant<-linear_settings$pollutant[i]
  linear_LOO_monitor<-linear_settings$LOO_monitor[i]
  
  if(linear_function=="counterfactuals"){
    #counterfactuals #####################
    load("data/all_data_5_cities_interpolated_by_monitor.RData")
    all_data<-data_no2_by_monitor
    
    data<-all_data%>%
      filter(!year==2012)
    
    if(!(data%>%filter(monitor==linear_LOO_monitor,State.Name==linear_state)%>%nrow())==0){
      data<-data%>%
        filter(!monitor==linear_LOO_monitor)
      
      nweeks<-8
      states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia")
      start_date<-as.POSIXct("2020-01-01 00:00:00",tz="")
      cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"),
                     as.Date("2020-03-07"),as.Date("2020-03-06"),
                     as.Date("2020-03-11"))
      end_date<-cutoff_date+7*nweeks
      dates<-data.frame(start_date,cutoff_date,end_date,weeks=8,State.Name=states)
      dates<-as.data.frame(right_join(dates,expand.grid(State.Name=states,year=2013:2020)))
      
      dates<-filter(dates,
                    !(year==2013 & State.Name=="massachusetts"),
                    !(year==2013 & State.Name=="pennsylvania"),
                    !(year==2014 & State.Name=="pennsylvania")
      )
      
      dates<-filter(dates,State.Name==linear_state)
      print(dates)
      
      dfs_validation<-lapply(seq(1,nrow(dates)), its_counterfactuals,data,dates,pollutant=linear_pollutant,method=linear_method)
      
      all_validation<-dfs_validation[[1]][[1]]%>%
        as.data.frame()%>%
        mutate(State.Name=dates$State.Name[1],year=dates$year[1])
      for(k in seq(2,nrow(dates))){
        all_validation<-rbind(all_validation,dfs_validation[[k]][[1]]%>%
                                as.data.frame()%>%
                                mutate(State.Name=dates$State.Name[k],year=dates$year[k]))
      }
      save(all_validation,file = paste0("results/sensitivity/linear_models_monitor/",linear_state,"_counterfactuals_",linear_pollutant,"_",linear_method,"_",linear_LOO_monitor,".RData"))
      
    }
  }
}
