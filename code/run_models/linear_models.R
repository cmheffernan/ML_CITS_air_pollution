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

#settings matrix
linear_settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                                    method=c("linear","linear_no_int"),
                                    func=c("counterfactuals","predict","predict2020","predictwholeperiod","predictwholeperiod2020"),
                                    pollutant="NO2")%>%
  filter(!(method=="linear_no_int" & func%in%c("predict","predictwholeperiod")))

#function: prediction ###############################
its_pred<-function(index,data,dates,pollutant,method){
  print(index)
  
  state<-dates$State.Name[index]
  data<-filter(data,State.Name==state)
  data$State.Name<-as.factor(data$State.Name)
  
  data<-filter(data,Date_2020>=dates$start_date[index],Date_2020<dates$end_date[index])
  
  data$post_cutoff<-ifelse(data$Date_2020>=dates$cutoff_date[index],T,F)
  
  #dependent variable
  data<-rename(data,Y=NO2)
  
  year<-dates$year[index]
  data$year<-ifelse(data$year==year,T,F)
  
    #select covariates
    if(state=="pennsylvania"){
      iris<-data%>%
        ungroup%>%
        select(day_index,year,post_cutoff,Y,Time,RH,Temp,WS,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor)
    }else if(state=="district of columbia"){
      iris<-data%>%
        ungroup%>%
        select(day_index,year,post_cutoff,Y,Time,RH,Temp,wind,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor)
    }else{
      iris<-data%>%
        ungroup%>%
        select(day_index,year,post_cutoff,Y,Time,RH,Temp,wind,pressure,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor)
    }
    
  #set up training and validation dataframes
  if(linear_function=="predict"){
      training_valeven <- iris%>%
        filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==0))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valeven <- iris%>%
        filter(year==1 & post_cutoff==1 & day(Time) %% 2 ==0)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      
      training_valodd <- iris%>%
        filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==1))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valodd <- iris%>%
        filter(year==1 & post_cutoff==1 & day(Time) %% 2 ==1)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
    }else if(linear_function=="predictwholeperiod"){
      training_valeven <- iris%>%
        filter(!(year==1 & day(Time) %% 2 ==0))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valeven <- iris%>%
        filter(year==1 & day(Time) %% 2 ==0)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      
      training_valodd <- iris%>%
        filter(!(year==1 & day(Time) %% 2 ==1))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valodd <- iris%>%
        filter(year==1 & day(Time) %% 2 ==1)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
    }
    
  #fit models and predict on validation sets
  
    knots <- quantile(data$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+.,data=training_valeven)
    
    validation1_valeven$prediction <- predict.lm(slope_model,validation1_valeven)
    
    
    knots <- quantile(data$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+.,data=training_valodd)
    
    validation1_valodd$prediction <- predict.lm(slope_model,validation1_valodd)
  
  
  year<-as.numeric(as.character(year))
  
  dataframe<-validation1_valeven%>%
    rbind(validation1_valodd)
  
  dataframe
}
#function: prediction ###############################
its_pred2020<-function(index,data,dates,pollutant,method){
  print(index)
  
  state<-dates$State.Name[index]
  data<-filter(data,State.Name==state)
  data$State.Name<-as.factor(data$State.Name)
  
  data<-filter(data,Date_2020>=dates$start_date[index],Date_2020<dates$end_date[index])
  
  data$post_cutoff<-ifelse(data$Date_2020>=dates$cutoff_date[index],T,F)
  
  #dependent variable
  data<-rename(data,Y=NO2)
  
  year<-dates$year[index]
  data$year<-ifelse(data$year==year,T,F)
  
  data$interaction<-data$post_cutoff * data$year 
  
  if(method=="linear"){
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
    
    #set up training and validation dataframes
    if(linear_function=="predict2020"){
      training_valeven <- iris%>%
        filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==0))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valeven <- iris%>%
        filter(year==1 & post_cutoff==1 & day(Time) %% 2 ==0)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      
      training_valodd <- iris%>%
        filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==1))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valodd <- iris%>%
        filter(year==1 & post_cutoff==1 & day(Time) %% 2 ==1)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
    }else if(linear_function=="predictwholeperiod2020"){
      training_valeven <- iris%>%
        filter(!(year==1 & day(Time) %% 2 ==0))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valeven <- iris%>%
        filter(year==1 & day(Time) %% 2 ==0)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      
      training_valodd <- iris%>%
        filter(!(year==1 & day(Time) %% 2 ==1))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valodd <- iris%>%
        filter(year==1 & day(Time) %% 2 ==1)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
    }
    
    #fit models and predict on validation sets
    
    knots <- quantile(data$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+bs(day_index,knots=knots)*interaction+.,data=training_valeven)
    
    validation1_valeven$prediction <- predict.lm(slope_model,validation1_valeven)
    
    
    knots <- quantile(data$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+bs(day_index,knots=knots)*interaction+.,data=training_valodd)
    
    validation1_valodd$prediction <- predict.lm(slope_model,validation1_valodd)
  }else if(method=="linear_no_int"){
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
    
    #set up training and validation dataframes
    if(linear_function=="predict2020"){
      training_valeven <- iris%>%
        filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==0))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valeven <- iris%>%
        filter(year==1 & post_cutoff==1 & day(Time) %% 2 ==0)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      
      training_valodd <- iris%>%
        filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==1))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valodd <- iris%>%
        filter(year==1 & post_cutoff==1 & day(Time) %% 2 ==1)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
    }else if(linear_function=="predictwholeperiod2020"){
      training_valeven <- iris%>%
        filter(!(year==1 & day(Time) %% 2 ==0))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valeven <- iris%>%
        filter(year==1 & day(Time) %% 2 ==0)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      
      training_valodd <- iris%>%
        filter(!(year==1 & day(Time) %% 2 ==1))%>%
        ungroup()%>%
        na.omit()%>%
        select(-Time)%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
      validation1_valodd <- iris%>%
        filter(year==1 & day(Time) %% 2 ==1)%>%
        ungroup()%>%
        na.omit()%>%
        mutate(day_index=as.numeric(day_index),
               daylight=ifelse(daylight,1,0),
               weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
        as.data.frame()
    }
    
    #fit models and predict on validation sets
    
    knots <- quantile(data$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+.,data=training_valeven)
    
    validation1_valeven$prediction <- predict.lm(slope_model,validation1_valeven)
    
    
    knots <- quantile(data$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+.,data=training_valodd)
    
    validation1_valodd$prediction <- predict.lm(slope_model,validation1_valodd)
  }
  
  
  year<-as.numeric(as.character(year))
  
  dataframe<-validation1_valeven%>%
    rbind(validation1_valodd)
  
  dataframe
}
#function: counterfactual ########################
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
    r2_train<-summary(slope_model)$r.squared
    
    validation1$interaction<-1
    validation1$Y1 <- predict.lm(slope_model,validation1)
    r2_test<-1 - sum((validation1$Y-validation1$Y1)^2)/sum((validation1$Y-mean(validation1$Y))^2)
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
    r2_train<-summary(slope_model)$r.squared
    
    validation1$interaction<-1
    validation1$Y1 <- predict.lm(slope_model,validation1)
    r2_test<-1 - sum((validation1$Y-validation1$Y1)^2)/sum((validation1$Y-mean(validation1$Y))^2)
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
  
  list(validation,pvalue,r2_train,r2_test)
}
#

#running functions ######################
for(i in seq(1,nrow(linear_settings))){
linear_method<-linear_settings$method[i]
linear_function<-linear_settings$func[i]
linear_state<-linear_settings$State.Name[i]
linear_pollutant<-linear_settings$pollutant[i]

if(linear_function%in%c("predict","predictwholeperiod")){
  #predictions #####################
  
  load("data/all_data_5_cities_interpolated_by_monitor.RData")
  all_data<-data_no2_by_monitor
  
  data<-all_data%>%
    filter(!year==2012)
  data<-filter(data,!year%in%c("20","2020"))
  
  #start and end times dataframe
  nweeks<-8
  states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia")
  start_date<-as.POSIXct("2020-01-01 00:00:00",tz="")
  cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"), 
                 as.Date("2020-03-07"),as.Date("2020-03-06"),
                 as.Date("2020-03-11"))
  end_date<-cutoff_date+7*nweeks
  dates<-data.frame(start_date,cutoff_date,end_date,weeks=8,State.Name=states)
  dates<-as.data.frame(right_join(dates,expand.grid(State.Name=states,year=2013:2019)))
  
  dates<-filter(dates,
                !(year==2013 & State.Name=="massachusetts"),
                !(year==2013 & State.Name=="pennsylvania"),
                !(year==2014 & State.Name=="pennsylvania")
  )
  
  dates<-filter(dates,State.Name==linear_state)
  print(dates)
  
  #run function
  results_pred<-lapply(seq(1,nrow(dates)), its_pred,data,dates,pollutant=linear_pollutant,method=linear_method)
  
  all_validation<-results_pred[[1]]%>%
    as.data.frame()%>%
    mutate(State.Name=dates$State.Name[1],year=dates$year[1],method=linear_method)
  for(k in seq(2,nrow(dates))){
    all_validation<-rbind(all_validation,results_pred[[k]]%>%
                            as.data.frame()%>%
                            mutate(State.Name=dates$State.Name[k],year=dates$year[k],method=linear_method))
  }
  save(all_validation,file = paste0("results/linear_models_predict/dataframe_",linear_state,"_",linear_function,"_",linear_pollutant,"_",linear_method,".RData"))
  
}else if(linear_function%in%c("predict2020","predictwholeperiod2020")){
  #predictions #####################
  
  load("data/all_data_5_cities_interpolated_by_monitor.RData")
  all_data<-data_no2_by_monitor
  
  data<-all_data%>%
    filter(!year==2012)
  
  #start and end times dataframe
  nweeks<-8
  states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia")
  start_date<-as.POSIXct("2020-01-01 00:00:00",tz="")
  cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"), 
                 as.Date("2020-03-07"),as.Date("2020-03-06"),
                 as.Date("2020-03-11"))
  end_date<-cutoff_date+7*nweeks
  dates<-data.frame(start_date,cutoff_date,end_date,weeks=8,State.Name=states)
  dates<-as.data.frame(right_join(dates,expand.grid(State.Name=states,year=2020)))
  
  dates<-filter(dates,State.Name==linear_state)
  print(dates)
  
  #run function
  results_pred<-lapply(seq(1,nrow(dates)), its_pred2020,data,dates,pollutant=linear_pollutant,method=linear_method)
  
  all_validation<-results_pred[[1]]%>%
    as.data.frame()%>%
    mutate(State.Name=dates$State.Name[1],year=dates$year[1],method=linear_method)
  save(all_validation,file = paste0("results/linear_models_predict/dataframe_",linear_state,"_",linear_function,"_",linear_pollutant,"_",linear_method,".RData"))
  
}else if(linear_function=="counterfactuals"){
  #counterfactuals #####################
  load("data/all_data_5_cities_interpolated_by_monitor.RData")
  all_data<-data_no2_by_monitor
  
  data<-all_data%>%
    filter(!year==2012)
  
  #start and end times dataframe
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
  
  #run function
  dfs_validation<-lapply(seq(1,nrow(dates)), its_counterfactuals,data,dates,pollutant=linear_pollutant,method=linear_method)
  
  all_validation<-dfs_validation[[1]][[1]]%>%
    as.data.frame()%>%
    mutate(State.Name=dates$State.Name[1],year=dates$year[1])
  for(k in seq(2,nrow(dates))){
    all_validation<-rbind(all_validation,dfs_validation[[k]][[1]]%>%
                            as.data.frame()%>%
                            mutate(State.Name=dates$State.Name[k],year=dates$year[k]))
  }
  dates$pvalue_model<-as.numeric(map(dfs_validation,2))
  dates$r2_train<-as.numeric(map(dfs_validation,3))
  dates$r2_test<-as.numeric(map(dfs_validation,4))
  
  save(all_validation,dates,file = paste0("results/linear_models_counterfactuals/",linear_state,"_counterfactuals_",linear_pollutant,"_",linear_method,".RData"))
}
}
