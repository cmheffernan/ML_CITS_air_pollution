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

j=as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

#settings matrix to run 50 replicates per cluster run
settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                      year=seq(2013,2020),
                      method=c("rf","xgboost","boosting","bart"),
                      func=c("counterfactuals","predict","predictwholeperiod","counterfactualswholeperiod"),
                      window=c("day","week"),
                      resample=seq(50,500,by=50))%>%
  bind_rows(expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                        year=seq(2013,2020),
                        method=c("rf","xgboost","boosting","bart"),
                        func=c("counterfactuals","predict","predictwholeperiod","counterfactualswholeperiod"),
                        window=c("3day"),
                        resample=seq(50,500,by=50)))%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))%>%
  filter(!(year==2020 & func%in%c("predict","predictwholeperiod")),
         !(year<2020 & func=="counterfactualswholeperiod"))
settings<-settings%>%
  filter(func=="counterfactuals",
         method%in%c("rf","xgboost"))

cluster_settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                              year=seq(2013,2020),
                              method=c("rf","xgboost","boosting","bart"),
                              func=c("counterfactuals","predict","predictwholeperiod","counterfactualswholeperiod"),
                              pollutant="NO2",
                              window=c("day","week"),
                              resample=1:500)%>%
  bind_rows(expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                        year=seq(2013,2020),
                        method=c("rf","xgboost","boosting","bart"),
                        func=c("counterfactuals","predict","predictwholeperiod","counterfactualswholeperiod"),
                        pollutant="NO2",
                        window=c("3day"),
                        resample=1:500))%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))%>%
  filter(!(year==2020 & func%in%c("predict","predictwholeperiod")),
         !(year<2020 & func=="counterfactualswholeperiod"))
cluster_settings$seed<-seq(1,nrow(cluster_settings))*25

cluster_settings<-cluster_settings%>%
  filter(func==settings$func[j],
         method==settings$method[j],
         State.Name==settings$State.Name[j],
         year==settings$year[j],
         window==settings$window[j],
         resample%in%(settings$resample[j]+(-49:0)))

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
  
  #set up training and validation dataframes
  if(cluster_function=="counterfactuals"){
    validation1_valeven <- iris%>%
      filter(year==1 & post_cutoff==1 & day(Time) %% 2 ==0)%>%
      ungroup()%>%
      na.omit()%>%
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
  }
  
  #prepare start of resampling blocks 
  iris_initial<-iris%>%
    na.omit()%>%
    mutate(Time_2020=Time,
           year_num=year(Time))
  year(iris_initial$Time_2020)<-2020
  hours_all<-iris_initial$Time_2020%>%unique()%>%sort()
  last_hour<-max(hours_all)
  first_hour<-min(hours_all)
  ndays<-case_when(
    cluster_window=="day"~1,
    cluster_window=="3day"~3,
    cluster_window=="week"~7)
  start_hours<-expand.grid(
    start=seq(first_hour,last_hour-days(ndays)+hours(1),by="hour"),
    year_num=iris_initial$year_num%>%unique()
  )%>%
    mutate(post_cutoff=ifelse(start>=as.POSIXct(as.character(dates$cutoff_date-hours(12*ndays))),1,0))
  year(start_hours$start)<-start_hours$year_num
  start_hours<-start_hours%>%
    filter(!is.na(start))
  if(!year%in%c("20","2020")){
    hours_2020<-(iris_initial%>%filter(year_num==2020))$Time_2020%>%unique()%>%sort()
    last_hour_2020<-max(hours_2020)
    start_hours<-start_hours%>%
      filter(!(year_num==2020 & start>last_hour_2020-days(ndays)+hours(1)))
  }
  
  #resample
  start_hours<-start_hours%>%
    mutate(y=ifelse(year_num==year,T,F),
           index=1:nrow(start_hours))
  index_0F<-(start_hours%>%filter(y==F,post_cutoff==0))$index
  index_1F<-(start_hours%>%filter(y==F,post_cutoff==1))$index
  index_0T<-(start_hours%>%filter(y==T,post_cutoff==0))$index
  index_1T<-(start_hours%>%filter(y==T,post_cutoff==1))$index
  s_0F<-sample(index_0F,floor(length(index_0F)/(24*ndays)),replace = T)
  s_1F<-sample(index_1F,ceiling(length(index_1F)/(24*ndays)),replace = T)
  s_0T<-sample(index_0T,floor(length(index_0T)/(24*ndays)),replace = T)
  s_1T<-sample(index_1T,ceiling(length(index_1T)/(24*ndays)),replace = T)
  s<-c(s_0F,s_1F,s_0T,s_1T)
  d_resampled<-start_hours[s,]
  vec_blocks<-lapply(1:nrow(d_resampled),function(k){
    print(k)
    t<-seq(d_resampled$start[k],min(d_resampled$start[k]+hours(23),d_resampled$start[k]+hours(24),na.rm = T),by="hour")
    paste0(year(t),"-",month(t),"-",day(t)," ",hour(t),":00:00")
  })
  vec_blocks<-vec_blocks%>%unlist()%>%as.POSIXct(format="%Y-%m-%d %H:%M:%S")
  
  iris<-data.frame(Time=vec_blocks,i=1:length(vec_blocks))%>%
    left_join(iris_initial)%>%
    select(-c(i,Time_2020,year_num))
  
  if(cluster_function=="counterfactuals"){
    training_valeven <- iris%>%
      filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==0))%>%
      ungroup()%>%
      na.omit()%>%
      select(-c(Time,year))%>%
      mutate(day_index=as.numeric(day_index),
             daylight=ifelse(daylight,1,0),
             weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
      as.data.frame()
    
    training_valodd <- iris%>%
      filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==1))%>%
      ungroup()%>%
      na.omit()%>%
      select(-c(Time,year))%>%
      mutate(day_index=as.numeric(day_index),
             daylight=ifelse(daylight,1,0),
             weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
      as.data.frame()
  }
  

  #fit models and predict on validation sets
  if(method=="rf"){
    rf_classifier = rfsrc(Y ~ ., data=training_valeven, ntree=100, mtry=2)
    fitted_even<-rf_classifier$predicted
    
    validation1_valeven$interaction<-1
    validation1_valeven$Y1 <- predict.rfsrc(rf_classifier,validation1_valeven)$predicted
    validation1_valeven$interaction<-0
    validation1_valeven$Y0 <- predict.rfsrc(rf_classifier,validation1_valeven)$predicted
    
    rf_classifier = rfsrc(Y ~ ., data=training_valodd, ntree=100, mtry=2)
    fitted_odd<-rf_classifier$predicted
    
    validation1_valodd$Y1 <- predict.rfsrc(rf_classifier,validation1_valodd)$predicted
    validation1_valodd$interaction<-0
    validation1_valodd$Y0 <- predict.rfsrc(rf_classifier,validation1_valodd)$predicted
    
    r2_train<-1 - sum((c(training_valeven$Y,training_valodd$Y)-c(fitted_even,fitted_odd))^2)/sum((c(training_valeven$Y,training_valodd$Y)-mean(c(training_valeven$Y,training_valodd$Y)))^2)
    r2_test<-1 - sum((c(validation1_valeven$Y,validation1_valodd$Y)-c(validation1_valeven$Y1,validation1_valodd$Y1))^2)/sum((c(validation1_valeven$Y,validation1_valodd$Y)-mean(c(validation1_valeven$Y,validation1_valodd$Y)))^2)
    
  }else if(method=="xgboost"){
    m<-as.matrix(select(training_valeven,-Y))
    class(m)<-"numeric"
    gbm_classifier = xgboost(data=m,label = training_valeven$Y,
                             max_depth = 10, nthread = 2, nrounds = 10)
    fitted_even<-predict(gbm_classifier, m)
    
    validation1_valeven$interaction<-1
    m1<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    validation1_valeven$Y1 <- predict(gbm_classifier, m1)
    validation1_valeven$interaction<-0
    m0<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m0)<-"numeric"
    validation1_valeven$Y0 <- predict(gbm_classifier, m0)
    
    m<-as.matrix(select(training_valodd,-Y))
    class(m)<-"numeric"
    gbm_classifier = xgboost(data=m,label = training_valodd$Y,
                             max_depth = 10, nthread = 2, nrounds = 10)
    fitted_odd<-predict(gbm_classifier, m)
    
    validation1_valodd$interaction<-1
    m1<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    validation1_valodd$Y1 <- predict(gbm_classifier, m1)
    validation1_valodd$interaction<-0
    m0<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m0)<-"numeric"
    validation1_valodd$Y0 <- predict(gbm_classifier, m0)
    
    r2_train<-1 - sum((c(training_valeven$Y,training_valodd$Y)-c(fitted_even,fitted_odd))^2)/sum((c(training_valeven$Y,training_valodd$Y)-mean(c(training_valeven$Y,training_valodd$Y)))^2)
    r2_test<-1 - sum((c(validation1_valeven$Y,validation1_valodd$Y)-c(validation1_valeven$Y1,validation1_valodd$Y1))^2)/sum((c(validation1_valeven$Y,validation1_valodd$Y)-mean(c(validation1_valeven$Y,validation1_valodd$Y)))^2)
  }
  
  year<-as.numeric(as.character(year))
  if(year<2000){
    year<-year+2000
  }
  validation<-validation1_valeven%>%
    rbind(validation1_valodd)
  
  validation
}
#

#run functions #################
i<-1
  cluster_method<-cluster_settings$method[i]
  cluster_function<-cluster_settings$func[i]
  cluster_state<-cluster_settings$State.Name[i]
  cluster_pollutant<-cluster_settings$pollutant[i]
  cluster_year<-cluster_settings$year[i]
  cluster_window<-cluster_settings$window[i]
  cluster_resample<-cluster_settings$resample[i]
  
if(cluster_function%in%c("counterfactuals","counterfactualswholeperiod")){
#counterfactuals #####################

load("data/all_data_5_cities_interpolated_by_monitor.RData")
all_data<-data_no2_by_monitor

data<-all_data%>%
  filter(!year==2012)

nweeks<-8
states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia")
start_date<-as.POSIXct("2020-01-01 00:00:00",tz="")
cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"),
               as.Date("2020-03-07"),as.Date("2020-03-06"),
               as.Date("2020-03-11"))
end_date<-cutoff_date+7*nweeks
dates<-data.frame(start_date,cutoff_date,end_date,weeks=8,State.Name=states)
dates<-as.data.frame(right_join(dates,expand.grid(State.Name=states,year=2013:2020)))

dates<-filter(dates,State.Name==cluster_state,year==cluster_year)
print(dates)

#run function for 50 iterations 
all_metrics<-data.frame()
for(i in 1:50){
  print(i)
  cluster_method<-cluster_settings$method[i]
  cluster_function<-cluster_settings$func[i]
  cluster_state<-cluster_settings$State.Name[i]
  cluster_pollutant<-cluster_settings$pollutant[i]
  cluster_year<-cluster_settings$year[i]
  cluster_window<-cluster_settings$window[i]
  cluster_resample<-cluster_settings$resample[i]
  
set.seed(cluster_settings$seed[i])
dfs_validation<-its_counterfactuals(1,data,dates,pollutant=cluster_pollutant,method=cluster_method)
att<-(dfs_validation%>%
  filter(year==T,post_cutoff==1)%>%
  summarise(Y1=mean(Y1),Y0=mean(Y0))%>%
  mutate(ATT=Y1-Y0))$ATT
percent_diff<-(dfs_validation%>%
                 filter(year==T,post_cutoff==1)%>%
                 summarise(Y1=mean(Y1),Y0=mean(Y0))%>%
                 mutate(pd=100*(Y1-Y0)/Y0))$pd
metrics<-cluster_settings[i,]%>%
  mutate(ATT=att,
         percent_diff=percent_diff)%>%
  select(-seed)
all_metrics<-bind_rows(all_metrics,metrics)

}

  save(all_metrics,
       file = paste0("results/BB_2models_constant_validation_set/",cluster_state,"_",cluster_function,"_",
                     cluster_pollutant,"_",cluster_method,"_",cluster_year,"_",cluster_window,"_",cluster_resample,"_df",".RData"))
}
