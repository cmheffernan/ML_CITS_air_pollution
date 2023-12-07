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

i=as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

cluster_settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                              year=seq(2017,2020),
                              method=c("rf","boosting","xgboost"),
                              func=c("counterfactuals"),
                              pollutant="NO2")%>%
  bind_rows(expand.grid(State.Name=c("pennsylvaniacomplete"),
                        year=seq(2017,2020),
                        method=c("rf","boosting","xgboost"),
                        func=c("counterfactuals"),
                        pollutant="NO2"))%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"),
         !(year==2013 & State.Name=="pennsylvaniacomplete"),
         !(year==2014 & State.Name=="pennsylvaniacomplete"))%>%
  filter(!(year==2020 & func=="predict"))
cluster_settings$seed<-seq(1,nrow(cluster_settings))*19
cluster_settings<-cluster_settings%>%
  filter(!method=="boosting")

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
  
  #set up training and validation dataframes
  training_valeven <- iris%>%
    filter(!(year==1 & post_cutoff==1 & day(Time) %% 2 ==0))%>%
    ungroup()%>%
    na.omit()%>%
    select(-c(Time,year))%>%
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
  validation2_valeven <- iris%>%
    filter(interaction==0 & year==T & day(Time) %% 2 ==0)%>%
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
    select(-c(Time,year))%>%
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
  validation2_valodd <- iris%>%
    filter(interaction==0 & year==T & day(Time) %% 2 ==1)%>%
    ungroup()%>%
    na.omit()%>%
    mutate(day_index=as.numeric(day_index),
           daylight=ifelse(daylight,1,0),
           weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
    as.data.frame()
  
  
  #fit models and predict on validation sets
  if(method=="boosting"){
    gbm_classifier = gbm(Y~.,distribution = "gaussian",data = training_valeven,
                         n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,cv.folds = 5,
                         n.cores = 2)
    
    validation1_valeven$interaction<-1
    validation1_valeven$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valeven)
    validation1_valeven$interaction<-0
    validation1_valeven$Y0 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valeven)
    
    validation2_valeven$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation2_valeven)
    validation2_valeven$Y0<-NA
    
    gbm_classifier = gbm(Y~.,distribution = "gaussian",data = training_valodd,
                         n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,cv.folds = 5,
                         n.cores = 2)
    
    validation1_valodd$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valodd)
    validation1_valodd$interaction<-0
    validation1_valodd$Y0 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valodd)
    
    validation2_valodd$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation2_valodd)
    validation2_valodd$Y0<-NA
  }else if(method=="rf"){
    rf_classifier = rfsrc(Y ~ ., data=training_valeven, ntree=100, mtry=2)
    
    validation1_valeven$interaction<-1
    validation1_valeven$Y1 <- predict.rfsrc(rf_classifier,validation1_valeven)$predicted
    validation1_valeven$interaction<-0
    validation1_valeven$Y0 <- predict.rfsrc(rf_classifier,validation1_valeven)$predicted
    
    validation2_valeven$Y1 <- predict.rfsrc(rf_classifier,validation2_valeven)$predicted
    validation2_valeven$Y0<-NA
    
    rf_classifier = rfsrc(Y ~ ., data=training_valodd, ntree=100, mtry=2)
    
    validation1_valodd$Y1 <- predict.rfsrc(rf_classifier,validation1_valodd)$predicted
    validation1_valodd$interaction<-0
    validation1_valodd$Y0 <- predict.rfsrc(rf_classifier,validation1_valodd)$predicted
    
    validation2_valodd$Y1 <- predict.rfsrc(rf_classifier,validation2_valodd)$predicted
    validation2_valodd$Y0<-NA
  }else if(method=="xgboost"){
    m<-as.matrix(select(training_valeven,-Y))
    class(m)<-"numeric"
    gbm_classifier = xgboost(data=m,label = training_valeven$Y,
                             max_depth = 10, nthread = 2, nrounds = 10)
    
    validation1_valeven$interaction<-1
    m1<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    validation1_valeven$Y1 <- predict(gbm_classifier, m1)
    validation1_valeven$interaction<-0
    m0<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m0)<-"numeric"
    validation1_valeven$Y0 <- predict(gbm_classifier, m0)
    
    m2<-as.matrix(validation2_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m2)<-"numeric"
    validation2_valeven$Y1 <- predict(gbm_classifier, m2)
    validation2_valeven$Y0<-NA
    
    m<-as.matrix(select(training_valodd,-Y))
    class(m)<-"numeric"
    gbm_classifier = xgboost(data=m,label = training_valodd$Y,
                             max_depth = 10, nthread = 2, nrounds = 10)
    
    validation1_valodd$interaction<-1
    m1<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    validation1_valodd$Y1 <- predict(gbm_classifier, m1)
    validation1_valodd$interaction<-0
    m0<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m0)<-"numeric"
    validation1_valodd$Y0 <- predict(gbm_classifier, m0)
    
    m2<-as.matrix(validation2_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m2)<-"numeric"
    validation2_valodd$Y1 <- predict(gbm_classifier, m2)
    validation2_valodd$Y0<-NA
  }
  
  year<-as.numeric(as.character(year))
  if(year<2000){
    year<-year+2000
  }
  validation<-validation1_valeven%>%
    rbind(validation2_valeven)%>%
    rbind(validation1_valodd)%>%
    rbind(validation2_valodd)
  
  validation
}
#

#run functions #################
print(paste0("i=",i))
cluster_method<-cluster_settings$method[i]
cluster_function<-cluster_settings$func[i]
cluster_state<-cluster_settings$State.Name[i]
cluster_pollutant<-cluster_settings$pollutant[i]
cluster_year<-cluster_settings$year[i]

if(cluster_function=="counterfactuals"){
  #counterfactuals #####################
  
  load("data/all_data_5_cities_interpolated_by_monitor.RData")
  all_data<-data_no2_by_monitor
  
  data<-all_data%>%
    filter(year>=2017)
  
  if(cluster_state=="pennsylvaniacomplete"){
    data<-data%>%
      filter(!monitor==4)
  }
  cluster_state<-ifelse(cluster_state=="pennsylvaniacomplete","pennsylvania",cluster_state)
  
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
  
  set.seed(cluster_settings$seed[i])
  dfs_validation<-its_counterfactuals(1,data,dates,pollutant=cluster_pollutant,method=cluster_method)
  
  save(dfs_validation,file = paste0("results/sensitivity/ML_models_3yrs/",cluster_settings$State.Name[i],"_counterfactuals_",
                                    cluster_pollutant,"_",cluster_method,"_",cluster_year,"_df",".RData"))
}
