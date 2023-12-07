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

#settings matrix
cluster_settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                              year=seq(2013,2020),
                              method=c("rf","xgboost","boosting","bart"),
                              func=c("counterfactuals","predict","predictwholeperiod","counterfactualswholeperiod"),
                              pollutant="NO2")%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))%>%
  filter(!(year==2020 & func%in%c("predict","predictwholeperiod")),
         !(year<2020 & func=="counterfactualswholeperiod"))
cluster_settings$seed<-seq(1,nrow(cluster_settings))*24

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
  if(cluster_function=="predict"){
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
  }else if(cluster_function=="predictwholeperiod"){
    training_valeven <- iris%>%
      filter(!(year==1 & day(Time) %% 2 ==0))%>%
      ungroup()%>%
      na.omit()%>%
      select(-c(Time,year))%>%
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
      select(-c(Time,year))%>%
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
  if(method=="boosting"){
    gbm_classifier = gbm(Y~.,distribution = "gaussian",data = training_valeven,
                         n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,cv.folds = 5,
                         n.cores = 2)
    
    validation1_valeven$prediction <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valeven)
    
    gbm_classifier = gbm(Y~.,distribution = "gaussian",data = training_valodd,
                         n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,cv.folds = 5,
                         n.cores = 2)
    
    validation1_valodd$prediction <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valodd)
  }else if(method=="rf"){
    rf_classifier = rfsrc(Y ~ ., data=training_valeven, ntree=100, mtry=2)
    
    validation1_valeven$prediction <- predict.rfsrc(rf_classifier,validation1_valeven)$predicted
    
    rf_classifier = rfsrc(Y ~ ., data=training_valodd, ntree=100, mtry=2)
    
    validation1_valodd$prediction <- predict.rfsrc(rf_classifier,validation1_valodd)$predicted
  }else if(method=="bart"){
    m<-as.matrix(training_valeven%>%select(-Y))
    class(m)<-"numeric"
    bart <- wbart(m, training_valeven$Y,ndpost=1000,nskip=100)
    
    m1<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    yhat <- predict(bart, m1)
    validation1_valeven$prediction <- colMeans(yhat)
    
    list_even<<-list("df"=validation1_valeven,"yhat"=yhat)
    
    m<-as.matrix(training_valodd%>%select(-Y))
    class(m)<-"numeric"
    bart <- wbart(m, training_valodd$Y,ndpost=1000,nskip=100)
    
    m1<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    yhat <- predict(bart, m1)
    validation1_valodd$prediction <- colMeans(yhat)
    
    list_odd<<-list("df"=validation1_valodd,"yhat"=yhat)
  }else if(method=="xgboost"){
    m<-as.matrix(select(training_valeven,-Y))
    class(m)<-"numeric"
    gbm_classifier = xgboost(data=m,label = training_valeven$Y,
                             max_depth = 10, nthread = 2, nrounds = 10)
    
    m1<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    validation1_valeven$prediction <- predict(gbm_classifier, m1)
    
    m<-as.matrix(select(training_valodd,-Y))
    class(m)<-"numeric"
    gbm_classifier = xgboost(data=m,label = training_valodd$Y,
                             max_depth = 10, nthread = 2, nrounds = 10)
    
    m1<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    validation1_valodd$prediction <- predict(gbm_classifier, m1)
  }
  
  year<-as.numeric(as.character(year))
  
  validation<-validation1_valeven%>%
    rbind(validation1_valodd)
  validation
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
  
  #set up training and validation dataframes
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
  }else if(cluster_function=="counterfactualswholeperiod"){
    training_valeven <- iris%>%
      filter(!(year==1 & day(Time) %% 2 ==0))%>%
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
      filter(year==1 & post_cutoff==0 & day(Time) %% 2 ==0)%>%
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
      filter(year==1 & post_cutoff==0 & day(Time) %% 2 ==1)%>%
      ungroup()%>%
      na.omit()%>%
      mutate(day_index=as.numeric(day_index),
             daylight=ifelse(daylight,1,0),
             weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
      as.data.frame()
  }
  

  #fit models and predict on validation sets
  if(method=="boosting"){
    gbm_classifier = gbm(Y~.,distribution = "gaussian",data = training_valeven,
                         n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,cv.folds = 5,
                         n.cores = 2)
    fitted_even<-gbm_classifier$fit
    
    validation1_valeven$interaction<-1
    validation1_valeven$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valeven)
    validation1_valeven$interaction<-0
    validation1_valeven$Y0 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valeven)
    
    validation2_valeven$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation2_valeven)
    validation2_valeven$Y0<-NA
    
    gbm_classifier = gbm(Y~.,distribution = "gaussian",data = training_valodd,
                         n.trees = 10000,interaction.depth = 1,shrinkage = 0.001,cv.folds = 5,
                         n.cores = 2)
    fitted_odd<-gbm_classifier$fit
    
    validation1_valodd$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valodd)
    validation1_valodd$interaction<-0
    validation1_valodd$Y0 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation1_valodd)
    
    validation2_valodd$Y1 <- predict(gbm_classifier, n.trees = gbm_classifier$n.trees, validation2_valodd)
    validation2_valodd$Y0<-NA
    
    r2_train<-1 - sum((c(training_valeven$Y,training_valodd$Y)-c(fitted_even,fitted_odd))^2)/sum((c(training_valeven$Y,training_valodd$Y)-mean(c(training_valeven$Y,training_valodd$Y)))^2)
    r2_test<-1 - sum((c(validation1_valeven$Y,validation1_valodd$Y)-c(validation1_valeven$Y1,validation1_valodd$Y1))^2)/sum((c(validation1_valeven$Y,validation1_valodd$Y)-mean(c(validation1_valeven$Y,validation1_valodd$Y)))^2)
    
  }else if(method=="rf"){
    rf_classifier = rfsrc(Y ~ ., data=training_valeven, ntree=100, mtry=2)
    fitted_even<-rf_classifier$predicted
    
    validation1_valeven$interaction<-1
    validation1_valeven$Y1 <- predict.rfsrc(rf_classifier,validation1_valeven)$predicted
    validation1_valeven$interaction<-0
    validation1_valeven$Y0 <- predict.rfsrc(rf_classifier,validation1_valeven)$predicted
    
    validation2_valeven$Y1 <- predict.rfsrc(rf_classifier,validation2_valeven)$predicted
    validation2_valeven$Y0<-NA
    
    rf_classifier = rfsrc(Y ~ ., data=training_valodd, ntree=100, mtry=2)
    fitted_odd<-rf_classifier$predicted
    
    validation1_valodd$Y1 <- predict.rfsrc(rf_classifier,validation1_valodd)$predicted
    validation1_valodd$interaction<-0
    validation1_valodd$Y0 <- predict.rfsrc(rf_classifier,validation1_valodd)$predicted
    
    validation2_valodd$Y1 <- predict.rfsrc(rf_classifier,validation2_valodd)$predicted
    validation2_valodd$Y0<-NA
    
    r2_train<-1 - sum((c(training_valeven$Y,training_valodd$Y)-c(fitted_even,fitted_odd))^2)/sum((c(training_valeven$Y,training_valodd$Y)-mean(c(training_valeven$Y,training_valodd$Y)))^2)
    r2_test<-1 - sum((c(validation1_valeven$Y,validation1_valodd$Y)-c(validation1_valeven$Y1,validation1_valodd$Y1))^2)/sum((c(validation1_valeven$Y,validation1_valodd$Y)-mean(c(validation1_valeven$Y,validation1_valodd$Y)))^2)
    
  }else if(method=="bart"){
    m<-as.matrix(training_valeven%>%select(-Y))
    class(m)<-"numeric"
    bart <- wbart(m, training_valeven$Y,ndpost=1000,nskip=100)
    fitted_even<-colMeans(predict(bart, m))
    
    validation1_valeven$interaction<-1
    m1<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    yhat1 <- predict(bart, m1)
    validation1_valeven$Y1 <- colMeans(yhat1)
    
    validation1_valeven$interaction<-0
    m0<-as.matrix(validation1_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m0)<-"numeric"
    yhat0 <- predict(bart, m0)
    validation1_valeven$Y0 <- colMeans(yhat0)
    
    m2<-as.matrix(validation2_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m2)<-"numeric"
    yhat2 <- predict(bart, m2)
    validation2_valeven$Y1 <- colMeans(yhat2)
    validation2_valeven$Y0<-NA
    
    list_even<<-list("df"=validation1_valeven,"yhat1"=yhat1,"yhat0"=yhat0,"yhat2"=yhat2)
    
    m<-as.matrix(training_valodd%>%select(-Y))
    class(m)<-"numeric"
    bart <- wbart(m, training_valodd$Y,ndpost=1000,nskip=100)
    fitted_odd<-colMeans(predict(bart, m))
    
    validation1_valodd$interaction<-1
    m1<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m1)<-"numeric"
    yhat1 <- predict(bart, m1)
    validation1_valodd$Y1 <- colMeans(yhat1)
    
    validation1_valodd$interaction<-0
    m0<-as.matrix(validation1_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m0)<-"numeric"
    yhat0 <- predict(bart, m0)
    validation1_valodd$Y0 <- colMeans(yhat0)
    
    m2<-as.matrix(validation2_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m2)<-"numeric"
    yhat2 <- predict(bart, m2)
    validation2_valodd$Y1 <- colMeans(yhat2)
    validation2_valodd$Y0<-NA
    
    list_odd<<-list("df"=validation1_valodd,"yhat1"=yhat1,"yhat0"=yhat0,"yhat2"=yhat2)
    
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
    
    m2<-as.matrix(validation2_valeven%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m2)<-"numeric"
    validation2_valeven$Y1 <- predict(gbm_classifier, m2)
    validation2_valeven$Y0<-NA
    
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
    
    m2<-as.matrix(validation2_valodd%>%select(training_valodd%>%select(-Y)%>%colnames()))
    class(m2)<-"numeric"
    validation2_valodd$Y1 <- predict(gbm_classifier, m2)
    validation2_valodd$Y0<-NA
    
    r2_train<-1 - sum((c(training_valeven$Y,training_valodd$Y)-c(fitted_even,fitted_odd))^2)/sum((c(training_valeven$Y,training_valodd$Y)-mean(c(training_valeven$Y,training_valodd$Y)))^2)
    r2_test<-1 - sum((c(validation1_valeven$Y,validation1_valodd$Y)-c(validation1_valeven$Y1,validation1_valodd$Y1))^2)/sum((c(validation1_valeven$Y,validation1_valodd$Y)-mean(c(validation1_valeven$Y,validation1_valodd$Y)))^2)
    
  }
  
  year<-as.numeric(as.character(year))
  if(year<2000){
    year<-year+2000
  }
  validation<-validation1_valeven%>%
    rbind(validation2_valeven)%>%
    rbind(validation1_valodd)%>%
    rbind(validation2_valodd)

  list(validation,r2_train,r2_test)
}
#

#run functions #################
  print(paste0("i=",i))
  cluster_method<-cluster_settings$method[i]
  cluster_function<-cluster_settings$func[i]
  cluster_state<-cluster_settings$State.Name[i]
  cluster_pollutant<-cluster_settings$pollutant[i]
  cluster_year<-cluster_settings$year[i]
  
if(cluster_function%in%c("predict","predictwholeperiod")){
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

dates<-filter(dates,State.Name==cluster_state,year==cluster_year)
print(dates)

#run function
set.seed(cluster_settings$seed[i])
results_pred<-its_pred(1,data,dates,pollutant=cluster_pollutant,method=cluster_method)

save(results_pred,file = paste0("results/ML_models_predict/dataframe_",
                                cluster_state,"_",cluster_function,"_",cluster_pollutant,"_",cluster_method,"_",cluster_year,"_df",".RData"))

}else if(cluster_function%in%c("counterfactuals","counterfactualswholeperiod")){
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

dates<-filter(dates,State.Name==cluster_state,year==cluster_year)
print(dates)

#run function
set.seed(cluster_settings$seed[i])
dfs_validation_list<-its_counterfactuals(1,data,dates,pollutant=cluster_pollutant,method=cluster_method)
dfs_validation<-dfs_validation_list[[1]]
r2_train<-dfs_validation_list[[2]]
r2_test<-dfs_validation_list[[3]]

if(cluster_method=="bart"){
  #bart intervals
  even1<-list_even$yhat1
  odd1<-list_odd$yhat1
  even0<-list_even$yhat0
  odd0<-list_odd$yhat0
  
  even_diff<-even1-even0
  odd_diff<-odd1-odd0
  
  even_sums_df<-even_diff%>%
    t()%>%
    as.data.frame()%>%
    mutate(Time=list_even$df$Time,monitor=list_even$df$monitor)%>%
    pivot_longer(-c(Time,monitor),names_to = "iteration",values_to = "diff")%>%
    mutate(iteration=as.numeric(str_replace(iteration,"V","")))%>%
    group_by(iteration,Time)%>%
    summarize(diff=mean(diff,na.rm=T))%>%
    ungroup()%>%
    group_by(iteration)%>%
    mutate(n=1)%>%
    summarise(sum=sum(diff),
              n=sum(n))%>%
    arrange(iteration)
  even_sums<-even_sums_df$sum
  even_n<-mean(even_sums_df$n)
  
  odd_sums_df<-odd_diff%>%
    t()%>%
    as.data.frame()%>%
    mutate(Time=list_odd$df$Time,monitor=list_odd$df$monitor)%>%
    pivot_longer(-c(Time,monitor),names_to = "iteration",values_to = "diff")%>%
    mutate(iteration=as.numeric(str_replace(iteration,"V","")))%>%
    group_by(iteration,Time)%>%
    summarize(diff=mean(diff,na.rm=T))%>%
    ungroup()%>%
    group_by(iteration)%>%
    mutate(n=1)%>%
    summarise(sum=sum(diff),
              n=sum(n))%>%
    arrange(iteration)
  odd_sums<-odd_sums_df$sum
  odd_n<-mean(odd_sums_df$n)
  
  product_ATTs<-matrix(NA,ncol=length(even_sums),nrow=length(odd_sums))
  for(e in 1:length(even_sums)){
    for(o in 1:length(odd_sums)){
      product_ATTs[o,e]<-(even_sums[e]+odd_sums[o])/(even_n+odd_n)
    }
  }
  product_ATTs_vec<-as.numeric(product_ATTs)
  
  
  df_bart_intervals<-data.frame(state=cluster_state,method=cluster_method,year=cluster_year,
               product_ATT=mean(product_ATTs_vec),product_lower=quantile(product_ATTs_vec,0.025),product_upper=quantile(product_ATTs_vec,0.975))
  
  save(dfs_validation,r2_train,r2_test,
       df_bart_intervals,product_ATTs,
       file = paste0("results/ML_models_counterfactuals/",cluster_state,"_",cluster_function,"_",
                                                     cluster_pollutant,"_",cluster_method,"_",cluster_year,"_df",".RData"))
}else{
  save(dfs_validation,r2_train,r2_test,
       file = paste0("results/ML_models_counterfactuals/",cluster_state,"_",cluster_function,"_",
                     cluster_pollutant,"_",cluster_method,"_",cluster_year,"_df",".RData"))
}

}
