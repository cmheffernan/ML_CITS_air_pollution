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
library(boot)

i=as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

cluster_settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                      year=seq(2013,2020),
                      method=c("rf","xgboost","boosting","bart"),
                      func=c("counterfactuals","predict","predictwholeperiod","counterfactualswholeperiod"),
                      pollutant="NO2",
                      nsamples=500)%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))%>%
  filter(!(year==2020 & func%in%c("predict","predictwholeperiod")),
         !(year<2020 & func=="counterfactualswholeperiod"))
cluster_settings$seed<-seq(1,nrow(cluster_settings))*30
cluster_settings<-cluster_settings%>%
  filter(func=="counterfactuals",
         method%in%c("rf","xgboost"))

#function: counterfactual plots ########################
get_att<-function(data,indices,iris_initial,method,validation1_valeven,validation1_valodd){
  print(indices)
  
  d_resampled<-data[indices,]
  
  iris<-d_resampled%>%
    left_join(iris_initial)%>%
    select(-c(group,odd))

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
    
  if(method=="xgboost"){
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
  }else if(method=="rf"){
    print(colnames(training_valeven))
    print(colnames(validation1_valeven))
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
  }
  
  year<-as.numeric(as.character(year))
  if(year<2000){
    year<-year+2000
  }
  validation<-validation1_valeven%>%
    rbind(validation1_valodd)
  
  att<-(validation%>%
          filter(year==T,post_cutoff==1)%>%
          summarise(Y1=mean(Y1),Y0=mean(Y0))%>%
          mutate(ATT=Y1-Y0))$ATT
  print(att)
  return(att)
}
#

#run functions #################
cluster_method<-cluster_settings$method[i]
cluster_function<-cluster_settings$func[i]
cluster_state<-cluster_settings$State.Name[i]
cluster_pollutant<-cluster_settings$pollutant[i]
cluster_year<-cluster_settings$year[i]
cluster_nsamples<-cluster_settings$nsamples[i]

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
  
    state<-dates$State.Name
    data<-filter(data,State.Name==state)
    data$State.Name<-as.factor(data$State.Name)
    
    data<-filter(data,Date_2020>=dates$start_date,Date_2020<dates$end_date)
    
    data$post_cutoff<-ifelse(data$Date_2020>=dates$cutoff_date,T,F)
    
    data<-rename(data,Y=NO2)
    
    year<-dates$year
    
    if(!year%in%c("20","2020")){
      data<-filter(data,!year%in%c("20","2020") | Time<dates$cutoff_date)
    }
    
    data$year<-ifelse(data$year==year,T,F)
    
    data$interaction<-data$post_cutoff * data$year 
    
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
    
    #stratify data
    iris_initial<-iris%>%
      na.omit()
      d_tosample<-iris_initial%>%
        select(Time,year,post_cutoff)%>%unique()%>%
        mutate(odd=ifelse(day(Time)%%2==1,1,0))%>%
        mutate(group=case_when(
          year==F & post_cutoff==0~1,
          year==F & post_cutoff==1~2,
          year==T & post_cutoff==0~3,
          year==T & post_cutoff==1 & odd==0~4,
          year==T & post_cutoff==1 & odd==1~5
        ))

    #perform hourly bootstrap
    set.seed(cluster_settings$seed[i])
    bootstrap_results<-boot(data=d_tosample, statistic=get_att,
         R=cluster_nsamples, strata = d_tosample$group,iris_initial=iris_initial,method=cluster_method,validation1_valeven=validation1_valeven,validation1_valodd=validation1_valodd)#
    
    metrics<-data.frame(
      method=cluster_method,
      State.Name=cluster_state,
      year=cluster_year,
      nsamples=cluster_nsamples,
      ATT=bootstrap_results$t0,
      lower=quantile(bootstrap_results$t,0.025),
      upper=quantile(bootstrap_results$t,0.975)
    )%>%
      mutate(significant=(lower>0 | upper<0))
    att_vec<-bootstrap_results$t
    
    save(metrics,att_vec,
           file = paste0("results/NB_2models_constant_validation_set_hourly/",cluster_state,"_",cluster_function,"_",
                         cluster_pollutant,"_",cluster_method,"_",cluster_year,"_",cluster_nsamples,"_results",".RData"))
