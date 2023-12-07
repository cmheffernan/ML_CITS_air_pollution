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

cluster_settings<-expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                              year=2020,
                              method=c("rf","xgboost","linear","linear_no_int"),
                              func=c("counterfactuals"),
                              pollutant="NO2")
cluster_settings$seed<-seq(1,nrow(cluster_settings))*91

#function: AR order ########################
AR_orders<-function(all_ts_interpolated,method){
  state<-cluster_state
  ts_interpolated<-filter(all_ts_interpolated,State.Name==state)
  ts_interpolated$State.Name<-as.factor(ts_interpolated$State.Name)
  
  year<-cluster_year
  
  if(!year%in%c("20","2020")){
    ts_interpolated<-filter(ts_interpolated,!year(Time)%in%c("20","2020") | Time<cutoff_date[states==state])
  }
  ts_interpolated$year<-ifelse(year(ts_interpolated$Time)==year,T,F)
  ts_interpolated$interaction<-ts_interpolated$post_cutoff * ts_interpolated$year 
  
  if(state=="pennsylvania"){
    iris<-ts_interpolated%>%
      ungroup%>%
      select(day_index,year,post_cutoff,Y,Time,RH,Temp,WS,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }else if(state=="district of columbia"){
    iris<-ts_interpolated%>%
      ungroup%>%
      select(day_index,year,post_cutoff,Y,Time,RH,Temp,wind,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }else{
    iris<-ts_interpolated%>%
      ungroup%>%
      select(day_index,year,post_cutoff,Y,Time,RH,Temp,wind,pressure,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }
  
  training_valeven <- iris%>%
    ungroup()%>%
    na.omit()%>%
    select(-c(Time,year))%>%
    mutate(day_index=as.numeric(day_index),
           daylight=ifelse(daylight,1,0),
           weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
    as.data.frame()
  training_valeven_linear <- iris%>%
    ungroup()%>%
    na.omit()%>%
    select(-c(Time))%>%
    mutate(day_index=as.numeric(day_index),
           daylight=ifelse(daylight,1,0),
           weekend=ifelse(weekend,1,0),post_cutoff=ifelse(post_cutoff,1,0))%>%
    as.data.frame()
  
  if(state=="pennsylvania"){
    df<-ts_interpolated%>%
      ungroup%>%
      select(t,day_index,year,post_cutoff,Y,Time,RH,Temp,WS,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }else if(state=="district of columbia"){
    df<-ts_interpolated%>%
      ungroup%>%
      select(t,day_index,year,post_cutoff,Y,Time,RH,Temp,wind,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }else{
    df<-ts_interpolated%>%
      ungroup%>%
      select(t,day_index,year,post_cutoff,Y,Time,RH,Temp,wind,pressure,daylight,weekend,hod,theta,pblh,prate,lag1:lag7,monitor,interaction)
  }
  data<-df%>%
    na.omit()
  
  if(method=="rf"){
    rf_classifier = rfsrc(Y ~ ., data=training_valeven, ntree=100, mtry=2)
    
    data$fitted<-predict.rfsrc(rf_classifier,data)$predicted
    data$residual<-data$Y-data$fitted
  }else if(method=="xgboost"){
    m<-as.matrix(select(training_valeven,-Y))
    class(m)<-"numeric"
    gbm_classifier = xgboost(data=m,label = training_valeven$Y,
                             max_depth = 10, nthread = 2, nrounds = 10)
    
    m_i<-as.matrix(data%>%select(training_valeven%>%select(-Y)%>%colnames()))
    class(m_i)<-"numeric"
    data$fitted<-predict(gbm_classifier, m_i)
    data$residual<-data$Y-data$fitted
  }else if(method=="linear"){
    knots <- quantile(training_valeven_linear$day_index, p = seq(0.1,1,by=0.1),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+bs(day_index,knots=knots)*interaction+.,data=training_valeven_linear)
    
    data$fitted <- predict.lm(slope_model,data)
    data$residual<-data$Y-data$fitted
  }else if(method=="linear_no_int"){
    knots <- quantile(training_valeven_linear$day_index, p = c(0.25, 0.5, 0.75),na.rm=T)
    slope_model<-lm(Y~bs(day_index,knots=knots)+year+post_cutoff+day_index*year+bs(day_index,knots=knots)*post_cutoff+interaction+.,data=training_valeven_linear)
    
    data$fitted <- predict.lm(slope_model,data)
    data$residual<-data$Y-data$fitted
  }
  
  data_average<-data%>%
    group_by(Time,t)%>%
    summarise(residual_mean=mean(residual))
  data<-data%>%
    left_join(data_average)%>%
    mutate(difference=residual-residual_mean)
  
  #calculate order of autoregression 
  year_vec<-year(data_average$Time)%>%unique()
  df_orders<-data.frame()
  for(y in year_vec){
    print(y)
    t<-data_average%>%
      filter(year(Time)==y)%>%
      arrange(t)
    epsilon<-t$residual_mean
    
    fit<-ar(epsilon,aic = T,order.max = 24*7)
    
    o<-fit$order
    names(o)<-"order"
    c<-fit$ar
    names(c)<-paste0("C",1:length(c))
    df_orders<-df_orders%>%
      bind_rows(c(o,c)%>%as.data.frame()%>%t()%>%as.data.frame())%>%
      mutate(year=y,State.Name=state)
    
  }
  
  df_orders
}
#

for(i in seq(1,20)){
print(paste0("i=",i))
cluster_method<-cluster_settings$method[i]
cluster_function<-cluster_settings$func[i]
cluster_state<-cluster_settings$State.Name[i]
cluster_pollutant<-cluster_settings$pollutant[i]
cluster_year<-cluster_settings$year[i]

load("results/interpolated_data.RData")

states<-c("maryland","massachusetts","new york","pennsylvania","district of columbia")
cutoff_date<-c(as.Date("2020-03-05"),as.Date("2020-03-10"),
               as.Date("2020-03-07"),as.Date("2020-03-06"),
               as.Date("2020-03-11"))

set.seed(cluster_settings$seed[i])
df_orders<-AR_orders(all_ts_interpolated,method=cluster_method)

save(df_orders,
     file = paste0("results/AR/orders_",cluster_state,"_",cluster_function,"_",
                   cluster_pollutant,"_",cluster_method,"_",cluster_year,"_df",".RData"))

}
