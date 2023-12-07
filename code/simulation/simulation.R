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

k=as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

#settings matrix
settings<-expand.grid(dataset=1:100,
                      ATT_goal=c(-10,0),
                      model=c("linear","nonlinear"),
                      methods="8methods")

simulation_num<-settings$dataset[k]
ATT_goal<- settings$ATT_goal[k]
model_k<-settings$model[k]
methods_set<-settings$methods[k]

df_att<-data.frame()
df_validation_all<-data.frame()

methods_vec<-c("dppc","dib","dd","linear","linear_no_int","rf","xgboost")

  print(simulation_num)
  
  #simulate data
  
  beta_RH70<-4
  beta_Temp<-1/20
  power_Temp<-1
  beta_wind<-0.05
  tau_RH<-18
  tau_Temp<-2
  
  set.seed(2352*simulation_num*(101^(abs(sign(ATT_goal)))))
  n_points<-24*7*8
  data<-data.frame(
    time=rep(-n_points:(n_points-1),12),
    year=rep(c(rep(T,4),rep(F,8)),each=2*n_points),
    year_num=rep(1:12,each=2*n_points),
    day_number=rep(1:(2*n_points/24),each=24)
  )%>%
    mutate(post_cutoff=ifelse(time>=0,1,0),
           interaction=post_cutoff*year,
           wind=runif(12*(2*n_points),0,20),
           RH=0.00003*time^2+0.03*time+30+tau_RH*rnorm(12*(2*n_points)),
           Temp=5*(year==T)+(time)/(2*n_points+1)*(40*year+30*(1-year))+50+tau_Temp*rnorm(12*(2*n_points)))#%>%
  
  if(model_k=="linear"){
    data<-data%>%
      mutate(EY0=20-0.02*time-5*post_cutoff+0.3*(RH-50)-0.5*(Temp-50)+1*(wind-10))%>%
      mutate(EY1=EY0-5)
  }else{
    data<-data%>%
      mutate(EY0=20-(time*3/n_points)^3-5*post_cutoff+0.3*(RH-50)-0.5*(Temp-50)-5*(Temp>60)+0.3*(wind-10)^2)%>%
      mutate(EY1=EY0-5-time/500+beta_RH70*(RH>70)-beta_Temp*Temp^power_Temp-beta_wind*(wind-7)^2)
  } 
  if(ATT_goal==0){ 
    data$EY1<-data$EY0
  }
  
  #ensure correct true ATT
  validation<-data%>%
    filter(interaction==1)
  ATT_true<-validation%>%
    mutate(ATT_true=EY1-EY0)%>%
    summarise(ATT_true=mean(ATT_true))%>%
    as.numeric()
  data<-data%>%
    mutate(EY1=EY1+(ATT_goal-ATT_true))%>%
    mutate(Y=ifelse(interaction,EY1,EY0)+5*rnorm(12*(2*n_points)))
  
  validation<-data%>%
    filter(interaction==1)
  ATT_true<-validation%>%
    mutate(ATT_true=EY1-EY0)%>%
    summarise(ATT_true=mean(ATT_true))%>%
    as.numeric()
  validation_even<-validation%>%
    filter(day_number%%2==0)
  validation_odd<-validation%>%
    filter(day_number%%2==1)
  
  #fit methods
  for(method in methods_vec){
    print(method)
    if(method=="dppc"){
      d<-data%>%
        filter(year==T)%>%
        group_by(post_cutoff)%>%
        summarise(mean=mean(Y,na.rm=T))%>%
        pivot_wider(names_from = post_cutoff,values_from = mean)%>%
        mutate(ATT=`1`-`0`)
    }else if(method=="dib"){
      d<-data%>%
        filter(post_cutoff==1)%>%
        group_by(year)%>%
        summarise(mean=mean(Y,na.rm=T))%>%
        pivot_wider(names_from = year,values_from = mean)%>%
        mutate(ATT=`TRUE`-`FALSE`)
    }else if(method=="dd"){
      d<-data%>%
        group_by(year,post_cutoff)%>%
        summarise(mean=mean(Y,na.rm=T))%>%
        pivot_wider(names_from = year,values_from = mean)%>%
        mutate(ATT=`TRUE`-`FALSE`)%>%
        select(post_cutoff,ATT)%>%
        pivot_wider(names_from = post_cutoff,values_from = ATT)%>%
        mutate(ATT=`1`-`0`)
    }else if(method%in%c("linear","linear_no_int","linear_no_splines")){
      knots <- quantile(data$time, p = c(0.25, 0.5, 0.75),na.rm=T)
      if(method=="linear"){
        slope_model<-lm(Y~bs(time,knots=knots)+year+post_cutoff+time*year+bs(time,knots=knots)*post_cutoff+interaction+bs(time,knots=knots)*interaction+RH+Temp+wind,
                        data=data)
      }else if(method=="linear_no_int"){
        slope_model<-lm(Y~bs(time,knots=knots)+year+post_cutoff+time*year+bs(time,knots=knots)*post_cutoff+interaction+RH+Temp+wind,
                        data=data)
      }
      validation$Y1<-predict(slope_model,validation%>%mutate(interaction=1))
      validation$Y0<-predict(slope_model,validation%>%mutate(interaction=0))
      
    }else if(method=="rf"){
      set.seed(2353*simulation_num*(101^(abs(sign(ATT_goal)))))
      rf_classifier = rfsrc(Y ~ time+post_cutoff+interaction+RH+Temp+wind, 
                            data=data%>%filter(!(year==T&post_cutoff==1&day_number%%2==0)), ntree=100, mtry=2)
      validation_even$Y1<-predict.rfsrc(rf_classifier,validation_even%>%mutate(interaction=1))$predicted
      validation_even$Y0<-predict.rfsrc(rf_classifier,validation_even%>%mutate(interaction=0))$predicted
      
      rf_classifier = rfsrc(Y ~ time+post_cutoff+interaction+RH+Temp+wind, 
                            data=data%>%filter(!(year==T&post_cutoff==1&day_number%%2==1)), ntree=100, mtry=2)
      validation_odd$Y1<-predict.rfsrc(rf_classifier,validation_odd%>%mutate(interaction=1))$predicted
      validation_odd$Y0<-predict.rfsrc(rf_classifier,validation_odd%>%mutate(interaction=0))$predicted
      
      validation<-validation%>%
        left_join(bind_rows(validation_even,validation_odd))
      
    }else if(method=="xgboost"){
      set.seed(2354*simulation_num*(101^(abs(sign(ATT_goal)))))
      m<-as.matrix(data%>%filter(!(year==T&post_cutoff==1&day_number%%2==0))%>%select(time,post_cutoff,interaction,RH,Temp,wind))
      class(m)<-"numeric"
      gbm_classifier = xgboost(data=m,label = (data%>%filter(!(year==T&post_cutoff==1&day_number%%2==0)))$Y,
                               max_depth = 10, nthread = 2, nrounds = 10)
      m1<-as.matrix(validation_even%>%mutate(interaction=1)%>%select(m%>%colnames()))
      class(m1)<-"numeric"
      validation_even$Y1 <- predict(gbm_classifier, m1)
      m0<-as.matrix(validation_even%>%mutate(interaction=0)%>%select(m%>%colnames()))
      class(m0)<-"numeric"
      validation_even$Y0 <- predict(gbm_classifier, m0)
      
      m<-as.matrix(data%>%filter(!(year==T&post_cutoff==1&day_number%%2==1))%>%select(time,post_cutoff,interaction,RH,Temp,wind))
      class(m)<-"numeric"
      gbm_classifier = xgboost(data=m,label = (data%>%filter(!(year==T&post_cutoff==1&day_number%%2==1)))$Y,
                               max_depth = 10, nthread = 2, nrounds = 10)
      m1<-as.matrix(validation_odd%>%mutate(interaction=1)%>%select(m%>%colnames()))
      class(m1)<-"numeric"
      validation_odd$Y1 <- predict(gbm_classifier, m1)
      m0<-as.matrix(validation_odd%>%mutate(interaction=0)%>%select(m%>%colnames()))
      class(m0)<-"numeric"
      validation_odd$Y0 <- predict(gbm_classifier, m0)
      
      validation<-validation%>%
        left_join(bind_rows(validation_even,validation_odd))
      
    }
    
    #summarize method fits ############
    if(method%in%c("dppc","dib","dd")){
      new_row<-data.frame(ATT=d$ATT,
                          ATT_true=ATT_true,
                          method=method,
                          simulation_num=simulation_num)
    }else{
      dataframe<-validation%>%
        group_by(day_number,year_num)%>%
        summarise(Y1=mean(Y1),Y0=mean(Y0))
      
      new_row<-validation%>%
        mutate(ATT=Y1-Y0)%>%
        summarise(ATT=mean(ATT))%>%
        mutate(ATT_true=ATT_true,
               method=method,
               simulation_num=simulation_num)%>%
        bind_cols(validation%>%
                    summarise(RMSE_1=sqrt(mean((Y1-EY1)^2)),
                              RMSE_0=sqrt(mean((Y0-EY0)^2)),
                              bias_1=mean(Y1-EY1),
                              bias_0=mean(Y0-EY0),
                              Y1bar=mean(Y1),
                              Y0bar=mean(Y0),
                              EY1bar=mean(EY1),
                              EY0bar=mean(EY0)))
      
      validation<-validation%>%
        select(-c(Y1,Y0))
    }
    
    df_att<-bind_rows(
      df_att,
      new_row
    )
    df_validation_all<-bind_rows(
      df_validation_all,
      validation%>%
        mutate(method=method,
               simulation_num=simulation_num,
               ATT_true=ATT_true)
    )
  }
  
save(df_att,df_validation_all,file =
       paste0("results/sim/results_",
              "_trueATT_",ATT_goal,
              "_sim_",simulation_num,
              "_model_",model_k,
              "_methodsset_",methods_set,"_dfs.RData"))
