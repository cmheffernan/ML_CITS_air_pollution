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

#load results
load("results/linear_ML_significance_levels.RData")

df_ATT<-att%>%
  filter(method%in%c("boosting","xgboost"))%>%
  mutate(city=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  mutate(name_shortened=case_when(
    method=="boosting"~"GBM package",
    method=="xgboost"~"XGBoost package"
  ))


#load predictions
L<-list.files("results/ML_models_predict")
L<-L[as.character(purrr::map(strsplit(L,c("_")),3))=="predictwholeperiod"]
methods<-as.character(purrr::map(strsplit(L,c("_")),5))
L<-L[methods%in%c("boosting","xgboost")]

methods<-as.character(purrr::map(strsplit(L,c("_")),5))
states<-as.character(purrr::map(strsplit(L,c("_")),2))
years<-as.numeric(purrr::map(strsplit(L,c("_")),6))

load(paste0("results/ML_models_predict/",L[1]))
results_pred$method<-methods[1]
results_pred$State.Name<-states[1]
results_pred$year<-years[1]
df_predict<-results_pred
for(j in seq(2,length(L))){
  print(j)
  load(paste0("results/ML_models_predict/",L[j]))
  results_pred$method<-methods[j]
  results_pred$State.Name<-states[j]
  results_pred$year<-years[j]
  df_predict<-bind_rows(df_predict,results_pred)
}

L<-list.files("results/ML_models_counterfactuals")
func<-as.character(purrr::map(strsplit(L,c("_")),2))
L<-L[func=="counterfactualswholeperiod"]
years<-as.numeric(purrr::map(strsplit(L,c("_")),5))
methods<-as.character(purrr::map(strsplit(L,c("_")),4))
L<-L[years==2020 & methods%in%c("boosting","xgboost")]
years<-as.numeric(purrr::map(strsplit(L,c("_")),5))
methods<-as.character(purrr::map(strsplit(L,c("_")),4))
states<-as.character(purrr::map(strsplit(L,c("_")),1))

for(j in seq(1,length(L))){
  print(j)
  load(paste0("results/ML_models_counterfactuals/",L[j]))
  dfs_validation$method<-methods[j]
  dfs_validation$State.Name<-states[j]
  dfs_validation$year<-years[j]
  df_predict<-bind_rows(df_predict,dfs_validation%>%
                          rename(prediction=Y1)%>%
                          select(-Y0))
}

df_predict<-df_predict%>%
  mutate(city=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  mutate(name_shortened=case_when(
    method=="boosting"~"GBM package",
    method=="xgboost"~"XGBoost package"
  ))

  rmse_daily<-df_predict%>%
    mutate(day=as.POSIXct(paste0("2020-",month(Time),"-",day(Time))))%>%
    mutate(day=as.Date(day))%>%
    group_by(day,year,State.Name,method,city,name_shortened)%>%
    summarise(Y=mean(Y,na.rm=T),
              prediction=mean(prediction,na.rm=T))%>%
    ungroup()%>%
    group_by(year,State.Name,method,city,name_shortened)%>%
    summarise(rmse=sqrt(mean((prediction-Y)^2,na.rm=T))) 
  
#figure
  fig1<-df_ATT%>%
    left_join(rmse_daily%>%rename(RMSE=rmse))%>%
    select(city,name_shortened,year,ATT)%>%
    mutate(group=factor(ifelse(year==2020,"2020","baseline"),levels=c("baseline","2020"),ordered=T))%>%
    ggplot(aes(x=name_shortened,y=ATT))+
    geom_boxplot()+
    geom_hline(yintercept = 0)+
    facet_grid(.~group)+
    xlab("Method")+
    theme_bw()+
    ggtitle("Estimated ATT by GBM and XGBoost packages")
  fig1
  fig2<-df_ATT%>%
    left_join(rmse_daily%>%rename(RMSE=rmse))%>%
    select(city,name_shortened,year,RMSE)%>%
    pivot_wider(names_from = "name_shortened",values_from = "RMSE")%>%
    ggplot(aes(x=`XGBoost package`,y=`GBM package`))+
    geom_point()+
    # facet_wrap(vars(estimand),scales = "free")+
    geom_line(aes(y=`XGBoost package`),col="black")+
    theme_bw()+
    ggtitle("Out-of-sample RMSE of GBM and XGBoost packages")+
    theme(legend.position = "bottom",
          legend.title = element_blank())
  fig2
  fig<-ggarrange(fig1,fig2,nrow=1)
  fig
  ggsave("figures/pdfs/S6_GBM_v_XGB.pdf",fig,width = 10,height=4,dpi=600)
  ggsave("figures/pngs/S6_GBM_v_XGB.png",fig,width = 10,height=4,dpi=600)
  
  