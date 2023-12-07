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
L<-list.files(paste0("results/ML_models_counterfactuals/"))
L<-L[str_detect(L,"bart") & (!str_detect(L,"wholeperiod"))]
methods<-as.character(purrr::map(strsplit(L,c("_")),4))
states<-as.character(purrr::map(strsplit(L,c("_")),1))
years<-as.numeric(purrr::map(strsplit(L,c("_")),5))

name<-load(paste0("results/ML_models_counterfactuals/",L[1]))
dfs_validation$method<-methods[1]
dfs_validation$State.Name<-states[1]
dfs_validation$year<-years[1]
df<-dfs_validation

df_metrics<-data.frame(method=methods[1],State.Name=states[1],year=years[1],
                       r2_train=r2_train,r2_test=r2_test)
if("df_bart_intervals" %in% name){
  df_metrics<-df_metrics%>%
    left_join(df_bart_intervals%>%
                rename(ATT_bart=ATT,State.Name=state))
}

for(j in seq(2,length(L))){
  print(j)
  name<-load(paste0("results/ML_models_counterfactuals/",L[j]))
  dfs_validation$method<-methods[j]
  dfs_validation$State.Name<-states[j]
  dfs_validation$year<-years[j]
  df<-bind_rows(df,dfs_validation)
  
  metrics_row<-data.frame(method=methods[j],State.Name=states[j],year=years[j],
                          r2_train=r2_train,r2_test=r2_test)
  if("df_bart_intervals" %in% name){
    metrics_row<-metrics_row%>%
      left_join(df_bart_intervals%>%
                  rename(ATT_bart=ATT,State.Name=state))
  }
  df_metrics<-bind_rows(df_metrics,metrics_row)
}
df_ML<-df

colnames(df_metrics)

if(daily_average){
  df<-df%>%
    rename(Time=Date_2020)
}

#calculate ATTs
att<-df%>%
  filter(post_cutoff==1)%>%
  select(method,State.Name,year,Time,Y0,Y1,day_index)%>%
  group_by(method,State.Name,year,Time,day_index)%>%
  summarise(Y0=mean(Y0,na.rm=T),
            Y1=mean(Y1,na.rm=T))%>%
  ungroup()%>%
  group_by(State.Name,method,year)%>%
  summarize(Y1=mean(Y1,na.rm=T),
            Y0=mean(Y0,na.rm=T))%>%
  mutate(ATT=Y1-Y0,percent_diff=100*(Y1-Y0)/Y0)%>%
  ungroup()%>%
  left_join(df_metrics%>%
              rename(percent_diff_bart=percent_diff))

df_ATT<-att%>%
  mutate(city=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  mutate(name_shortened=factor(case_when(
    method=="boosting"~"GBM",
    method=="linear_no_int"~"LC",
    method=="linear"~"LTV",
    method=="dppc"~"DPPC",
    method=="rf"~"RF",
    method=="dib"~"DIB",
    method=="dd"~"DiD",
    method=="xgboost"~"GB",
    method=="bart"~"BART"
  ),
  levels = c("DPPC","DIB","DiD","LC","LTV","GBM","GB","RF","BART"),
  ordered = T))%>%
  filter(!name_shortened=="GBM")

#figure
fig<-df_ATT%>%
  mutate(significant=ifelse(lower>0 | upper<0,T,F))%>%
  ggplot(aes(x=year,y=ATT,col=significant))+
  geom_point()+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  facet_grid(.~city)+
  geom_hline(yintercept = 0)+
  theme_bw()+
  ylab("ATT (ppb)")+
  ggtitle(paste0("95% interval from BART model "))+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  ylim(c(-9,5.5))
fig
ggsave(paste0("figures/pdfs/S26_bart_intervals.pdf"),fig,width = 8,height = 2)
ggsave(paste0("figures/pngs/S26_bart_intervals.png"),fig,width = 8,height = 2)

