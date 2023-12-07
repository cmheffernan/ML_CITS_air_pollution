library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(ggpattern)
library(dtw)
library(splines)

#load ML ##############
L<-list.files("results/ML_models_counterfactuals/")
L<-L[!str_detect(L,"wholeperiod")]

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

#load linear #####
L_subset<-list.files("results/linear_models_counterfactuals/")

methods<-as.character(purrr::map(strsplit(L_subset,c("NO2_")),2))
methods<-substr(methods,1,nchar(methods)-6)

for(j in seq(1,length(L_subset))){
  print(j)
  load(paste0("results/linear_models_counterfactuals/",L_subset[j]))
  all_validation$method<-methods[j]
  df<-bind_rows(df,all_validation)
  df_metrics<-bind_rows(
    df_metrics,
    dates%>%
      mutate(method=methods[j])%>%
      rename(p_model=pvalue_model)%>%
      select(method,State.Name,year,p_model,r2_train,r2_test))
}

df_metrics$significant_model<-ifelse(df_metrics$p_model<0.05,"significant","not significant")
df_metrics$significant_bart<-ifelse(df_metrics$lower>0 | df_metrics$upper<0,"significant","not significant")
df_metrics$significant_bart_sd<-ifelse(df_metrics$lower_sd>0 | df_metrics$upper_sd<0,"significant","not significant")

#linear and ML ATT #####
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

save(att,file = "results/linear_ML_significance_levels.RData")
