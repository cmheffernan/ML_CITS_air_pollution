library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(ggpattern)
library(dtw)
library(splines)

#load ML ##############
L<-list.files("results_sept2023/ML_models_counterfactuals/")
L<-L[str_detect(L,"wholeperiod")]

methods<-as.character(purrr::map(strsplit(L,c("_")),4))
states<-as.character(purrr::map(strsplit(L,c("_")),1))
years<-as.numeric(purrr::map(strsplit(L,c("_")),5))

name<-load(paste0("results_sept2023/ML_models_counterfactuals/",L[1]))
dfs_validation$method<-methods[1]
dfs_validation$State.Name<-states[1]
dfs_validation$year<-years[1]
df<-dfs_validation

df_metrics<-data.frame(method=methods[1],State.Name=states[1],year=years[1],
                       p_noerror=p,p_werror=p_werror,p_werror_averaged=p_werror_averaged,p_werror_daily=p_werror_daily,
                       r2_train=r2_train,r2_test=r2_test)
# r2_train=ifelse("r2_train"%in%name,r2_train,NA),r2_test=ifelse("r2_test"%in%name,r2_test,NA))
if("df_bart_intervals" %in% name){
  df_metrics<-df_metrics%>%
    left_join(df_bart_intervals%>%
                rename(ATT_bart=ATT,State.Name=state))
}

for(j in seq(2,length(L))){
  print(j)
  name<-load(paste0("results_sept2023/ML_models_counterfactuals/",L[j]))
  dfs_validation$method<-methods[j]
  dfs_validation$State.Name<-states[j]
  dfs_validation$year<-years[j]
  df<-bind_rows(df,dfs_validation)
  
  metrics_row<-data.frame(method=methods[j],State.Name=states[j],year=years[j],
                          p_noerror=p,p_werror=p_werror,p_werror_averaged=p_werror_averaged,p_werror_daily=p_werror_daily,
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
L_subset<-list.files("results_sept2023/linear_models_predict/")
L_subset<-L_subset[str_detect(L_subset,"predictwholeperiod2020")]

methods<-as.character(purrr::map(strsplit(L_subset,c("NO2_")),2))
methods<-substr(methods,1,nchar(methods)-6)

for(j in seq(1,length(L_subset))){
  print(j)
  load(paste0("results_sept2023/linear_models_predict/",L_subset[j]))
  all_validation$method<-methods[j]
  df<-bind_rows(df,all_validation%>%rename(Y1=prediction))
}

#residuals ###########
d<-df%>%
  # filter(method%in%c("linear","linear_no_int"))%>%
  filter(year==2020)%>%
  mutate(month=month(Time),day=day(Time))%>%
  mutate(Time_2020=as.POSIXct(paste0("2020-",month,"-",day," ",hod,":00:00")))%>%
  mutate(true=Y,residual=Y1-true)%>%
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

city_list<-c("Boston","New York City","Philadelphia","Baltimore","Washington DC")
for(l in seq(1,5)){
  city_l<-city_list[l]
  
  fig<-d%>%
    filter(!method=="bart")%>%
    filter(city==city_l)%>%
    mutate(Month=factor(case_when(
      month==1~"Jan",
      month==2~"Feb",
      month==3~"Mar",
      month%in%c(4,5)~"Apr-May"
    ),levels=c("Jan","Feb","Mar","Apr-May"),ordered = T))%>%
    mutate(date=date(Time_2020))%>%
    group_by(date,Month,name_shortened,city)%>%
    summarise(true=mean(true,na.rm = T),
              residual=mean(residual,na.rm = T))%>%
    ggplot(aes(x=date,y=residual))+
    geom_point(alpha=1)+
    # geom_jitter(width=0.1)+
    geom_hline(yintercept = 0)+
    facet_grid(name_shortened~.)+
    ggtitle(paste0(city_l, " daily residuals in 2020"))+
    theme_bw()
  fig
  ggsave(paste0("figures_sept2023/final pdfs/S",l+13,"_residuals_ts_",city_l,"_daily.pdf"),fig,width = 7.5,height=6,dpi=600)
  ggsave(paste0("figures_sept2023/pngs/S",l+13,"_residuals_ts_",city_l,"_daily.png"),fig,width = 7.5,height=6,dpi=600)
  
}


