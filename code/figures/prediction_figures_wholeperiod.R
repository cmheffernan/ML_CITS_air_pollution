library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(ggpattern)

#load results #######################
L<-list.files("results/ML_models_predict")
L<-L[as.character(purrr::map(strsplit(L,c("_")),3))=="predictwholeperiod"]
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

L<-list.files("results/linear_models_predict")
func<-as.character(purrr::map(strsplit(L,c("_")),3))
L<-L[(func=="predictwholeperiod")]

for(j in seq(1,length(L))){
  print(j)
  load(paste0("results/linear_models_predict/",L[j]))
  df_predict<-bind_rows(df_predict,all_validation)
}

L<-list.files("results/ML_models_counterfactuals")
func<-as.character(purrr::map(strsplit(L,c("_")),2))
L<-L[func=="counterfactualswholeperiod"]
years<-as.numeric(purrr::map(strsplit(L,c("_")),5))
L<-L[years==2020]
years<-years[years==2020]
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

L<-list.files("results/linear_models_predict")
type<-as.character(purrr::map(strsplit(L,c("_")),1))
func<-as.character(purrr::map(strsplit(L,c("_")),3))
L<-L[type=="dataframe" & func=="predictwholeperiod2020"]

for(j in seq(1,length(L))){
  print(j)
  load(paste0("results/linear_models_predict/",L[j]))
  df_predict<-bind_rows(df_predict,all_validation)
}

df_predict<-df_predict%>%
  bind_rows(df_predict%>%
              filter(method=="linear",year<2020)%>%
              mutate(method="linear_no_int"))

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

#make figs #########
city_list<-c("Boston","New York City","Philadelphia","Baltimore","Washington DC")
for(l in seq(1,5)){
city_l<-city_list[l]
df_restricted<-df_predict%>%
  filter(!method=="bart")%>%
  filter(city==city_l)%>%
  mutate(day=as.POSIXct(paste0("2020-",month(Time),"-",day(Time))))%>%
  mutate(day=as.Date(day))%>%
  group_by(day,year,State.Name,method,city,name_shortened)%>%
  summarise(true=mean(Y,na.rm=T),
            prediction=mean(prediction,na.rm=T))
rmse_daily<-df_predict%>%
  filter(!method=="bart")%>%
  filter(city==city_l)%>%
  mutate(day=as.POSIXct(paste0("2020-",month(Time),"-",day(Time))))%>%
  mutate(day=as.Date(day))%>%
  group_by(day,year,State.Name,method,city,name_shortened)%>%
  summarise(Y=mean(Y,na.rm=T),
            prediction=mean(prediction,na.rm=T))%>%
  ungroup()%>%
  group_by(year,State.Name,method,city,name_shortened)%>%
  summarise(rmse=sqrt(mean((prediction-Y)^2,na.rm=T))) 
fig<-df_restricted%>%
  pivot_longer(cols=c(true,prediction),names_to="series",values_to="value")%>%
  ggplot(mapping = aes(x=day,y=value,col=series))+
  geom_line(size=0.5)+
  facet_grid(name_shortened~year)+
  geom_hline(yintercept = 0)+
  geom_text(aes(label=paste0("RMSE=",round(rmse,1)),x=as.Date("2020-03-25")),y=max(df_restricted$true)*0.8,col="black",size=2.5,data=rmse_daily)+
  ggtitle(paste0(city_l, " daily predictions"))+
  xlab("Date")+
  ylab(expression("NO"[2]~(ppb)))+
  labs(col="")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=30,hjust=1),
        axis.title.y = element_text(angle=270))
ggsave(paste0("figures/pdfs/S",(l+6),"_ts_pred_",city_l,".pdf"),fig,width = 10,height=6,dpi=600)
ggsave(paste0("figures/pngs/S",(l+6),"_ts_pred_",city_l,".png"),fig,width = 10,height=6,dpi=600)

}
