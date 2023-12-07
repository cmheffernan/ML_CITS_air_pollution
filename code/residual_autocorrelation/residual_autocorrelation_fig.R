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
L<-list.files("results/AR/")
methods<-as.character(purrr::map(str_split(L,"NO2_"),2))
methods<-as.character(purrr::map(str_split(methods,"_2020"),1))

all_orders<-data.frame()
for(i in 1:length(L)){
  load(paste0("results_sept2023/AR/",L[i]))
  all_orders<-bind_rows(all_orders,df_orders%>%mutate(method=methods[i],year=seq((2021-nrow(df_orders)),2020)))
}

#autoregressive order figure
fig<-all_orders%>%
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
  mutate(order=order/24)%>%
  ggplot(aes(x=year,y=order))+
  geom_point()+
  facet_grid(name_shortened~city,scales = "free_x")+
  theme_bw()+
  scale_y_continuous(minor_breaks = seq(0 , 6,by=0.5), breaks = seq(0, 6))+
  ggtitle("Order of autoregression of hourly residuals")+
  ylab("order (days)")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
fig
ggsave("figures/pdfs/S19_AR_residuals.pdf",fig,width = 8,height = 6)
ggsave("figures/pngs/S19_AR_residuals.png",fig,width = 8,height = 6)
