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

load("data/all_data_5_cities_interpolated_by_monitor.RData")

d<-data_no2_by_monitor%>%
  filter(month(Time)%in%c(1,2,3,4,5))%>%
  filter(State.Name=="pennsylvania")%>%
  mutate(Date=date(Time))%>%
  mutate(year=year(Date))%>%
  filter(year>=2015,year<=2020)%>%
  filter(!round(Latitude,3)%in%c(39.945,39.991))%>%
  select(Latitude,Longitude)%>%
  unique()
library(ggmap)
mdmap = ggmap(get_stamenmap(bbox=c(left=-75.274491,bottom=39.852747,right=-74.97,top=40.1),zoom=12))
fig<-mdmap+
  geom_point(data=d,aes(x=Longitude,y=Latitude,color=as.factor(Latitude)),size=5)+
  theme(legend.position = "none")+
  ggtitle("Philadelphia monitor locations")+
  xlab("Longitude")+ylab("Latitude")+
  theme(title = element_text(size=10))
fig1<-fig

#figure of philly monitors
fig<-data_no2_by_monitor%>%
  filter(month(Time)%in%c(1,2,3,4,5))%>%
  filter(State.Name=="pennsylvania")%>%
  mutate(Date=date(Time))%>%
  group_by(Date,Latitude,Longitude,State.Name)%>%
  summarise(NO2=mean(NO2,na.rm=T))%>%
  mutate(year=year(Date))%>%
  filter(year>=2015,year<=2020)%>%
  mutate(Date2020=as.POSIXct(paste0("2020-",month(Date),"-",day(Date))))%>%
  ungroup()%>%
  mutate(NO2=pmin(NO2,40))%>%
  filter(!round(Latitude,3)%in%c(39.945,39.991))%>%
  ggplot(aes(x=Date2020,y=NO2,col=as.factor(Latitude)))+
  geom_line()+
  facet_wrap(vars(year))+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle("Philadelphia time series by monitor")+
  xlab("Date")+
  ylab(expression("NO"[2]~(ppb)))+
  ylim(c(0,40))+
  theme(title = element_text(size=10))

fig<-ggarrange(fig1,fig,nrow=1,widths = c(1,1.5))
ggsave("figures/pdfs/S24_philly.pdf",fig,width = 7.5,height=3,dpi=600)
ggsave("figures/pngs/S24_philly.png",fig,width = 7.5,height=3,dpi=600)
