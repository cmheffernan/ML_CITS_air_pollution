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

mobility<-read.csv("data/2020_US_Region_Mobility_Report.csv")

df_cutoffs<-data.frame(
  State.Name=c("Maryland","Massachusetts","New York","Pennsylvania","District of Columbia"),
  cutoff_date=c(as.Date("2020-03-05"),as.Date("2020-03-10"),
                 as.Date("2020-03-07"),as.Date("2020-03-06"),
                 as.Date("2020-03-11"))
)

df<-mobility%>%
  filter(sub_region_1%in%c("District of Columbia","Maryland","Massachusetts","New York","Pennsylvania"))%>%
  filter(sub_region_1=="District of Columbia" | 
           (sub_region_1=="Maryland" & sub_region_2=="Baltimore") | 
           (sub_region_1=="Massachusetts" & sub_region_2=="Suffolk County") | 
           (sub_region_1=="New York" & sub_region_2 %in%c("Bronx County","Kings County","New York County","Queens County","Richmond County")) | 
           (sub_region_1=="Pennsylvania" & sub_region_2=="Philadelphia County"))%>%
  mutate(date=as.POSIXct(date))%>%
  pivot_longer(retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline,names_to = "type",values_to = "change")%>%
  mutate(type=gsub("_percent_change_from_baseline","",type))%>%
  rename(State.Name=sub_region_1)%>%
  group_by(State.Name,type,date)%>%
  summarise(change=mean(change,na.rm=T))%>%
  left_join(df_cutoffs)%>%
  filter(date>=cutoff_date)

#mobility figure 
fig<-df%>%
  mutate(city=factor(case_when(
    State.Name=="District of Columbia"~"Washington DC",
    State.Name=="Maryland"~"Baltimore",
    State.Name=="Massachusetts"~"Boston",
    State.Name=="New York"~"New York City",
    State.Name=="Pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  filter(date<as.POSIXct("2020-08-01"))%>%
  filter(type%in%c("parks","workplaces","grocery_and_pharmacy"))%>%
  mutate(type=ifelse(type=="grocery_and_pharmacy","grocery/pharmacy",type))%>%
  ggplot(aes(x=date,y=change))+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_grid(type~city,scales = "free")+
  theme_bw()+
  ylab("percent change since baseline")+
  ggtitle("Mobility trends after the start of the lockdown")+
  theme(axis.text.x = element_text(angle=45,hjust=1))
fig
ggsave("figures/pdfs/S5_mobility.pdf",fig,width = 8,height = 5)
ggsave("figures/pngs/S5_mobility.png",fig,width = 8,height = 5)
