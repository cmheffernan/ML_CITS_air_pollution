library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(ggpattern)

load("data/all_data_5_cities_interpolated_by_monitor.RData")

#summary table #############
data_2020<-data_no2_by_monitor%>%
  filter(Date_2020<=as.POSIXct("2020-05-07"))%>%
  filter(year==2020,month%in%c(1,2,3,4,5))

tab<-data_2020%>%
  mutate(wind=ifelse(State.Name=="Pennsylvania",WS,wind))%>%
  mutate(State.Name=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  mutate(direction2=factor(case_when(
    theta<(-pi/4) & theta>=(-3*pi/4)~"S",
    theta<(pi/4) & theta>=(-pi/4)~"E",
    theta<(3*pi/4) & theta>=(pi/4)~"N",
    theta<(-3*pi/4) | theta>=(3*pi/4)~"W"
  ),levels=c("W","SW","S","SE","E","NE","N","NW")))%>%
  group_by(State.Name)%>%
  summarise(NO2=signif(mean(NO2,na.rm=T),3),
            RH=signif(mean(RH,na.rm=T),3),
            Temp=signif(mean(Temp,na.rm=T),3),
            pressure=signif(mean(pressure,na.rm=T),3),
            wind=signif(mean(wind,na.rm=T),3),
            pblh=signif(mean(pblh,na.rm=T),3),
            prate=signif(mean(prate,na.rm=T),3))%>%
  t()%>%
  as.data.frame()
tab<-cbind(variable=rownames(tab),tab)  
colnames(tab)<-tab[1,]
rownames(tab)<-seq(1,nrow(tab))
tab<-tab[-1,]
tab

tab2<-data_2020%>%
  mutate(wind=ifelse(State.Name=="Pennsylvania",WS,wind))%>%
  mutate(State.Name=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  mutate(direction=factor(case_when(
    theta<(-pi/4) & theta>=(-3*pi/4)~"S",
    theta<(pi/4) & theta>=(-pi/4)~"E",
    theta<(3*pi/4) & theta>=(pi/4)~"N",
    theta<(-3*pi/4) | theta>=(3*pi/4)~"W"
  ),levels=c("W","SW","S","SE","E","NE","N","NW")))%>%
  group_by(State.Name,direction)%>%
  dplyr::count()%>%
  left_join(data_2020%>%
              mutate(State.Name=factor(case_when(
                State.Name=="district of columbia"~"Washington DC",
                State.Name=="maryland"~"Baltimore",
                State.Name=="massachusetts"~"Boston",
                State.Name=="new york"~"New York City",
                State.Name=="pennsylvania"~"Philadelphia"
              ),
              levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
              ordered = T))%>%
              group_by(State.Name)%>%
              dplyr::count()%>%
              rename(total=n))%>%
  mutate(percent=signif(100*n/total,3))%>%
  select(State.Name,percent,direction)%>%
  pivot_wider(names_from = "State.Name",values_from = "percent")%>%
  rename(State.Name=direction)
tab2

tab_sd<-data_2020%>%
  mutate(wind=ifelse(State.Name=="Pennsylvania",WS,wind))%>%
  mutate(State.Name=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  mutate(direction2=factor(case_when(
    theta<(-pi/4) & theta>=(-3*pi/4)~"S",
    theta<(pi/4) & theta>=(-pi/4)~"E",
    theta<(3*pi/4) & theta>=(pi/4)~"N",
    theta<(-3*pi/4) | theta>=(3*pi/4)~"W"
  ),levels=c("W","SW","S","SE","E","NE","N","NW")))%>%
  group_by(State.Name)%>%
  summarise(NO2=signif(sd(NO2,na.rm=T),3),
            RH=signif(sd(RH,na.rm=T),3),
            Temp=signif(sd(Temp,na.rm=T),3),
            pressure=signif(sd(pressure,na.rm=T),3),
            wind=signif(sd(wind,na.rm=T),3),
            pblh=signif(sd(pblh,na.rm=T),3),
            prate=signif(sd(prate,na.rm=T),3))%>%
  t()%>%
  as.data.frame()
tab_sd<-cbind(variable=rownames(tab_sd),tab_sd)  
colnames(tab_sd)<-tab_sd[1,]
rownames(tab_sd)<-seq(1,nrow(tab_sd))
tab_sd<-tab_sd[-1,]
tab_sd

tab_final<-matrix(NA,nrow=nrow(tab),ncol=ncol(tab))%>%
  as.data.frame()
tab_final[,1]<-tab[,1]
rownames(tab_final)<-rownames(tab)
colnames(tab_final)<-colnames(tab)
for(i in seq(1,nrow(tab_final))){
  for(j in seq(2,ncol(tab_final))){
    tab_final[i,j]<-paste0(tab[i,j]," (",tab_sd[i,j],")")
  }
}
tab_final
write.csv(rbind(tab_final,tab2),file = "figures/pdfs/ST1_table1.csv",row.names = F)


#time series ############
df_cutoff_posixct<-data.frame(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                      cutoff_date=c(as.POSIXct("2020-03-05"),as.POSIXct("2020-03-10"),
                                    as.POSIXct("2020-03-07"),as.POSIXct("2020-03-06"),
                                    as.POSIXct("2020-03-11")))%>%
  mutate(name=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  left_join(expand.grid(State.Name=c("maryland","massachusetts","new york","pennsylvania","district of columbia"),
                        year=2013:2019)%>%
              filter(!(year==2013 & State.Name=="massachusetts"),
                     !(year==2013 & State.Name=="pennsylvania"),
                     !(year==2014 & State.Name=="pennsylvania")))

fig<-data_no2_by_monitor%>%
  filter(!year==2012)%>%
  filter(Date_2020<=as.POSIXct("2020-05-07"))%>%
  group_by(Date_2020,year,State.Name)%>%
  summarise(NO2=mean(NO2,na.rm=T))%>%
  mutate(name=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  filter(!year==2020)%>%
  mutate(series="baseline year")%>%
  bind_rows(data_no2_by_monitor%>%
              filter(year==2020)%>%
              filter(Date_2020<=as.POSIXct("2020-05-07"))%>%
              group_by(Date_2020,year,State.Name)%>%
              summarise(NO2=mean(NO2,na.rm=T))%>%
              mutate(name=factor(case_when(
                State.Name=="district of columbia"~"Washington DC",
                State.Name=="maryland"~"Baltimore",
                State.Name=="massachusetts"~"Boston",
                State.Name=="new york"~"New York City",
                State.Name=="pennsylvania"~"Philadelphia"
              ),
              levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
              ordered = T))%>%
              ungroup()%>%
              select(-year)%>%
              mutate(series="2020")%>%
              left_join(expand.grid(series="2020",year=2013:2019)))%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))%>%
  ggplot(aes(x=Date_2020,y=NO2,col=series,linetype=series))+
  geom_line()+
  facet_grid(year~name)+
  geom_vline(data=df_cutoff_posixct,aes(xintercept = cutoff_date),col="black")+
  ggtitle(expression(Daily~time~series~of~NO[2]~concentrations))+
  xlab("Date")+
  ylab(expression("NO"[2]~(ppb)))+
  # labs(col="Year")+
  scale_linetype_manual(values = c("2020"="dashed","baseline year"="solid"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.title.y = element_text(angle=270))
fig
ggsave("figures/pdfs/S1_ts_all_years.pdf",fig,width = 7.5,height=6,dpi=600)
ggsave("figures/pngs/S1_ts_all_years.png",fig,width = 7,height=6,dpi=600)

fig<-data_no2_by_monitor%>%
  filter(!year==2012)%>%
  filter(Date_2020<=as.POSIXct("2020-05-07"))%>%
  filter(!(year==2013 & State.Name=="massachusetts"),
         !(year==2013 & State.Name=="pennsylvania"),
         !(year==2014 & State.Name=="pennsylvania"))%>%
  group_by(Date_2020,year,State.Name)%>%
  summarise(NO2=mean(NO2,na.rm=T))%>%
  mutate(year=ifelse(year==2020,"2020","Baseline"))%>%
  group_by(Date_2020,year,State.Name)%>%
  summarise(NO2=mean(NO2,na.rm=T))%>%
  mutate(name=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  ggplot(aes(x=Date_2020,y=NO2,col=factor(year),linetype=factor(year)))+
  geom_line()+
  facet_grid(name~.)+
  geom_vline(data=df_cutoff_posixct,aes(xintercept = cutoff_date),col="black")+
  ggtitle(expression(Daily~time~series~of~NO[2]~concentrations))+
  xlab("Date")+
  ylab(expression("NO"[2]~(ppb)))+
  labs(col="Year",linetype="Year")+
  theme_bw()+
  scale_color_brewer(palette = "Dark2")+
  theme(legend.position = "bottom")
fig
ggsave("figures/pdfs/F1_ts.pdf",fig,width = 5,height=6,dpi=600)
ggsave("figures/pngs/F1_ts.png",fig,width = 6,height=6,dpi=600)

