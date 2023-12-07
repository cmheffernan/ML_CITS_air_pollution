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

#load results #####
L<-list.files("results/BB_2models_constant_validation_set/")
df_metrics<-data.frame()
i<-1
for(i in 1:length(L)){
  load(paste0("results/BB_2models_constant_validation_set/",L[i]))
  df_metrics<-bind_rows(df_metrics,all_metrics)
}
df_metrics<-df_metrics%>%
  group_by(State.Name,year,method,window)%>%
  summarise(sd=sd(ATT),
            sd_pd=sd(percent_diff))

L<-list.files("results/NB_2models_constant_validation_set_hourly/")
i<-1
for(i in 1:length(L)){
  load(paste0("results/NB_2models_constant_validation_set_hourly/",L[i]))
  s<-sd(att_vec)
  df_metrics<-bind_rows(df_metrics,metrics%>%mutate(window="hour",sd=s)%>%
                          select(State.Name,year,method,window,sd))
}

load("results/linear_ML_significance_levels.RData")

data<-df_metrics%>%
  left_join(att%>%
              select(State.Name,method,year,ATT))%>%
  mutate(lower=ATT-1.96*sd,upper=ATT+1.96*sd)%>%
  mutate(significant=ifelse(lower>0 | upper<0,T,F))%>%
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
  filter(!name_shortened=="GBM")%>%
  mutate(window=factor(window,levels=c("hour","day","3day","week"),ordered = T))

#intervals ####
fig<-data%>%
  filter(name_shortened=="GB")%>%
  ggplot(aes(x=year,y=ATT,col=significant))+
  geom_point()+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  facet_grid(window~city,scales = "free_x")+
  geom_hline(yintercept = 0,col="black")+
  theme_bw()+
  ggtitle("Block bootstrap ATT for Gradient Boosting")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  ylim(c(-9,5.5))
fig
ggsave("figures/pdfs/S3_BB_sd_GB.pdf",fig,width = 8,height = 6)
ggsave("figures/pngs/S3_BB_sd_GB.png",fig,width = 8,height = 6)

fig<-data%>%
  filter(name_shortened=="RF")%>%
  ggplot(aes(x=year,y=ATT,col=significant))+
  geom_point()+
  geom_errorbar(aes(ymin=lower,ymax=upper))+
  facet_grid(window~city,scales = "free_x")+
  geom_hline(yintercept = 0,col="black")+
  theme_bw()+
  ggtitle("Block bootstrap ATT for Random Forest")+
  theme(axis.text.x = element_text(angle=45,hjust=1))+
  ylim(c(-8,5.5))
fig
ggsave("figures/pdfs/S4_BB_sd_RF.pdf",fig,width = 8,height = 6)
ggsave("figures/pngs/S4_BB_sd_RF.png",fig,width = 8,height = 6)

tab<-data%>%
  filter(window=="week")%>%
  mutate(text=paste0(round(ATT,2)," (",round(lower,2),",",round(upper,2),")"))%>%
  ungroup()%>%
  select(city,name_shortened,year,text)%>%
  arrange(city)%>%
  pivot_wider(names_from = "city",values_from = "text")%>%
  arrange(name_shortened,year)
tab
write.csv(tab,file = "figures/pdfs/ST3_ATT_and_CI_ML.csv")

#percent diff #######
colors_ML<-brewer.pal(12,"Paired")[c(1,10,12)]
pattern_ML<-c("crosshatch","none","stripe")
names(colors_ML)<-c("GB","RF","BART")
names(pattern_ML)<-c("GB","RF","BART")

data<-df_metrics%>%
  left_join(att%>%
              select(State.Name,method,year,percent_diff))%>%
  mutate(lower=percent_diff-1.96*sd_pd,upper=percent_diff+1.96*sd_pd)%>%
  mutate(significant=ifelse(lower>0 | upper<0,T,F))%>%
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
  filter(!name_shortened=="GBM")%>%
  mutate(window=factor(window,levels=c("hour","day","3day","week"),ordered = T))%>%
  filter(year==2020,window=="week")
fig<-data%>%
  ggplot(aes(x=city,y=percent_diff,fill=name_shortened,pattern=name_shortened))+
  geom_col_pattern(position = position_dodge(preserve = "single"),
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = pattern_ML) +
  geom_errorbar(aes(ymin=lower,ymax=upper),position = position_dodge(preserve = "single"))+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values = colors_ML)+
  xlab("City")+
  ylab("% difference")+
  theme_bw()+
  ggtitle(expression(Percent~difference~"in"~NO[2]~concentrations))+
  theme(legend.title = element_blank())
fig  
ggsave("figures/pdfs/F3_percent_diff_werror_sd.pdf",fig,width = 7,height=3,dpi=600)
ggsave("figures/pngs/F3_percent_diff_werror_sd.png",fig,width = 7,height=3,dpi=600)

tab<-data%>%
  mutate(text=paste0(round(percent_diff,2)," (",round(lower,2),",",round(upper,2),")"))%>%
  ungroup()%>%
  select(city,name_shortened,text)%>%
  arrange(name_shortened)%>%
  pivot_wider(names_from = "name_shortened",values_from = "text")%>%
  arrange(city)
tab
write.csv(tab,file = "figures/pdfs/ST4_percent_diff.csv")
