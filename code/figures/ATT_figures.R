library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(ggpattern)

load("results/mean_difference_significance_levels.RData")
load("results/linear_ML_significance_levels.RData")

#all ATT df ######################
df_ATT<-settings%>%
  rename(p_model=pvalue_model)%>%
  bind_rows(att)%>%
  mutate(significant=ifelse(p_model<0.05,"significant","not significant"))%>%
  mutate(city=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC"),
  ordered = T))%>%
  mutate(type=case_when(
    year<2020 & significant=="significant"~"significant effect in baseline year",
    year<2020 & significant=="not significant"~"no significant effect in baseline year",
    year==2020 & significant=="significant"~"significant effect in 2020",
    year==2020 & significant=="not significant"~"no significant effect in 2020",
    significant=="no data"~"no data"
  ))%>%
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

#main plot of paper #####################
fig<-df_ATT%>%
  filter(!method=="bart")%>%
  mutate(intervention_year=ifelse(year==2020,"2020","baseline year"))%>%
  ggplot(aes(x=as.character(year),y=ATT,
             fill=intervention_year))+
  geom_col(col="black")+
  geom_hline(yintercept = 0)+
  facet_grid(name_shortened~city,scales = "free_x")+
  scale_fill_manual(values = c("2020"="blue","baseline year"="orange"))+
  ggtitle("ATTs from all methods in 2020 and baseline years")+
  xlab("Year")+
  ylab("ATT (ppb)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust = 1,size=7),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.key.size = unit(0.6,"line"))
fig
ggsave("figures/pdfs/F2_ATTs.pdf",fig,width = 8,height=10.5,dpi=600)
ggsave("figures/pngs/F2_ATTs.png",fig,width = 8,height=10.5,dpi=600)

colors<-c("no significant effect in baseline year"="light gray",
          "significant effect in baseline year"="red",
          "no significant effect in 2020"="yellow",
          "significant effect in 2020"="green")
fig<-df_ATT%>%
  filter(!method=="bart")%>%
  filter(!is.na(p_model))%>%
  mutate(intervention_year=ifelse(year==2020,"2020","baseline year"))%>%
  ggplot(aes(x=as.character(year),y=ATT,
             fill=factor(type,levels=c("no significant effect in baseline year","significant effect in baseline year","no significant effect in 2020","significant effect in 2020","no data"))))+
  geom_col(col="black")+
  geom_hline(yintercept = 0)+
  facet_grid(name_shortened~city,scales = "free_x")+
  scale_fill_manual(values = colors)+
  ggtitle("ATTs and analytical p-values for mean difference and linear models")+
  xlab("Year")+
  ylab("ATT (ppb)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust = 1,size=7),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.key.size = unit(0.6,"line"))
fig
ggsave("figures/pdfs/S2_ATTs_pvalues_model.pdf",fig,width = 8,height=7.5,dpi=600)
ggsave("figures/pngs/S2_ATTs_pvalues_model.png",fig,width = 8,height=7.5,dpi=600)


fig<-df_ATT%>%
  mutate(intervention_year=ifelse(year==2020,"2020","baseline year"))%>%
  ggplot(aes(x=as.character(year),y=ATT,
             fill=intervention_year))+
  geom_col(col="black")+
  geom_hline(yintercept = 0)+
  facet_grid(name_shortened~city,scales = "free_x")+
  scale_fill_manual(values = c("2020"="blue","baseline year"="orange"))+
  ggtitle("ATTs from all methods in 2020 and baseline years")+
  xlab("Year")+
  ylab("ATT (ppb)")+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust = 1,size=7),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.key.size = unit(0.6,"line"))
fig
ggsave("figures/pdfs/S25_ATTs.pdf",fig,width = 8,height=12,dpi=600)
ggsave("figures/pngs/S25_ATTs.png",fig,width = 8,height=12,dpi=600)

#ATT and p-values table #####
tab1<-df_ATT%>%
  mutate(ATT=round(ATT,2))%>%
  arrange(name_shortened)%>%
  select(name_shortened,year,city,ATT)%>%
  rename(Method=name_shortened,Year=year)%>%
  arrange(city)%>%
  pivot_wider(names_from = "city",values_from = "ATT")%>%
  arrange(Method,Year)
tab1
tab2<-df_ATT%>%
  select(city,name_shortened,year,p_model)%>%
  arrange(name_shortened)%>%
  arrange(city)%>%
  pivot_wider(names_from = "city",values_from = "p_model")%>%
  rename(Method=name_shortened,Year=year)%>%
  arrange(Method,Year)
tab2
tab<-df_ATT%>%
  mutate(ATT=as.character(round(ATT,2)),p_model=as.character(signif(p_model,3)))%>%
  mutate(ATT_p=paste0(ATT,ifelse(is.na(p_model),"",paste0(" (",p_model,")"))))%>%
  select(name_shortened,year,city,ATT_p)%>%
  rename(Method=name_shortened,Year=year)%>%
  arrange(city)%>%
  pivot_wider(names_from = "city",values_from = "ATT_p")%>%
  arrange(Method,Year)
tab
write.csv(tab,file = "figures/pdfs/ST3_ATT_and_pvalues.csv")

#R2 figures ###############
fig<-df_ATT%>%
  filter(!method=="bart")%>%
  rename(train=r2_train,test=r2_test)%>%
  pivot_longer(c(train,test),names_to = "data",values_to = "R2")%>%
  filter(data=="train",!is.na(R2))%>%
  ggplot(aes(x=name_shortened,y=R2))+
  geom_col(col="black",fill="light blue")+
  geom_hline(yintercept = 0)+
  facet_grid(year~city,scales = "free_x")+
  theme_bw()+
  xlab("Method")+
  ggtitle(expression(R^2~of~training~data))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle=45,hjust=1))
fig
ggsave("figures/pdfs/S12_R2_train.pdf",fig,width = 6.5,height = 6)
ggsave("figures/pngs/S12_R2_train.png",fig,width = 6.5,height = 6)

fig<-df_ATT%>%
  filter(!method=="bart")%>%
  rename(train=r2_train,test=r2_test)%>%
  pivot_longer(c(train,test),names_to = "data",values_to = "R2")%>%
  filter(data=="test",!is.na(R2))%>%
  ggplot(aes(x=name_shortened,y=R2))+
  geom_col(col="black",fill="light blue")+
  geom_hline(yintercept = 0)+
  facet_grid(year~city,scales = "free_x")+
  theme_bw()+
  xlab("Method")+
  ggtitle(expression(R^2~of~testing~data))+
  ylim(c(0,1))+
  theme(axis.text.x = element_text(angle=45,hjust=1))
fig
ggsave("figures/pdfs/S13_R2_test.pdf",fig,width = 6.5,height = 6)
ggsave("figures/pngs/S13_R2_test.png",fig,width = 6.5,height = 6)

