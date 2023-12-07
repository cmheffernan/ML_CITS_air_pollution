library(tidyverse)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(ggpattern)
library(dtw)
library(splines)

for(index in 1:2){
sensitivity_type<-c("monitor","3yrs")[index]
#
#load ML ##############
L<-list.files(paste0("results/sensitivity/ML_models_",sensitivity_type))
L<-L[!as.character(purrr::map(str_split(L,"_"),1))=="dataframe"]

methods<-as.character(purrr::map(strsplit(L,c("_")),4))
states<-as.character(purrr::map(strsplit(L,c("_")),1))
years<-as.numeric(purrr::map(strsplit(L,c("_")),5))
if(sensitivity_type=="3yrs"){
  LOO_monitors<-rep(0,length(L))
}else if(sensitivity_type=="monitor"){
  LOO_monitors<-as.numeric(purrr::map(strsplit(L,c("_")),6))
}

load(paste0("results/sensitivity/ML_models_",sensitivity_type,"/",L[1]))
dfs_validation$method<-methods[1]
dfs_validation$State.Name<-states[1]
dfs_validation$year<-years[1]
dfs_validation$LOO_monitor<-LOO_monitors[1]
df<-dfs_validation
for(j in seq(2,length(L))){
  print(j)
  load(paste0("results/sensitivity/ML_models_",sensitivity_type,"/",L[j]))
  dfs_validation$method<-methods[j]
  dfs_validation$State.Name<-states[j]
  dfs_validation$year<-years[j]
  dfs_validation$LOO_monitor<-LOO_monitors[j]
  df<-bind_rows(df,dfs_validation)
}
df_ML<-df

#load linear #####
L<-list.files(paste0("results/sensitivity/linear_models_",sensitivity_type))

filetype<-!as.character(purrr::map(strsplit(L,c("cs")),2))=="v"
type<-!as.character(purrr::map(strsplit(L,c("_")),1))%in%c("dataframe","dates")
L_subset<-L[filetype & type]

methods<-as.character(purrr::map(strsplit(L_subset,c("NO2_")),2))
methods<-substr(methods,1,nchar(methods)-ifelse(sensitivity_type=="3yrs",6,8))
states<-as.character(purrr::map(strsplit(L_subset,c("_")),1))
if(sensitivity_type=="3yrs"){
  LOO_monitors<-rep(0,length(L))
}else if(sensitivity_type=="monitor"){
  LOO_monitors<-as.character(purrr::map(strsplit(L_subset,c("_")),last))
  LOO_monitors<-as.numeric(substr(LOO_monitors,1,nchar(LOO_monitors)-6))
}

for(j in seq(1,length(L_subset))){
  print(j)
  load(paste0("results/sensitivity/linear_models_",sensitivity_type,"/",L_subset[j]))
  all_validation$method<-methods[j]
  all_validation$State.Name<-states[j]
  all_validation$LOO_monitor<-LOO_monitors[j]
  df<-bind_rows(df,all_validation)
}

#linear and ML ATT #####
att<-df%>%
  filter(post_cutoff==1)%>%
  select(method,State.Name,year,Time,Y0,Y1,day_index,LOO_monitor)%>%
  group_by(method,State.Name,year,Time,day_index,LOO_monitor)%>%
  summarise(Y0=mean(Y0,na.rm=T),
            Y1=mean(Y1,na.rm=T))%>%
  ungroup()%>%
  group_by(State.Name,method,year,LOO_monitor)%>%
  summarize(Y1=mean(Y1,na.rm=T),
            Y0=mean(Y0,na.rm=T))%>%
  mutate(ATT=Y1-Y0,percent_diff=100*(Y1-Y0)/Y0)%>%
  ungroup()

save(att,file = paste0("results/sensitivity/linear_ML_significance_levels_",sensitivity_type,".RData"))


#figure ##########
load("results/mean_difference_significance_levels.RData")
load("results/linear_ML_significance_levels.RData")
att_full_analysis<-att%>%
  bind_rows(att%>%
              filter(State.Name=="pennsylvania")%>%
              mutate(State.Name="pennsylvaniacomplete"))%>%
  mutate(LOO_monitor=0,analysis="full")
settings_full_analysis<-settings%>%
  bind_rows(settings%>%
              filter(State.Name=="pennsylvania")%>%
              mutate(State.Name="pennsylvaniacomplete"))%>%
  mutate(LOO_monitor=0,analysis="full")


load(paste0("results/sensitivity/mean_difference_significance_levels_",sensitivity_type,".RData"))
load(paste0("results/sensitivity/linear_ML_significance_levels_",sensitivity_type,".RData"))
att<-att%>%
  mutate(analysis="sensitivity")
settings<-settings%>%
  mutate(analysis="sensitivity")

#all ATT df ######################
df_ATT<-settings%>%
  bind_rows(settings_full_analysis)%>%
  bind_rows(att)%>%
  bind_rows(att_full_analysis)%>%
  select(State.Name,year,method,ATT,significant,significant_werror_daily,analysis,LOO_monitor)%>%
  mutate(city=factor(case_when(
    State.Name=="district of columbia"~"Washington DC",
    State.Name=="maryland"~"Baltimore",
    State.Name=="massachusetts"~"Boston",
    State.Name=="new york"~"New York City",
    State.Name=="pennsylvania"~"Philadelphia",
    State.Name=="pennsylvaniacomplete"~"Philadelphia - restricted"
  ),
  levels = c("Boston","New York City","Philadelphia","Baltimore","Washington DC","Philadelphia - restricted"),
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

#plot #####################
colors<-c("no significant effect in baseline year"="light gray",
          "significant effect in baseline year"="red",
          "no significant effect in 2020"="yellow",
          "significant effect in 2020"="green")
colors_tiles<-c("no significant effect in baseline year"="white",
                "significant effect in baseline year"="red",
                "no significant effect in 2020"="yellow",
                "significant effect in 2020"="green",
                "no data"="gray")
if(sensitivity_type=="3yrs"){
    fig<-df_ATT%>%
      filter(year>=2017,!method=="bart")%>%
      select(analysis,ATT,year,name_shortened,city)%>%
      mutate(analysis=ifelse(analysis=="sensitivity","2017-2019","2013-2019"))%>%
      ggplot(aes(x=as.character(year),y=ATT,fill=analysis))+
      geom_col(col="black",position = "dodge")+
      geom_hline(yintercept = 0)+
      facet_grid(name_shortened~city,scales = "free_x")+
      ggtitle(paste0("Comparison of using 2017-2019 as baseline data to 2013-2019"))+
      xlab("Year")+
      ylab("Difference in ATT (ppb)")+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45,hjust = 1,size=7),
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size=7),
            legend.key.size = unit(0.6,"line"))
    fig
    ggsave(paste0("figures/pdfs/S21_sensitivity_3yrs_difference.pdf"),fig,width = 9.5,height=9,dpi=600)
    ggsave(paste0("figures/pngs/S21_sensitivity_3yrs_difference.png"),fig,width = 9.5,height=9,dpi=600)
}else{
  fig<-df_ATT%>%
    filter(!State.Name=="pennsylvaniacomplete")%>%
    filter(!name_shortened=="BART")%>%
    mutate(day=as.Date("2020-04-10"),
           value=30)%>%
    mutate(shape=ifelse(analysis=="full","all monitors","leave out one monitor"))%>%
    ggplot(aes(x=as.character(year),y=ATT,shape=shape,col=shape))+
    geom_point(position=position_dodge(width=0.5))+
    geom_hline(yintercept = 0)+
    geom_vline(xintercept = 0.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 1.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 2.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 3.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 4.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 5.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 6.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 7.5,alpha=0.4,linetype="dashed")+
    geom_vline(xintercept = 8.5,alpha=0.4,linetype="dashed")+
    facet_grid(name_shortened~city,scales = "free_x")+
    scale_shape_manual(values = c("all monitors"=17,"leave out one monitor"=16))+
    ggtitle(paste0("ATTs leaving out one monitor per city"))+
    xlab("Year")+
    ylab("ATT (ppb)")+
    theme_bw()+
    theme(axis.text.x = element_text(angle=45,hjust = 1,size=7),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size=7),
          legend.key.size = unit(0.6,"line"),
          legend.direction = "horizontal", legend.box = "vertical")+
    guides(fill = guide_legend(override.aes = list(shape = 22,size=3)))
  fig
  ggsave(paste0("figures/pdfs/S20_sensitivity_",sensitivity_type,"_att.pdf"),fig,width = 8,height=10,dpi=600)
  ggsave(paste0("figures/pngs/S20_sensitivity_",sensitivity_type,"_att.png"),fig,width = 8,height=10,dpi=600)
}

}