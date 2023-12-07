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

load("results/sim_combined/att_combined_8methods.RData")

#average results 
df_no_average<-all_att%>%
  mutate(ATT_true=round(ATT_true,4),
         effect=ifelse(ATT_true==0,"null","effect"))%>%
  mutate(name_shortened=factor(case_when(
    method=="boosting"~"GBM",
    method=="linear_no_int"~"LC",
    method=="linear"~"LTV",
    method=="linear_no_splines"~"LTVL",
    method=="dppc"~"DPPC",
    method=="rf"~"RF",
    method=="dib"~"DIB",
    method=="dd"~"DiD",
    method=="xgboost"~"GB",
    method=="BART"~"BART"
  ),
  levels = c("DPPC","DIB","DiD","LC","LTV","LTVL","GBM","GB","RF","BART"),
  ordered = T))%>%
  mutate(effect=factor(effect,levels=c("null","effect"),ordered = T))
df<-df_no_average%>%
  group_by(method,effect,model,name_shortened)%>%
  summarise(ATT=mean(ATT),
            ATT_true=mean(ATT_true),
            abs_diff=abs(ATT-ATT_true),
            p_significant=mean(significant),
            p_significant_werror=mean(significant_werror),
            p_significant_model=mean(significant_model),
            RMSE_1=mean(RMSE_1),
            RMSE_0=mean(RMSE_0),
            bias_1=mean(bias_1),
            bias_0=mean(bias_0))%>%
  ungroup()

#figures 
fig<-df%>%
  filter(!name_shortened%in%c("LTVL","BART"))%>%
  ggplot(aes(x=name_shortened,y=ATT-ATT_true))+
  geom_col()+
  geom_hline(yintercept = 0)+
  facet_grid(model~effect)+
  xlab("Fitted model")+
  ylab("Bias")+
  ggtitle("Difference between estimated and true ATT")+
  theme_bw()
fig

fig<-df%>%
  filter(!name_shortened%in%c("LTVL","BART"))%>%
  ggplot(aes(x=name_shortened,y=abs(ATT-ATT_true)))+
  geom_col()+
  geom_hline(yintercept = 0)+
  facet_grid(model~effect)+
  xlab("Fitted model")+
  ylab("Absolute Bias")+
  ggtitle("Absolute difference between estimated and true ATT")+
  theme_bw()+
  scale_y_sqrt()
fig
ggsave("figures/pdfs/S23_att_bias.pdf",fig,width = 6,height = 6)
ggsave("figures/pngs/S23_att_bias.png",fig,width = 6,height = 6)


fig<-df%>%
  filter(!name_shortened%in%c("LTVL","BART"))%>%
  pivot_longer(c(bias_1,bias_0),names_to = "potential_outcome",values_to = "bias")%>%
  filter(!is.na(bias))%>%
  mutate(potential_outcome=ifelse(potential_outcome=="bias_1","Y1","Y0"))%>%
  ggplot(aes(x=name_shortened,y=bias))+
  geom_col()+
  geom_hline(yintercept = 0)+
  facet_grid(model~effect+potential_outcome)+
  xlab("Fitted model")+
  ylab("bias")+
  ggtitle("Bias of the two potential outcomes")+
  theme_bw()
fig
ggsave("figures/pdfs/S22_potential_outcome_bias.pdf",fig,width = 10,height = 6)
ggsave("figures/pngs/S22_potential_outcome_bias.png",fig,width = 10,height = 6)

