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

methods_set<-"8methods"

all_att<-data.frame()
all_validation<-data.frame()

#list files
L<-list.files("results/sim/")

methods<-as.character(purrr::map(str_split(L,"_"),10))
L<-L[methods==methods_set]

m<-as.character(purrr::map(str_split(L,"_"),8))

print(length(L))

#combine results
for(i in 1:length(L)){
  print(i)
  load(paste0("results/sim/",L[i]))
  
  all_att<-bind_rows(
    all_att,
    df_att%>%
      mutate(model=m[i])
  )
  all_validation<-bind_rows(
    all_validation,
    df_validation_all%>%
      mutate(model=m[i])
  )
}

#save ###########
save(all_att,
     file = paste0("results/sim_combined/",
                   "att_combined_",methods_set,".RData"))
save(all_validation,
     file = paste0("results/sim_combined/",
                   "validation_combined_",methods_set,".RData"))
