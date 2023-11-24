# Analyze ET model output from VENSIM with Agriment and Climate Engine
# Kendra Kaiser
# 11/24/2023
# Model developed by Dylan-Hedden Nicely

library(tidyverse)
library(ggplot2)

setwd("~/github/IrrDemand")

data<- read.csv('et_data.csv')
data$date<- as.Date(data$date, format= '%m/%d/%y')

rmse<- as.data.frame(matrix(data = NA, nrow =5, ncol=1))
row.names(rmse)<- c('ETc Metric', 'ETc Agrimet', 'ETr Metric', 'ETr Agrimet', 'ETr Climate Engine')
colnames(rmse)<- c('RMSE')

rmse[1,1]<-sqrt(mean((data$etc_etId - data$etc_est)^2, na.rm = TRUE))
rmse[2,1]<-sqrt(mean((data$etc_agrimet - data$etc_est)^2, na.rm = TRUE))
rmse[3,1]<-sqrt(mean((data$etr_etId - data$etr_est)^2, na.rm = TRUE))
rmse[4,1]<-sqrt(mean((data$etr_agrimet - data$etr_est)^2, na.rm = TRUE))
rmse[5,1]<-sqrt(mean((data$etr_ce - data$etr_est)^2, na.rm = TRUE))

data_long <- data[,c(1, 5:11)] %>% pivot_longer(cols = -c('date'), names_sep = '_', names_to = c('type', 'method'), 
                                   values_to = 'et_mm')

data_etc<- data_long %>% filter(type == "etc")

ggplot() +
  geom_point(data=data, aes(x = etc_agrimet, y = etc_est, color = 'Agrimet'), color ='red')+
  geom_point(data=data, aes(x = etc_etId, y = etc_est, color='ET Idaho'), color ='blue') +
  labs(x = "Observational ET (mm)", y = "Predicted ET (mm)", color = "Method") +
  theme_bw()






