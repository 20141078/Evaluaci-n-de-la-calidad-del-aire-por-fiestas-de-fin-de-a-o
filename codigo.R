setwd("C:/ACCION AMBIENTAL/2022 FDA")

library(readr)
library(readxl)
library(openair)
library(tidyverse)
library(lubridate)
library(latticeExtra)
library(ggplot2)
library(modeest)
library(plotly)


sensor1 <- read_csv("PM2.5-data-2023-01-01 08_17_42.csv") ### PeruHuachipaE4_1753b4

software <-function(qhxx){
  
  names(qhxx)    <-c("date","PM2.5")
  
  #Convertir valores fuera del rango de medicion
  #particulas
  qhxx$PM2.5[qhxx$PM2.5 <= 5]   <-NA
  qhxx$PM2.5[qhxx$PM2.5 > 750] <-NA
  
  ### diario PM2.5
  
  qhxx_hora <- data.frame(qhxx$date, qhxx$PM2.5)
  names(qhxx_hora) <- c("date", "PM2.5")
  qhxx_hora_t <- timeAverage(qhxx_pm, avg.time  = "hour", data.thresh = 75, 
                           statistic = "mean", type = "default")
  
  qhxx_pm <- data.frame(qhxx$date, qhxx$PM2.5)
  names(qhxx_pm) <- c("date", "PM2.5")
  qh_pm_dia <- timeAverage(qhxx_pm, avg.time  = "day", data.thresh = 75, 
                           statistic = "mean", type = "default")
  
  qh_pm_dia$PM2.5 <- round(qh_pm_dia$PM2.5, 0)
  
  ### horario PM2.5
  qhxx_pm_h      <- qhxx_pm
  qhxx_pm_h$hora <- hour(qhxx_pm_h$date)
  qhxx_pm_h <- qhxx_pm_h %>% 
    group_by(hora) %>%
    summarise( PM2.5 = mean(PM2.5, na.rm = T )) 
  
  qhxx_pm_h$hora  <- qhxx_pm_h$hora + 1
  qhxx_pm_h$PM2.5 <- round(qhxx_pm_h$PM2.5,0)
  qhxx_pm_h
  
  qh_pm_mes <- timeAverage(qh_pm_dia, avg.time  = "month", data.thresh = 75, statistic = "mean", type = "default")
  qh_pm_mes$PM2.5 <- round(qh_pm_mes$PM2.5, 0)
  
  list(qhxx_hora_t, qh_pm_dia, qh_pm_mes, qhxx_pm_h, qhxx)
}     

##################
sensor1_r <- software(sensor1)
sensor1_r_d <- data.frame(sensor1_r[1])
sensor1_r_h <- data.frame(sensor1_r[3])
sensor1_r_m <- data.frame(sensor1_r[2])
sensor1_r_T <- data.frame(sensor1_r[4])

write.csv(x = UNALM_d,       file = "UNALM_d.csv")
write.csv(x = HUACHIPA_h,    file = "HUACHIPA_h1.csv")

