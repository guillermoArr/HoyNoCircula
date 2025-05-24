#################################################################################
###############  Script para la creaciOn de OLS con medias movil ################

# Cargar librerías
library(tidyverse)
library(stargazer)
library(dplyr)
library(MASS)
library(data.table)
library(ggplot2)

## Establece el directorio base
setwd("ITAM/10 Sem/PolPub/HoyNoCircula")
### Establece directorios de fuentes de datos
DATA.PATH <- paste0(getwd(), "/Datos/procesados/")
data <- read.csv(paste0(DATA.PATH, "promedios_horas_moving_avg_24h.csv"))
data <- data %>% 
  mutate(
    date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%OS")
  )
summary(data)

data <- data %>% 
  mutate(
    # Month of year (1-12)
    month = month(date),
    
    # Day of week (1=Sunday, 7=Saturday)
    day_of_week = wday(date),
    
    # Hour of day (0-23)
    hour_of_day = hour(date),
    
    # Weekend indicator (Saturday=7, Sunday=1)
    is_weekend = ifelse(day_of_week %in% c(1, 7), 1, 0),
    
    # Interaction: weekend * hour
    weekend_hour_interaction = is_weekend * hour_of_day
  )
################################################################################
####################### OLS - sin controles
# CO
reg_CO <- lm(CO ~ activacion_doble_no_circula, data = data)
# NO
reg_NO <- lm(NO ~ activacion_doble_no_circula, data = data)
# O3
reg_O3 <- lm(O3 ~ activacion_doble_no_circula, data = data)
#PM10
reg_PM10 <- lm(PM10 ~ activacion_doble_no_circula, data = data)
#PM2.5
reg_PM2.5 <- lm(PM2.5 ~ activacion_doble_no_circula, data = data)
# SO2
reg_SO2 <- lm(SO2 ~ activacion_doble_no_circula, data = data)

####################### Summaries
# CO
summary(reg_CO) 
# NO
summary(reg_NO) 
# O3
summary(reg_O3)
#PM10
summary(reg_PM10)
#PM2.5
summary(reg_PM2.5)
# SO2
summary(reg_SO2)

####################### OLS - controles
# CO
reg_CO_cont <- lm(CO ~ activacion_doble_no_circula +
                    RH + TMP + WDR + WSP + 
                    as.factor(month) + as.factor(day_of_week) + 
                    as.factor(hour_of_day)*is_weekend, data = data)
# NO
reg_NO_cont <- lm(NO ~ activacion_doble_no_circula +
                    RH + TMP + WDR + WSP + 
                    as.factor(month) + as.factor(day_of_week) + 
                    as.factor(hour_of_day)*is_weekend, data = data)
# O3
reg_O3_cont <- lm(O3 ~ activacion_doble_no_circula +
                    RH + TMP + WDR + WSP + 
                    as.factor(month) + as.factor(day_of_week) + 
                    as.factor(hour_of_day)*is_weekend, data = data)
#PM10
reg_PM10_cont <- lm(PM10 ~ activacion_doble_no_circula +
                      RH + TMP + WDR + WSP + 
                      as.factor(month) + as.factor(day_of_week) + 
                      as.factor(hour_of_day)*is_weekend, data = data)
#PM2.5
reg_PM2.5_cont <- lm(PM2.5 ~ activacion_doble_no_circula +
                       RH + TMP + WDR + WSP + 
                       as.factor(month) + as.factor(day_of_week) + 
                       as.factor(hour_of_day)*is_weekend, data = data)
# SO2
reg_SO2_cont <- lm(SO2 ~ activacion_doble_no_circula +
                     RH + TMP + WDR + WSP + 
                     as.factor(month) + as.factor(day_of_week) + 
                     as.factor(hour_of_day)*is_weekend, data = data)

####################### Summaries
# CO
summary(reg_CO_cont) 
# NO
summary(reg_NO_cont) 
# O3
summary(reg_O3_cont)
#PM10
summary(reg_PM10_cont)
#PM2.5
summary(reg_PM2.5_cont)
# SO2
summary(reg_SO2_cont)

################################################################################
####################### GENERACION DE TABLAS

stargazer::stargazer(reg_CO, reg_NO, reg_O3, reg_PM10, reg_PM2.5, reg_SO2,
                     type = "latex",
                     font.size = "tiny",
                     star.cutoffs = c(0.1, 0.05, 0.01),
                     column.sep.width = "1pt",
                     omit.stat = "ser",
                     covariate.labels = c("DHNC", "Cte"),
                     column.labels = c("CO", "NO", "O3", "PM10", "PM2.5", "SO2"),
                     dep.var.caption = "Contaminantes",
                     dep.var.labels.include = FALSE,
                     title = "Resultados promedios diarios sin controles ",
                     df = FALSE,
                     header = FALSE)

stargazer::stargazer(reg_CO_cont, reg_NO_cont, reg_O3_cont,
                     type = "latex",
                     font.size = "tiny",
                     star.cutoffs = c(0.1, 0.05, 0.01),
                     column.sep.width = "1pt",
                     omit.stat = "ser",
                     keep = c("circula", "RH", "TMP", "WDR", "WSP", "Cons"),
                     covariate.labels = c("DHNC", "Humedad", "Temperatura", "Dir Viento", "Vel Viento", "Cte"),
                     column.labels = c("CO", "NO", "O3"),
                     dep.var.caption = "Contaminantes",
                     dep.var.labels.include = FALSE,
                     title = "Resultados promedios diarios con controles (1)",
                     df = FALSE,
                     header = FALSE)

stargazer::stargazer(reg_PM10_cont, reg_PM2.5_cont, reg_SO2_cont,
                     type = "latex",
                     font.size = "tiny",
                     star.cutoffs = c(0.1, 0.05, 0.01),
                     column.sep.width = "1pt",
                     omit.stat = "ser",
                     keep = c("circula", "RH", "TMP", "WDR", "WSP", "Cons"),
                     covariate.labels = c("DHNC", "Humedad", "Temperatura", "Dir Viento", "Vel Viento", "Cte"),
                     column.labels = c("PM10", "PM2.5", "SO2"),
                     dep.var.caption = "Contaminantes",
                     dep.var.labels.include = FALSE,
                     title = "Resultados promedios diarios con controles (2)",
                     df = FALSE,
                     header = FALSE)

################################################################


