#################################################################################
###############  Script para la creaciOn de OLS y RDs del P. HNC ################

# Cargar librerías
library(tidyverse)
library(stargazer)
library(dplyr)
library(MASS)
library(data.table)
library(ggplot2)

## Establece el directorio base
setwd("C:/Users/Memit/OneDrive - INSTITUTO TECNOLOGICO AUTONOMO DE MEXICO/Documentos/ITAM/10 sem/PolPub/HoyNoCircula")
### Establece directorios de fuentes de datos
DATA.PATH <- paste0(getwd(), "/Datos/final/")
data <- read.csv(paste0(DATA.PATH, "contaminantes_meteorologia.csv"))
summary(data)
################################################################################
####################### OLS - sin controles
# CO
reg_CO <- lm(CO_mean ~ activacion_doble_no_circula, data = data)
# NO
reg_NO <- lm(NO_mean ~ activacion_doble_no_circula, data = data)
# O3
reg_O3 <- lm(O3_mean ~ activacion_doble_no_circula, data = data)
#PM10
reg_PM10 <- lm(PM10_mean ~ activacion_doble_no_circula, data = data)
#PM2.5
reg_PM2.5 <- lm(PM2.5_mean ~ activacion_doble_no_circula, data = data)
# SO2
reg_SO2 <- lm(SO2_mean ~ activacion_doble_no_circula, data = data)

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

####################### OLS - controles meteorologicos
# CO
reg_CO_cont <- lm(CO_mean ~ activacion_doble_no_circula +
               RH_mean + TMP_mean + WDR_mean + WSP_mean, data = data)
# NO
reg_NO_cont <- lm(NO_mean ~ activacion_doble_no_circula +
               RH_mean + TMP_mean + WDR_mean + WSP_mean, data = data)
# O3
reg_O3_cont <- lm(O3_mean ~ activacion_doble_no_circula +
               RH_mean + TMP_mean + WDR_mean + WSP_mean, data = data)
#PM10
reg_PM10_cont <- lm(PM10_mean ~ activacion_doble_no_circula +
               RH_mean + TMP_mean + WDR_mean + WSP_mean, data = data)
#PM2.5
reg_PM2.5_cont <- lm(PM2.5_mean ~ activacion_doble_no_circula +
               RH_mean + TMP_mean + WDR_mean + WSP_mean, data = data)
# SO2
reg_SO2_cont <- lm(SO2_mean ~ activacion_doble_no_circula +
               RH_mean + TMP_mean + WDR_mean + WSP_mean, data = data)

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
                     type = "text",
                     font.size = "tiny",
                     star.cutoffs = c(0.1, 0.05, 0.01),
                     column.sep.width = "1pt",
                     omit.stat = "ser",
                     covariate.labels = c("DHNC", "Cte"),
                     column.labels = c("CO", "NO", "O3", "PM10", "PM2.5", "SO2"),
                     dep.var.caption = "Contaminantes",
                     title = "Resultados de regresiones sin controles ",
                     header = FALSE)

stargazer::stargazer(reg_CO_cont, reg_NO_cont, reg_O3_cont,
                     type = "text",
                     font.size = "tiny",
                     star.cutoffs = c(0.1, 0.05, 0.01),
                     column.sep.width = "1pt",
                     omit.stat = "ser",
                     covariate.labels = c("DHNC", "Humedad", "Temperatura", "Dir Viento", "Vel Viento", "Cte"),
                     column.labels = c("CO", "NO", "O3"),
                     dep.var.caption = "Contaminantes",
                     title = "Resultados de regresiones con controles 1",
                     header = FALSE)

stargazer::stargazer(reg_PM10_cont, reg_PM2.5_cont, reg_SO2_cont,
                     type = "text",
                     font.size = "tiny",
                     star.cutoffs = c(0.1, 0.05, 0.01),
                     column.sep.width = "1pt",
                     omit.stat = "ser",
                     covariate.labels = c("DHNC", "Humedad", "Temperatura", "Dir Viento", "Vel Viento", "Cte"),
                     column.labels = c("PM10", "PM2.5", "SO2"),
                     dep.var.caption = "Contaminantes",
                     title = "Resultados de regresiones con controles 2",
                     header = FALSE)


################################################################################
###################### RD 

to_title_case <- function(x) {
  s <- strsplit(x, "_")[[1]]
  s <- paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)), sep = "", collapse = " ")
  return(s)
}

# Load data
data <- fread('')
ventana <- 7

tratamiento <- 1
totales <- 1
marginales <- 0

if (tratamiento == 1) {
  var <- 'CUMULATIVE_INGRESOS_FINANCIEROS_TREATMENT'
} else {
  var <- 'CUMULATIVE_INGRESOS_FINANCIEROS_CONTROL'
}

if (totales == 1) {
  var <- gsub('FINANCIEROS', 'TOTALES', var)
} 

if (marginales == 1) {
  var <- gsub('CUMULATIVE', 'MARGINAL', var)
} 

fecha_campanya <- as.Date(data$DATE[6])

pre_incremento <- data[data$DATE < fecha_campanya,]
post_incremento <- data[data$DATE > fecha_campanya,]  

post_incremento <- head(post_incremento, ventana)
pre_incremento <- tail(pre_incremento, ventana)

# Use get() to reference the column dynamically
model_pre <- lm(get(var) ~ DATE, data = pre_incremento)
model_post <- lm(get(var) ~ DATE, data = post_incremento)

# Adjust the slope from per day to per month
slope_pre_monthly <- coef(model_pre)[2] * 30.44
slope_post_monthly <- coef(model_post)[2] * 30.44

# Calculate the incremental rate of marginal income per month
incremental_rate_monthly <- slope_post_monthly - slope_pre_monthly
incremental_ratio_monthly <- slope_post_monthly / slope_pre_monthly

# Create the plot and add the annotation for the incremental rate in pesos per month
ggplot(data = data, aes(x = DATE, y = get(var))) + 
  geom_point() +
  geom_smooth(data = pre_incremento, aes(x = DATE, y = get(var)), method = 'lm') +
  geom_smooth(data = post_incremento, aes(x = DATE, y = get(var)), method = 'lm') +
  annotate("text", x = max(data$DATE), y = 300, 
           label = paste0("Ingresos marginales antes de CLI: ", round(slope_pre_monthly, 2), " pesos/mes"), 
           hjust = 1, vjust = 1, size = 3, color = "black") +
  annotate("text", x = max(data$DATE), y = 200, 
           label = paste0("Ingresos marginales despues de CLI: ", round(slope_post_monthly, 2), " pesos/mes"), 
           hjust = 1, vjust = 1, size = 3, color = "black") +
  annotate("text", x = max(data$DATE), y = 100, 
           label = paste0("Ingresos incrementales de: ", round(incremental_rate_monthly, 2), " pesos/mes"), 
           hjust = 1, vjust = 1, size = 3, color = "black") +
  annotate("text", x = max(data$DATE), y = 0, 
           label = paste0("Crecimiento de: ", round(incremental_ratio_monthly * 100, 2), "% vs BAU"), 
           hjust = 1, vjust = 1, size = 3, color = "black") +
  labs(title = paste0(to_title_case(var)," post CLI: ", round(incremental_rate_monthly, 2), " pesos/mes"),
       subtitle = 'Efectos Causales Medidos por Series de Tiempo Interrumpidas',
       x = 'Fecha',
       y = to_title_case(var)) +
  geom_vline(xintercept = fecha_campanya, linetype = 2, color = 'red') +
  theme_bw()