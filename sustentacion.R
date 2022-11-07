#########################################################
##########        Brigitte Castaneda           ##########
#########             CARBON TAX               ##########
#########        Heterogeneus effects          ##########
#########             01/06/2022               ##########
#########################################################
rm(list = ls()) # limpia el entorno de R
## Carga de librerias
lista = c('readr','readxl','lubridate', 'ggplot2',
          'hrbrthemes','dplyr','plotly','tseries',
          'fUnitRoots','forecast','FitAR', 'write.xlsx')

for (i in 1:length(lista) ) {
  if(lista[i] %in% rownames(installed.packages()) == FALSE) {
    install.packages(lista[i])
  }
  lapply(lista[i], library, character.only = TRUE)
  
}
library(sqldf)
library(tidyr)
library(readr)
library(readxl)
library(haven)
library(write.xlsx)
library(openxlsx)
library(fixest)
path= "C:/Users/USER/OneDrive - Universidad de los Andes (1)/2022-10/R- CT/Carbontax1"
#path = "C:/Users/USER/Desktop/Bri"
# Direcccion del archivo
setwd(path)

#### 1- Carga de archivos
source("read_excel_title.R")
#base_1 <- read_csv_title("output/baselimpia_65.csv")
#base_1 <- read_csv_title("output/baselimpia_81.csv")
base_1 <- read_csv_title("output/baselimpia_74.csv")
base_1 <- base_1[,-c(1)]
revision_tabla(base_1)# "ANOTACION: PAISES: 74, OBS: 2294, COLS: 51"
################# VARIABLES NUMERICAS #################
glimpse(base_1)
sec = c(3:7, 11:51)
for (i in sec) {
  base_1[[i]] = as.numeric(base_1[[i]])
}
data.frame(colnames(base_1))
colnames(base_1)[34] = 'RYD_GDP' 
unique(base_1$COUNTRY)
#######################ESTADISTICAS DESCRIPTIVAS##########################
data.frame(colnames(base_1))
glimpse(base_1)

#creo muestra PAISES CON CT
paises_tratados = unique(subset(base_1, base_1$HAS_CARB_TAX ==  1)[1])
base_1$TREAT = ifelse(base_1$COUNTRY  %in% paises_tratados$COUNTRY, 1, 0)
table(base_1$TREAT)
unique(subset(base_1, base_1$TREAT ==  1)[1])


#base muestra completa paises con ct
base_ct = subset(base_1, base_1$TREAT ==  1)
base_ct$EMPLYMENT_TOTAL = 100 - base_ct$UNEMPLOYMENT_ILO_MODEL_ESTIMATE 
lista =  c("CONSUMPTION_ENERGY", "GOV_EFFECTIVENESS","UNEMPLOYMENT_ILO_MODEL_ESTIMATE","EMPLYMENT_TOTAL", "GDP_CAPITA", "GDP_CURRENT", "GDP_GROWTH")
base_desc = select(base_ct, lista)
esta_desc = data.frame()
for (i in colnames(base_desc)) {
  temp = data.frame("variable" = i, 
                    "mean"  =  mean( base_desc [[i]], na.rm = T )  ,
                    "median" = median(base_desc[[i]], na.rm = T ) ,
                    "sd" = sd(base_desc[[i]], na.rm = T ))
  esta_desc = rbind(temp, esta_desc)
}
length(unique(base_ct$COUNTRY))
paises_ct =esta_desc

#base muestra completa paises sin ct
base_no_ct = subset(base_1, base_1$TREAT !=  1)
base_no_ct$EMPLYMENT_TOTAL = 100 - base_no_ct$UNEMPLOYMENT_ILO_MODEL_ESTIMATE 
lista =  c("CONSUMPTION_ENERGY", "GOV_EFFECTIVENESS","UNEMPLOYMENT_ILO_MODEL_ESTIMATE","EMPLYMENT_TOTAL", "GDP_CAPITA", "GDP_CURRENT", "GDP_GROWTH")
base_desc = select(base_no_ct, lista)
esta_desc = data.frame()
for (i in colnames(base_desc)) {
  temp = data.frame("variable" = i, 
                    "mean"  =  mean( base_desc [[i]], na.rm = T )  ,
                    "median" = median(base_desc[[i]], na.rm = T ) ,
                    "sd" = sd(base_desc[[i]], na.rm = T ))
  esta_desc = rbind(temp, esta_desc)
}
length(unique(base_no_ct$COUNTRY))
paises_no_ct =esta_desc

df=rbind(paises_ct, paises_no_ct)
write.csv(df, "output/descriptive.csv")

#base muestra High Income paises con ct
base_ct = subset(base_1, base_1$TREAT ==  1 & base_1$INCOME_GROUP == "High income")
base_ct$EMPLYMENT_TOTAL = 100 - base_ct$UNEMPLOYMENT_ILO_MODEL_ESTIMATE 
lista =  c("CONSUMPTION_ENERGY", "GOV_EFFECTIVENESS","UNEMPLOYMENT_ILO_MODEL_ESTIMATE","EMPLYMENT_TOTAL", "GDP_CAPITA", "GDP_CURRENT", "GDP_GROWTH")
base_desc = select(base_ct, lista)
esta_desc = data.frame()
for (i in colnames(base_desc)) {
  temp = data.frame("variable" = i, 
                    "mean"  =  mean( base_desc [[i]], na.rm = T )  ,
                    "median" = median(base_desc[[i]], na.rm = T ) ,
                    "sd" = sd(base_desc[[i]], na.rm = T ))
  esta_desc = rbind(temp, esta_desc)
}
length(unique(base_ct$COUNTRY))
paises_ct =esta_desc

#base High Income paises sin ct
base_no_ct = subset(base_1, base_1$TREAT !=  1 & base_1$INCOME_GROUP == "High income")
base_no_ct$EMPLYMENT_TOTAL = 100 - base_no_ct$UNEMPLOYMENT_ILO_MODEL_ESTIMATE 
lista =  c("CONSUMPTION_ENERGY", "GOV_EFFECTIVENESS","UNEMPLOYMENT_ILO_MODEL_ESTIMATE","EMPLYMENT_TOTAL", "GDP_CAPITA", "GDP_CURRENT", "GDP_GROWTH")
base_desc = select(base_no_ct, lista)
esta_desc = data.frame()
for (i in colnames(base_desc)) {
  temp = data.frame("variable" = i, 
                    "mean"  =  mean( base_desc [[i]], na.rm = T )  ,
                    "median" = median(base_desc[[i]], na.rm = T ) ,
                    "sd" = sd(base_desc[[i]], na.rm = T ))
  esta_desc = rbind(temp, esta_desc)
}
length(unique(base_no_ct$COUNTRY))
paises_no_ct =esta_desc

df=rbind(paises_ct, paises_no_ct)
write.csv(df, "output/descriptive.csv")

#base muestra Upper Middle Income paises con ct
base_ct = subset(base_1, base_1$TREAT ==  1 & base_1$INCOME_GROUP == "Upper middle income")
base_ct$EMPLYMENT_TOTAL = 100 - base_ct$UNEMPLOYMENT_ILO_MODEL_ESTIMATE 
lista =  c("CONSUMPTION_ENERGY", "GOV_EFFECTIVENESS","UNEMPLOYMENT_ILO_MODEL_ESTIMATE","EMPLYMENT_TOTAL", "GDP_CAPITA", "GDP_CURRENT", "GDP_GROWTH")
base_desc = select(base_ct, lista)
esta_desc = data.frame()
for (i in colnames(base_desc)) {
  temp = data.frame("variable" = i, 
                    "mean"  =  mean( base_desc [[i]], na.rm = T )  ,
                    "median" = median(base_desc[[i]], na.rm = T ) ,
                    "sd" = sd(base_desc[[i]], na.rm = T ))
  esta_desc = rbind(temp, esta_desc)
}
length(unique(base_ct$COUNTRY))
paises_ct =esta_desc

#base muestra Upper Middle Income paises sin ct
base_no_ct = subset(base_1, base_1$TREAT !=  1 & base_1$INCOME_GROUP == "Upper middle income")
base_no_ct$EMPLYMENT_TOTAL = 100 - base_no_ct$UNEMPLOYMENT_ILO_MODEL_ESTIMATE 
lista =  c("CONSUMPTION_ENERGY", "GOV_EFFECTIVENESS","UNEMPLOYMENT_ILO_MODEL_ESTIMATE","EMPLYMENT_TOTAL", "GDP_CAPITA", "GDP_CURRENT", "GDP_GROWTH")
base_desc = select(base_no_ct, lista)
esta_desc = data.frame()
for (i in colnames(base_desc)) {
  temp = data.frame("variable" = i, 
                    "mean"  =  mean( base_desc [[i]], na.rm = T )  ,
                    "median" = median(base_desc[[i]], na.rm = T ) ,
                    "sd" = sd(base_desc[[i]], na.rm = T ))
  esta_desc = rbind(temp, esta_desc)
}
length(unique(base_no_ct$COUNTRY))
paises_no_ct =esta_desc

df=rbind(paises_ct, paises_no_ct)
write.csv(df, "output/descriptive.csv")


################# MODELO CON MUESTRA COMPLETA ################# 
#################creo VARIABLE DE TRATAMIENTO ##############
base_treat = subset(base_1, base_1$HAS_CARB_TAX == 1)
treated_list = unique(base_treat$COUNTRY)
base_1$treat  = ifelse(base_1$COUNTRY     %in%  treated_list , 1, 0) 
BASE = base_1

base_1$year_treated  = as.integer( base_1$YEARI)
base_1$time_to_treat= ifelse(base_1$treat == 1, 
                             base_1$YEAR - base_1$year_treated ,
                             0 )

hist(base_1$time_to_treat, 10)
#limita años a futuro y pasado
base_1 =  subset( base_1, base_1$time_to_treat <= 10 & base_1$time_to_treat >= -15)

################# Regresiones GDP #################
####GDP Growth %
mod_twfe_GDP = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) 
                     #+ GOV_EFFECTIVENESS + CONSUMPTION_ENERGY  + GDP_1960  # Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_1)
png("output/sustentacion/GDP_growth.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP growth')
dev.off()
summary(mod_twfe_GDP)
####GDP capita %
mod_twfe_GDP = feols(log(GDP_CAPITA) ~ i(time_to_treat, treat, ref = 0) # 
                    #+  CONTROL_CORRUPTION + RYD_GDP + CONSUMPTION_ENERGY # Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_1)
png("output/heterogeneo/GDP_capita.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP per capita')
dev.off()
summary(mod_twfe_GDP)
################# Regresiones EMPLEO #################
####Empleo total
mod_twfe_GDP = feols(100-UNEMPLOYMENT_ILO_MODEL_ESTIMATE ~ i(time_to_treat, treat, ref = 0) # + CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF## Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_1)
png("output/sustentacion/EMPLEO.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for employment (% of total labor)')
dev.off()
summary(mod_twfe_GDP)
####Empleo por educacion
mod_twfe_1 = feols(UNEMPLOY_ADV_EDU ~ i(time_to_treat, treat, ref = 0) # 
                   + CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF ## Our key interaction: time × treatment status
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_1)

mod_twfe_2 = feols(UNEMPLOY_INT_EDU ~ i(time_to_treat, treat, ref = 0) # 
                   + CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF ## Our key interaction: time × treatment status
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_1)


mod_twfe_3 = feols(UNEMPLOY_BAS_EDU ~ i(time_to_treat, treat, ref = 0) # 
                   + CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF## Our key interaction: time × treatment status
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_1)

png("output/sustentacion/EMP_AK.png", width = 1030, height = 598)
iplot(  list(mod_twfe_1,mod_twfe_2, mod_twfe_3 ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for Employment by academic level'
)
legend( "bottom", col = c(1,2,3), bty = "n", pch = c(20,17,15), 
        legend = c("Unemployment with advanced education (% of total labor force with advanced education)", 
                   "Unemployment with intermediate education (% of total labor force with intermediate education)",
                   "Unemployment with basic education (% of total labor force with basic education)"))
dev.off()

################# Analisis por nivel de ingreso GDP ################# 
##### GDP growth
mod_twfe_GDP_hi = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_1, base_1$INCOME_GROUP == 'High income'))

mod_twfe_GDP_mi = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_1, base_1$INCOME_GROUP == 'Upper middle income'))
png("output/sustentacion/GDP_GROWTH_HI_MI.png", width = 1030, height = 598)
iplot(  list(mod_twfe_GDP_hi,mod_twfe_GDP_mi ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for GDP growth (annual %)'
        
)
legend( "bottom", col = c(1,2), bty = "n", pch = c(20,17), 
        legend = c("High Income countries", 
                   "Upper Middle Income countries"))
dev.off()
summary(mod_twfe_GDP_mi)
##### GDP capita
mod_twfe_GDP_hi = feols(log(GDP_CAPITA) ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_1, base_1$INCOME_GROUP == 'High income'))

mod_twfe_GDP_mi = feols(log(GDP_CAPITA) ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_1, base_1$INCOME_GROUP == 'Upper middle income'))
png("output/sustentacion/GDP_CAPITA_HI_MI.png", width = 1030, height = 598)
iplot(  list(mod_twfe_GDP_hi,mod_twfe_GDP_mi ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for log GDP per capita (current US$)'
        
)
legend( "bottom", col = c(1,2), bty = "n", pch = c(20,17), 
        legend = c("High Income countries", 
                   "Upper Middle Income countries"))
dev.off()
summary(mod_twfe_GDP_mi)

################# Analisis por nivel de ingreso EMPLEO ################# 
###UNEMPLOYMENT_ILO_MODEL_ESTIMATE
mod_twfe_EMP_hi = feols(100-UNEMPLOYMENT_ILO_MODEL_ESTIMATE ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ GDP_1960 + GOV_EFFECTIVENESS + CONSUMPTION_ENERGY #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_1, base_1$INCOME_GROUP == 'High income'))

mod_twfe_EMP_mi = feols(100-UNEMPLOYMENT_ILO_MODEL_ESTIMATE ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ GDP_1960 + GOV_EFFECTIVENESS + CONSUMPTION_ENERGY #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_1, base_1$INCOME_GROUP == 'Upper middle income'))
png("output/sustentacion/EMP_HI_MI.png", width = 1030, height = 598)
iplot(  list(mod_twfe_EMP_hi,mod_twfe_EMP_mi ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment'
        
)
legend( "bottom", col = c(1,2), bty = "n", pch = c(20,17), 
        legend = c("High Income countries", 
                   "Upper Middle Income countries"))
dev.off()
#########################################################################
################# MODELO CON MUESTRA DE PAISES CT 1990-2000 ################# 
#################creo VARIABLE DE TRATAMIENTO ##############
base_treat = subset(base_1, base_1$HAS_CARB_TAX == 1)
treated_list = unique(base_treat$COUNTRY)
base_1$treat  = ifelse(base_1$COUNTRY     %in%  treated_list , 1, 0) 
BASE = base_1

base_1$year_treated  = as.integer( base_1$YEARI)
base_1$time_to_treat= ifelse(base_1$treat == 1, 
                             base_1$YEAR - base_1$year_treated ,
                             0 )

hist(base_1$time_to_treat, 10)
#limita años a futuro y pasado
#base_1 =  subset( base_1, base_1$time_to_treat <= 10 & base_1$time_to_treat >= -15)
#################creo VARIABLE DE TRATAMIENTO ##############
base_1 <-BASE
base_90_09 = base_1 %>% subset( as.numeric(base_1$YEARI) < 2000)
base_na = base_1 %>% subset( base_1$treat != 1 |  base_1$YEARI >= 2000)
base_na$treat = 0
base_na$YEARI = NA
base_90_09 = rbind(base_90_09, base_na)

base_90_09$year_treated = base_90_09$YEARI
base_90_09$time_to_treat= ifelse(base_90_09$treat == 1, 
                                 as.numeric(base_90_09$YEAR) - as.numeric(base_90_09$year_treated) ,
                                 0 )
hist(base_90_09$time_to_treat, 10)
#limita años a futuro
base_90_09 =  subset( base_90_09, base_90_09$time_to_treat <= 10 & base_90_09$time_to_treat >= -15)

################# Regresiones GDP################# 
####GDP Growth %
mod_twfe_GDP = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) # + CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF## Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_90_09)

png("output/sustentacion/GDP_growth_90_10.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP growth')
dev.off()
summary(mod_twfe_GDP)

####GDP capita %
mod_twfe_GDP = feols(log(GDP_CAPITA) ~ i(time_to_treat, treat, ref = 0) # 
                     #+ CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF## Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_90_09)
png("output/sustentacion/GDP_cap_90_10.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP per capita')
dev.off()
summary(mod_twfe_GDP)

####GDP current %
mod_twfe_GDP = feols(log(GDP_CURRENT) ~ i(time_to_treat, treat, ref = 0) # 
                     #+ CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF## Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_90_09)
png("output/sustentacion/GDP_current_90_10.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP current')
dev.off()
summary(mod_twfe_GDP)

################# Regresiones de patentes ################# 
#patentes_publications muestra total
mod_twfe_PP = feols(PATENTS_PUBLICATIONS ~ i(time_to_treat, treat, ref = 0) # 
                    + GDP_GROWTH + RYD_GDP #+ GOV_EFFECTIVENESS + CO2_TOTALEXCLUDINGLUCF ## #+ REG_QUALITY + UNEMPLOY_ADV_EDU
                    |  COUNTRY + YEAR,                             ## FEs
                    cluster = ~REGION,                              ## Clustered SEs
                    data = base_90_09)
glimpse(base_90_09)
png("output/sector/patents_pub_90_10.png", width = 1030, height = 598)
iplot(mod_twfe_PP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for number of Patents publications (Carbon Tax implemented 1990-2010)')
summary(mod_twfe_PP)
dev.off()
#patentes_GRANTS muestra total
mod_twfe_PG = feols(PATENTS_GRANTS ~ i(time_to_treat, treat, ref = 0) 
                    + GDP_GROWTH + RYD_GDP #+ GOV_EFFECTIVENESS + CO2_TOTALEXCLUDINGLUCF #+ REG_QUALITY + UNEMPLOY_ADV_EDU
                    |  COUNTRY + YEAR,                             ## FEs
                    cluster = ~REGION,                              ## Clustered SEs
                    data = base_90_09)
png("output/sector/patents_grants_90_10.png", width = 1030, height = 598)
iplot(mod_twfe_PG, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for number of patents grants (Carbon Tax implemented 1990-2010)')
summary(mod_twfe_PG)
dev.off()

################# Regresiones CO2 ################# 
mod_twfe_1 = feols(YCO2_TRANSPORT ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                   + GDP_GROWTH + RYD_GDP
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_90_09)

mod_twfe_2 = feols(YCO2_INDUSTRY ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                   + GDP_GROWTH + RYD_GDP
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_90_09)



mod_twfe_3 = feols(YCO2_PUBLIC_SECTOR ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                   + GDP_GROWTH + RYD_GDP
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_90_09)

mod_twfe_4= feols(YCO2_OTHERSECTORS ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                  + GDP_GROWTH + RYD_GDP
                  |  COUNTRY+ YEAR,                             ## FEs
                  cluster = ~REGION,                              ## Clustered SEs
                  data = base_90_09)

mod_twfe_5= feols(YCO2_ELECTRICITY ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                  + GDP_GROWTH + RYD_GDP
                  |  COUNTRY+ YEAR,                             ## FEs
                  cluster = ~REGION,                              ## Clustered SEs
                  data = base_90_09)

png("output/sector/co2_sector1_90_10.png", width = 1030, height = 598)
iplot(  list(mod_twfe_1, mod_twfe_2, mod_twfe_3, mod_twfe_4, mod_twfe_5 ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for CO2 emissions by sector (Carbon Tax implemented 1990-2010)'
)
legend( "bottom", col = c(1,2,3,4,5), bty = "n", pch = c(20,17,15,1, 2), 
        legend = c("CO2_transport", 
                   "CO2_industry",
                   "CO2_public_sector ", 
                   "CO2_othersectors",
                   "CO2_electricity"))
dev.off()

################# Analisis por nivel de ingreso GDP ################# 
###GDP per cap 
mod_twfe_GDP_hi = feols(log(GDP_CAPITA) ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_90_09, base_90_09$INCOME_GROUP == 'High income'))
iplot(mod_twfe_GDP_hi, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for logGDP_capita High income')
summary(mod_twfe_GDP_hi)



##### GDP growth
mod_twfe_GDP_hi = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_90_09, base_90_09$INCOME_GROUP == 'High income'))
iplot(mod_twfe_GDP_hi, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for logGDP_capita High income')
summary(mod_twfe_GDP_hi)

###UNEMPLOYMENT_ILO_MODEL_ESTIMATE
mod_twfe_EMP_hi = feols(100-UNEMPLOYMENT_ILO_MODEL_ESTIMATE ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_90_09, base_90_09$INCOME_GROUP == 'High income'))
iplot(mod_twfe_EMP_hi, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for employment in High income')


#########################################################################
################# MODELO CON MUESTRA DE PAISES CT 2010-2020 ################# 
#################creo VARIABLE DE TRATAMIENTO ##############
base_1 <-BASE
base_10_20 = base_1 %>% subset( as.numeric(base_1$YEARI) >= 2010)
base_na = base_1 %>% subset( base_1$treat != 1)
base_na$treat = 0
base_na$YEARI = NA
base_10_20 = rbind(base_10_20, base_na)

base_10_20$year_treated = base_10_20$YEARI
base_10_20$time_to_treat= ifelse(base_10_20$treat == 1, 
                                 as.numeric(base_10_20$YEAR) - as.numeric(base_10_20$year_treated) ,
                                 0 )
hist(base_10_20$time_to_treat, 10)
#limita años a futuroy pasado
base_10_20 =  subset( base_10_20, base_10_20$time_to_treat >= -15)

################# Regresiones GDP################# 
#GDP SECTORIAL
mod_twfe_agri_10_20 = feols(AGRICULTURE_GDP ~ i(time_to_treat, treat, ref = 0) # 
                            # + CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP ## Our key interaction: time × treatment status
                            |  COUNTRY+ YEAR,                             ## FEs
                            cluster = ~REGION,                              ## Clustered SEs
                            data = base_10_20)


mod_twfe_idust_10_20 = feols(INDUSTRY_GDP ~ i(time_to_treat, treat, ref = 0) #  
                             # + CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP ## Our key interaction: time × treatment status
                             |  COUNTRY+ YEAR,                             ## FEs
                             cluster = ~REGION,                              ## Clustered SEs
                             data = base_10_20)



mod_twfe_servi_10_20 = feols(SERVICES_GDP ~ i(time_to_treat, treat, ref = 0) #  
                             #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP ## Our key interaction: time × treatment status
                             |  COUNTRY+ YEAR,                             ## FEs
                             cluster = ~REGION,                              ## Clustered SEs
                             data = base_10_20)


png("output/sustentacion/GDP_sector_10_20.png", width = 1030, height = 598)
iplot(  list(mod_twfe_servi_10_20,mod_twfe_agri_10_20, mod_twfe_idust_10_20 ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for GDP by sector (Carbon Tax implemented 2010-2020)' #in countries that implemented a Carbon tax after 2010
)
legend( "bottom", col = c(1,2,3), bty = "n", pch = c(20,17,15), 
        legend = c("GDP in services (% of GDP)", 
                   "GDP in agriculture (% of GDP)",
                   "GDP in industry (% of GDP) "))
dev.off()

####GDP Growth %
mod_twfe_GDP = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) # 
                     #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP ## Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_10_20)
png("output/sustentacion/GDP_growth_10_20.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP growth')
summary(mod_twfe_GDP)
dev.off()
####GDP capita %
mod_twfe_GDP = feols(GDP_CAPITA ~ i(time_to_treat, treat, ref = 0) # 
                     + CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF## Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_10_20)
png("output/sustentacion/GDP_cap_10_20.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP per capita')
summary(mod_twfe_GDP)
dev.off()
####GDP_CURRENT %
mod_twfe_GDP = feols(GDP_CURRENT ~ i(time_to_treat, treat, ref = 0) # 
                     + CONTROL_CORRUPTION  + RYD_GDP + CO2_TOTALEXCLUDINGLUCF## Our key interaction: time × treatment status
                     |  COUNTRY + YEAR,                             ## FEs
                     cluster = ~REGION,                              ## Clustered SEs
                     data = base_10_20)
png("output/sustentacion/GDP_current_10_20.png", width = 1030, height = 598)
iplot(mod_twfe_GDP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for GDP_CURRENT')
summary(mod_twfe_GDP)
dev.off()

################# Regresiones de patentes ################# 
#patentes_publications muestra total
mod_twfe_PP = feols(PATENTS_PUBLICATIONS ~ i(time_to_treat, treat, ref = 0) # 
                    + GDP_CAPITA + RYD_GDP #+ CO2_TOTALEXCLUDINGLUCF #+ GOV_EFFECTIVENESS + REG_QUALITY + UNEMPLOY_ADV_EDU
                    |  COUNTRY + YEAR,                             ## FEs
                    cluster = ~REGION,                              ## Clustered SEs
                    data = base_10_20)
png("output/sector/patents_pub_10_20.png", width = 1030, height = 598)
iplot(mod_twfe_PP, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for number of Patents publications (Carbon Tax implemented 2010-2020)')
summary(mod_twfe_PP)
dev.off()
#patentes_GRANTS muestra total
mod_twfe_PG = feols(PATENTS_GRANTS ~ i(time_to_treat, treat, ref = 0) 
                    + GDP_CAPITA + RYD_GDP #+ CO2_TOTALEXCLUDINGLUCF #+ GOV_EFFECTIVENESS + REG_QUALITY + UNEMPLOY_ADV_EDU
                    |  COUNTRY + YEAR,                             ## FEs
                    cluster = ~REGION,                              ## Clustered SEs
                    data = base_10_20)
png("output/sector/patents_grants_10_20.png", width = 1030, height = 598)
iplot(mod_twfe_PG, 
      xlab = 'Time to treatment',
      main = 'Event study: TWFE for number of patents grants (Carbon Tax implemented 2010-2020)')
summary(mod_twfe_PG)
dev.off()

################# Regresiones CO2 ################# 
mod_twfe_1 = feols(YCO2_TRANSPORT ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                   + GDP_GROWTH + RYD_GDP
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_10_20)

mod_twfe_2 = feols(YCO2_INDUSTRY ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                   + GDP_GROWTH + RYD_GDP
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_10_20)



mod_twfe_3 = feols(YCO2_PUBLIC_SECTOR ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                   + GDP_GROWTH + RYD_GDP
                   |  COUNTRY+ YEAR,                             ## FEs
                   cluster = ~REGION,                              ## Clustered SEs
                   data = base_10_20)

mod_twfe_4= feols(YCO2_OTHERSECTORS ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                  + GDP_GROWTH + RYD_GDP
                  |  COUNTRY+ YEAR,                             ## FEs
                  cluster = ~REGION,                              ## Clustered SEs
                  data = base_10_20)

mod_twfe_5= feols(YCO2_ELECTRICITY ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                  + GDP_GROWTH + RYD_GDP
                  |  COUNTRY+ YEAR,                             ## FEs
                  cluster = ~REGION,                              ## Clustered SEs
                  data = base_10_20)

png("output/sector/co2_sector1_10_20.png", width = 1030, height = 598)
iplot(  list(mod_twfe_1, mod_twfe_2, mod_twfe_3, mod_twfe_4, mod_twfe_5 ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for CO2 emissions by sector (Carbon Tax implemented 2010-2020)'
)
legend( "bottom", col = c(1,2,3,4,5), bty = "n", pch = c(20,17,15,1, 2), 
        legend = c("CO2_transport", 
                   "CO2_industry",
                   "CO2_public_sector ", 
                   "CO2_othersectors",
                   "CO2_electricity"))
dev.off()
################# Analisis por nivel de ingreso GDP ################# 
###GDP per cap 
mod_twfe_GDP_hi = feols(log(GDP_CAPITA) ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'High income'))

mod_twfe_GDP_mi = feols(log(GDP_CAPITA) ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'Upper middle income'))
png("output/sector/GDP_cap_10_20_uphi.png", width = 1030, height = 598)
iplot(  list(mod_twfe_GDP_hi,mod_twfe_GDP_mi ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for log GDP_capita (Carbon Tax implemented 2010-2020)'
        
)
legend( "bottom", col = c(1,2), bty = "n", pch = c(20,17), 
        legend = c("Log GDP per capita in High Income countries (% of GDP)", 
                   "Log GDP per capita in Upper middle income countries (% of GDP)"))
dev.off()
###GDP per cap 
mod_twfe_GDP_hi = feols(log(GDP_CURRENT) ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        + CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'High income'))

mod_twfe_GDP_mi = feols(log(GDP_CURRENT) ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        + CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'Upper middle income'))
png("output/sector/GDP_cu_10_20_uphi.png", width = 1030, height = 598)
iplot(  list(mod_twfe_GDP_hi,mod_twfe_GDP_mi ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for log GDP current (Carbon Tax implemented 2010-2020)'
        
)
legend( "bottom", col = c(1,2), bty = "n", pch = c(20,17), 
        legend = c("Log GDP current in High Income countries (% of GDP)", 
                   "Log GDP current in Upper middle income countries (% of GDP)"))

dev.off()
##### GDP growth
mod_twfe_GDP_hi = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'High income'))

mod_twfe_GDP_mi = feols(GDP_GROWTH ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        #+ CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'Upper middle income'))
png("output/sector/GDP_growth_10_20_uphi.png", width = 1030, height = 598)
iplot(  list(mod_twfe_GDP_hi,mod_twfe_GDP_mi ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for log GDP_GROWTH (Carbon Tax implemented 2010-2020)'
        
)
legend( "bottom", col = c(1,2), bty = "n", pch = c(20,17), 
        legend = c("GDP_GROWTH in High Income countries (% of GDP)", 
                   "GDP_GROWTH in Upper middle income countries (% of GDP)"))
dev.off()
###UNEMPLOYMENT_ILO_MODEL_ESTIMATE
mod_twfe_EMP_hi = feols(100-UNEMPLOYMENT_ILO_MODEL_ESTIMATE ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        + CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'High income'))

mod_twfe_EMP_mi = feols(100-UNEMPLOYMENT_ILO_MODEL_ESTIMATE ~ i(time_to_treat, treat, ref = 0) # + ## Our key interaction: time × treatment status
                        + CONTROL_CORRUPTION  +  CO2_TOTALEXCLUDINGLUCF + RYD_GDP# + CO2emissions + corruption_control +rate_coverage
                        |  COUNTRY+ YEAR,                             ## FEs
                        cluster = ~REGION,                              ## Clustered SEs
                        data = subset(base_10_20, base_10_20$INCOME_GROUP == 'Upper middle income'))

iplot(  list(mod_twfe_EMP_hi,mod_twfe_EMP_mi ), 
        sep = 0.2 ,ref.line = 0,
        xlab = 'Time to treatment',
        main = 'Event study: TWFE for log GDP_GROWTH (Carbon Tax implemented 2010-2020)'
        
)
legend( "bottom", col = c(1,2), bty = "n", pch = c(20,17), 
        legend = c("GDP_GROWTH in High Income countries (% of GDP)", 
                   "GDP_GROWTH in Upper middle income countries (% of GDP)"))


##############################################################################
################# CONTROL SINTETICO ##########################################
###################################################
### Load file: carbontax_data.dta 
###################################################
library("Synth")
library(foreign)
carbontax <- read.dta(file.choose())
attach(carbontax)

###################################################
dataprep.out <-
  dataprep(foo = carbontax,
           predictors = c("GDP_per_capita" , "gas_cons_capita" , "vehicles_capita" ,
                          "urban_pop") ,
           predictors.op = "mean" ,
           time.predictors.prior = 1980:1989 ,
           special.predictors = list(
             list("CO2_transport_capita" , 1989 , "mean"),
             list("CO2_transport_capita" , 1980 , "mean"),
             list("CO2_transport_capita" , 1970 , "mean")
           ),
           dependent = "CO2_transport_capita",
           unit.variable = "Countryno",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = 13,
           controls.identifier = c(1:12, 14:15),
           time.optimize.ssr = 1960:1989,
           time.plot = 1960:2005
  )


###################################################
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "All")

###################################################
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out
)

###################################################
### Table 1: CO2 Emissions From Transport Predictor Means Before Tax Reform	
###################################################
synth.tables$tab.pred[1:7, ]

###################################################
### Table 2: Country Weights in Synthetic Sweden
###################################################
synth.tables$tab.w[1:14, ]

###################################################
### Figure 4: Path Plot of per capita CO2 Emissions from Transport
###################################################
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "Metric tons per capita (CO2 from transport)",
          Xlab = "Year",
          Ylim = c(0,3),
          Legend = c("Sweden","synthetic Sweden"),
          Legend.position = "bottomright"
)
# Add line 
abline(v=1990,lty="dotted",lwd=2)
arrows(1987,1.0,1989,1.0,col="black",length=.1)	
Cex.set <- 1
text(1981,1.0,"VAT + Carbon tax",cex=Cex.set)
