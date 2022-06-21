
### primer listado ###

#Expediente 1 
#Descripción 2
#Estado expediente 3
#Fecha de caratulación 5
#Repartición actual 44
#Sector actual 43
#Usuario actual 42
#Fecha del último pase 45
#Días desde el último movimiento (este dato hay que calcularlo) calcular
#Días desde inicio del trámite (hay que calcularlo) calcular


rm(list=ls())
library(pacman)
p_load(readxl,tidyverse,lubridate,R.utils,poorman,data.table,janitor,zoo)

## levanto la base de viáticos ##
## esto quita estas reparticiones de la base de viáticos
repa.off<-c("DAFYP#MS - Dirección de administración financiera y presupuestaria",
           "DGAGN#MI - Dirección general del archivo general de la nación")

tabla_viat <- fread("../Viaticos/detalle_viaticos.csv", data.table = FALSE, encoding = "Latin-1")%>% 
  clean_names() %>% 
  filter(estado=="Abierto") %>% 
  filter(!(reparticion_actual %in% repa.off)) %>% 
  select(1,2,3,5,44,43,42,45)

## leveanto fecha inicio viático ##

tabla_fecha_inicio_viaje <- fread("../Viaticos/Viáticos Nacionales del Ministerio de Salud de la Nación.csv",data.table = FALSE, encoding = "Latin-1") %>%  
  clean_names() %>% 
  select (2,8) %>% 
  rename(expediente="numero_de_expediente") %>% 
  distinct()

## reemplazo perdidos por NA en variablle expediente

tabla_fecha_inicio_viaje$expediente[tabla_fecha_inicio_viaje$expediente==""] <- NA

## junto base viáticos con fecha incio viaje

tabla_viat_final <- tabla_viat %>% 
  left_join (tabla_fecha_inicio_viaje, by=c("expediente")) 

## me quedo con la primera parte de las fechas ##
tabla_viat_final$fecha_de_partida<- word(tabla_viat_final$fecha_de_partida, 1, sep = fixed(" "))

## paso a formato date las fechas de arriba ##
tabla_viat_final$fecha_de_partida<-dmy(tabla_viat_final$fecha_de_partida)

## me quedo con la primera parte de las fechas ##
tabla_viat_final$fecha_de_caratulacion<- word(tabla_viat_final$fecha_de_caratulacion, 1, sep = fixed(" "))
tabla_viat_final$fecha_de_ultimo_pase<- word(tabla_viat_final$fecha_de_ultimo_pase, 1, sep = fixed(" "))

## paso a formato date las fechas de arriba ##
tabla_viat_final$fecha_de_caratulacion<-dmy(tabla_viat_final$fecha_de_caratulacion)
tabla_viat_final$fecha_de_ultimo_pase<-dmy(tabla_viat_final$fecha_de_ultimo_pase)

## calculo días desde la trmitación último pase y dias retraso ##
hoy <- as.Date(Sys.Date(), format="%d/%m/%y")

tabla_viat_final$`Días desde el inicio`<-hoy - 
  as.Date(tabla_viat_final$fecha_de_caratulacion, format="%d/%m/%y")

tabla_viat_final$`Días desde último pase`<-hoy - 
  as.Date(tabla_viat_final$fecha_de_ultimo_pase, format="%d/%m/%y")

tabla_viat_final$`Días de anticipo - retraso`<-tabla_viat_final$fecha_de_partida - 
  as.Date(tabla_viat_final$fecha_de_caratulacion, format="%d/%m/%y")

## paso a numéric las variables de días que están datediff
tabla_viat_final$`Días desde el inicio` <- as.numeric(tabla_viat_final$`Días desde el inicio`)
tabla_viat_final$`Días desde último pase` <- as.numeric(tabla_viat_final$`Días desde último pase`)
tabla_viat_final$`Días de anticipo - retraso` <- as.numeric(tabla_viat_final$`Días de anticipo - retraso`)

## genero una variable que sea reintegro u anticipo

names(tabla_viat_final)
tabla_viat_final<-tabla_viat_final %>% 
mutate(`Tipo de solicitud`=ifelse(`Días de anticipo - retraso`>0,"Anticipo - Solicitud","Reintegro"))

## nombre a las variablesikkkkkk

names(tabla_viat_final)<-c  ("Expediente",	
                      "Descripción"	,
                      "Estado expediente",	
                      "Fecha caratulación",
                      "Repartición Actual",
                      "Sector Actual",
                      "Usuario actual",
                      "Fecha desde último pase",
                      "Fecha inicio viaje",
                      "Días desde la caratulación",
                      "Días desde último pase",
                      "Días de anticipo - retraso",
                      "Tipo de solicitud")

## agrupamos por repatición y sector

library(tidyverse)
tabla_viat <- tabla_viat %>% 
  mutate_if(is.Date,as.numeric)

dap_dcyt<-c("DAP#MS - Dirección de administración de personal",
"DCYT#MS - Dirección de contabilidad y tesorería")

tabla.resumen.via.repa.dap.cit <- tabla_viat_final %>%
  group_by(`Sector Actual`,`Repartición Actual`) %>% 
  summarise(`Cantidad de expedientes`=n(),
      `Promedio de días desde la creación documento`=(round(mean(`Días desde la caratulación`,na.rm = TRUE))),       
      `Promedio de días desde el últimos pase`=(round(mean(`Días desde último pase`,na.rm = TRUE)))) %>% 
  filter(`Repartición Actual`%in% dap_dcyt) %>% 
  ungroup()

tabla.resumen.via.repa.resto <- tabla_viat %>%
  group_by(`Sector Actual`,`Repartición Actual`) %>% 
  summarise(`Cantidad de expedientes`=n(),
            `Promedio de días desde la creación documento`=(round(mean(`Días desde la caratulación`,na.rm = TRUE))),       
            `Promedio de días desde el últimos pase`=(round(mean(`Días desde último pase`,na.rm = TRUE)))) %>% 
  filter(!(`Repartición Actual`%in% dap_dcyt)) %>% 
  ungroup()

tabla.resumen.via.repa.resto$`Promedio de días desde el últimos pase`<- as.numeric(tabla.resumen.via.repa.resto$`Promedio de días desde el últimos pase`, units="days")
tabla.resumen.via.repa.resto$`Promedio de días desde la creación documento`<- as.numeric(tabla.resumen.via.repa.resto$`Promedio de días desde la creación documento`, units="days")
class(tabla.resumen.via.repa.resto$`Promedio de días desde la creación documento`)


