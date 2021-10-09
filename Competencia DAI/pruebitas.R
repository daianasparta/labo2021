rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("tidyverse")
require("dplyr")

setwd("C:/Users/Jonathan/Desktop/MCD - Laboratorio/7.Labo_1")

dataset <- fread("./datasets/datasets_dataset_pruebas_34meses.csv")

dataset[ ,  Master_Fvencimiento:= Master_Fvencimiento *(-1)]
dataset[ ,  Master_mpagospesos := Master_mpagospesos *(-1)]
dataset[ ,  Visa_Fvencimiento := Visa_Fvencimiento *(-1)]
dataset[ ,  Visa_mpagospesos := Visa_mpagospesos *(-1)]


#variables en pesos a pesos constantes a precios de enero 2021
campos_en_pesos <- c("mrentabilidad", "mrentabilidad_annual", "mcomisiones", 
                     "mactivos_margen", "mpasivos_margen", "mcuenta_corriente_adicional", 
                     "mcuenta_corriente", "mcaja_ahorro", "mcaja_ahorro_adicional", 
                     "mcaja_ahorro_dolares", "mdescubierto_preacordado", "mcuentas_saldo", 
                     "mautoservicio", "mtarjeta_visa_consumo", "mtarjeta_master_consumo", 
                     "mprestamos_personales", "mprestamos_prendarios", "mprestamos_hipotecarios",
                     "mplazo_fijo_dolares", "mplazo_fijo_pesos", "minversion1_pesos", 
                     "minversion1_dolares", "minversion2", "mpayroll", "mpayroll2", 
                     "mcuenta_debitos_automaticos", "mttarjeta_visa_debitos_automaticos", 
                     "mttarjeta_master_debitos_automaticos", "mpagodeservicios", 
                     "mpagomiscuentas", "mcajeros_propios_descuentos", "mtarjeta_visa_descuentos",
                     "mtarjeta_master_descuentos", "mcomisiones_mantenimiento", "mcomisiones_otras", 
                     "mforex_buy", "mforex_sell", "mtransferencias_recibidas", "mtransferencias_emitidas",
                     "mextraccion_autoservicio", "mcheques_depositados", "mcheques_emitidos",
                     "mcheques_depositados_rechazados", "mcheques_emitidos_rechazados", "matm",
                     "matm_other", "Master_mfinanciacion_limite", "Master_msaldototal", "Master_msaldopesos",
                     "Master_msaldodolares", "Master_mconsumospesos", "Master_mconsumosdolares",
                     "Master_mlimitecompra", "Master_madelantopesos", "Master_madelantodolares", 
                     "Master_mpagado", "Master_mpagospesos", "Master_mpagosdolares", "Master_mconsumototal",
                     "Master_mpagominimo", "Visa_mfinanciacion_limite", "Visa_msaldototal", "Visa_msaldopesos",
                     "Visa_msaldodolares", "Visa_mconsumospesos", "Visa_mconsumosdolares", "Visa_mlimitecompra", 
                     "Visa_madelantopesos", "Visa_madelantodolares", "Visa_mpagado", "Visa_mpagospesos",
                     "Visa_mpagosdolares", "Visa_mconsumototal", "Visa_mpagominimo")
 

#agrego al dataset la columna de ipc:
ipc <- fread("./datasets/IPC base enero 2021.csv")


dataset <- dataset[ipc, on = .(foto_mes = anio_mes)]

dataset[, paste0(campos_en_pesos, "_real_ene21") := lapply(.SD, function(.name) dataset[, ..campos_en_pesos] * 2),
         .SDcols= campos_en_pesos ]

dataset[, for (var in campos_en_pesos) 
                paste0(var, "_real_ene21") := (var / `ipc_base_enero 21` * 100)]


campos_lags <-  setdiff( colnames(dataset) , c("numero_de_cliente", "foto_mes", "clase_ternaria") )

dataset[ , paste0( campos_lags, "_lag1") := shift(.SD, 1, NA, "lag"), 
         by=numero_de_cliente, 
         .SDcols= campos_lags ]    

dataset[ , paste0( campos_lags, "_var_im") := (.SD / shift(.SD, 1, NA, "lag")-1)*100, 
         by=numero_de_cliente, 
         .SDcols= campos_lags ]    

#Variacion respecto al maximo de los ultimos 3 meses

dataset[ , paste0( campos_lags, "_var_max_3m") := (.SD / shift(frollapply(.SD, 3,"max"), 1, NA, "lag")-1), 
         by=numero_de_cliente, 
         .SDcols= campos_lags ]

dataset[ , paste0( campos_lags, "_var_max_3mNUEVA") := .SD / shift(frollapply(.SD, FUN = "max", n=3, na.rm=TRUE, align="right"), 1, NA, "lag")-1, 
         by=numero_de_cliente, 
         .SDcols= campos_lags ]

dataset[ , paste0( campos_lags, "_var_max_3mGUSTAVO") := .SD/ frollapply(x=.SD, FUN="max", n=3, na.rm=TRUE, align="right"), 
         by=numero_de_cliente, 
         .SDcols= campos_lags ]





select(dataset, foto_mes, 
       Visa_mpagospesos, 
       #Visa_mpagospesos_lag1, 
       #Visa_mpagospesos_var_im,
       Visa_mpagospesos_var_max_3m,
       Visa_mpagospesos_var_max_3mNUEVA)


select(dataset, foto_mes,
       EZE_c_seguros,
       cseguro_vida, 
       cseguro_auto, 
       cseguro_vivienda, 
       cseguro_accidentes_personales)
