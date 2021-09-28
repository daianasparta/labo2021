#Necesita para correr en Google Cloud
#16 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#clase_binaria1   1={BAJA+2,BAJA+1}    0={CONTINUA}
#Optimizacion Bayesiana de hiperparametros de  lightgbm
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

vendor <- NA
if( Sys.info()[['sysname']]== "Linux" ) vendor  <- system("sudo dmidecode -s bios-vendor", intern = TRUE)

#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <- ifelse( vendor=="Google", 
                                                "~/buckets/b1/",             #Google Cloud
                                                "~/buckets/b1/crudo/" ) }    #Su propio Linux
)
#defino la carpeta donde trabajo
setwd( directory.root )

#-------------------------------------------------------------------------------
# FEATURE ENGINEERING
#-------------------------------------------------------------------------------
EnriquecerDataset <- function( dataset , arch_destino )
{
  columnas_originales <-  copy(colnames( dataset ))
  
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas
  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  
  #FE sobre mismo mes-------------------------------------------------------------  
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]
  
  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]
  
  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]
  
  
  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]
  
  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]
  
  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]
  
  dataset[ , mv_mconsumototal  := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , m_cacc    := rowSums( cbind( mcaja_ahorro,  mcuenta_corriente) , na.rm=TRUE ) ]
  dataset[ , mvr_cacc   := mv_mconsumototal  / m_cacc ]
  dataset[ , mvr_limite   := mv_mconsumototal  / (Master_mlimitecompra + Visa_mlimitecompra) ]
  
  # Debitos automaticos
  dataset[ , c_debitos    := rowSums( cbind( ccuenta_debitos_automaticos,ctarjeta_master_debitos_automaticos,  ctarjeta_visa_debitos_automaticos) , na.rm=TRUE ) ]
  dataset[ , m_debitos    := rowSums( cbind( mcuenta_debitos_automaticos,mttarjeta_master_debitos_automaticos,  mttarjeta_visa_debitos_automaticos) , na.rm=TRUE ) ]
  
  # Prestamos y seguros
  dataset[ , EZE_m_prestamos    := rowSums( cbind(mprestamos_personales, mprestamos_prendarios, mprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , EZE_c_prestamos    := rowSums( cbind(cprestamos_personales, cprestamos_prendarios, cprestamos_hipotecarios) , na.rm=TRUE ) ]
  dataset[ , EZE_c_seguros    := rowSums( cbind(cseguro_vida, cseguro_auto, cseguro_vivienda, cseguro_accidentes_personales) , na.rm=TRUE ) ]
  
  # Rentabilidad sobre promociones
  dataset[ , SEB_mmg_r_promociones :=  mrentabilidad_annual / (mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos) ]
  
  #FE combinacion de meses------------------------------------------------------  
  campos_lags <-  setdiff( colnames(dataset) , c("numero_de_cliente", "clase_ternaria") )
  
  #ordeno primero y luego creo los LAGS de un mes para cada variable
  setorderv( dataset, c("numero_de_cliente","foto_mes") ) 
  
  dataset[ , paste0( campos_lags, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by=numero_de_cliente, 
           .SDcols= campos_lags ]    
  
  dataset[ , paste0( campos_lags, "var_im") := .SD / shift(.SD, 1, NA, "lag")*100, 
           by=numero_de_cliente, 
           .SDcols= campos_lags ]    
  
  #Variacion respecto al maximo de los ultimos 3 meses
  
  dataset[ , paste0( campos_lags, "var_max_3m") := .SD / pmax(shift(.SD, c(1,2,3), NA, "lag"))*100, 
           by=numero_de_cliente, 
           .SDcols= campos_lags ]  
  
  #SEGURIDAD--------------------------------------------------------------------
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }
  
  
  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }
  
  #FIN de la seccion donde se deben hacer cambios con variables nuevas
  
  
  
  columnas_extendidas <-  copy( setdiff(  colnames(dataset), columnas_originales ) )
  
  #grabo con nombre extendido
  fwrite( dataset,
          file=arch_destino,
          sep= "," )
}
#------------------------------------------------------------------------------

dir.create( "./datasets/" )


#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset  <- fread("./datasetsOri/paquete_premium.csv.gz")

EnriquecerDataset( dataset, "./datasets/paquete_premium_lag.csv.gz" )

#fin feature engineering
#-------------------------------------------------------------------------------


kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "672_lgb_binaria2"
karch_generacion  <- "./datasetsOri/paquete_premium_202011.csv"
karch_aplicacion  <- "./datasetsOri/paquete_premium_202101.csv"
kBO_iter    <-  150   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",    lower= 0.01 , upper=    0.1),
  makeNumericParam("feature_fraction", lower= 0.2  , upper=    1.0),
  makeIntegerParam("min_data_in_leaf", lower= 0    , upper= 8000),
  makeIntegerParam("num_leaves",       lower=16L   , upper= 1024L),
  makeNumericParam("prob_corte",       lower= 0.020, upper=    0.055)
)

campos_malos  <- c( "mpasivos_margen" )   #aqui se deben cargar todos los campos culpables del Data Drifting

ksemilla_azar  <- 100001  #Aqui poner la propia semilla
#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )
  
  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento
  
  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly
  
  return( experimento_actual )
}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------

PROB_CORTE  <- 0.025

fganancia_logistic_lightgbm   <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")
  
  #aqui esta el inmoral uso de los pesos para calcular la ganancia correcta
  gan  <- sum( (probs > PROB_CORTE  ) *
                 ifelse( vlabels== 1 & vpesos > 1, 48750, -1250 ) )
  
  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  
  gc()
  PROB_CORTE <<- x$prob_corte   #asigno la variable global
  
  kfolds  <- 5   # cantidad de folds para cross validation
  
  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= 100001,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
  )
  
  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
  
  param_completo  <- c( param_basicos, param_variable, x )
  
  set.seed( 100001 )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
  )
  
  
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  
  ganancia_normalizada  <-  ganancia* kfolds  
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra
  
  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL
  
  #si tengo una ganancia superadora, genero el archivo para Kaggle
  if(  ganancia > GLOBAL_ganancia_max )
  {
    GLOBAL_ganancia_max  <<- ganancia  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-
    
    set.seed(ksemilla_azar)
    
    modelo  <- lightgbm( data= dtrain,
                         param= param_completo,
                         verbose= -100
    )
    
    #calculo la importancia de variables
    tb_importancia  <- lgb.importance( model= modelo )
    fwrite( tb_importancia, 
            file= paste0(kimp, "imp_", GLOBAL_iteracion, ".txt"),
            sep="\t" )
    
    prediccion  <- predict( modelo, data.matrix( dapply[  , campos_buenos, with=FALSE]) )
    
    Predicted  <- as.integer( prediccion > x$prob_corte )
    
    entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                     "Predicted"= Predicted)  )
    
    #genero el archivo para Kaggle
    fwrite( entrega, 
            file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
            sep= "," )
  }
  
  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  loguear( xx,  arch= klog )
  
  return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
kbayesiana  <- paste0("./work/E",  kexperimento, "_", kscript, ".RDATA" )
klog        <- paste0("./work/E",  kexperimento, "_", kscript, ".txt" )
kimp        <- paste0("./work/E",  kexperimento, "_", kscript, "_" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_", kscript, "_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog)
  GLOBAL_iteracion  <- nrow( tabla_log ) -1
  GLOBAL_ganancia_max  <- tabla_log[ , max(ganancia) ]
}


#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread(karch_generacion)

#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]



#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
#uso el weight como un truco ESPANTOSO para saber la clase real
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01,
                        weight=  dataset[ , ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )


#cargo los datos donde voy a aplicar el modelo
dapply  <- fread(karch_aplicacion, stringsAsFactors= TRUE) #leo los datos donde voy a aplicar el modelo

#Aqui comienza la configuracion de la Bayesian Optimization


funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar, #la funcion que voy a maximizar
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,     #definido al comienzo del programa
  has.simple.signature = FALSE   #paso los parametros en una lista
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}



#apagado de la maquina virtual, pero NO se borra
if( vendor =="Google" ) { system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE) }

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


quit( save="no" )


