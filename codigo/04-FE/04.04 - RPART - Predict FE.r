#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

library("data.table")
library("rlist")
library("yaml")

library("rpart")
library("parallel")

#paquetes necesarios para la Bayesian Optimization
library("DiceKriging")
library("mlrMBO")

#para poder usarlo en la PC y en la nube
switch ( Sys.info()[['sysname']],
         Windows = { directory.root   <-  "C:/Users/Jonathan/Desktop/MCD - Laboratorio/7.Labo_1" },   #Microsoft Windows
         Darwin  = { directory.root   <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root   <-  "~/buckets/b1/crudo/" }  #Entorno Google Cloud
       )
#defino la carpeta donde trabajo
setwd( directory.root )

kexperimento  <- 1000

kscript           <- "04.01 - RPART - Predict"
karch_generacion  <- "./datasets/paquete_premium_202011_ext.csv"
karch_aplicacion  <- "./datasets/paquete_premium_202101_ext.csv"
#karch_generacion  <- "./datasetsOri/paquete_premium_202011.csv"
#karch_aplicacion  <- "./datasetsOri/paquete_premium_202101.csv"

klog        <- paste0("./work/E",  kexperimento, "_rpart_log.txt" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_rpart_kaggle_" )

ksemilla_azar  <- 100001

GLOBAL_iteracion <- 99999

#Leo hiperparametros
hiperparams <- fread(klog)



#Muestro los mejores 3 resultados
hiperparams[order(-ganancia)][1:3]

#Selecciono el primero
opt_param = hiperparams[order(-ganancia)][1]

#ME quedo solo con los parametros para pasarle al algoritmo
opt_param[,c('fecha','xval_folds','ganancia'):=NULL]

opt_param

#cargo los datasets
dataset  <- fread(karch_generacion)   #donde entreno
dapply  <- fread(karch_aplicacion)    #donde aplico el modelo

#Ejemplo FE
#dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
#dataset[ , m_cacc        := rowSums( cbind( mcaja_ahorro,  mcuenta_corriente) , na.rm=TRUE ) ]
#dataset[ , mvr_cacc       := mv_mconsumototal  / m_cacc ]

#dapply[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
#dapply[ , m_cacc        := rowSums( cbind( mcaja_ahorro,  mcuenta_corriente) , na.rm=TRUE ) ]
#dapply[ , mvr_cacc       := mv_mconsumototal  / m_cacc ]



#------------------------------------------------------------------------------
#funcion para particionar, es la que Andres reemplaza con caret
particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, param, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a 5 si posee Linux o Mac OS

  data[ , fold := NULL ]
  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo la ganancia
}

#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia  <- function( x )
{

   xval_folds  <- 5
   ganancia  <-  ArbolesCrossValidation( dataset, param=x, qfolds= xval_folds, pagrupa="clase_ternaria", semilla=ksemilla_azar )

    
     modelo  <- rpart("clase_ternaria ~ .",
                      data= dataset,
                      xval= 0,
                      control= x )

     #genero el vector con la prediccion, la probabilidad de ser positivo
     prediccion  <- predict( modelo, dapply)

     prob_baja2  <- prediccion[, "BAJA+2"]
     Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

     entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )

     #genero el archivo para Kaggle
     fwrite( entrega, 
             file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
             sep=  "," )
   return( ganancia )
}
#------------------------------------------------------------------------------

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .", 
                   data= data[ fold != fold_test, ],
                   xval= 0,
                   control= param )

  #aplico el modelo a los datos de testing, fold==2
  prediccion  <- predict( modelo, data[ fold==fold_test, ], type = "prob")

  prob_baja2  <- prediccion[, "BAJA+2"]

  ganancia_testing  <- sum(  data[ fold==fold_test ][ prob_baja2 >0.025,  ifelse( clase_ternaria=="BAJA+2", 48750, -1250 ) ] )

  return( ganancia_testing )
}

#Estimo la ganancia
EstimarGanancia( opt_param )

#Reentreno solo para ver la importancia de variables
modelo  <- rpart("clase_ternaria ~ .", 
                   data= dataset,
                   xval= 0,
                   control= opt_param )

#Grafico
barplot(modelo$variable[1:15],las=2)

#Grabo en archivo la importancia
fwrite(data.table( names = names(modelo$variable.importance), importance = modelo$variable.importance),'./work/importance.csv')


