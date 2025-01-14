#Necesita para correr en Google Cloud
#16 GB de memoria RAM
#256 GB de espacio en el disco local

#8 vCPU

#clase_binaria1   1={BAJA+2}    0={BAJA+1,CONTINUA}
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

vendor <- "Google"
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



kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "671_lgb_binaria1"
karch_generacion  <- "./datasetsOri/paquete_premium_202011.csv"
karch_aplicacion  <- "./datasetsOri/paquete_premium_202101.csv"
kBO_iter    <-  150   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros. 
# !!! Un buen criterio ser?a: si en el mejor conjunto, alguno de los par?metros est? muy en los l?mites que le fij?, puedo ampliarle un poco el l?mite
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower= 0.01 , upper=    0.1), # podr?a poner 0.3 como upeer limit y va bien tb!
         makeNumericParam("feature_fraction", lower= 0.2  , upper=    1.0), # con qu? proporci?n de datos va a entrenar cada ?rbol (20% a 100%). es un param tomado del ranger!
         makeIntegerParam("min_data_in_leaf", lower= 0    , upper= 8000),
         makeIntegerParam("num_leaves",       lower=16L   , upper= 1024L), # 1024 hojas con un min_data_in_leaf tan alto no tiene mucho sentido (con 8000 obs en cada hoja, en un dataset de 240.000 datos, no podr?ahaber m?s de 30 hojas!)
         makeNumericParam("prob_corte",       lower= 0.020, upper=    0.035)
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

  gan  <- sum( (probs > PROB_CORTE  ) *
               ifelse( vlabels== 1, 48750, -1250 ) )

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

  param_basicos  <- list( objective= "binary", # binary es para clasificaci?n. Devuelve prob entre 0 y 1
                          metric= "custom", # determinamos nosotros la m?trica, que estar? relacionada con la ganancia. Si no pusiera custom, tomar?a log loss (gcia en t?rminos de informaci?n). Podr?a pedir que eval?e con ?rea bajo la curva ROC (se usa "AUC")
                          first_metric_only= TRUE,
                          boost_from_average= TRUE, # por default es TRUE (es para que tome el promedio como primer arbol)
                          feature_pre_filter= FALSE, # FALSE para que no haga 
                          verbosity= -100, # con -100 pedimos que no reporte el avance del proceso. A mayores nros m?s cantidad de reportes ir? dando el programa
                          seed= 100001,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo # si est?s aprendiendo algo que te aporta muy poco entonces no hagas el split!
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo  # penaliza el aprendizaje: el c?lculo de la m?trica de ganancia va a ser peor en lambda_l1 (para crecer "hay que pagar lambda_l1!)
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
  # el learning rate regula cu?ntos ?rboles sigo generando luego de haber alcanzado una gcia m?xima, antes de cortar el algoritmo
  # el learning rate es como el "tama?o" de los pasos que da para aprender
  param_completo  <- c( param_basicos, param_variable, x )

  # ac? llamamos a lightGBM con cross validation para que calcule la gcia en cada iteraci?n y poder optimizar los par?metros: early stopping, learning rate
  set.seed( 100001 )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )


  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ] # traigo el conjunto de poar?metros que result? en la mejor ganancia: "best_iter"

  ganancia_normalizada  <-  ganancia* kfolds  # extrapolamos la ganancia
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL #anulamos el early stopping, porque queremos que construya la misma cantidad de ?rboles que determinamos ?ptima en el cross validation

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

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )


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
if( vendor=="Google" ) { system( "sleep 10  &&  sudo shutdown -h now", wait=FALSE) }

#suicidio,  elimina la maquina virtual directamente
#system( "sleep 10  && 
#        export NAME=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/name -H 'Metadata-Flavor: Google') &&
#        export ZONE=$(curl -X GET http://metadata.google.internal/computeMetadata/v1/instance/zone -H 'Metadata-Flavor: Google') &&
#        gcloud --quiet compute instances delete $NAME --zone=$ZONE",
#        wait=FALSE )


quit( save="no" )


