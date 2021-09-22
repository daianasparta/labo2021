#limpio la memoria
rm(list=ls())
gc()

#cargo librerias
library("data.table")
library("ROCR")
library("rpart")
library("rpart.plot")
library("caret")

#Parametros entrada

#kcarpeta_datasets    <- "../input/laboratorio-de-implementacion-i-2021/"   #KAGGLE
kcarpeta_datasets    <- "C:/Users/Jonathan/Desktop/MCD - Laboratorio/7.Labo_1/datasetsOri"                          #VM o Ubuntu

#Archivo con datos etiquetados para entrenamiento
karchivo_entrada      <-  paste0(kcarpeta_datasets, "/paquete_premium_202011.csv")

#Archivo con datos sin etiquetar para generar la predicción
karchivo_predecir      <-  paste0(kcarpeta_datasets, "/paquete_premium_202101.csv")

#Formato para submit en Kaggle
#karchivo_score      <-  "../input/uamds2020ldi1f1/Sample_201910_Fase_I.txt"

#Separador de campos en archivos
kcampos_separador     <-  "\t"

#Campo que identifica las muestras
kcampo_id             <-  "numero_de_cliente"

#Campo que contiene la clase a estimar
kclase_nomcampo       <-  "clase_ternaria"

#Valor de interés
kclase_valor_positivo <-  "BAJA+2"

#Campos a borrar para el entrenamiento
kcampos_a_borrar      <-  c(kcampo_id,kclase_nomcampo,"foto_mes")

#Campo que contendrá a la variable objetivo generada
kobjetivo             <-  "clase"

#Identificación del modelo
kmodelo               <-  "02-RPART"

#Ganancia por TP
kTPGain               <-  48750

#Pérdida por FP
kFPGain               <-  -1250

kSeed <- 130007


#Establezco semilla aleatoria
set.seed(kSeed)


#cargo los datos
dataset <- fread(karchivo_entrada)


#Para hacer pruebas rapidas puedo reducir el dataset a una fraccion
#subsample <- sample(1:nrow(dataset), .1 * nrow(dataset))
#dataset <- dataset[subsample,]


#Genero la clase
dataset[,(kobjetivo) := as.integer(dataset[,..kclase_nomcampo] == kclase_valor_positivo)] 

# NOTAS
# Los dos puntos en ..kclase_nomcampo buscan dentro de las variables del data.table a la que tenga el nombre asignado a kclase_nomcampo 
#(tb se podría haber usado dataset[,"clase_ternaria"] o dataset[,.(clase_ternaria)]. En todos los casos devuelve la columna del 
# dataset que se llama clase_ternaria. Dato: .() es un alias de list().
# Hacer dataset[,clase_ternaria] devuelve un vector (no una columna).

#Limpio campos a borrar
dataset[ ,  (kcampos_a_borrar) := NULL    ] 

#Completo missings
dataset[is.na(dataset)] <- 0

# generacion del modelo
formula  <-  formula(paste(kobjetivo, "~ ."))

#Genero los 5 folds
folds <- createFolds(dataset$clase, 5, returnTrain = TRUE)



set.seed(1345)
#Genero los 5 folds
folds <- createFolds(dataset$clase, 5, returnTrain = TRUE)

#Inicializo ganancia
ganancia_total <- 0


for (fold in c(1,2,3,4,5))
{
  
  #Selecciono los registros train/test para cada fold
  train <- dataset[unlist(folds[paste0('Fold',fold)]),] # unlist: para transformar una lista en un vector
  test <- dataset[-unlist(folds[paste0('Fold',fold)]),]
  
  #Entreno el modelo
  modelo   <-  rpart(formula,   data = train, maxdepth = 5,  cp=0,   xval=0)   
  
  #Aplico al set de test
  predicted <- predict(modelo, test) # predicted values
  
  #Calculo ganancia sobre dataset de pruebas
  test$score <- predicted
  test$Predicted <- test$score > 0.025
  
  #calculo la ganancia de incentivar cada caso
  test[  , ganancia:= kFPGain ] #-1250 # creamos la variable "ganancia" dentro del dataset test
  test[ clase==1,   ganancia:= kTPGain] #48750 # cambiamos la ganancia para los casos en que clase ==1 (los que predijimos que son BAJA+2)
  
  #Determino ganancia
  print(paste("La Ganancia Fold:",fold, 5*sum(test$Predicted*test$ganancia)))
  ganancia_total <- ganancia_total + sum(test$Predicted*test$ganancia)   
  
}
print(paste("La Ganancia Total:",ganancia_total))

#funcion que entrena/evalua ganancia para un fold
calculo_ganancia <-function(){
  modelo   <-  rpart(formula,   data = train, maxdepth = 5,  cp=0,   xval=5)   
  
  predicted <- predict(modelo, test) # predicted values
  
  #Calculo ganancia sobre dataset de pruebas
  test$score <- predicted
  test$Predicted <- test$score > 0.025
  
  #calculo la ganancia de incentivar cada caso
  test[  , ganancia:= kFPGain ] #-1250
  test[ clase==1,   ganancia:= kTPGain] #48750
  
  return(sum(test$Predicted*test$ganancia))
}




#Numero de semilla que estoy probando
i <- 0

#Inicializo ganancias CV
ganancia_total_cv <- list()

#Inicializo ganancias Train/Test
ganancia_total_tt <- list()


for (seed in c(21023, 39761,20707,2049,7187,7207,9436519, 30689))
{
  i <- i + 1
  
  #Fija semilla    
  set.seed(seed)
  
  ##### 5 Fold cross Validation ######
  #Genero folds
  folds <- createFolds(dataset$clase, 5, returnTrain = TRUE)
  ganancia_total_cv[[i]] <- 0
  for (fold in c(1,2,3,4,5))
  {
    #train_rows <- sample(1:nrow(dataset), .66 * nrow(dataset))
    train <- dataset[unlist(folds[paste0('Fold',fold)]),]
    test <- dataset[-unlist(folds[paste0('Fold',fold)]),]
    
    #Determino ganancia
    ganancia_total_cv[[i]]<- ganancia_total_cv[[i]] + calculo_ganancia()
    
  }
  #######
  
  train_rows <- createDataPartition(dataset$clase, p = .66, list = FALSE)
  ganancia_total_tt[[i]] <- 0
  #train_rows <- sample(1:nrow(dataset), .66 * nrow(dataset))
  train <- dataset[train_rows,]
  test <- dataset[-train_rows,]
  #Determino ganancia
  ganancia_total_tt[[i]]<- ganancia_total_tt[[i]] + 3*calculo_ganancia()
  
}


ganancia_total_cv

ganancia_total_tt