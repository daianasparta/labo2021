rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

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


dataset  <- fread("./datasetsOri/paquete_premium.csv.gz")
dataset1 <- dataset[numero_de_cliente == 4572266,]

fwrite (dataset1, 
        file = "./datasets/dataset_pruebas_34meses.csv",
        sep = "\t")
