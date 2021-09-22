require( "data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( mejor, peloton )

#veo que tiene el vector
jugadores

#hago que los 100 jugadores tiren 10 veces cada uno
vaciertos <- mapply(  ftirar, jugadores, 10 )


mejor_25 <- round(15000/(length(jugadores)*1.25+length(jugadores)*0.25*0.25),0)
mejor_20 <- round(15000/(length(jugadores)*1.2+length(jugadores)*0.2*0.2),0)
mejor_15 <- round(15000/(length(jugadores)*1.15+length(jugadores)*0.15*0.15),0)
mejor_10 <- round(15000/(length(jugadores)*1.1+length(jugadores)*0.1*0.1),0)

for(tiros_libres in c(mejor_10, mejor_15, mejor_20, mejor_25) )
{  
 
    for (corte in c(0.1 , 0.15 , 0.2 , 0.25))
    {
      primero_ganador <- 0  
    
      for (i in 1:10000)
      {
      vaciertos  <- mapply( ftirar, jugadores, tiros_libres ) 
      
      mejores_2davuelta  <-  order(vaciertos,decreasing = TRUE)[1:(length(vaciertos)*corte)] 
    
      aciertos_segunda  <- mapply( ftirar, jugadores[mejores_2davuelta], tiros_libres )
    
      mejores_3ravuelta  <-  order(aciertos_segunda,decreasing = TRUE)[1:(length(aciertos_segunda)*corte)] 
    
      aciertos_tercera  <- mapply( ftirar, jugadores[mejores_3ravuelta], tiros_libres )
    
      mejor <- which.max(aciertos_tercera)
    
      if( mejor == 1 )  primero_ganador <- primero_ganador + 1
      }
    
  cat( sum(length(jugadores), length(mejores_2davuelta), length(mejores_3ravuelta))*tiros_libres,
       corte, 
       tiros_libres,
       primero_ganador/10000, 
       "\n" )
    }
}  
##################################################################################
for(tiros_libres in c(10, 20, 50, 100, 200, 300, 400, 500, 600, 700, 1000 ) )
{  
  
  primero_ganador <- 0  
    
    for (i in 1:10000)
    {
      vaciertos  <- mapply( ftirar, jugadores, tiros_libres ) 
      
      mejores_2davuelta  <-  order(vaciertos,decreasing = TRUE)[1:(length(vaciertos)*0.1)] 
      
      aciertos_segunda  <- mapply( ftirar, jugadores[mejores_2davuelta], tiros_libres )
      
      mejor <- which.max(aciertos_segunda)
      
      if( mejor == 1 )  primero_ganador <- primero_ganador + 1
    }
    
    cat( sum(length(jugadores), length(mejores_2davuelta), length(mejores_3ravuelta))*tiros_libres,
         tiros_libres,
         primero_ganador/10000, 
         "\n" )
}  

###################################################################################
primero_ganador <- 0

for (i in 1:10000)
{
  vaciertos  <- mapply( ftirar, jugadores, 114 )  #cada jugador tira 128 tiros libres
  
  mejores_2davuelta  <-  order(vaciertos,decreasing = TRUE)[1:(length(vaciertos)*0.1)] 
  
  aciertos_segunda  <- mapply( ftirar, jugadores[mejores_2davuelta], 114 )
  
  mejores_3ravuelta  <-  order(aciertos_segunda,decreasing = TRUE)[1:(length(aciertos_segunda)*0.1)]
  
  aciertos_tercera  <- mapply( ftirar, jugadores[mejores_3ravuelta], 114 )
  
  mejor <- which.max(aciertos_tercera)
  
  if( mejor == 1 )  primero_ganador <- primero_ganador + 1
}
sum(length(jugadores), length(mejores_2davuelta), length(mejores_3ravuelta))*114
primero_ganador/10000
###################################################################################

for(  tiros_libres  in c(10, 20, 50, 100, 200, 300, 400, 500, 600, 700, 1000 ) )
{

  primero_ganador  <- 0

  for( i in 1:10000 )  #diez mil experimentos
  {
    vaciertos <- mapply( ftirar, jugadores, tiros_libres ) 
    mejor  <- which.max( vaciertos )
    if( mejor == 1 )  primero_ganador <- primero_ganador + 1
  }

  cat( tiros_libres, primero_ganador/10000, "\n" )
}

