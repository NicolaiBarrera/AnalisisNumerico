PolinomioTaylor <- function(exponente,cifras){ #la funcion recibe un exponente y las cifras que quiero mostrar (5)
  iterador<-cifras-1
  sumaTotal <-1
  
  while(iterador>0){
    sumaTotal <- 1+(exponente*sumaTotal)/iterador
    iterador<-iterador-1
  }
  resultado <- signif(sumaTotal,digits=cifras) # (signif)acota el resultado a la cantidad de cifras que necesita
  cat("La aproximancion de e^0.5 es:",resultado)
}

PolinomioTaylor(0.5,5)

## La aproximancion de e^0.5 es: 1.6484