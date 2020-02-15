raiz <- function(numero, valor, tolerancia){ #La funcion recibe el numero a calcular
                                            #junto con un valor inicial y el error que se puede permitir
  resul<-((0.5)*(valor+(numero/valor))) #valor aproximado del calculo raiz de 7
 
 
  while(abs(valor-resul) > tolerancia) #calculando el valor hasta que no exeda el error permitido
  {
    valor<- resul
    resul<- (0.5)*(valor+(numero/valor))
   
  }
  return(cat("La raiz da:", resul, ", el error permitido es de:", tolerancia))
}

raiz(7,3,0.000001)

## La raiz da: 2.645751 , el error permitido es de: 1e-06