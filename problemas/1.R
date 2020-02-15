

redondear <- function (numero, dig){ #Recibe como parametro Numero a redondear y digitos deseados
  contador=0
  while(numero>1){ #divide y cuenta cuantas divisiones
    numero<-numero/10
    contador=contador+1
  } 
  
  E<-(numero-trunc(numero*10^4)/10^4)# obtiene el numero redondeado
  
  izq<-1*10^(contador-dig)
  der<-1*10^(contador-dig)
  cat("El error es de ",izq,"<",E,"Y",E,"<",der)
}
redondear(536.78,4)

## El error es de  0.1 < 8e-05 Y 8e-05 < 0.1