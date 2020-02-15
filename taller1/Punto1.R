#1.b
Hornner <- function(x,cantidadCoe){
  total<-0
  for(i in 1:length(cantidadCoe))
  {
    total<-total * x + cantidadCoe[i] #mutiplica los coeficientes hasta que i llegue al tamaño del array
  }
  
  cat("el total mediante las iteraciones es:",total)

}
c<-c(1,1,1,1,1)
Hornner(5 ,c)

## el total mediante las iteraciones es: 781



#1.c
Horner <- function(x,cantidadCoe){
  totalIterativo<-0
  
  totalFormula <- ((x^51)-1)/(x-1) #calcula el valor mediante la formula
  
  for(i in 1:length(cantidadCoe))
  {
    totalIterativo<-totalIterativo * x + cantidadCoe[i] #mutiplica los coeficientes hasta que i llegue al tamaño del array
  }
  
  error<-totalIterativo-totalFormula #calcula el error hallando la diferencia
  
  cat("el total mediante las iteraciones es:",totalIterativo," y el resultado mendiante la formula es :", totalFormula,"\n el error que se produce en estas dos es:", error )

}
c<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1) #50 terminos como lo indica el problema
Horner(1.0001 ,c)
## el total mediante las iteraciones es: 51.12771  y el resultado mendiante la formula es : 51.12771 
##  el error que se produce en estas dos es: 7.81597e-14

#1.d
imprimir <-function(arra2){
  
  contador<-length(arra2)
  while (contador!=0){
    cat(arra2[contador])
    contador=contador-1
  }
}
codificar <- function(n,arra){
  
  int<-trunc(n)
 float<- (n-int)
 
cat("el numero",n, "en binario es:")
    
#CODIFICA LA PARTE ENTERA
 copyN<-int
 cont<-1
 arra2<-c()
 while(copyN>0){
   if((copyN%%2)==0)
   {
     arra2[cont]<-0
   }
   if((copyN%%2)==1){
      arra2[cont]<-1
      
      
   }
   cont<-cont+1
   copyN<-(copyN%/%2)
 }
  if(length(arra2)%%4!=0){
    
    while (length(arra2)%%4!=0) {
      arra2[cont]<-0
    cont<-cont+1
      }
  }
 
  imprimir(arra2)
  cat(",")
  #CODIFICA LA PARTE FLOTANTE
 if(float!=0.0){
  contadorf<-0
  copiaf<-float
  arra3<-c()
  while(contadorf!=11){
    copiaf<- copiaf*2
    enterof<-trunc(copiaf)
    residuof<-copiaf-enterof
    arra3[contadorf]<-enterof
    copiaf<-residuof
    contadorf<-contadorf+1
  }
  imprimir(arra3)
 }
}


codificar(3.1415,arra)
## el numero 3.1415 en binario es:0011,1000010010


#1.f
codificar <- function(n,arra){
  
  int<-trunc(n)
 float<- (n-int)
 
cat("el numero",n, "en binario es:")
    
#CODIFICA LA PARTE ENTERA
 copyN<-int
 cont<-1
 arra2<-c()
 while(copyN>0){
   if((copyN%%2)==0)
   {
     arra2[cont]<-0
   }
   if((copyN%%2)==1){
      arra2[cont]<-1
      
      
   }
   cont<-cont+1
   copyN<-(copyN%/%2)
 }
  if(length(arra2)%%4!=0){
    
    while (length(arra2)%%4!=0) {
      arra2[cont]<-0
    cont<-cont+1
      }
  }
 
  imprimir(arra2)
  cat(",")
  #CODIFICA LA PARTE FLOTANTE
 if(float!=0.0){
  contadorf<-0
  copiaf<-float
  arra3<-c()
  while(contadorf!=11){
    copiaf<- copiaf*2
    enterof<-trunc(copiaf)
    residuof<-copiaf-enterof
    arra3[contadorf]<-enterof
    copiaf<-residuof
    contadorf<-contadorf+1
  }
  imprimir(arra3)
 }
  cat("\n")
}
codificar(11.25,arra)
## el numero 11.25 en binario es:1011,0000000001
codificar(1.5,arra)
## el numero 1.5 en binario es:0001,0000000000
codificar(30.6,arra)
## el numero 30.6 en binario es:00011110,0011001100
codificar(99.9,arra)
## el numero 99.9 en binario es:01100011,1100110011


#1.g. ¿ Como se ajusta un numero binario inﬁnito en un numero ﬁnito de bits?

#Un numero binario infinito se ajusta mediante el metodo de truncamineto de bits lo que hace es recortar a un numero “n” de bits con el cual el numero sera representado, pero pierde bastante valor y a su vez se propaga el error en los calculos proximos que se haran con el numero.

#1.h. ¿Cual es la diferencia entre redondeo y recorte?

#La principal diferencia entre el redondeo y el recorte es que; Si queremos aproximar un numero por el metodo de REDONDEO a n cifras para esta aproximación se tiene en cuenta la cifra n+1, en este caso si es igual o mayor a 5 se incrementa la cifra numero n. a diferencia del RECORTE que solo tiene en cuenta las n cifras que la persona quiere dejar y no importa cual sea la cifra n+1.

#Esto tiene una diferencia muy grande ya que con el redondeo estamos incluyendo mas precision a nuestro numero y en el recorte podemos perder valor importante por ello debemos conocer muy bien sus diferencias y en que casos implementar cada tecnica, segun sea el caso

#1.i. Idique el numero de punto ﬂotante (IEEE) de precision doble asociado a x, el cual se denota como fl(x); para x(0.4) 4*10^1 1.k. Veriﬁcar si el tipo de datos basico de R y Python es de precision doble IEEE y Revisar en R y Phython el format long

#Si el dato basico de r es de precision doble de IEEE se puede configuarar para que de la cantidad de decimales que el usuario prefiera dejear en la mantisa asi modificando a su ves el exponente.

#El format long es un fromato de dato que permite almacenar de -2147483648 a 2147483647, esto le permite al usuario realizar operaciones con alta precision a la hora de no perder decimales.