funcion<-function(a,b,tol){
  error=0
  xa=a
  xb=b
  xmedio=(xa+xb)/2
  xanterior=0
  it=0
  bool=FALSE
  Errores<-(0)
  indice=0
  itMax=0
  v1=log(abs((xa-xb)/tol))          
  v2=(1/log(2)) 
  ErroresSignificativo<-c(0)
  j=0
  
  itMax=v1*v2
  
  if(sign( cos(1/xa)  ) ==  sign( cos(1/xmedio) ) ){
    
    bool=TRUE
  }
  
  if(bool==TRUE){
    
    cat("Son iguales los signos\n")
  }
  if(bool==FALSE){
    
    if(cos(1/xa)   *  cos(1/xmedio)  > 0){
      
      xanterior=xmedio
      xa=xmedio
      xmedio=(xmedio+xb)/2
      
      
    }else if(cos(1/xa)   *  cos(1/xmedio)  < 0){
      
      xanterior=xmedio
      xb=xmedio
      xmedio=(xa+xmedio)/2
      
    }
    
    error=abs((xmedio+xanterior)/xmedio)
    
    Errores[indice]=error
    
    
    while( error > tol && it < itMax){
      
      it=it+1
      indice=indice+1
      
      if( cos(1/xa)  *  cos(1/xmedio)  > 0){
        
        xa=xmedio
        xanterior=xmedio
        xmedio=(xmedio+xb)/2
      }
      if( cos(1/xa)  *  cos(1/xmedio)  < 0){
        
        xb=xmedio
        xanterior=xmedio
        xmedio=(xa+xmedio)/2
        
      }
      
      
      error=abs((xmedio+xanterior)/xmedio)
      Errores[indice]=error
      ErroresSignificativo[j]=error
      j=j+1
      
      
    }
    
    ErroresSignificativo[j]=0
    
    itrs=seq(1,it)
    tabla=data.frame(itrs,Errores)
    print(tabla)
    
    plot(x = Errores, y = ErroresSignificativo,type = 'o',main = "GRAFICA ERRORES",ylab = "Error i+1",xlab = "Error i", xlim=c(2,3),ylim=c(0,2.5),xaxp = c(2,3,10))
    
    cat("\n")
    cat("\n")
    cat("\n")
    cat("\n")
  }
  valoresAitken<-c(0)
  j=1
  indice=1
  longitud=length(Errores)-3
  it=1
  valor = Errores[indice+2] - ((Errores[indice+2] - Errores[indice+1]) * (Errores[indice+2] - Errores[indice+1])/ (Errores[indice+2] - (2*Errores[indice+1]) + Errores[+1] ))
  valoresAitken[j]=valor
  j=j+1
  indice=indice+1  
  while(longitud > 0 ){
    it=it+1
    valor = Errores[indice+2] - ((Errores[indice+2] - Errores[indice+1]) * (Errores[indice+2] - Errores[indice+1])/ (Errores[indice+2] - (2*Errores[indice+1]) + Errores[+1] ))
    valoresAitken[j]=valor
    j=j+1
    indice=indice+1
    longitud=longitud-1
  }
  
  itrs=seq(1,it)
  tabla=data.frame(itrs,valoresAitken)
  print(tabla)
  
}

funcion(0.002,2,10e-7)