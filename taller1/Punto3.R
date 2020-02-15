#3Parte A
logaritmo<- function(x){
  log (x+2)
}
sen<- function(x)
{
  sin(x)
}
tol = 0.00000001

func<- function(x)
{
  
  log(x+2)-sin(x)
}
metodo = uniroot(func, c(-1.8,-1), tol = 1e-9) 
plot(logaritmo, xlim = c(-10,10), ylim=c(-10,10), ylab = "f(x)", col = "blue")
par(new = TRUE)
plot(sen, xlim = c(-10,10), ylim=c(-10,10), ylab = "g(x)", col = "red")
plot(func, xlim = c(-10,10), ylim=c(-10,10), ylab = "f(x) - g(x)", col = "brown")
par(new = TRUE)
abline(0,0, col = "black")
a = -1.8
b = -1
arreglo = c(0)
arreglo[1] = a
arreglo[2] = b
i=3
E = c() 
E_1 = c()
while(abs(metodo$root-arreglo[i-2]) > tol){
  E[i-2]=abs(metodo$root-arreglo[i-2])
  arreglo[i]= arreglo[i-1]-((func(arreglo[i-1])*(arreglo[i-1]-arreglo[i-2]))/(func(arreglo[i-1])-func(arreglo[i-2])))
  i = i+1
}

i=1
while(i<length(E)+1){
  E_1[i]=E[i+1]
  i = i+1
}
plot(E,E_1, type="l")

numero_iteraciones =length(arreglo)
Met = c(0)
error = c(0)
error2 = c(0)
Met[1] = a
Met[2] = b
i=3
while(abs(metodo$root-Met[i-2]) > tol){
  error[i-2]=abs(metodo$root-arreglo[i-2])
  Met[i]= Met[i-1]-((func(Met[i-1]))*(Met[i-1]-Met[i-2])/(func(Met[i-1])-func(Met[i-2])))
  i = i+1
}
i=1
while(i<length(error)+1){
  error2[i]=error[i+1]
  i = i+1
}
plot(error,error2, type ="l",col ="blue")


#3Parte B
metodo<- function(x)
{
  #Utilizamos el valor del numero de euler
  a<-exp(1)
  resultadoA <- (a^x)-x-1
  return(resultadoA)
}

comprobacion<-function(y,multiplicidad)
{
  #Hallamos la raiz del la ecuacion
  p<-metodo(0)
  
  resultado = (y-p)^multiplicidad
  
  
  if(resultado != p)
  {
    cat("para F(",y,")","decimos que es un cero de multiplicidad ", multiplicidad)
  }
  else
  {
    
    cat("para F(",y,")","decimos que no es un cero de multiplicidad", multiplicidad)
  }
  
}
metodo(0)
comprobacion(0,2)
#B

#Metodo de newton


newton1 = function(f, fp, x0, tol, maxiter){
  k = 0
  # Imprimir estado
  cat("---------------------------------------------------------------------------\n")
  cat(formatC( c("x_k"," f(x_k)","Error est."), width = -20, format = "f", flag = " "), "\n")
  cat("---------------------------------------------------------------------------\n")
  repeat{
    correccion = f(x0)/fp(x0)
    x1 = x0 - correccion
    dx = abs(x1-x0)
    # Imprimir iteraciones
    cat(formatC( c(x1 ,f(x1), dx), digits=15, width = -15, format = "f", flag = " "), "\n")
    x0 = x1
    k = k+1
    # until
    if(dx <= tol || k > maxiter ) break;
  }
  cat("---------------------------------------------------------------------------\n")
  if(k > maxiter){
    cat("Se alcanzó el máximo número de iteraciones.\n")
    cat("k = ", k, "Estado: x = ", x1, "Error estimado <= ", correccion)
  } else {
    cat("k = ", k, " x = ", x1, " f(x) = ", f(x1), " Error estimado <= ", correccion) }
}
#Prueba
x0=1;  tol=1.e-5 ; N=100
v_a = 0 ; v_b = 3
f=function(x) exp(x)-(pi*x)
fp = function(x) exp(x)-pi
newton1(f,fp, x0, tol, N)