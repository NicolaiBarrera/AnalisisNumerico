library(PolynomF)

require(PolynomF)


x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)
y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3) # se definen los puntos que seran graficados en la grafica
plot(x,y, pch=19, cex=0.5, col = "red", asp=1) 
i= 1
Minimos = i
Maximos = i+1
bool = 0
cont = 1

repeat
{
  m = (y[i+1]-y[i])/(x[i+1]-x[i])# 
  cont = cont +1
  if (i == 1 && m > 0)
  {
    bool = 1}
  else if(i == 1 && m < 0)
  {
    bool = 0}
  if (m < 0)
  {
    if (bool == 0)
    {
      j = i + 1
      Maximos= j
      i = j
    }
    else 
    {
      
      datx = x[Minimos:Maximos]; daty = y[Minimos:Maximos]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "Minimos", Minimos, "Maximos", Maximos, "cont", cont-1,"<0\n")
      curve(polyAjuste,from=x[Minimos],to=x[Maximos],add=T, lwd=1,col="blue")
      Minimos = Maximos
      cont = 1
      bool = 0
      i = Maximos
      j = i + 1
      Maximos= j
    }
  }
  else
  {
    if (bool == 0)
    {
      datx = x[Minimos:Maximos]; daty = y[Minimos:Maximos]
      polyAjuste = poly_calc(datx, daty)
      cat("bool", bool, "Minimos", Minimos, "Maximos", Maximos, "cont", cont-1,">0\n")
      curve(polyAjuste,from=x[Minimos],to=x[Maximos],add=T, lwd=1,col="blue")
      Minimos = Maximos
      cont = 1
      bool = 1
      i = Maximos
      j = i + 1
      Maximos= j
    }
    else
    {
      j = i + 1
      Maximos= j
      i = j
    }
  }
  if (cont == 3)
  {
    datx = x[Minimos:Maximos]; daty = y[Minimos:Maximos]
    polyAjuste = poly_calc(datx, daty)
    cat("bool", bool, "Minimos", Minimos, "Maximos", Maximos, "cont", cont-1,"<>0\n")
    curve(polyAjuste,from=x[Minimos],to=x[Maximos],add=T, lwd=1,col="blue")
    Minimos = Maximos
    cont = 1
    i = Maximos
    j = i + 1
    Maximos= j 
    
    if(m < 0)
    {
      bool = 0 
    }
    else
    {
      bool = 1
    }
  }
  
  if (i==length(x))
  {
    Maximos = i
    datx = x[Minimos:Maximos]; daty = y[Minimos:Maximos]
    polyAjuste = poly_calc(datx, daty)
    cat("bool", bool, "Minimos", Minimos, "Maximos", Maximos, "cont", cont-1,"<>0\n")
    curve(polyAjuste,from=x[Minimos],to=x[Maximos],add=T, lwd=1,col="blue")
    break;
  }
}