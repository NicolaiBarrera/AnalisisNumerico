DistanciaEror<- function (velocidad,errorVel,tiempo,errorTiem) #recibe los datos para calcular distancia y sus respectivos errores
{
  distancia<- velocidad*tiempo #calcula la distancia
  errorRela = (errorVel/velocidad)+(errorTiem/tiempo) #calculo error relativo
  errorAbso = (velocidad*errorVel)+(tiempo*errorTiem) #calculo error absoluto
  cat("La distancia calculada es:" ,distancia, "\n","Error absoluto: ",errorAbso, " \n Error RElativo: ",errorRela)
 
}
DistanciaEror(4,0.1,5,0.1)

## La distancia calculada es: 20 
##  Error absoluto:  0.9  
##  Error RElativo:  0.045