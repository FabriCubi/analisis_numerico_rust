####Funcion del polinomio que deseamos analizar ####
ecuacion <- function(x){
  
  f <- exp(x)
  #f <- 2/(x-4)

  return(f) 
}

#Funcion SUMATORIA
#Entrada: paso(inicio+salto), b (limite sup), h (subdivision e increm)
#Salida: sumatoria de las imagenes dependiendo de X sub i
sumatoria <- function(paso,b,h){
  sumar <-0
  while(paso < b){
    sumar <- sumar + ecuacion(paso)
    paso<- paso + 2*h  
  }
  return(sumar)
}

#ENTRADA : extremos a,b y entero positivo n par
#SALIDA: Aproximacion de una Integral 
regla_simpson_Compuesta <- function(a, b, n){
  #Verificamos si n es par...
  if(n %% 2){
    return('N no es par, imposible calcular por simpson')
  }
  
#Paso 1  
  h = (b - a)/n
  
#Paso 2 
  sumatoria_extremo <- ecuacion(a) + ecuacion(b) #Sumamos la imagen de los extremos  
  sumatoria1 <-0  #Definicion para X sub i impar
  sumatoria2 <-0  #Definicion para X sub i par
  
#PASO 3
    #Sumatoria de las X sub i pares (suma de imagenes) 
  paso <- a + 2*h  #Iniciamos en un X sub i par (X2, ....)
  sumatoria2 <- sumatoria(paso,b,h) #Saltamos en el intervalo de a dos (x2, x4, x6,...)

   #Sumatoria de las X sub i impares
  paso <- a + h  #Iniciamos en un X sub i impar (X1,..) y  luego saltamos de a dos (X1,X3,x5,...)
  sumatoria1 <- sumatoria(paso,b,h)

  
  #Aplicamos la regla de simpson compuesta..
  sumatoria_final <- h*(sumatoria_extremo + 2*sumatoria2 + 4*sumatoria1)/3
  
  #Error que se tiene con esta n
  error_real <- ecuacion(b) - ecuacion(a)
  diferencia_error <- error_real - sumatoria_final
  
  return(paste('Aproximacion= ',sumatoria_final,' <<>> Error con el Valor real: ', diferencia_error))
}
