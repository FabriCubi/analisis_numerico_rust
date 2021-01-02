####Funcion del polinomio que deseamos analizar ####
ecuacion <- function(x){
  #Modificar la ecuacion por la que desea analizar.
  
  # <- x^3-7*x^2 +14*x -6
  
  #f <- x^0.5 -cos(x)
  
  f <- -x^3 - cos(x)
  return(f) 
}

####   METODO para resolver ecuaciones no lineales  #### 
##                   SECANTE                         ##
metodo_secante <-function(p_cero,p_uno, tolerancia, num_iteraciones){
  #Guardamos los valores iniciales en la tabla.
  tabla <- matrix(c(0),nrow=num_iteraciones,ncol=3)
  colnames(tabla) <- c('n','p_n','Error de Aproximacion') #Cambiamos el nombre de las columnas
  tabla[1,1] <- 0
  tabla[1,2] <- p_cero
  tabla[2,1] <- 1
  tabla[2,2] <-p_uno

#PASO 1 
  #Iteramos al 2do punto dado que tengo los dos primeros puntos.
  i<-2
  q_cero <- ecuacion(p_cero) #Evaluamos en el punto cero
  q_uno <- ecuacion(p_uno)  #Evaluamos en el punto uno
  
  #Verificamos si el polimonio cambia de signo en el intervalo.
  if(q_cero * q_uno >0){
    return(paste('La ecuacion no cambia de signo en el intervalo: [',p_cero,':',p_uno,']'))
  }
#PASO 2
  #Bucle de repeticion de acuerdo al numero de iteraciones.
  while(i <= num_iteraciones){
#PASO 3
    #Aplicamos la formula del metodo secante
<<<<<<< HEAD
    p <- p_uno - q_uno * ((p_uno - p_cero) / (q_uno - q_cero))
=======
    p <- p_uno - (q_uno * ((p_uno - p_cero)/ (q_uno - q_cero)))
>>>>>>> af133846963a07f6a540dfa987ee196d51b96e94
    
#PASO 4  
    #Verificamos que la tolerancia o el error de aproximacion es menor al dado.
    if(abs(p - p_uno) < tolerancia){
      #Graficamos los puntos y con su error de aproximacion.
      plot(tabla[,2],tabla[,3], type='o',main="Metodo Secante",ylab = "Error de aproximacion", xlab = "p_n" ) 
    
      print(tabla)
      print("---------------------------------------------------------")
      return(paste('Procedimiento completado exitosamente, p= ',p))
    }
#PASO 5  
    #Pasamos a la siguiente iteracion
    i <- i+1
    
    #Guardamos el n* de iteracion, el punto p_n,y el error de aproximacion
    tabla[i,1] <- i
    tabla[i,2] <- p
    tabla[i,3] <- abs(p - p_uno) #ERROR DE APROXIMACION
#PASO 6
    #Realizamos los cambios de puntos para la siguiente iteracion
    p_cero <- p_uno
    q_cero <- q_uno
    p_uno <- p
    q_uno <- ecuacion(p)
  }
  
#PASO 7
  #En caso de que no se llegue al resultado, por que se alcanzo el limite de iteraciones.
  return(paste('El metodo fracaso luego de ',num_iteracciones, ' iteraciones'))
}

