
####Funcion del polinomio que deseamos analizar ####
polinomio <- function(x){
  #Modificar la ecuacion por la que desea analizar.
  
  #f <- x^3-7*x^2 +14*x -6
  
  #f <- x^0.5 -cos(x)
  
  f <- -x^3 - cos(x)
  return(f) 
}




####   METODO para resolver ecuaciones no lineales  #### 
##                   BISECCCION                      ##
metodo_biseccion <- function(limite_inferior,limite_superior,tolerancia,num_iteracciones){
  #Tabla de cambios hechos por el algoritmo
  tabla <- matrix(c(0),nrow=num_iteracciones,ncol=5)
  colnames(tabla) <- c('n','a_n','b_n','p_n','f(p)_n') #Cambiamos el nombre de las columnas
  
#Paso 1
  #Asignar a la variable i = 1.
  i <- 1
  tabla[1,1] <- i
  tabla[1,2] <- limite_inferior
  tabla[1,3] <- limite_superior
  #Evaluamos el polinomio en el limite inferior y guardamos en FA
  FA <- polinomio(limite_inferior)
  
  if(polinomio(limite_inferior)*polinomio(limite_superior) >0){
    return("La ecuacion no cambia de signo")
  }
#Paso 2
  #Bucle de repeticion de acuerdo al numero de iteraciones.
  while (i <= num_iteracciones){
    tabla[i,3] <- limite_superior #Guardamos el limite superior, inferior , iteracion en la tabla
    tabla[i,2] <- limite_inferior
    tabla[i,1] <- i
#Paso 3
    #Obtenemos el punto p del intervalo.
    punto_medio <- limite_inferior + ((limite_superior - limite_inferior) / 2 )
    tabla[i,4] <- punto_medio
    #Evaluamos el punto p en el polimonio y asignamos a FP
    FP <- polinomio(punto_medio)
    tabla[i,5] <- FP
    
#Paso 4
    #Llegue a f(p) = 0 o Cumpli el nivel de tolerancia
    if (FP == 0 | ((limite_superior - limite_inferior) / 2 ) < tolerancia){
      #Graficamos el punto medio y su imagen.
      plot(tabla[,4],tabla[,5], type='o',main="Metodo Biseccion",ylab = "f(p)_n", xlab = "p_n") 
      
      #Imprimmos la tabla 
      print(tabla)
      print("--------------------------------------------------------------")
      return(paste('Procedimiento completado exitosamente, p= ',punto_medio))
    }
    
#Paso 5
    #Aumentamos el numero de iteraciones en 1
    i <- i + 1
    
#Paso 6
    #Para calcular el cambio de los limites del intervalo
    if(FP*FA > 0){
      limite_inferior <- punto_medio
      FA <-FP
    }else{
      limite_superior <-punto_medio
    }

  }
  
#Paso 7 
  return(paste('El metodo fracaso luego de ',num_iteracciones, ' iteraciones'))
}

