
#Ecuacion de dos variable
ecuacion <- function(x,y){
  #Modificar la ecuacion por la que desea analizar.
  #f <- x*sqrt(y)
  f <- x*x + y*y
  return(f) 
}

#FUNCION RUNGE-KUTTA
#ENTRADA: condicion-inicial x e y, donde x inicial es el inicio del intervalo y el x_final el final del intervalo, h tamanho de paso
#Imprime la tabla aplicando runge kutta.
metodo_runge<-function(x_inicial,x_final,y_inicial,tam_paso){
  
  if(tam_paso == 0 ){
    return('El tamanho de paso es igual a cero')
  }else if(x_final == x_inicial){
    return('El punto final e inicial son los mismos')
  }
  
  #Determinamos el numero de iteraciones necesarias dentro de ese intervalo
  num_iteraciones <- ((x_final - x_inicial)/tam_paso)
  print(paste('Numero de iteraciones: ',num_iteraciones))
  
  num_iteraciones <- num_iteraciones +1 #Guardamos los valores o puntos iniciales 
  #Guardamos los valores iniciales en la tabla.
  tabla <- matrix(c(0),nrow=num_iteraciones,ncol=4)
  colnames(tabla) <- c('n','x_i','y_i','y_i+1') #Cambiamos el nombre de las columnas
  
  tabla[1,2]<- x_inicial #Xinicial
  tabla[1,3]<- y_inicial #Yinicial
  paso <-1 #seteamos en uno para el while, en la tabla se guarda desde el 0 (visualmente)
  
  while(paso < num_iteraciones+1){
    #Cargamos en la tabla posicion actual -iniciales-
    
    k1<- ecuacion(tabla[paso,2],tabla[paso,3])
    
    k2<- ecuacion(tabla[paso,2]+0.5*tam_paso,tabla[paso,3]+0.5*tam_paso*k1)
    
    k3<- ecuacion(tabla[paso,2]+0.5*tam_paso,tabla[paso,3]+0.5*tam_paso*k2)
    
    k4<- ecuacion(tabla[paso,2]+tam_paso,tabla[paso,3]+tam_paso*k3)
    
    #Guardamon y+1  
    tabla[paso,4]<- (tabla[paso,3]+(1/6)*tam_paso*(k1+2*k2+2*k3+k4)) #Formula
    tabla[paso,1] <-paso-1  #Numero iteraciones con la posicion de valores iniciales ysub0 y xsub0
    

   
  
    paso <-paso +1
    #Luego actualizamos con los valores para la sgte iteracion

    #Verificamos que no realice un paso mas fuera de los limites de la tabla
    if(paso <= num_iteraciones){
      tabla[paso,2] <- tabla[paso-1,2] + tam_paso
      tabla[paso,3] <- tabla[paso-1,4]
    }
  }

  plot(tabla[,2],tabla[,3], type='o',main="Rungge Kutta",ylab = "y", xlab = "x" ) 
  print(tabla)
}