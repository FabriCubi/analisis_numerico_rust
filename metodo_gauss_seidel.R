############################################################## 2x2
ecuacion1<-function(x){
  f <- (99+9*x)/11
    return(f)
}

ecuacion2<-function(y){
  f <- (286-11*y)/13
  
    return(f)
}

metodo_gauss_seidel2<-function(num_iteracciones,tolerancia){
  #Tabla de cambios hechos por el algoritmo
  tabla <- matrix(c(0),nrow=num_iteracciones,ncol=2)
  ban <- 0
  k<-2
  while(k <= num_iteracciones){
    tabla[k,1]<-ecuacion1(tabla[k-1,2])  #Viene hacer X
    tabla[k,2]<-ecuacion2(tabla[k,1]) #Viene hacer y
  
    if((abs(tabla[k-1,1]-tabla[k,1])/tabla[k,1]) < tolerancia | 
       (abs(tabla[k-1,2]-tabla[k,2])/tabla[k,2]) < tolerancia ){
      ban<-1
      break
    }
    
    k<-k+1 
  }
  if(ban){
    print(tabla)
    print("Se alcanzo la tolerancia maxima, procedimiento fue exitoso")
  }else{
    print(tabla)
    print("Numero Maximo de interacciones excedido")
  }
} 



#########################################################3x3


ecuacionx<-function(y,z){
  f <- (14+4*y+3*z)/8
  return(f)
}

ecuaciony<-function(x,z){
  f <- (2*x+3*z+1)/5
  return(f)
}
ecuacionz<-function(x,y){
  f<- (9+3*x-y)/9
  return(f)
}

metodo_gauss_seidel<-function(num_iteracciones, tolerancia){
  #Tabla de cambios hechos por el algoritmo
  tabla <- matrix(c(0),nrow=num_iteracciones,ncol=3)
  ban<-0
  k<-2
  while(k <= num_iteracciones){
    tabla[k,1]<-ecuacionx(tabla[k-1,2],tabla[k-1,3]) #Evaluar a X
    tabla[k,2]<-ecuaciony(tabla[k,1],tabla[k-1,3])
    tabla[k,3]<-ecuacionz(tabla[k,1],tabla[k,2])
    
    if((abs(tabla[k-1,1]-tabla[k,1])/tabla[k,1]) < tolerancia |
        (abs(tabla[k-1,2]-tabla[k,2])/tabla[k,2]) < tolerancia |
          (abs(tabla[k-1,3]-tabla[k,3])/tabla[k,3]) < tolerancia ){
      ban <-1
    }
    
    k<-k+1 
  }
  
  if(ban){
    print(tabla)
    print("Se alcanzo la tolerancia maxima, procedimiento fue exitoso")
  }else{
    print(tabla)
    print("Numero Maximo de interacciones excedido")
  }
  
} 


########################################################## 4x4
ecuacionx1<-function(x2,x3,x4){
  f <- (x2-2*x3+0*x4+6)/10
  return(f)
}

ecuacionx2<-function(x1,x3,x4){
  f <- (25+x1+x3-3*x4)/11
  return(f)
}
ecuacionx3<-function(x1,x2,x4){
  f<- (-11-2*x1+x2+x4)/10
  return(f)
}

ecuacionx4<-function(x1,x2,x3){
  f<- (0*x1+x3-3*x2+15)/8
  return(f)
}


metodo_gauss_seidel<-function(num_iteracciones,tolerancia){
  #Tabla de cambios hechos por el algoritmo
  tabla <- matrix(c(0),nrow=num_iteracciones,ncol=4)
  ban<-0
  k<-2
  while(k <= num_iteracciones){
    tabla[k,1]<-ecuacionx1(tabla[k-1,2],tabla[k-1,3],tabla[k-1,4])
    tabla[k,2]<-ecuacionx2(tabla[k,1],tabla[k-1,3],tabla[k-1,4])
    tabla[k,3]<-ecuacionx3(tabla[k,1],tabla[k,2],tabla[k-1,4])
    tabla[k,4]<-ecuacionx4(tabla[k,1],tabla[k,2],tabla[k,3])
    
    if((abs(tabla[k-1,1]-tabla[k,1])/tabla[k,1]) < tolerancia |
        (abs(tabla[k-1,2]-tabla[k,2])/tabla[k,2]) < tolerancia |
          (abs(tabla[k-1,3]-tabla[k,3])/tabla[k,3]) < tolerancia |
            (abs(tabla[k-1,3]-tabla[k,3])/tabla[k,3]) < tolerancia ){
      
      ban <-1
    }
    
    k<-k+1 
  }
  if(ban){
    print(tabla)
    print("Se alcanzo la tolerancia maxima, procedimiento fue exitoso")
  }else{
    print(tabla)
    print("Numero Maximo de interacciones excedido")
  }
} 


