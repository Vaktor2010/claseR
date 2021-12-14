
#library
library(tidyverse)

#analizar constantes

Conteo = 1
conteo <- "soy un texto"

comparacion = !conteo = Conteo


conteo == Conteo

mi_vector = c(1,2,3,4,6, NA)
mi_vector_dos = c(1,2, "texto",4,6,NA)

mi_vector_dos[1]
mi_vector[1]

str (Conteo)

sum(mi_vector,na.rm = T)

length(mi_vector)

as.numeric("67")
as.integer(19.7)


#listas

mi_lista = list(mi_vector,mi_vector_dos)

mi_lista[1]

length(mi_lista)

length(mi_lista[[1]])


#factores

estrato = 1:6
estrato_factor = as.factor(estrato)
length(estrato)

summary(estrato)
summary(estrato_factor)


a=c("Felipe","Bibiana,","Daniel","Alejandro","Leidy","Edwin","Victor")
b=c("masculino","femenino,","masculino","masculino","femenino","masculino","masculino"))


mis_estudiantes = data.frame("nombre"= a
                             "genero"= b)

str(mis_estudiantes)

summary(mis_estudiantes$nombre)
summary(as.factory(mis_estudiantes$nombre))

dim(mis_estudiantes)


muestra = c(1,as.integer(nrow(mis_estudiantes)/2),nrow(mis_estudiantes))
mis_estudiantes[muestra,]


#tranformacion a los datos

mis_estudiantes$genero=as.factor(mis_estudiantes$genero)
mis_estudiantes$genero_como_numero = as.numeric(mis_estudiantes$genero)
mis_estudiantes$nota = as.factor(c(5,2,1,1,5,2,4))
mis_estudiantes$nota_numero=as.numeric(as.character(mis_estudiantes$nota))
mis_estudiantes$paso = ifelse(mis_estudiantes$nota_numero)


#funciones

mi_primera_funcion = function(parametro1,parametro2){
  
  if(is.numeric(parametro1)&is.numeric(parametro2)){
  
 conteo=parametro1+parametro2
 conteo
  }
  else {
    mensaje = "no son variables numericas"
    mensaje
        }

}

mi_primera_funcion(parametro1 = "10", parametro2 = 8)
 



#funcion de cadena de fibonacci

1,1,2,3,5,8


fibonacci = function(n){
  
  i = 1
  
  while (i<=n) {
  
    respuesta = 0
    
    
    if (i>=2){
      respuesta = c(fibonacci(i-1)+fibonacci(i-2))
      
      
    }
    print(i)
    i=i+1 
  }
  return(0)
}

   


fibonacci = function(n){
  
  if(n<0){
    
    "este valor no es permitido"
  }
  
  else if (n==0){
    return(0)
  }
  
  else if (n==1){
    return(1)
  }
  else if (n>=2){
    return(fibonacci(n-1)+fibonacci(n-2))
    
  }
  
  fib = function(x){
    
    i = 1
    
    while (i<=x) {
      
      respuesta = 0
      
      
      if (i>=2){
        respuesta = c(fibonacci(i-1)+fibonacci(i-2))
        
        
      }
      print(i)
      i=i+1 
    }
    return(0)
  
  } 
}



















