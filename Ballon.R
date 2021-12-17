##Llamamos la libreria
library(tidyverse)

##Cambiamos el tipo de dato de la columna MARCADOR
diplomado$MARCADOR = as.character(diplomado$MARCADOR)

##Creamos las 2 nuevas colombas de MARCADOR, una para el del local y la otra para el visitante
diplomado = separate(diplomado,"MARCADOR",c("MARCADOR_LOCAL","MARCADOR_VISITANTE"), sep = ":",F )  
str(diplomado$MARCADOR)  
diplomado$MARCADOR_VISITANTE = substr(diplomado$MARCADOR_VISITANTE,2,2)  

## Arreglo de las columnas
#local = gsub("( )", "", diplomado$LOCAL)
#visitante = gsub("( )", "", diplomado$VISITANTE)
#summary(diplomado)

names(diplomado)[1] <- "JUGADOR"


columnas = gsub (".","_",columnas)
names(diplomado) = columnas

names(diplomado$X.U.FEFF.JUGADOR) =

  

finalistas = diplomado %>% filter(NACIONALIDAD %in% c("Belgium","argentina")

promedio_minutos_jugados = mean(TIEMPO..JUGADO)


goles_messi = diplomado %>% filter(JUGADOR == "Lionel Messi") %>%
  filter(GOLES > 0) %>% group_by(JUGADOR) %>%  summarise(promedio_minutos_jugados = mean(TIEMPO..JUGADO))

gol = colSums (goles_messi[ , 3:6])
gol
 




































































