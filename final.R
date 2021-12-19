diplomado <- read.csv("~/diplomado/diplomado.csv", encoding="UTF-8", sep=";", stringsAsFactors=TRUE)

##Llamamos la libreria
library(tidyverse)
library(fmsb)
library(DT)

##Cambiamos el tipo de dato de la columna MARCADOR
diplomado$MARCADOR = as.character(diplomado$MARCADOR)

##Creamos las 2 nuevas colombas de MARCADOR, una para el del local y la otra para el visitante
diplomado = separate(diplomado,"MARCADOR",c("MARCADOR_LOCAL","MARCADOR_VISITANTE"), sep = ":",F )  
str(diplomado$MARCADOR)  
diplomado$MARCADOR_VISITANTE = substr(diplomado$MARCADOR_VISITANTE,2,2)  



names(diplomado)[1] <- "JUGADOR"
names(diplomado)[13] <- "TARJETA_AMARILLA"
names(diplomado)[14] <- "SEGUNDA_AMARILLA"
names(diplomado)[15] <- "TARJETA_ROJA"
names(diplomado)[16] <- "MIN_INICIAL"
names(diplomado)[17] <- "MIN_FINAL"
names(diplomado)[18] <- "TIEMPO_JUGADO"


ligas = c("LEAGUE 1","SERIE A","PREMIER LEAGUE","LA LIGA","BUNDESLIGA","UEFA CHAMPIONS LEAGUE","EUROPA LEAGUE")

lista_ligas = list()

for (comp in ligas) {
  
  
  
  datos_nominados = diplomado %>% filter(COMPETENCIA == comp) %>%  group_by(JUGADOR) %>% summarise(TOTAL_GOLES = sum(GOLES),
                                                                                                   TOTAL_PARTIDOS = n(),
                                                                                                   TOTAL_ASISTENCIAS = sum(ASISTENCIAS),
                                                                                                   TOTAL_TA = sum(TARJETA_AMARILLA),
                                                                                                   TOTAL_STA = sum(SEGUNDA_AMARILLA),
                                                                                                   TOTAL_ROJAS = sum(TARJETA_ROJA),
                                                                                                   TOTAL_TIEMPO_JUGADO = sum(TIEMPO_JUGADO)) %>% mutate(PROMEDIO_GOL = round(TOTAL_GOLES / TOTAL_PARTIDOS,2))  
  str(datos_nominados)
  lista_ligas[[comp]] = datos_nominados
  
}


base_ligas = do.call("rbind",lista_ligas)

base_ligas = base_ligas %>%  group_by(JUGADOR) %>% summarise(TOTAL_GOLES = sum(TOTAL_GOLES),
                                                                                                 TOTAL_PARTIDOS = n(),
                                                                                                 TOTAL_ASISTENCIAS = sum(TOTAL_ASISTENCIAS),
                                                                                                 TOTAL_TA = sum(TOTAL_TA),
                                                                                                 TOTAL_STA = sum(TOTAL_STA),
                                                                                                 TOTAL_ROJAS = sum(TOTAL_ROJAS),
                                                                                                 TOTAL_TIEMPO_JUGADO = sum(TOTAL_TIEMPO_JUGADO)) %>% mutate(PROMEDIO_GOL = round(TOTAL_GOLES / TOTAL_PARTIDOS,2))


#informacion de las seleciones

selecciones = c("EURO 2020","ELIMINATORIAS MUNDIALISTAS","COPA AMERICA")

lista_selecciones = list()

for (comp in selecciones) {
  
  
  
  datos_nominados = diplomado %>% filter(COMPETENCIA == comp) %>%  group_by(JUGADOR) %>% summarise(TOTAL_GOLES = sum(GOLES),
                                                                                                   TOTAL_PARTIDOS = n(),
                                                                                                   TOTAL_ASISTENCIAS = sum(ASISTENCIAS),
                                                                                                   TOTAL_TA = sum(TARJETA_AMARILLA),
                                                                                                   TOTAL_STA = sum(SEGUNDA_AMARILLA),
                                                                                                   TOTAL_ROJAS = sum(TARJETA_ROJA),
                                                                                                   TOTAL_TIEMPO_JUGADO = sum(TIEMPO_JUGADO)) %>% mutate(PROMEDIO_GOL = round(TOTAL_GOLES / TOTAL_PARTIDOS,2))  
  str(datos_nominados)
  lista_selecciones[[comp]] = datos_nominados
  
}




base_selecciones = do.call("rbind",lista_selecciones)

base_selecciones = base_selecciones %>%  group_by(JUGADOR) %>% summarise(TOTAL_GOLES = sum(TOTAL_GOLES),
                                                             TOTAL_PARTIDOS = n(),
                                                             TOTAL_ASISTENCIAS = sum(TOTAL_ASISTENCIAS),
                                                             TOTAL_TA = sum(TOTAL_TA),
                                                             TOTAL_STA = sum(TOTAL_STA),
                                                             TOTAL_ROJAS = sum(TOTAL_ROJAS),
                                                             TOTAL_TIEMPO_JUGADO = sum(TOTAL_TIEMPO_JUGADO)) %>% mutate(PROMEDIO_GOL = round(TOTAL_GOLES / TOTAL_PARTIDOS,2))


## unificacion bases





names(base_ligas) = paste0(names(base_ligas),"_liga")
names(base_selecciones) = paste0(names(base_selecciones),"_selecciones")


#join entre bases

base_total = left_join(base_ligas, base_selecciones, by = c("JUGADOR_liga" = "JUGADOR_selecciones")) %>% filter(!JUGADOR_liga %in% c("Riyad Mahrez","Mohamed Salah","Giorgio Chiellini","Gianluigi Donnarumma","Cesar Azpilicueta"))

base_total = base_total %>% select(-c(TOTAL_TA_liga,TOTAL_STA_liga,TOTAL_ROJAS_liga,TOTAL_TA_selecciones,TOTAL_STA_selecciones,TOTAL_ROJAS_selecciones))

names(base_total)[1] <- "JUGADOR"

ggplot(base_total, aes(x = TOTAL_GOLES_liga, y = JUGADOR)) + geom_col(fill= "red") 

ggplot(base_total, aes(x = TOTAL_GOLES_selecciones, y = JUGADOR)) + geom_col(fill ="green") 

ggplot(base_total, aes(x = TOTAL_GOLES_selecciones, y = JUGADOR)) + geom_col(aes(fill =(TOTAL_GOLES_liga), position = "dodge")) 

radarchart(base_total ) 

finalistas = base_total %>% filter(JUGADOR %in% c("Lionel Messi", "Robert Lewandowski" , "Jorginho" , "Karim Benzema" , "Kante"))
 
radarchart(finalistas) 

mejores = finalistas %>% filter(JUGADOR %in% c("Lionel Messi", "Robert Lewandowski"))

radarchart(mejores) 
