##Llamamos la libreria
library(tidyverse)

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


partidos_jugados = diplomado %>% count(JUGADOR)
names(partidos_jugados)[2] <- "TOTAL_PARTIDOS"

promedio_gol = data.frame(round(datos_nominados$TOTAL_GOLES / datos_nominados$TOTAL_PARTIDOS,2))
names(promedio_gol)[1] <- "PROMEDIO_GOL"



datos_nominados = diplomado %>% filter(JUGADOR %in% c("Kante","Jorginho","Lionel Messi","Karim Benzema","Robert Lewandowski",
                                                      "Bruno Fernandes","Cesar Azpilicueta","Cristiano Ronaldo","Erling Haaland",
                                                      "Gerard Moreno","Gianluigi Donnarumma","Giorgio Chiellini","Harry Kane",
                                                      "Kevin De Bruyne","Kylian Mbappe","Lautaro Martinez","Leonardo Bonucci",
                                                      "Luis Suarez","Luka Modric","Mason Mount","Mohamed Salah","Neymar","Pedri",
                                                      "Nicolo Barella","Phil Foden","Raheem Sterling","Riyad Mahrez","Romelu Lukaku",
                                                      "Ruben Dias","Simon Kjaer")) %>% group_by(JUGADOR)%>%summarise (TOTAL_GOLES = sum(GOLES),TOTAL_ASISTENCIAS = sum(ASISTENCIAS),TOTAL_TA = sum(TARJETA_AMARILLA),TOTAL_STA = sum(SEGUNDA_AMARILLA),
                                                      TOTAL_ROJAS = sum(TARJETA_ROJA),TOTAL_TIEMPO_JUGADO = sum(TIEMPO_JUGADO))

datos_nominados$TOTAL_PARTIDOS = partidos_jugados$TOTAL_PARTIDOS
datos_nominados$PROMEDIO_GOL = promedio_gol$PROMEDIO_GOL





finalistas = diplomado %>% filter(JUGADOR %in% c("Kante","Jorginho","Lionel Messi","Karim Benzema","Robert Lewandowski"))








goles_messi = diplomado %>% filter(JUGADOR == "Lionel Messi") %>%
  filter(GOLES > 0) %>% group_by(JUGADOR) %>%  summarise (promedio_minutos_jugados = mean(TIEMPO_JUGADO))

goles_fin = diplomado %>% filter(JUGADOR %in% c("Kante","Jorginho","Lionel Messi","Karim Benzema","Robert Lewandowski")) %>%
  filter(GOLES > 0) %>% group_by(JUGADOR)%>%  summarise (promedio_minutos_jugados = sum(TIEMPO_JUGADO))









































































