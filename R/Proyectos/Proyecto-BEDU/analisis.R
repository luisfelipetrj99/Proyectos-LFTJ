library(dplyr)
library(ggplot2)
library(tidyverse)
library(geometry)
library(data.table)

#Leemos los datos del data set generado en el anterior script
videogames <- na.omit(read.csv("https://raw.githubusercontent.com/luisfelipetrj99/Proyectos-LFTJ/main/R/Datasets/data_bedu1.csv", header=TRUE))

#Aseguramos dejar a los desarrolladores independientes en el data frame
videogames<- videogames %>% filter(genre_Indie == TRUE)

#Filtrado para eliminar columnas vacías creadas durante la unión de los data sets
s<-c()
for (i in 1:length(videogames)) {
  if(is.logical(videogames[,i])){
    if(sum(videogames[,i]) == 0){
      s <- c(s,i)
    }
  }
}
videogames <- videogames[,-s]

#Corrige las filas y las colmnas del data frame
videogames <- videogames[!duplicated(videogames),]
videogames <- videogames %>% mutate(release_date = as.Date(release_date, format = "%d/%m/%Y"))
videogames <- arrange(videogames, release_date)
videogames<-videogames%>%mutate(Meta.Score=Meta.Score/10)

# Revisamos la estructura del data frame
class(videogames); str(videogames);

# Visualiza un resumen para cada columna
summary(videogames)


# PREGUNTAS...
#=============== ¿Cuales son los géneros más implementados en los juegos?================0
genres<- videogames %>% select(starts_with('genre_'))
colnames(genres) <- gsub("genre_","",names(genres))

genres<- as.data.frame(cbind(names(genres),apply(genres, 2, sum)))
genres<- genres %>% 
  mutate(V2 = as.numeric(V2)) %>% 
  filter(V1!="Indie")

ggplot(genres, aes(V1,V2, fill = V1)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Género', y = 'Cantidad de Juegos', title = 'Frecuencia de Géneros') +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )

summary(genres)
tail(genres%>%arrange(V2))

#Los generos mas jugados son accion y aventuras

# ¿Cual es la plataforma preferida por los desarrolladores?
plataforms<- videogames %>% select(starts_with('platform_'))
colnames(plataforms) <- gsub('platform_','',names(plataforms))

plataforms<- as.data.frame(cbind(names(plataforms),apply(plataforms, 2, sum)))
plataforms<- plataforms %>% 
  mutate(V2 = as.numeric(V2)) %>% 
  filter(V2 > 0)

ggplot(plataforms, aes(V1,V2, fill = V1)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Plataformas', y = 'Cantidad de Juegos', title = 'Plataformas') +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90)
  )

summary(plataforms)
tail(plataforms %>% arrange(V2))

#La plataforma mas popular es windows con 846 juegos


#=================== ¿Qué videojuego ha sido el más/menos jugado?==========================

columns <- c("name", "release_date", "average_playtime", "median_playtime","Meta.Score","User.Score")
videogames2<-filter(videogames,average_playtime>0)
#Menos jugado tomando el promedio
menos.jugado.a <- videogames2[which.min(videogames2$average_playtime), columns]
menos.jugado.a

#Este es un videojuego perteneciente al genero de Estrategia, tuvo una calificación de
#la critica de 7.8 y de los usuarios de 8.2 o sea fue un juego bastante aceptable

#Mas jugado tomando el promedio
mas.jugado.a <- videogames2[which.max(videogames2$average_playtime), columns]
mas.jugado.a

#Era un juego free-to-play del genero de estrategia,sus servidores cerraron este año
#Tuvo calificación de la crítica de 8.2 y de los usuarios de 7.7

#El menos jugado tomando la media
menos.jugado.m <- videogames2[which.min(videogames2$median_playtime), columns]
menos.jugado.m

#Mismo juego que el promedio

#El mas jugado tomando la media 
mas.jugado.m <- videogames2[which.max(videogames2$median_playtime), columns]
mas.jugado.m

#Mismo juego que el promedio

#De aqui no podemos concluir algo muy claro ya que al ver la calificación que recibieron estos juegos
#del menos jugado no fue baja ni del mas jugado fue muy alta, solo podemos pensar que 
#el juego menos jugado era un juego corto por lo tanto fue jugado menos horas por quienes 
#poseen.
#Por otro lado el juego mas jugado era un juego gratuito de estrategia online, un género que 
#los jugadores en computadora adoran así que no es extraño que haya sido el juego mas jugado
#en la plataforma

#Veamos las gráficas del tiempo promedio de juego y la media de juego
#Grafica de average_playtime
annotation <- data.frame(
  x = c(10,10),
  y = c(menos.jugado.a$average_playtime+1500,mas.jugado.a$average_playtime-1500),
  label = c(paste0("< min:",menos.jugado.a$average_playtime, " > ", menos.jugado.a$name), 
            paste0("< max:",mas.jugado.a$average_playtime," > ", mas.jugado.a$name))
)

#Podemos ver en un histograma como se distribuye average_playtime
ggplot(filter(videogames,average_playtime>0), aes(x=average_playtime)) +
  geom_histogram()+
  ggtitle("Historama Tiempo promedio de juego")+
  labs(x="Tiempo promedio en horas",y="Cantidad de juegos")


#Notamos que se encuentra muy sesgada hacia la izuierda nuestra distribución
#casi todo se concentra entre 0 y 5000 horas, restringiendo los datos en este intervalo
ggplot(filter(videogames,average_playtime>0 & average_playtime<5000), aes(x=average_playtime)) +
  geom_histogram()+
  ggtitle("Historama Tiempo promedio de juego")+
  labs(x="Tiempo promedio en horas",y="Cantidad de juegos")

#Ahora se puede ver que average_playtime sigue sesgada hacia la izquierda, refinemos un poco mas 
#el intervalo de observacion, ahora entre las 0 y 1000 horas
ggplot(filter(videogames,average_playtime>0 & average_playtime<1000 ), aes(x=average_playtime)) +
  geom_histogram()+
  ggtitle("Historama Tiempo promedio de juego")+
  labs(x="Tiempo promedio en horas",y="Cantidad de juegos")

#De esta grafica podemos observar que el promedio de tiempo que los juagdores le dedicaron
#a la mayoria de los titulos independientes esta entre las 0 y 300 horas


#Veamos como se ve el boxplot
ggplot(filter(videogames,average_playtime>0), aes(x=0, y=average_playtime)) +
  geom_boxplot(width = 10, outlier.color = "#f5b041", outlier.alpha = 0.7, size = 0.5) +
  ggtitle("Tiempo promedio jugado") +
  labs(y = "Tiempo[Horas]") +
  theme_minimal() +
  coord_cartesian(xlim = c(-20,20)) +
  geom_hline(yintercept = c(menos.jugado.a$average_playtime, mas.jugado.a$average_playtime), 
             color = c('#a569bd','#12a083'), 
             linetype = 1,
             alpha = 0.5,
             size = .8) +
  geom_label(data=annotation, aes( x=x, y=y, label=label),
             color = c('#a569bd','#12a083'), 
             size=3 , 
             angle=45, 
             fontface="bold" )

#Notemos que tiene muchos datos atípicos, al igual que con los histogramas vamos a restingir 
#el intervalo de observación para deshacernos de estos datos atípicos

ggplot(filter(videogames,average_playtime>0 & average_playtime<1000), aes(x=0, y=average_playtime)) +
  geom_boxplot(width = 10, outlier.color = "#f5b041", outlier.alpha = 0.7, size = 0.5) +
  ggtitle("Tiempo promedio jugado") +
  labs(y = "Tiempo[Horas]") +
  theme_minimal() +
  coord_cartesian(xlim = c(-20,20)) +
  stat_summary(fun.y = mean)

#Se ve mejor la distribución de las horas de juego, aunqeu hay se notan varios valores atípicos
mean(filter(videogames,average_playtime>0 & average_playtime<1000)$average_playtime)

#El promedio de horas jugadas de este intervalo es de 250 horas

#Gráfica de median_playtime
annotation <- data.frame(
  x = c(10,10),
  y = c(menos.jugado.m$median_playtime+1500,mas.jugado.m$median_playtime-1500),
  label = c(paste0("< min:", menos.jugado.m$median_playtime, " > ", menos.jugado.m$name), 
            paste0("< max:", mas.jugado.m$median_playtime,   " > ", mas.jugado.m$name))
)

#Hacemos un analisis similiar al tiempo medio de juego
ggplot(filter(videogames,median_playtime>0), aes(x=median_playtime)) +
  geom_histogram()+
  ggtitle("Historama Tiempo medio de juego")+
  labs(x="Tiempo medio en horas",y="Cantidad de juegos")

#Aqui casi todos se encuentran aabjo de mil horas
ggplot(filter(videogames,median_playtime>0 & median_playtime<1000), aes(x=median_playtime)) +
  geom_histogram()+
  ggtitle("Historama Tiempo medio de juego")+
  labs(x="Tiempo medio en horas",y="Cantidad de juegos")

#Sigue sesgado hacia la izquierda pero de aqui se puede ver que el tiempo medio de juego
#esta casi en el mismo intervalo que el tiempo promdio de juego, entre 0 y 300 horas

#Veamos los boxplots
ggplot(filter(videogames,median_playtime>0), aes(x=0, y=median_playtime)) +
  geom_boxplot(width = 10, outlier.color = "#9ccc65", outlier.alpha = 0.7, size = 0.5) +
  ggtitle('Medias del tiempo jugado') +
  labs(y = 'Tiempo:[Horas]') +
  theme_minimal() +
  coord_cartesian(xlim = c(-20,20)) +
  geom_hline(yintercept = c(menos.jugado.m$median_playtime, mas.jugado.m$median_playtime), 
             color = c('#2980b9','#a1887f'), 
             linetype = 1,
             alpha = 0.5,
             size = .8) +
  geom_label(data=annotation, aes( x=x, y=y, label=label),
             color= c('#2980b9','#a1887f'), 
             size=3 , 
             angle=45, 
             fontface="bold" )

ggplot(filter(videogames,median_playtime>0 & median_playtime<1000), aes(x=0, y=median_playtime)) +
  geom_boxplot(width = 10, outlier.color = "#9ccc65", outlier.alpha = 0.7, size = 0.5) +
  ggtitle('Medias del tiempo jugado') +
  labs(y = 'Tiempo:[Horas]') +
  theme_minimal() +
  coord_cartesian(xlim = c(-20,20))+
  stat_summary(fun.y = mean)

mean(filter(videogames,median_playtime>0 & median_playtime<1000)$median_playtime)
#Es una grafica muy similar a la de average_playtime y tiene un valor promedio bastante parecido
# es de 263 horas 

#En general los usuarios pasan en este tipo de juegos unas 250 horas


#===================== ¿Qué videojuego es el mejor/peor valorado?==========================
columns <- c('name', 'Meta.Score', 'User.Score', 'release_date')
#Juego peor valorado por la críticia
juego.peor.v.c<-videogames[which.min(filter(videogames,Meta.Score>0)$Meta.Score), columns]
juego.peor.v.c
#Este un juego de acción 

#Juego peor valorado por los usuarios
juego.peor.v.u<-videogames[which.min(filter(videogames,User.Score>0)$User.Score), columns]
juego.peor.v.u
#Es un juego de aventura

#Juego mejor valorado por la critica
juego.mejor.v.c<-videogames[which.max(filter(videogames,Meta.Score>0)$Meta.Score), columns]
juego.mejor.v.c

#Este es un juego de aventuras

#Mejor valorado por los usuarios
juego.mejor.v.u<-videogames[which.max(filter(videogames,User.Score>0)$User.Score), columns]
juego.mejor.v.u

#Es un juego de aventura

#Veamos las gráficas de estas calificaciones

#Calificaciones recibidas por la crítica
ggplot(videogames,aes(x=Meta.Score))+
  geom_histogram()+
  ggtitle("Calificaciones recibidas por la crítica")+
  labs(x="Calificación",y="Cantidad de juegos")


#La gráfica posee un sesgo a la derecha, y la mayoria de las calificaciones se encuentran entre 
#7 y 8, o sea por parte de la crítica reciben en su mayoria calificaciones positivas

#Existen datos atípicos de juegos con menos de 4 de valoración, los eliminamos para sacar
#el promedio de calificaciones
mean(filter(videogames,Meta.Score>4)$Meta.Score)
  

#La calificacion promedio es de 7.19, una calificacion aceptable

#Calificaciones de los usuarios
ggplot(videogames,aes(x=User.Score))+
  geom_histogram()+
  ggtitle("Calificaciones recibidas por los usuarios")+
  labs(x="Calificación",y="Cantidad de juegos")

#Por los usuarios cambia la situación, l mayoria de las calificaciones esta entre 6 y 8
#O sea el publico es un poco mas rudo calificando los juegos

#Eliminemos datos atípicos y veamos el promedio de calificaciones recibidad por los usuarios
mean(filter(videogames,User.Score>4)$User.Score)
#El promedio de calificaciones dadas por el publico es de 7.17
#Son casi el mismo el que da el publico y la critica, veamos que tan relacionadas estan

ggplot(videogames,aes(x=User.Score,y=Meta.Score))+
  geom_point()+  
  ggtitle("Relación Calificaciones de la critica-usuarios")+
  labs(x="Calificación Usuarios",y="Calificacion Criticos")

#Al ver la grafica parece que si estan muy relacionados, veamos cual es su indice de 
#correlacion
cor(videogames$User.Score,videogames$Meta.Score)

#Es de 0.518989, o sea tienen una correlacion positiva

#Ahora veamos que tan relacionado esta la critica del publico con el tiempo promedio de juego
ggplot(filter(videogames,average_playtime>0),aes(x=Meta.Score,y=average_playtime))+
  geom_point()+
  ggtitle("Relación Calificaciones de la critica- tiempo promedio de juego")+
  labs(x="Calificación Critica",y="Tiempo promedio de juego")

cor(filter(videogames,average_playtime>0)$average_playtime,filter(videogames,average_playtime>0)$Meta.Score)

#Viendo la gráfica y el coeficiente de correlacion que es de 0.06200801 podemos ver que no estan casi 
#nada relacionados

#Repitamos el mismo procedimiento con la critica e los usuarios
ggplot(filter(videogames,average_playtime>0),aes(x=User.Score,y=average_playtime))+
  geom_point()+
  ggtitle("Relación Calificaciones de los usuarios- tiempo promedio de juego")+
  labs(x="Calificación Usuarios",y="Tiempo promedio de juego")

cor(filter(videogames,average_playtime>0)$average_playtime,filter(videogames,average_playtime>0)$User.Score)

#De igual forma tienen casi nula correlacion, tienen un porcentaje de -0.08188436

#Podemos concluir que la relacion entre el tiempo promedio de juego y su valoracion por
#parte de los usuarios y la critica no tienen ninguna correlacion

#===========================videojuegos mas caros/baratos==================================
#Primero no vamos a connsiderar a los juegos gratis
# # ¿Cual es el videojuego mas caro/barato?
columns <- c('name', 'release_date', 'price', 'publisher','Meta.Score','User.Score')
videogames3<-videogames%>%filter(price>0)
#Videojuego mas barato
videogames3[which.min(videogames3$price), columns]

#Este es un juego de accion con calificacion de 8.1 de la critica y 7.5 de los usuarios

#videojuego mas caro
videogames[which.max(videogames$price), columns]
#Este es un juego RPG de supervivencia que tuvo una calificación de 7 por parte de los críticos
#pero de 4 por parte del público

#Veamos que las gráficas del precio de los juegos 
ggplot(videogames3,aes(x=price))+
  geom_histogram()+
  ggtitle("Precios de los juegos")+
  labs(x="Precio",y="Cantidad de juegos")
#Vemos que la mayoria de los juegos se ubica en un precio de 0 a 20 dolares
ggplot(filter(videogames3,price<20),aes(x=price))+
  geom_histogram()+
  ggtitle("Precios de los juegos")+
  labs(x="Precio",y="Cantidad de juegos")
#Viendolo mas de cerca la mayoria se ubica en unos 8 ,12 y 15 dolares.

#Veamos que tanto se relaciona el precio del juego con el promedio de tiempo jugado
ggplot(filter(videogames3,average_playtime>0),aes(x=price,y=average_playtime))+
  geom_point()+
  ggtitle("Precio del juego vs tiempo jugado")+
  labs(x="Precio",y="Tiempo medigo jugado")
cor(videogames3$price,videogames3$average_playtime)

#Se tiene una ligera correlacion positiva de 0.2458 entre el precio del videojuego y el tiempo
#jugado

#Veamos que tanto se relaciona el precio del juego con la calificación de los críticos
ggplot(filter(videogames3,average_playtime>0),aes(x=price,y=Meta.Score))+
  geom_point()+
  ggtitle("Precio del juego vs Calificacion criticos")+
  labs(x="Precio",y="calificacion")

cor(videogames3$price,videogames3$Meta.Score)

#Se observa muy poca correlacion entre la calificacion de la crítica y el precio del juego
#correlacion de 1.675668

#Veamos que tanto se relaciona el precio del juego con la calificación de los usuarios
ggplot(filter(videogames3,average_playtime>0),aes(x=price,y=User.Score))+
  geom_point()+
  ggtitle("Precio del juego vs Calificacion Usuarios")+
  labs(x="Precio",y="calificacion")

cor(videogames3$price,videogames3$User.Score)

# Igual se observa muy poca correlacion entre la calificacion de los usuarios y el precio
#del juego, correlacion de 0.05413074

#========================== ¿Cual es el con mas juegos publicados?====================

# Desarrolladores con más juegos publicados
desarrolladores<-as.data.frame(table(videogames$developer))
names(desarrolladores)<-c('developer','frecuencia')
#summary(desarrolladores)
head(desarrolladores%>%arrange(desc(frecuencia)))

#El desarrollador con mas videojuegos fue Daedalic Entertaiment, veamos que calificacion resivieron sus juegos
d1<-videogames%>%filter(developer=="Daedalic Entertainment")

#Calificaciones por parte de la critica
max(d1$Meta.Score)
min(d1$Meta.Score)
mean(d1$Meta.Score)
#Sus juegos en promedio tuvieron calificaciones entre 5.6 y 8 con un promedio de 7.82

#Calificaciones por parte de los usuarios
max(d1$User.Score)
min(d1$User.Score)
mean(d1$User.Score)

#Las calificaciones estuvieron en un rango de 6.5 a 8.3 con un promedio de 7.53125

#Veamos las de NeocoreGames
d2<-videogames%>%filter(developer=="NeocoreGames")

#Calificaciones por parte de la critica
max(d2$Meta.Score)
min(d2$Meta.Score)
mean(d2$Meta.Score)
#Sus juegos en promedio tuvieron calificaciones entre 6.4 y 7.9 con un promedio de 7.828

#Calificaciones por parte de los usuarios
max(d2$User.Score)
min(d2$User.Score)
mean(d2$User.Score)
#Sus juegos tuvieron calificaciones entre 6.5 y 8.2 con un promedio de 7.45

#Veamos la relacion entre el desarrollador y distintos aspectos del juego

#Unimos videogames con la frecuencia de los desarroladores
datos<-merge(desarrolladores,videogames,by="developer")

#Relacion desarrolador calificacion de la critica
datos$frecuencia<-as.factor(datos$frecuencia)
ggplot(datos,aes(x=frecuencia,y=Meta.Score,fill=frecuencia))+
  geom_boxplot()+
  ggtitle("Calificaciones de la critica respecto a  no  de juegos desarrollados")+
  labs(x="Juegos desarollados",y="Calificacion")+
  theme(legend.position="none")

#Vemos que las calificaciones se encuentran en rangos parecidos con una media parecida 
#Al parecer los dearrolladores que obtienen mejores calificaciones son los que han desarrolado
#entre 1 y 4 juegos

cor(datos$Meta.Score,as.numeric(datos$frecuencia))
#No hay correlacion alguna entre la critica y el numero de juegos desarrolados por un estudio u otro

#Relacion desarrolador calificacion del usuario
datos$frecuencia<-as.factor(datos$frecuencia)
ggplot(datos,aes(x=frecuencia,y=User.Score,fill=frecuencia))+
  geom_boxplot()+
  ggtitle("Calificaciones de losusuarios respecto a no  de juegos desarrollados")+
  labs(x="Juegos desarollados",y="Calificacion")+
  theme(legend.position="none")

#Vemos que al igual que con la crítica los datos se encuentran en el mismo rango con mdias parecidad
#al publico paerecioegustarle mas los juegos que fueron desarrollados por un estudio con 3 juegos

cor(datos$User.Score,as.numeric(datos$frecuencia))
#Hay una pequeña correlacion de 0.11312 entre el número de juegos desarrollados y la 
#calificación del público

