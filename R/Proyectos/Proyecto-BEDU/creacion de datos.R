library(tidyverse)

# Cargamos documento a descomponer
data <- read.csv("https://raw.githubusercontent.com/luisfelipetrj99/Proyectos-LFTJ/main/R/Datasets/steam.csv")

# ================================= Funciones ==============================================#
#Extrae opciones únicas de un vector compuesto
optUniques<- function(vect){
  
  #Primer filtro
  filter.one<- unique(vect)
  
  optu<-c()
  for (i in 1:length(filter.one)) {
    #Extraemos y unimos las opciones en un vector
    optu <- c( optu, unlist(strsplit( filter.one[i], ";" )))  
  }
  #Comparamos y retornamos las opciones únicas
  return(unique(optu))
}

#Genera columnas lógicas
logicalCols<- function(names, vec, nameInject){
  
  data<-c()
  for (i in 1:length(vec)) {                         
    
    #Se crea un vector de FALSE
    vLogical<- rep(FALSE, times = length(names))    
    
    #Descomponemos la cadena de cada fila
    rCurrent<- unlist(strsplit( vec[i], ";" ))      
    
    #Verificamos coincidencia de manera ordenada
    for (j in 1:length(rCurrent)) {
      #Asigna TRUE a coincidencias
      vLogical[which(names == rCurrent[j])] = TRUE;  
    }
    #Unimos los datos
    data<- rbind(data, vLogical)                    
  }
  
  #Coloca nombre clave a la columna
  for (i in 1:length(names)) {                       
    names[i]<-paste0(nameInject, names[i])     
  }
  colnames(data)<-names
  
  #Retorna los datos
  return(data)                                       
}
# =============================Generar columnas de otras ===================================#
#Extraemos los vectores de referencia
platformss<- optUniques(data$platforms)
categoriess<- optUniques(data$categories)
genress<- optUniques(data$genres)

#Genera nuevas columnas para unir
platformss_c<- logicalCols(platformss, data$platforms,  "platform_")
categoriess_c<- logicalCols(categoriess,data$categories, "categorie_")
genress_c<- logicalCols(genress,    data$genres,     "genre_")


#Unimos columnas
data <- cbind(data, platformss_c, categoriess_c, genress_c)

# Remueve variables de uso temporal
remove( platformss, categoriess, genress)
remove( platformss_c, categoriess_c, genress_c)

# ============================Unir con otro dataset==========================================#

#Importamos el dataset con el que vamos a unir
juegos<-read.csv("https://raw.githubusercontent.com/luisfelipetrj99/Proyectos-LFTJ/main/R/Datasets/metacritic.csv")

#Hacemos las transformaciones necesarias para combinar el dataset
juegos<- juegos %>%
  mutate(nombre=str_replace_all(gsub(" ","",toupper(juegos$Title)),c("-"="",":"="","'"=""))) %>%
  arrange(nombre) %>%
  select(-Release.Date)


datos<- data %>% 
  mutate(nombre=str_replace_all(gsub(" ","",toupper(data$name)),c("-"="",":"="","'"=""))) %>%
  arrange(nombre)

#Juntamos los datastes por nombres
data.Juegos<-merge(datos,juegos,by="nombre")

#Eliminamos lo que no necesitamos
data.Juegos<-select(data.Juegos,-nombre,-platforms,-categories,-genres)
data.Juegos<-select(data.Juegos,-Title,-Columna1,-appid)

#Creamos el csv
write.csv(data.Juegos,file="dataset.csv")

