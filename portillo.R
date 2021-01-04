# El directorio
setwd("~/GitHub/Trabajo_BigData/Trabajo_BigData")

# BORRANDO LAS VARIABLES DE ENTORNO
rm(list = ls())

#install.packages("rvest")
#install.packages("gdata")
#install.packages("stringr")

library('rvest')
library('gdata')
library('stringr')

################################################################################
##################### Desarrollo de la extracción de datos #####################
################################################################################

# Se creo un dataframe vacío, para agregar la extracción
todaLaInformacion <- data.frame()

# El día 02-01-2021 eran 15 páginas con un total de 283 autos
for(nroPagina in 1:15){
  
  # La paginación
  urlportillo <- paste('https://www.portillousados.cl/web/autos-usados?page=',nroPagina,sep = "")
  # El link
  portillo <- read_html(urlportillo)
  # El espacio donde se encuentran los autos en cada pagina
  listaProduc <- html_nodes(portillo, css = ".card")
  
  
  # El titulo + precio
  titulo <- html_nodes(listaProduc, ".card-title")
  textTitulo <- html_text(titulo)
  print(textTitulo)
  # Elimino el precio para conservar solo el titulo
  textTitulo <- gsub("  ","",textTitulo)
  textTitulo <- gsub("0","",textTitulo)
  textTitulo <- gsub("1","",textTitulo)
  textTitulo <- gsub("2","",textTitulo)
  textTitulo <- gsub("3","",textTitulo)
  textTitulo <- gsub("4","",textTitulo)
  textTitulo <- gsub("5","",textTitulo)
  textTitulo <- gsub("6","",textTitulo)
  textTitulo <- gsub("7","",textTitulo)
  textTitulo <- gsub("8","",textTitulo)
  textTitulo <- gsub("9","",textTitulo)
  textTitulo <- gsub("\n","",textTitulo)
  textTitulo <- gsub("[$]","",textTitulo)
  textTitulo <- gsub("[.]","",textTitulo)
  # Identifico solo el titulo por sus espacios
  textTitulo <- str_subset(textTitulo, " ")
  # Lo llevo a minusculas
  textTitulo <- tolower(textTitulo)
  print(textTitulo)
  

  # Precio
  precio <- html_nodes(listaProduc, ".card-price")
  textPrecio <- html_text(precio)
  textPrecio <- gsub("\n","",textPrecio)
  textPrecio <- gsub(" ","",textPrecio)
  textPrecio <- gsub("[$]","",textPrecio)
  textPrecio <- gsub("[.]","",textPrecio)
  print(textPrecio)
  
  # Datos: año, kl y transmisión estan juntos separados por espacios
  datos <- html_nodes(listaProduc, ".card-list")
  textDatos <- html_text(datos)
  print(textDatos)
  
  
  # El año
  # trunco las primeras 12 letras
  textoAnio <- str_trunc(textDatos, 12, "right")
  textoAnio <- gsub("[.]","",textoAnio)
  textoAnio <- gsub("Año:","",textoAnio)
  textoAnio <- gsub(" ","",textoAnio)
  print(textoAnio)
  
  
  # Los kilometros
  # trunco las primeras 35 letras
  textoKl <- str_trunc(textDatos, 35, "right")
  # trunco las ultimas 25 letras
  textoKl <- str_trunc(textoKl, 25, "left")
  textoKl <- gsub("[.]","",textoKl)
  textoKl <- gsub("kms\n","",textoKl)
  textoKl <- gsub(" ","",textoKl)
  print(textoKl)
  
  
  # La transmicion
  textoTransmi <- str_trunc(textDatos, 20, "left")
  textoTransmi <- gsub("[.]","",textoTransmi)
  textoTransmi <- gsub("\n","",textoTransmi)
  textoTransmi <- gsub(" ","",textoTransmi)
  textoTransmi <- gsub("á","a",textoTransmi)
  textoTransmi <- tolower(textoTransmi)
  print(textoTransmi)
  
  # boton ver mas
  entrando <- html_nodes(listaProduc, ".card-link")
  print(entrando)
  
  
  # El link de cada auto
  linkAuto <- html_attr(entrando, "href")
  textoLink <- gsub("/web","https://www.portillousados.cl/web",linkAuto)
  print(textoLink)
  
  # Se crea dataframe con cada variable que se quiere incluir
  item <- data.frame(marca = textTitulo, precio = textPrecio, año = textoAnio,
                     kilometraje = textoKl,transmicion = textoTransmi, link = textoLink)
  # Se almacena la informacion de todas las paginas y se unen
  # con los datos totales
  todaLaInformacion <- rbind(todaLaInformacion, item)
}

# Guardo los datos en formato csv
write.csv(todaLaInformacion,"info_portillo.csv")

################################################################################
########################## Desarrollo de los gráficos ##########################
################################################################################

### Para la visualización de los siguientes datos se usaron los datos en 
### la fecha 02-01-2021, día a día cambian las marcas de vehículos, lo que 
### cambia los gráficos.

#Data <- read.csv("info_portillo.csv")
#Data$X <- NULL

library(tidyverse)
library(dplyr)
library(ggplot2)

Data <- todaLaInformacion
names(Data)

# El tipo de variable
sapply(Data, mode)

# Necesito que sean valores numericos
Data <- transform(Data, precio = as.numeric(precio), 
          kilometraje = as.numeric(kilometraje))

sapply(Data, mode)

# le doy formato a la cariable
Data <- transform(Data, año = as.numeric(año))

### 1ero.- Año de los vehiculos ###
ggplot(data = Data) + geom_bar(mapping = aes(x=año), stat = "count",
                               fill="dodgerblue4",col="black")+
  labs(title = "Año del vehículo",subtitle = "Cantidad en venta")+
  theme_bw()

# necesito solo la marca
# Creo la Data 1, donde elimino las características del modelo 
# para agrupar por marca

Data1 <- mutate(Data, marca = gsub(" ","                              ",marca))
Data1 <- mutate(Data1, marca = str_trunc(marca, 25, "right"))
Data1 <- mutate(Data1, marca = gsub("[.]","",marca))
Data1 <- mutate(Data1, marca = gsub(" ","",marca))

# Marcas de los vehiculos
ggplot(data = Data1) + geom_bar(mapping = aes(x=marca), stat = "count",
                                fill="dodgerblue3",col="black")+
  labs(title = "Marca del vehículo",subtitle = "Cantidad en venta")

#install.packages("data.table")
library(data.table)

# cuantos autos hay en cada marca
C_Data1 <- select(Data1,marca) %>% count(marca) %>% arrange(desc(n))

# para filtrar las marcas mas repetidas
# creo subconjuntos de cada marca
nissan1 <- Data1%>%filter(marca == "nissan")
peugeot2 <- Data1%>%filter(marca == "peugeot")
toyota3 <- Data1%>%filter(marca == "toyota")
suzuki4 <- Data1%>%filter(marca == "suzuki")
kia5 <- Data1%>%filter(marca == "kia")
hyundai6 <- Data1%>%filter(marca == "hyundai")
chevrolet7 <- Data1%>%filter(marca == "chevrolet")
# marcas con más de 5 vehiculos
bmw8 <- Data1%>%filter(marca == "bmw")
honda9 <- Data1%>%filter(marca == "honda")
mitsubishi10 <- Data1%>%filter(marca == "mitsubishi")
subaru11 <- Data1%>%filter(marca == "subaru")
volkswagen12 <- Data1%>%filter(marca == "volkswagen")

## uniendo las datas de cada marca
g1_2 <- as.data.table(rbind(nissan1,peugeot2))
g1_3 <- as.data.table(rbind(g1_2,toyota3))
g1_4 <- as.data.table(rbind(g1_3,suzuki4))
g1_5 <- as.data.table(rbind(g1_4,kia5))
g1_6 <- as.data.table(rbind(g1_5,hyundai6))
g1_7 <- as.data.table(rbind(g1_6,chevrolet7))
# marcas con más de 5 vehiculos
g1_8 <- as.data.table(rbind(g1_7,bmw8))
g1_9 <- as.data.table(rbind(g1_8,honda9))
g1_10 <- as.data.table(rbind(g1_9,mitsubishi10))
g1_11 <- as.data.table(rbind(g1_10,subaru11))
g1_12 <- as.data.table(rbind(g1_11,volkswagen12))

### 2.-Marcas de los vehiculos mas repetidos ###
ggplot(data = g1_7) + geom_bar(mapping = aes(x=marca), stat = "count",
                               fill=c("ghostwhite","ghostwhite","ghostwhite",
                                      "royalblue","dodgerblue4", "blue",
                                      "navy"),col="black")+
  labs(title = "Marcas de vehículos",subtitle = "Con más disponibilidad")+
  theme_bw()

# para evitar la notacion cientifica en los ejes
options(scipen=999)

# elimino outliers, valores mayores a $30.000.000
g1_7 <- g1_7[!(g1_7$precio > 30000000), ]

### 3ro.- Dispercion de los precios segun marca y precio ###
ggplot(g1_7, aes(x=marca, y=precio))+ 
  geom_boxplot(color="black")+
  labs(title = "Precios de los vehículos",
       subtitle = "Con más disponibilidad")+
  theme_dark()+
  stat_sum(color="dodgerblue4")+
  stat_summary(fun = "mean", colour = "red", size = 2, geom = "point")

# elimino outliers, valores mayores a $30.000.000
Data1 <- Data1[!(Data1$precio > 30000000), ]

# precios de todas las marcas
ggplot(Data1, aes(x=marca, y=precio))+ 
  geom_boxplot(color="black")+
  labs(title = "Precios de los vehículos",
       subtitle = "Con más disponibilidad")+
  theme_dark()

# elimino outliers, valores mayores a $30.000.000
g1_12 <- g1_12[!(g1_12$precio > 30000000), ]

### 3ero.- dispercion de las marcas con 5 o mas autos ###
ggplot(g1_12, aes(x=marca, y=precio))+ 
  geom_boxplot(color="black")+
  labs(title = "Precios de los vehículos",
       subtitle = "Con más disponibilidad")+
  theme_dark()+
  stat_sum(color="dodgerblue4")+
  stat_summary(fun = "mean", colour = "red", size = 2, geom = "point")

# necesito que sean numericos para graficar
g1_7 <- transform(g1_7, precio = as.numeric(precio), 
                  kilometraje = as.numeric(kilometraje),
                  año = as.numeric(año))

sapply(g1_7, mode)

# Dispercion de los años segun marca
ggplot(g1_7, aes(x=marca, y=año))+ 
  geom_boxplot(color="black", fill="white")+
  labs(title = "Años de los vehículos",
       subtitle = "Con más disponibilidad")+
  theme_dark()

### 4to.- Transmicion de los vehiculos ###
ggplot(data = Data1) + geom_bar(mapping = aes(x=transmicion), stat = "count",
                                fill=c("dodgerblue4","firebrick4"),col="black")+
  labs(title = "Transmisión de los vehículos",subtitle = "Cantidad en venta")

# elimino outliers #
g1_12 <- g1_12[!(g1_12$kilometraje > 300000), ]

### 5to.- kilometraje por marca ###
ggplot(g1_12, aes(x=marca, y=kilometraje))+ 
  geom_boxplot(color="black", fill="white")+
  labs(title = "Kilometraje de los vehículos",
       subtitle = "Por marca")+
  theme_dark()+ stat_sum(color="dodgerblue4")+
  stat_summary(fun = "mean", colour = "red", size = 2, geom = "point")

# matriz de correlacion
sapply(Data1, mode)
D_cor <- Data1[-6]

# Creo la variable D_cor, para hacer una matriz de correlación
# Todas las variables deben ser numéricas
# La transmisión a numero
D_cor <- mutate(D_cor, transmicion = gsub("automatica","1",transmicion))
D_cor <- mutate(D_cor, transmicion = gsub("manual","0",transmicion))

# cuantos autos hay en cada marca
D_cor1 <- select(D_cor,marca) %>% count(marca) %>% arrange(desc(n))

# la marca a numero ## las marcas van cambaindo con el tiempo ##
D_cor <- mutate(D_cor, marca = gsub("audi","0",marca))
D_cor <- mutate(D_cor, marca = gsub("bmw","1",marca))
D_cor <- mutate(D_cor, marca = gsub("brilliance","2",marca))
D_cor <- mutate(D_cor, marca = gsub("byd","3",marca))
D_cor <- mutate(D_cor, marca = gsub("chery","4",marca))
D_cor <- mutate(D_cor, marca = gsub("chevrolet","5",marca))
D_cor <- mutate(D_cor, marca = gsub("citroen","6",marca))
D_cor <- mutate(D_cor, marca = gsub("daihatsu","7",marca))
D_cor <- mutate(D_cor, marca = gsub("ferrari","8",marca))
D_cor <- mutate(D_cor, marca = gsub("fiat","9",marca))
D_cor <- mutate(D_cor, marca = gsub("ford","10",marca))
D_cor <- mutate(D_cor, marca = gsub("honda","11",marca))
D_cor <- mutate(D_cor, marca = gsub("hyundai","12",marca))
#D_cor <- mutate(D_cor, marca = gsub("jeep","13",marca))
D_cor <- mutate(D_cor, marca = gsub("kia","14",marca))
D_cor <- mutate(D_cor, marca = gsub("mahindra","15",marca))
D_cor <- mutate(D_cor, marca = gsub("mazda","16",marca))
D_cor <- mutate(D_cor, marca = gsub("mercedes","17",marca))
D_cor <- mutate(D_cor, marca = gsub("mini","18",marca))
D_cor <- mutate(D_cor, marca = gsub("mitsubishi","19",marca))
D_cor <- mutate(D_cor, marca = gsub("nissan","20",marca))
D_cor <- mutate(D_cor, marca = gsub("opel","21",marca))
D_cor <- mutate(D_cor, marca = gsub("peugeot","22",marca))
D_cor <- mutate(D_cor, marca = gsub("renault","23",marca))
D_cor <- mutate(D_cor, marca = gsub("ssangyong","24",marca))
D_cor <- mutate(D_cor, marca = gsub("subaru","25",marca))
D_cor <- mutate(D_cor, marca = gsub("suzuki","26",marca))
D_cor <- mutate(D_cor, marca = gsub("toyota","27",marca))
D_cor <- mutate(D_cor, marca = gsub("volkswagen","28",marca))
D_cor <- mutate(D_cor, marca = gsub("volvo","29",marca))
D_cor <- mutate(D_cor, marca = gsub("changan","30",marca))
D_cor <- mutate(D_cor, marca = gsub("jaguar","31",marca))
D_cor <- mutate(D_cor, marca = gsub("lexus","32",marca))
D_cor <- mutate(D_cor, marca = gsub("skoda","33",marca))
D_cor <- D_cor[!is.na(D_cor$marca),]

# para sacar correlacion necesito que sean numericas
D_cor <- transform(D_cor, marca = as.integer(marca), 
                  año = as.integer(año),
                  transmicion = as.numeric(transmicion))

sapply(D_cor, mode)

cor(D_cor)

### grafico de la correlacion: kl y precio
Data2 <- Data1[-1]
Data2 <- Data2[-2]
Data2 <- Data2[-3]
Data2 <- Data2[-3]

library("corrplot")
mcor <- round(cor(Data2),2)
corrplot(mcor, method = "shade",
         shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black",
         addcolorlabel ="no", order = "AOE")

### grafico de la correlacion: año y precio
Data3 <- Data1[-1]
Data3 <- Data3[-3]
Data3 <- Data3[-3]
Data3 <- Data3[-3]
names(Data3)

# necesito que año sea numero
Data3 <- transform(Data3, año = as.numeric(año))

mcor <- round(cor(Data3),2)
corrplot(mcor, method = "shade",
         shade.col = NA, tl.col = "black",
         tl.srt = 45, addCoef.col = "black",
         addcolorlabel ="no", order = "AOE")

