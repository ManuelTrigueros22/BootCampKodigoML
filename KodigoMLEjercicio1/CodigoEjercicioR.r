library(ggplot2)
library(plyr)
library(dplyr)
#1)Carga de datos
#Se hacen los ajustes para poder leer el archivo csv
getwd()
#Me muevo a la carpeta donde tengo el Archivo para leerlo despues
setwd("/home/manueltrigueros/Documents/KodigoMachine/Entregable1/KodigoMLEjercicio1")
getwd()

#Se lee el DataSet y no se carga todo 
dfArchivoKaggle <-read.table("vgsales.csv",sep = ",", header = TRUE, fill = TRUE)
#2)Exploracion de Datos
#Se muestran los primeros 5 Registros
head(dfArchivoKaggle,5)

#Se muestran los ultimos 5 Registros
tail(dfArchivoKaggle,5)

# Estructura del dataframe
str(dfArchivoKaggle)

# Resumen estadístico
summary(dfArchivoKaggle)

#NombreColumnas
colnames(dfArchivoKaggle)

#Intentamos contar todos los Valores Nullos y se presentan
ValoresNulos <- sapply(dfArchivoKaggle, function(x) sum(is.na(x)))
print(ValoresNulos)

# Número total de registros
total_registros <- nrow(dfArchivoKaggle)

# Conteo de NA
total_na <- sum(is.na(dfArchivoKaggle))

# Mostrar información
cat("Total de registros:", total_registros, "\n")
cat("Total de valores nulos:", total_na, "\n")

#3) Limpieza de Datos
dfArchivoKaggle <- na.omit(dfArchivoKaggle)
# Número total de registros
total_registros <- nrow(dfArchivoKaggle)
# Conteo de NA
total_na <- sum(is.na(dfArchivoKaggle))
# Mostrar información
cat("Total de registros:", total_registros, "\n")
cat("Total de valores nulos:", total_na, "\n")

#4)Visualizacion del Resultado
#Conteo de Columna Plataforma
conteo_platform <- table(dfArchivoKaggle$Platform)

#Se hace conteo de cantidad de registros por plataforma
conteo_platform <- dfArchivoKaggle %>%
  count(Platform, sort = TRUE)

#Se presenta Cantidad de Registros por plataforma en grafico de barras
#Esto puede ayudar a determinar las tendencias de una plataforma
# Crear el gráfico de conteo
ggplot(conteo_platform, aes(x = reorder(Platform, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Conteo de Plataformas", x = "Plataforma", y = "Conteo") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Se hace conteo de cantidad de registros por plataforma
conteo_platform <- dfArchivoKaggle %>%
  count(Genre, sort = TRUE)

#Se presenta Cantidad de Registros por Genero en grafico de barras
#Esto puede ayudar a determinar las tendencias de un Genero y enfocarse en publicar mas de ese tipo 
# Crear el grafico del conteo de Genero 
ggplot(conteo_platform, aes(x = reorder(Genre, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(title = "Conteo de Plataformas", x = "Plataforma", y = "Conteo") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Se Genera un DataFrame Agrupado por Plataforma y que calcula la suma de Ventas globales (por plataforma)
ventas_por_plataforma <- dfArchivoKaggle %>%
  group_by(Platform) %>%
  summarise(Total_Global_Sales = sum(Global_Sales, na.rm = TRUE))
# Mostrar el resultado
print(ventas_por_plataforma)

#Se calcula la media de las ventas globales por plataforma y se muestra
Platform_avg <- dfArchivoKaggle %>%
  group_by(Platform) %>%
  summarise(Average_Global_Sales = mean(Global_Sales, na.rm = TRUE))
# Mostrar el resultado
print(Platform_avg)

#Grafico de pastel que representa el promedio de ventas globales por plataforma
# Crear el gráfico de pastel
ggplot(Platform_avg, aes(x = "", y = Average_Global_Sales, fill = Platform)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Promedio de Ventas Globales por Plataforma") +
  theme_void()  # Elimina el fondo y los ejes

#Se Genera un DataFrame Agrupado por genero y que calcula l suma de Ventas globales (por genero)
# Agrupar por 'Genre' y sumar 'Global_Sales'
ventas_por_genero <- dfArchivoKaggle %>%
  group_by(Genre) %>%
  summarise(Total_Global_Sales = sum(Global_Sales, na.rm = TRUE))
# Mostrar el resultado
print(ventas_por_genero)

#Se calcula la media de las ventas globales por genero
# Calcular el promedio de 'Global_Sales' por 'Genre'
dfGenre <- dfArchivoKaggle %>%
  group_by(Genre) %>%
  summarise(Average_Global_Sales = mean(Global_Sales, na.rm = TRUE))
# Mostrar el resultado
print(dfGenre)

#Grafico de pastel que representa el promedio de ventas globales por genero
# Crear el gráfico de pastel
ggplot(dfGenre, aes(x = "", y = Average_Global_Sales, fill = Genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Promedio de Ventas Globales por Género") +
  theme_void() +  # Eliminar fondo y ejes
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título

#Luego de haber levantado el mismo analisis en R y python no se si es porque me siento un poco mas familiarizado, veo que se pueden 
#Levantar el mismo analisis la misma limpieza y la misma presentacion de este DataSet, me gustaria practicar un poco mas con R para poder brindar 
#una conclusion mas tecnica
#5) Documentacion y Presentacion
#Se realiza un analisis del Dataset enfocandonos en dos Variables Plataforma y Genero para poder determinar en el periodo de tiempo que tiene el DataSet, que plataforma y categorias han sido compartidos, este analisis busca presentar la informacion para poder determinar estrategias tanto para los publisher como las personas de Marketing suponiendo que este anailisis se genera mes a mes, se podria buscar perfilar el comportamiento tanto de las plataformas como el genero, un ejemplo podria ser que yo veo que la plataforma de ps2  o DS en el genero.
#Hallazgos encontrados en el momento que se genero este DataSet la plataforma de PS2 y del Genero accion son las mas colocadas, pero al ver los promedios de ventas de plataformas y generos podriamos encontrar que el Genero Shooter es el que tiene un promedio de Ventas mayor y que las plataformas de GB y Nes tienen un promedio mas elevado de Ventas, tambien se podria hacer el analisis de forma inversa y ver los peores promedios.
#La conclusion es que con esta informacion que se pudiera generar mes a mes (para poder comparar en el tiempo) y determinar prioridades de colocacion y desarrollo de juegos dependiendo del Genero y de la plataforma esta informacion puede ser util para las empresas que desarrollan Juegos.