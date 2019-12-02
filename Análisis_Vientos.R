library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(readxl)
library(sf)
library(mapview)
#library(viridis)


# PROMEDIAR MULTIBANDA 

# Dirección de la carpeta donde se encuentra el raster
# setwd("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/RASTER/BOFEDALES")

# Conversión 
# st <- stack("DO_Delimitado.tif") # Raster stack (multicapa)
# m <- mean(st) # multicapa -> monocapa

# Guardado del raster promedio

# writeRaster(m,"DO_Delimitado_mean.tif")

# Multiploteo

# par(mfrow=c(3,4)) 
# par(lab=c(5,10,10)); plot(1:5)
# setwd("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/RASTER/BOFEDALES")
# layout(matrix(c(9, 6, 7, 3,12, 10, 1, 4,2, 11, 8, 5), nrow=3, byrow=TRUE))
# par(mfrow=c(3,4))
# meses <- c('Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre','Enero', 'Febrero', 'Marzo', 'Abril')

#meses <- c ('Mayo', 'Diciembre', 'Julio', 'Agosto',
#            'Septiembre', 'Enero', 'Abril', 'Junio', 'Octubre', 'Febrero', 'Marzo', 'Noviembre')

# MULTIPLOTEO -------------------------------------------------------------

layout(matrix(c(9, 6, 7, 3,12, 10, 1, 4,2, 11, 8, 5), nrow=3, byrow=TRUE))
v <- c(9, 6, 7, 3,
  12, 10, 1, 4,
  2, 11, 8, 5)
for(i in v) {
  Vientos_multibanda <- raster("DO_Delimitado.tif", band = i)
  plot(Vientos_multibanda, main = meses[i])
  plot(punto, add = T)
}

# par(mfrow=c(3,4)) # Multiploteo de imagenes en 3 filas y 4 columnas (estaciones)
# layout(matrix(c(1,4,3,2),ncol=2)) # Se puede ordenar la ubicación
# coloca el primer diagrama a la izquierda, el siguiente a la derecha, el tercero a la derecha y el último a la izquierda.



# Extracción de valores del raster por cada pixel
# install.packages("velox")
# library(velox)

# Raster promedio

# raster_mean = raster("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/RASTER/BOFEDALES/DO_Delimitado_mean.tif")
# plot(raster_mean)
# punto = shapefile("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/VECTOR/Puntos/Puntos_Lima_Pasco_Junin/Doce_Octubre.shp") # Área como polígono
# punto  = spTransform(punto, proj4string(raster_mean))
# plot(punto, add = T)
# extract(raster_mean, punto)
# par(mfrow=c(1,1), lab=c(1,5,1))


# Visualización de la data con mapview ------------------------------------

setwd("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/RASTER/BOFEDALES")

m <- mapview(raster_mean, at = seq(0.2, 4,length.out = 20)) + punto
m <- mapview(raster_mean,
             at = seq(0.20, 4,length.out = 20),
             layer.name = 'Wind Speed Mean') + mapview(punto, layer.name = 'Estación Meteorologica 12 de octubre')
# meses <- c ('Mayo', 'Diciembre', 'Julio', 'Agosto',
#            'Septiembre', 'Enero', 'Abril', 'Junio', 'Octubre', 'Febrero', 'Marzo', 'Noviembre')
meses <- c('Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre','Enero', 'Febrero', 'Marzo', 'Abril')

for(i in 1:12){
  m = (m + mapview(raster("DO_Delimitado.tif", band = i),
  at = seq(0.20, 4,length.out = 20),
  layer.name = paste('Wind Speed',as.character(meses[i]))))
}
m
plot(raster("DO_Delimitado.tif", band = 1))

mapshot(m, url = 'ws_docemeses.html') # Para guardar como html
# plot(raster_mean, axes = T)
# col = sf.colors(n = 10, cutoff.tails = c(0, 0.1), alpha = 1,categorical = FALSE)
# legend(-76.7, -10.5, legend='hola', pch=14)
# legend('topleft', legend=1:4, pch=1:13, title='Código', bty='n', inset=0.01, cex=1.2)


# vx = velox(st)
# vxe = vx$extract(Area, fun = function(x){mean(x, na.rm=T)})            # Cambiar la funcion (en este caso es mean)
# write.csv(t(vxe), "D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/VECTOR/vientos_df_point.csv")    # Ruta para guardar como csv


# ANALISIS ESTACION -----------------------------------------------

estacion <- read_excel('D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/EXCEL/Doce_Octubre.xlsx')

estacion["VELOCIDAD"] <- as.numeric(estacion$VELOCIDAD)

# Correccion de datos

for(i in 1:length(estacion$VELOCIDAD)) {
  if(is.na(estacion$VELOCIDAD[i])) {
    estacion$VELOCIDAD[i] = estacion$VELOCIDAD[i - 1]
  } else {
    estacion$VELOCIDAD[i] = estacion$VELOCIDAD[i]
  }
}

# Filtrar por meses 

MES <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto',
            'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
#estacion %>% filter(MES == 7) %>% summarise(mean(VELOCIDAD))
lineal <- estacion %>% group_by(MES) %>% summarise(mean_V = mean(VELOCIDAD))
lineal["MES"] = MES
lineal

names(lineal) <- c("meses", "mean_V")
lineal

# ANALISIS RASTER  ----------------------------------------------

punto = shapefile("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/VECTOR/Puntos/Doce_Octubre.shp") # Área como polígono
raster_mean <- raster("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/RASTER/BOFEDALES/DO_Delimitado.tif")
punto  = spTransform(punto, proj4string(raster_mean))
ras_mean <- c()
for(i in 1:12) {
  Vientos_multibanda = raster("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/RASTER/BOFEDALES/DO_Delimitado.tif", band = i)
  ras_mean[i] = extract(Vientos_multibanda, punto)
}
meses <- c('Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre','Enero', 'Febrero', 'Marzo', 'Abril')

df <- data.frame(ras_mean, meses)
df


# JOIN, R y R2 ------------------------------------------------------------

a <- merge(lineal,df, by = "meses")
names(a) <- c("MES", "ESTACION", "FLDAS")
View(a)
a["ESTACION"] <- round(a$ESTACION,2)
a["FLDAS"] <- round(a$FLDAS,2)

write.csv(a, file = "D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/EXCEL/12_octubre_rl.csv")
l <- read.csv("D:/JULIUS/UNIVERSIDAD/CICLO IV/CLIMA/Trabajo_inv/procedimiento/GEE/EXCEL/12_octubre_rl.csv", sep = ";")
l


# REGRESION LINEAL --------------------------------------------------------

attach(a)
(cor(a$ESTACION,a$FLDAS))^2
plot(a$ESTACION,a$FLDAS)

modelo <- lm(ESTACION ~ FLDAS, data = a)
summary(modelo)


speedp <- predict(modelo) #Valores Predichos
b0 <- round(modelo$coefficients[1],2)
b1 <- round(modelo$coefficients[2],2)
b0

ggplot(a, aes(ESTACION, FLDAS), main = "REGRESIÓN LINEAL SIMPLE", ylab = "Velocidad (mph)",
       xlab = "Distancia(ft)")+
  geom_point(shape = 1)+ 
  geom_text(x =3.5 , y = 4.5, aes(label = paste("Y", " = ", b0, " + ", b1, "*","X"))) +
  geom_smooth(method = lm, se = FALSE, colour = "red")

# qplot(x = ESTACION, y = FLDAS,data = l, 
#       main = "Velocidad vs Distancia", ylab = "Velocidad (mph)",
#       xlab = "Distancia(ft)", geom = c("point")) + geom_line(aes(y=speedp), lwd =  0.1, color = 4) +
#   geom_text(x =2.3 , y = 3.1, aes(label = paste("Velocidad^", " = ", b0, " + ", b1, "*","Distancia"))) +
#   geom_smooth(method = lm, se = FALSE)

# qplot(Bwt, Hwt, data = cats) + 
#   stat_smooth(method = lm, se = FALSE) + 
  
