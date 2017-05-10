setwd("~/Documents/Metodos_II/data_project/paramos/")
library(vegan)
library(raster)
library(ape)
lapply()

for (i in 1:14){
    assign(letters[i],read.csv(paste("datos/veg_", i,".csv", sep= "")))
  }





##################################################################

dat <- read.csv("01_los-Nevados_Chili-Barragán/veg_01.csv")

## shapefile páramos

par <- shapefile("sectores_paramos_Col/Shapes files/Complejos_Paramos_100K/Complejos de Paramos_Escala100K.shp")
plot(par)
str(par)
dat <- par@data



r_par <- rasterize(par, COLr)
plot(r_par)
