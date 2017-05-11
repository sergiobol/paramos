setwd("~/Documents/Metodos_II/data_project/paramos/")
library(vegan)
library(raster)
library(ape)

for (i in 1:14){
    assign(letters[i],read.csv(paste("datos/veg_", i,".csv", sep= "")))
  }

merge(a, b, all.x = T, all.y = T) ## merge une los data frames por las filas que tienen en común en cierta columna

# d1 <- merge(a, b, all.x = T, all.y = T)
# d2 <- merge(d1, c, all.x = T, all.y = T)
# d3 <- merge(d2, d, all.x = T, all.y = T)
# d4 <- merge(d3, e, all.x = T, all.y = T)
# d5 <- merge(d4, f, all.x = T, all.y = T)
# d6 <- merge(d5, g, all.x = T, all.y = T)
# d7 <- merge(d6, h, all.x = T, all.y = T)
# d8 <- merge(d7, i, all.x = T, all.y = T)
# d9 <- merge(d8, j, all.x = T, all.y = T)
# d10 <- merge(d9, k, all.x = T, all.y = T)
# d11 <- merge(d10, l, all.x = T, all.y = T)
# d12 <- merge(d11, m, all.x = T, all.y = T)
# d13 <- merge(d12, n, all.x = T, all.y = T)

####suma de observaciones
lista <- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n)

s <- data.frame()
s[1,1] <- length(rownames(lista[[2]]))+length(rownames(lista[[1]]))

for (k in 2:13){
  s[k,1] <- s[k-1,1]+length(rownames(lista[[k+1]]))
} # s es la suma total de las observaciones


## Concatenando
lista[[1]] <- merge(lista[[1]], lista[[2]], all.x = T, all.y = T)
for (j in 2:13){
  assign(lista[[j]], merge(lista[[j-1]], lista[[j+1]], all.x = T, all.y = T))
}

dim(lista[[13]])

##################################################################

dat <- read.csv("01_los-Nevados_Chili-Barragán/veg_01.csv")

## shapefile páramos

par <- shapefile("sectores_paramos_Col/Shapes files/Complejos_Paramos_100K/Complejos de Paramos_Escala100K.shp")
plot(par)
str(par)
dat <- par@data



r_par <- rasterize(par, COLr)
plot(r_par)
