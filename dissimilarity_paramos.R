setwd("~/Documents/Metodos_II/")
library(vegan)
library(raster)
library(ape)
library(ggplot2)


for (p in 1:14){
    assign(letters[p],read.csv(paste("data_project/paramos/datos/veg_", p,".csv", sep= "")))
  }

merge(a, b, all.x = T, all.y = T) ## merge une los data frames por las filas que tienen en común en cierta columna

####suma de observaciones
lista <- list(a,b,c,d,e,f,g,h,i,j,k,l,m,n)

s <- data.frame()
s[1,1] <- length(rownames(lista[[2]]))+length(rownames(lista[[1]]))

for (q in 2:13){
  s[q,1] <- s[q-1,1]+length(rownames(lista[[q+1]]))
} # s es la suma total de las observaciones


## Concatenando
lista[[1]] <- merge(lista[[1]], lista[[2]], all.x = T, all.y = T)

for (r in 2:13){
  lista[[r]] <- merge(lista[[r-1]], lista[[r+1]], all.x = T, all.y = T)
}

data <- lista[[13]]

#NAs en las columnas
for (t in 1:length(dat3)){
  print(t)
  print(length(which(is.na(dat3[,t]))))

}

dat2 <- data[,13:35]
dat3 <- dat2[,-c(2:7, 14, 22)] 
##################################################################

dat <- read.csv("data_project/paramos/datos_trabajo.csv")

## Matriz de abundancia
abd <- table(dat$locationRemarks, dat$scientificName)
ab <- as.data.frame.matrix(abd)
pa <- decostand(abd, method = "pa", 1) ## presencia-ausencia
rel_abd <- decostand(ab, method = "total", 1) ## abundancia relativa
##abundancia
m_dbray <- vegdist(abd, method = "bray")

pcoa2 <- pcoa(m_dbray, correction = "cailliez")#correction = se utiliza para corregir eigenvalues negativos cailliez
biplot(pcoa2)
pcoa2$values

## Abundancia relativa
m_relabd <- vegdist(rel_abd, method= "bray")

pcoa_abdrel <- pcoa(m_relabd)
biplot(pcoa_abdrel)
pcoa2$values


##Presencia-ausencia
m_djac <- vegdist(pa, method = "jaccard")
m_dsor <- vegdist(pa, method = "bray")

pcoa3 <- pcoa(m_djac, correction = "cailliez")#correction = se utiliza para corregir eigenvalues negativos cailliez
biplot(pcoa3)

pcoa4 <- pcoa(m_dsor, correction = "cailliez")#correction = se utiliza para corregir eigenvalues negativos cailliez
biplot(pcoa4)

### Rasters clima ##

COL<-shapefile("Clim_data/COL_admin/COL_adm0.shp")
DEP<-shapefile("COL_admin/Col_admin_1.shp")
plot(COL)
plot(DEP)

temp<-raster("bioclim/bio1.bil")
temp<-temp/10
plot(temp)

TCOL<-crop(temp, extent(COL))
COLr<-rasterize(COL, TCOL)
plot(COLr)
COLr

mTCOL<-as.matrix(TCOL)
mCol<-as.matrix(COLr)
temp<-mTCOL*mCol

rTemp<-raster(temp, xmn=-81.84153 , xmx=-66.87033, ymn=-4.228429, ymx=15.91248)
plot(rTemp)


## shapefile páramos

par <- shapefile("sectores_paramos_Col/Shapes files/Complejos_Paramos_100K/Complejos de Paramos_Escala100K.shp")
plot(par)
str(par)
dat <- par@data



r_par <- rasterize(par, COLr)
plot(r_par)
