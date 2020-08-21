#set working directory to the imagen carpeta 
setwd("C:/Users/Diana/OneDrive - Universidad del rosario/Ms/ImaSurfRefle")
# crs: EPSG:32619 WGS 84 / UTM zone 19N

# load spatial packages
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(sf)
library(sp)
library(ggplot2)
library(tidyverse)
library(RGISTools)

# turn off factors
#options(stringsAsFactors = FALSE)

#cargar tifs y separarlos panet RE----
l <- list.files(path = "TIFS2012_2020", pattern = ".tif$", full.names = T)
planet <- lapply(l[c(5,6,7,8)], stack)
re <- lapply(l[c(1,2,3,4)], stack)


#cargar capa de casco para recortar NDVI-----
c<-read_sf("CascoShape/CUcomplete_projected19N.shp")
casco <- st_zm(c) #to drop z dimension
st_crs(casco)$epsg
st_crs(casco)$proj4string
crs(casco)
class(casco)
urbano <- readOGR("CascoShape/CUcomplete_projected19N.shp")


x2012n<-crop(re[[1]],urbano)
x2012n1 <- mask(x2012n,  urbano)
plot(x2012n1)
Ndvi2012n1<-(x2012n1[[5]] - x2012n1[[3]]) / (x2012n1[[5]] + x2012n1[[3]])
plot(Ndvi2012n1)

#Calcular NDVI Planet y RE-----
ndvi_pl <- function(x) return((x[[4]] - x[[3]]) / (x[[4]] + x[[3]]))
ndvi_re <- function(x) return((x[[5]] - x[[3]]) / (x[[5]] + x[[3]]))
NDVIs_planet<-lapply(planet,ndvi_pl)
NDVIs_rapideye<-lapply(re,ndvi_re)


#función NDVI dataframe cada año----
NDVIdf<-function(rlist,i,yyyy){
        NDVIyear<-crop(rlist[[i]],casco)
        NDVIyear_c <- mask(NDVIyear, casco)
        NDVIyear_c_df <- raster::as.data.frame(NDVIyear_c, xy = TRUE)
        NDVIyear_c_df <- na.omit(NDVIyear_c_df)
        NDVIyear_c_df$year<-as.factor(yyyy)
        as_tibble(NDVIyear_c_df)
        return(NDVIyear_c_df)
}

NDVI2012_c_df<-NDVIdf(NDVIs_rapideye,1,2012)
NDVI2013_c_df<-NDVIdf(NDVIs_rapideye,2,2013)
NDVI2014_c_df<-NDVIdf(NDVIs_rapideye,3,2014)
NDVI2016_c_df<-NDVIdf(NDVIs_rapideye,4,2016)
NDVI2017_c_df<-NDVIdf(NDVIs_planet,1,2017)
NDVI2018_c_df<-NDVIdf(NDVIs_planet,2,2018)
NDVI2019_c_df<-NDVIdf(NDVIs_planet,3,2019)
NDVI2020_c_df<-NDVIdf(NDVIs_planet,4,2020)




#graficar NDVIdataframe----
NDVIeach<-function(NDVIey,year){
        ggplot() + 
                geom_raster(data=NDVIey,aes(x = x, y = y, fill = layer))+
                scale_fill_gradientn(colors = c("#800026","#E31A1C","#FD8D3C","#FEB24C","#FED976",
                                                "#FFFFCC","#EDF8E9","#74C476","#31A354","#006D2C"),
                                     values = scales::rescale(c(-1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)))+
                theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(), #eliminar del eje y marcas y texto
                      axis.ticks.x=element_blank(),axis.text.x=element_blank(), #eliminar del eje x marcas y texto
                      panel.grid = element_line(colour = 'transparent'),
                      panel.background = element_rect(colour="#E0E0E0",fill=NA), 
                      strip.background = element_rect(colour="#E0E0E0"),
                      legend.position = "none")+
                coord_equal(ratio=1)+ #plot cuadrado para evitar la distorsión
                facet_grid(. ~ year)+
                labs(x="",y="") #sin nombres de ejes
}

a<-NDVIeach(NDVI2012_c_df,2012)
b<-NDVIeach(NDVI2013_c_df,2013)
c<-NDVIeach(NDVI2014_c_df,2014)
d<-NDVIeach(NDVI2016_c_df,2016)
e<-NDVIeach(NDVI2017_c_df,2017)
f<-NDVIeach(NDVI2018_c_df,2018)
g<-NDVIeach(NDVI2019_c_df,2019)
h<-NDVIeach(NDVI2020_c_df,2020)

b1<-b + theme(legend.position="top")+guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5))

#grilla de gráficas NDVI 
library(patchwork)
NDVIyearsgrid<-(a | b1 | c | d)/(e | f | g | h) #una debajo de la otra


grid.newpage()
grid.draw(legend)

guides(fill = guide_colourbar(barwidth = 0.5, barheight = 15))

par(mfrow=c(4,2))
display.brewer.all(colorblindFriendly = TRUE) #aparecen todas las paletas
display.brewer.pal(n = 6, name = 'Greens') #aparece la plaeta
brewer.pal(n = 6, name = "Greens") #aparecem los nombres

display.brewer.pal(n = 9, name = 'YlOrRd')
brewer.pal(n = 9, name = "YlOrRd")
par(mfrow=c(1,1))


#almacenar todos los NDVI en un solo dataframe----
NDVIcyears12_16 = bind_rows(NDVI2012_c_df,NDVI2013_c_df,NDVI2014_c_df,NDVI2016_c_df)

NDVIcyears17_20 = bind_rows(NDVI2017_c_df,NDVI2018_c_df,NDVI2019_c_df,NDVI2020_c_df)

#calcular carbono y biomasa con formula sobre NDVI para trees. >0.55 RE y >0.56 planet

trees1216<-NDVIcyears12_16%>%
        filter(layer>=0.55)

trees1216<-trees1216 %>% 
        mutate(Ckgpixel=107.2*exp(layer*0.0194)) #esta formuila solo es pa landsat

trees1216 %>% 
        group_by(year) %>% 
        summarise(total_Cyear=sum(Ckgpixel)) 

trees1720<-NDVIcyears17_20%>%
        filter(layer>=0.56)

trees1720<-trees1720 %>% 
        mutate(Ckgpixel=107.2*exp(layer*0.0194)) #esta formula solo es pa landsat

NDVIcyears<-bind_rows(trees1216,trees1720)

NDVIcyears = NDVIcyears %>% 
        rename(NDVI=layer)
glimpse(NDVIcyears)
class(NDVIcyears)

#calcular carbono por año
NDVIcyears %>% 
        group_by(year) %>% 
        summarise(total_Cyear=sum(Ckgpixel))



#raster de NDVI por cada año----- 
NDVIras<-function(rlist,i){
        NDVIyear<-crop(rlist[[i]],urbano)
        NDVIyear_c <- mask(NDVIyear, urbano)
}

NDVI2012_c_ras<-NDVIras(NDVIs_rapideye,1)
NDVI2013_c_ras<-NDVIras(NDVIs_rapideye,2)
NDVI2014_c_ras<-NDVIras(NDVIs_rapideye,3)
NDVI2016_c_ras<-NDVIras(NDVIs_rapideye,4)
NDVI2017_c_ras<-NDVIras(NDVIs_planet,1)
NDVI2018_c_ras<-NDVIras(NDVIs_planet,2)
NDVI2019_c_ras<-NDVIras(NDVIs_planet,3)
NDVI2020_c_ras<-NDVIras(NDVIs_planet,4)


# plot the data
par(mfrow=c(2,4))
a<-plot(NDVI2012_c_ras,
        main = "NDVI 2012",
        axes = FALSE, box = FALSE,
        col.regions=rev(terrain.colors(20)))
b<-plot(NDVI2013_c_ras,
        main = "NDVI 2013",
        axes = FALSE, box = FALSE)
c<-plot(NDVI2014_c_ras,
        main = "NDVI 2014",
        axes = FALSE, box = FALSE)
d<-plot(NDVI2016_c_ras,
        main = "NDVI 2016",
        axes = FALSE, box = FALSE)
e<-plot(NDVI2017_c_ras,
        main = "NDVI 2017",
        axes = FALSE, box = FALSE)
f<-plot(NDVI2018_c_ras,
        main = "NDVI 2018",
        axes = FALSE, box = FALSE)
g<-plot(NDVI2019_c_ras,
        main = "NDVI 2019",
        axes = FALSE, box = FALSE)
h<-plot(NDVI2020_c_ras,
        main = "NDVI 2020",
        axes = FALSE, box = FALSE,
        col = rev(terrain.colors(9)))
dev.off()

# view distribution of NDVI values
ah<-hist(NDVI2012_c_ras,
     main = "NDVI: Distribution of pixels\n 2012",
     col = "springgreen",
     xlab = "NDVI Index Value")
bh<-hist(NDVI2013_c_ras,
         main = "NDVI: Distribution of pixels\n 2013",
         col = "springgreen",
         xlab = "NDVI Index Value")
ch<-hist(NDVI2014_c_ras,
         main = "NDVI: Distribution of pixels\n 2014",
         col = "springgreen",
         xlab = "NDVI Index Value")
dh<-hist(NDVI2016_c_ras,
         main = "NDVI: Distribution of pixels\n 2016",
         col = "springgreen",
         xlab = "NDVI Index Value")
eh<-hist(NDVI2017_c_ras,
         main = "NDVI: Distribution of pixels\n 2017",
         col = "springgreen",
         xlab = "NDVI Index Value")
fh<-hist(NDVI2018_c_ras,
         main = "NDVI: Distribution of pixels\n 2018",
         col = "springgreen",
         xlab = "NDVI Index Value")
gh<-hist(NDVI2019_c_ras,
         main = "NDVI: Distribution of pixels\n 2019",
         col = "springgreen",
         xlab = "NDVI Index Value")
hh<-hist(NDVI2020_c_ras,
         main = "NDVI: Distribution of pixels\n 2020",
         col = "springgreen",
         xlab = "NDVI Index Value")



# calculate the NDVI image
img.mod.ndvi <- varNDVI(planet[[4]][[3]],planet[[4]][[4]])
# plot the image
spplot(NDVI2020_c_ras,col.regions=rev(terrain.colors(20)))


#Exportar raster----
writeRaster(x = NDVI2012_c_ras,
            filename="1Rasters/ndvi2012.tif",
            format = "GTiff", # save as a tif
            datatype='INT2S', # save as a INTEGER rather than a float
            overwrite = TRUE,options=c('TFW=YES'))  # OPTIONAL - be careful. This will OVERWRITE previous files.

#método para arcgis
library(arcgisbinding)
arc.check_product()
arc.write('1Rasters/re/x2012.tif', NDVI2012_c_ras)
arc.write('1Rasters/re/x2013.tif', NDVI2013_c_ras)
arc.write('1Rasters/re/x2014.tif', NDVI2014_c_ras)
arc.write('1Rasters/re/x2016.tif', NDVI2016_c_ras)
arc.write('1Rasters/planet/x2017.tif', NDVI2017_c_ras)
arc.write('1Rasters/planet/x2018.tif', NDVI2018_c_ras)
arc.write('1Rasters/planet/x2019.tif', NDVI2019_c_ras)
arc.write('1Rasters/planet/x2020.tif', NDVI2020_c_ras)

#importar rasters para hacer diferencia----
listaNDVIre<-list.files(path = "1Rasters/re",".tif$", full.names = T)
ras_re <- brick(stack(listaNDVIre))

listaNDVIpl<-list.files(path = "1Rasters/planet",".tif$", full.names = T)
ras_pl <- brick(stack(listaNDVIpl))


#guardar los rasters de rasre y pl en un solo tif
library(arcgisbinding)
arc.check_product()
arc.write('1Rasters/re/ndvi2012_2016.tif', ras_re)
arc.write('1Rasters/planet/ndvi2017_2020.tif', ras_pl)

#Hacer la diferencia entre los años 2012-2016 y 2017-2020
library(Kendall)
fun_k<-function(x){return(unlist(MannKendall(x)))}

#2012-2016
kendall_result12_16<-calc(ras_re,fun_k)
plot(kendall_result12_16$tau)

#2017-2020
kendall_result17_20<-calc(ras_pl,fun_k)
plot(kendall_result17_20$tau)

#recortamos al area del casco urbano 
urbano <- readOGR("CascoShape/CUcomplete_projected19N.shp")
tau_urbano12_16<-crop(kendall_result12_16$tau, urbano)
tau_urbano_f12_16 <-mask(tau_urbano12_16, urbano)
plot(tau_urbano_f12_16)

tau_urbano17_20<-crop(kendall_result17_20$tau, urbano)
tau_urbano_f17_20 <-mask(tau_urbano17_20, urbano)
plot(tau_urbano_f17_20)

# Exportamos la tendencia (tau) a un tif para 2012-2016 y 2017-2020
library(arcgisbinding)
arc.check_product()
arc.write('1Rasters/re/tau_urbano_f12_16.tif', tau_urbano_f12_16)
arc.write('1Rasters/planet/tau_urbano_f17_20.tif', tau_urbano_f17_20)


# Reclasificamos para ver mejor los sitios con tendencia muy positiva y muy negativa
m <- c(-1, -0.25, -1, -0.25, 0.5, 0, 0.5, 1, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

rc_tau12_16<-reclassify(tau_urbano_f12_16, rclmat)
rc_tau17_20 <- reclassify(tau_urbano_f17_20, rclmat)

plot(rc_tau_12_16) #blanco tendencia muy negativa, amarillo tendencia neutra y verdes tendencia positiva 
plot(rc_tau17_20)

#Exportamos tau reclasificado para 2012-2016 y 2017-2020
arc.write('1Rasters/re/rc_tau_12_16.tif', rc_tau12_16)
arc.write('1Rasters/planet/rc_tau_17_20.tif', rc_tau17_20)


