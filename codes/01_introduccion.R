#Packages----
library(sf)
library(tidyverse)
library(rworldxtra)
library(raster)
#Vectores----
#Data----
data("countriesHigh")
class(countriesHigh)
  #*Se en cuentra en el formato SpatialPolygonsDataFrama, un clase de archivo del paquete 'sp'
  #*#*Es un formato ?til, pero se transforma a 'sf' que es una forma m?s moderna de trabajar con datos vectoriales en R
mundo <- st_as_sf(countriesHigh);mundo
  #*st_as_sf(): trandformar formatos externos a sf
  #*sf:Simple Feature, formato que permite que el poligono sea una tabla siendo facil manejar los dato

#Visualizaci?n----
  #*ggplot(): nos permitir? m?s adelante mezclar pol?gonos con rasters (es uno de los pocos formatos de visualizaci?n que nos permite eso)
ggplot() + geom_sf(data = mundo, aes(fill = GDP_MD_EST))
#*gem_sf():funci?n de ggplot que permite visualizar objetos en formato sf
#*aes: aesthetics
colnames(mundo)
  
  #Filtro
#*IMPORTANTE: desde los datos anteriores puede aplicar un filtrado, esta es una de las gracias de sf
  #*Tambien se puede realizar con OGR (rgdal) pero se tiene que cambiar a data frame para manejar los datos y cambiarlo de vuelta a un OGR para graficar
africa <- mundo %>% dplyr::filter(continent == 'Africa') #'chaining'

af <- filter(mundo, continent == 'Africa') #'nesting'
  #*chain and nest don dos maneras distintas de hacer lo mismo, algunas personas prefieren emplear el comando de chain debido a que el codigo se lee de izquierda a derecha en vez de de dentro hacia afuera
ggplot() + geom_sf(data = af, aes(fill = POP_EST))

#Intento de realizar algo con OGR
setwd("~/Casanova/Universidad/Master/Tesis/Datos_Giancarlo/Trabajados/vector/version 3")
library(rgdal)
soilOGR=readOGR('soilData.shp')
soilOGR %>% as.data.frame() %>% select(Datos) 
soilOGR %>% as.data.frame() %>% plot() #no funciona

#Subsetear por GDP
PIB_Alto <- filter(mundo,GDP_MD_EST >= median(mundo$GDP_MD_EST))

ggplot() + geom_sf(data = PIB_Alto, 
                   aes(fill = GDP_MD_EST))
  #podemos observar que se generan vac?os en el mapa, esto es especialmente ?til en los vectores de tipo punto

#Superposici?n de capas
ggplot() + geom_sf(data = mundo) + geom_sf(data = PIB_Alto, 
                                           aes(fill = GDP_MD_EST)) + 
  theme_dark() + scale_fill_viridis_c()

#Modificar datos vectoriales----
africa=africa %>% mutate(poblacion_mill=POP_EST/1e+06) #chain
af=mutate(africa,poblacion_mill=POP_EST/1e+06) #nest
  #mutate(): permite agregar o modificar atributos, podemos ocupar el nombre de las variables (comodo de emplear)
ggplot() + geom_sf(data = africa,aes(fill=poblacion_mill))+
  theme_bw() + scale_fill_viridis_c()
  #*scale_fill nos permite cambiar la gama de colores, existen para datos continuos o discretos (scale_fill_viridis_c es continuo)

africa=africa %>% mutate(PIB_per_Cap=GDP_MD_EST/POP_EST)
af=mutate(africa,PIB_per_Cap=GDP_MD_EST/POP_EST)
ggplot()+geom_sf(data=af,aes(fill=PIB_per_Cap))+theme_bw()

africa %>% filter(PIB_per_Cap==max(PIB_per_Cap,na.rm = T)) %>% pull(NAME)
  #habian NA por lo que debieron ser removidos, Eq. Guinea es el?i??s con el PIB per capita m?s alto del continente africano

#Exportar y leer datos vectoriales (formato sf)----
  #seleccionar columnas para exportar
africa_sel=africa %>% select(NAME,poblacion_mill,PIB_per_Cap,GLOCAF)
  #de 57 variables se pas? a 5
write_sf(africa_sel,'africa.shp')
  #leer un archivo vectorial externo
afr2=read_sf('africa.shp')

ggplot()+geom_sf(data=afr2,aes(fill=GLOCAF))

#Descarga datos vectoriales desde R----
getData('ISO3') #lista de paises y sus denominaciones ISO3
NZ=getData(name='GADM',country='NZL',level=1) #country est? en formato ISO3 (nombre en 3 letras del pa?s)
  #*obtenci?n de datos geogr?ficos mediante descarga (formato OGR), poseen alta resoluci?n as? que pueden ser pesados
  #*niveles indican escalas; 0: pa?s, 1: regiones/estados
  #*Los datos se descargan y quedan en la carpeta indicada como wd
#Convertirlo en un sf
NZ=NZ %>% st_as_sf(NZ) #chain
NZ=st_as_sf(NZ) #nest

ggplot()+geom_sf(data=NZ,aes(fill=NAME_1)) 

#PERU
peru=getData(name='GADM',country='PER',level=1) %>% st_as_sf() #chaining
peru=st_as_sf(getData(name='GADM',country='PER',level=1)) #nesting

ggplot()+geom_sf(data=peru,aes(fill=NAME_1))+theme(legend.position='none')

#Creaci?n de puntos----
df=data.frame(lon=c(-76,-76),lat=c(-5,-4),casa=c('Grande','Chica')) %>%
  st_as_sf(coords=c(1,2),crs= '+proj=longlat +datum=WGS84 +no_defs')
  #pas? de ser un data frame a un simple feature (sf) apareciendo una columna de geometria

ggplot()+geom_sf(data=peru) + geom_sf(data = df,aes(col=casa))

#Filtrado----
peru$NAME_1 %>% unique() #chaining
unique(peru$NAME_1) #nesting

peru2=peru %>% filter(NAME_1 %in% c('Amazonas','Arequipa','Tacna'))
 # %in%: 'tiene que ser alguno de'
ggplot()+geom_sf(data=peru2)

#Raster----
#Descargar datos de precipitacion a trav?s de R
prec=getData('worldclim',res=10,var='prec');prec #podemos obtener otras variables tales como T? min, media o maxima entre otras
  #RasterStack de 12 capas (capas mensuales), resoluci?n de 0.16 x 0.16 GRADOS
plot(prec)
plot(prec,colNA='black')

#Subset de stacks
inv=prec[[c(6,7,8)]];inv;plot(inv)
  #IMPORTANTE: las escalas no son equivalentes, problema de los graficos estandar de los raster y por eso se emplea un paquete especial

rasterVis::levelplot(inv)
  #Si yo ocupo '::' no necesito cargar la libreria (library(rasterVis)), solo se necesita descargar el paquete previamente
  #*levelplot(): se genera una misma escala para los distintos rasters

#Operaciones r?pidas
total_inv=prec[[6]]+prec[[7]]+prec[[8]];plot(total_inv,colNA='black')
  #suma de los meses de invierno
pp_total=sum(prec);plot(pp_total,colNA='black')
  #suma de todos las capas (meses)

#Cortar un raster (en base a otro mapa)----
raster_africa=pp_total %>% crop(africa) #chaining
raster_africa=crop(pp_total,africa);raster_africa #nesting
  #IMPORTANTE: a pesar de que el pol?gono africa se encuentra en un formato sf (no OGR el cual es m?s similar a RasterLayer) esto no es impedimento para realizar este tipo de operaciones entre ellos
plot(raster_africa)

raster_peru=pp_total %>% crop(peru);plot(raster_peru)
  #obtengo el extent donde se encuentra peru pero este no se encuentra delimitado, una limitacion de solo emplear crop
raster_peru=pp_total %>% crop(peru) %>% mask(peru);plot(raster_peru) #chaining
raster_peru=mask(crop(pp_total,peru),peru);plot(raster_peru) #nesting
plot(crop(mask(pp_total,peru),peru))
  #*IMPORTANTE: el orden de aplicaci?n de una mascara o un crop es irrelevante desde el punto de vista que se obtiene el mismo resultado final
  #*A pesar de esto, se recomienda emplear un crop primero seguido de un mask esto es debido a que el crop es m?s r?pido 
  #*esta mayor rapidez puede ser debida a que se elimina m?s r?pido los datos que no son importante o ruido, al trabajar con menores volumenes de datos las operaciones se agilizan 

#Extracci?n de datos----
extract(raster_peru,df)
extract(raster_peru,df) %>% class() #puedo combinar nesting y chaining

#Creaci?n de columnas con los datos extraidos
  #1 dato extraido (1 capa)
df$prec=extract(raster_peru,df);df

  #12 datos extraidos provenientes de 12 capas
prec_casas=extract(prec,df);prec_casas
prec_casas %>% as.data.frame() %>% bind_cols(df)
#Exportar y leer un raster---
writeRaster(raster_africa,'pp_africa.grd',overwrite=T)
  #grd (grid) es un formato de raster, en este caso es el formato nativo de los datos de precipitacion
raster_africa2=raster('pp_africa.grd')
  #si fuese un stack debo emplear la funci?n stack() en vez de raster()
plot(raster_africa2)


#Proyecciones----
  #*IMPORTANTE: el ?rea de cada pixel no es el mismo para toda la imagen (las dimensiones son las mismas pero el area que representan no lo es)
  #*los sectores m?s extremos al sur y al norte presentan una menor ?rea por pixel que en la zona central
  #*Importantes consecuencias en hartas cosas como por ejemplo la proyecci?n
raster_africa2
proj4string(raster_africa2)

  #https://projectionwizard.org/#
  #*permite obtener proyecciones para trabajar con distintas propiedades de distorsi?n (pixeles de igual ?rea, pixeles equidistantes, mezclas entre ambos, etc)
newproj <- '+proj=laea +lon_0=17.578125 +lat_0=0 +datum=WGS84 +units=m +no_defs'
    #*proyecci?n para ese sector particular del mundo que permite generar mapas de igual ?rea

# Reproyecci?n
africa_igual <- projectRaster(raster_africa,
                              crs = newproj)
plot(africa_igual, colNA='black')
plot(raster_africa, colNA='black')
  #se puede observar que para la generaci?n de mapas con igual area las latitudes y longitudes presentan curvas

#Visualizacion de vectores y rasters en conjunto----
  #Transformar el raster a un data frame
africa_df <- raster_africa2 %>% as('SpatialPixelsDataFrame') %>% 
  as.data.frame() %>% 
  rename(Prec = layer) #renombrar la columna "layer" por "Prec"

head(africa_df, 10) #mostrar las primeras 10 filas
  #*el valor de precipitaci?n tiene el nomre de layer

ggplot() + 
  #Graficar el raster como base
  geom_tile(data = africa_df,
            aes(x = x, y = y, fill = Prec)) + 
  #Graficar el pol?gono superpuesto
  geom_sf(data = africa, 
          alpha = 0) + #*alpha=0: transparencia del relleno. Las l?neas seguir?n mostrandose
  #Escala de colores (continuo)
  scale_fill_viridis_c() + 
  #s/ los titulos de los ejes
  xlab('') + ylab('') + 
  #Tema de fono
  theme_bw()
  #*theme_dark(): tema oscuro
  #*theme_bw(): tema blanco y negro (fondo blanco)
  #*theme_gray(): fondo gris (default)

# Cambiar el sistema de coordenadas
ggplot() + geom_tile(data = africa_df,
                     aes(x = x, y = y, fill = Prec)) + 
  geom_sf(data = africa,
          alpha = 0) +
  coord_sf(crs = newproj) +
  theme_bw() + 
  scale_fill_viridis_c()
