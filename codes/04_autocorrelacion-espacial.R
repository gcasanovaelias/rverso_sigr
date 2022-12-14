#Packages----
install.packages('gstat')
library(gstat)
library(raster)
library(rworldxtra)
library(sf) #simple features
library(sp) #se instala en conjunto con rgdal. sp es la libreria que trabaja con el formato 'SpatialPointsPolygons' por ejemplo
library(tidyverse)

#1. Pre trabajo----
  #Cargar los datos de meuse----
  #datos en formato dataframe de concentracion de metales pesados, su cercania a un rio om y el uso de suelo
data(meuse)
class(meuse)

  #Transformamos a sf----
Meuse = st_as_sf(meuse, 
                 coords = c(1, 2),
                 crs = 28992) #? '+init=epsg:28992' (entre comillas)
 
class(Meuse)

  #Transformamos a sp (SpatialPointsDataframe)----
coordinates(meuse) = ~x + y
class(meuse)
  #ahora la clase de meuse esta en otro formato solo con emplear la funcion coordinates()

#?cual es la diferencia entre los formataos sp y sf?
View(Meuse) #en el formato sf puedo seguir observando los datos como df (emplearse en el tidyverse)
View(meuse) #no todas las herramientas aceptan el formato sf, a?n

  #Graficamos concentracion de Zn----
ggplot() + geom_sf(data = Meuse, aes(color = zinc)) + 
  scale_color_viridis_c() + 
  theme_bw()

#2. Modelos de interpolacion----
  #Interpolacion de valores en pixeles desconocidos en base a distancia
  #Por ejemplo, es util emplear modelos de interpolacion cuando trabajamos con datos de estaciones meteorologicas
  #objetivo: predecir cual va ser el valor entre los puntos de la variable de interes

  #Semi-variograma----
  #*Modelo nulo
  #*Que tanto var?an los valores seg?n la distancia
  #*medida de la variacion de la variable de interes de acuerdo a la distancia
  #*cuanto m?s distancia tenemos mayor ser? la variacion (semivarianza) de los valores (Primera ley de la geografia). Es decir, en la medida que los puntos se encuentren m?s cercanos entre s? sus valores tienden a ser homogeneos

Z_vgm_null = gstat::variogram(log(zinc) ~ 1, meuse) #modelo nulo

  #*log() se emplea para que los modelos construidos posteriormente tengan siempre valores positivos, si emplearamos la variable zinc sin ninguna transformacion obtendriamos en algunos casos valores negativos

ggplot() + geom_point(data = Z_vgm_null, aes(x = dist, y = gamma))
  
#*Interpretacion de los valores obtenidos
  #*np se refiere a los pares de puntos
  #*#*gamma hace referencia a una semivarianza
    #*57 pares de puntos presentan una distancia de 79.29 metros entre si y estos van a tener una semivarianza (gamma) de 0.12344
  
  #*Cuando la linea se vuelve as?ntotica eso quiere decir que la vairacion ya no es producto de la distancia entre los puntos sino que existen otros factores ambientales que inciden en ella

#Al variograma podemos agregarle otras variables que tenemos en la base de datos para ir eliminando esa varianza (en el modelo nulo se presenta toda la)

    #2.1. Modelo nulo----
  #tenemos toda la varianza

Z_vgm_null = variogram(log(zinc) ~ 1, meuse) %>% 
  # Agregarle la columna "Modelo"
  mutate(Modelo = 'Nulo')

ggplot() + geom_point(data = Z_vgm_null, aes(x = dist, y = gamma))

variogram(log(zinc) ~ 1, meuse, cutoff = 300) %>% #el parametro cutoff es sensible al valor indicado (distancia), mientras se observe al comportamiento asintotico de los datos esta todo relativamente bien
  mutate(Modelo = 'Nulo') %>% plot()

    #2.2. Modelo Espacial----
  #Agregar la componente espacial al modelo

Z_vgm_Spat = variogram(log(zinc) ~ x + y, meuse) %>% 
  mutate(Modelo = 'Espacial')

ggplot() + geom_point(data = Z_vgm_Spat, aes(x = dist, y = gamma))

  #*la varianza baj?
  #*la varianza en relacion a las coordenadas es una parte de la varianza total

    #2.3. Modelo distancia----

Z_vgm_Dist = variogram(log(zinc) ~ dist, meuse) %>% 
  mutate(Modelo = 'Distancia')

ggplot() + geom_point(data = Z_vgm_Dist, aes(x = dist, y = gamma))

    #2.4. Modelo sqrt distancia----
  #algunos fenomenos como la concentracion empiezan a disminuir no-linealmente, disminuyen de una manera m?s acelerada 

Z_vgm_Dist_sq = variogram(log(zinc) ~ sqrt(dist), meuse) %>% 
  mutate(Modelo = 'sqrt dist')

ggplot() + geom_point(data = Z_vgm_Dist_sq, aes(x = dist, y = gamma))

    #Union de modelos----

Z_vgm = list(Z_vgm_Dist, Z_vgm_null, Z_vgm_Spat, Z_vgm_Dist_sq) %>%
  purrr::reduce(bind_rows) #recordar que reduce() agrupa elementos de una lista seg?n una funci?n

ggplot() + 
  geom_point(data = Z_vgm, aes(x = dist, y =gamma, color = Modelo)) + 
  geom_line(data = Z_vgm, aes(x = dist, y =gamma, color = Modelo)) + 
  theme_bw()

  #Fit del variograma (modelo espacial)----
Abn_fit_Spat = fit.variogram(Z_vgm_Spat, 
                             model = vgm(psill = 1, #vgm() hace referencia a un variograma
                                         model = 'Sph', #tipo de correlaci?n, en este caso esf?rica
                                         range = 700, 
                                         nugget = 1))
#*Este fit busca encontrar el punto donde se inicia el comportamiento asintotico (a que distancia y que valor de gamma) 

#*IMPORTANTE: los puntos no fueron arbitrarios, son basados en las estimaciones visuales de los graficos anteriores y le indican a la funci?n fit desde que valores empezar a calcular
  #*esto es un modelo no lineal; siempre requieren de puntos iniciales de donde empezar
  #*es recomendable ocupar los mismos puntos de inicio para todos los modelos estimados,
    #*debido a esto es que los puntos de inicio sean valores intermedios entre las gr?ficas observadas

#*range = 1098.571: a esa distancia es a la cual llegamos a esa asintota
#*psill = (0.38866509 + 0.08234213) = ~0.47: la semivaianza estabilizada a la as?ntota
#*nugget: varianza esperada a distancia cero (tiene que ver con el intercepto)

# sessionInfo()
sessionInfo()

  # Fit del variograma (todos los modelos) ----
Abn_fit_null <- fit.variogram(Z_vgm_null, model = vgm(psill = 1, model = "Sph", range = 700, nugget = 1))

Abn_fit_Spat <- fit.variogram(Z_vgm_Spat, model = vgm(psill = 1, model = "Sph", range = 700, nugget = 1))

Abn_fit_Dist<- fit.variogram(Z_vgm_Dist, model = vgm(psill = 1, model = "Sph", range = 700, nugget = 1))

Abn_fit_Dist_sq <- fit.variogram(Z_vgm_Dist_sq, model = vgm(psill = 1, model = "Sph", range = 700, nugget = 1))

# 3. Krigging ----
  # Datos ----
data (meuse.grid)

Meuse_Grid <- st_as_sf(meuse.grid, coords = c(1,2), crs = 28992)

ggplot() + geom_sf(data = Meuse_Grid,
                   aes(color = dist)) + scale_color_viridis_c(direction = -1)

  # Ordinary kriging ----
Spat_pred <- krige(formula = log(zinc) ~ 1, 
                   data = Meuse, # Dato inicial
                   newdata = Meuse_Grid, # Datos por sobre los cuales va a predecir
                   model = Abn_fit_Spat) # Modelo empleado para predecir

# En el objeto Spat_pred podemos observar dos variables: los predichos (var1.pred) y el error est?ndar o varianza (var1.var)
# Importante: Si comparamos los valores predichos con los valores originales estos ?ltimos son muy bajos. Esto es producto de que estamos trabajando con el logaritmo (de la concentracion de Zn)
# Al graficar Spat_pred debemos aplicar una funcion exponencial de manera de eliminar el logaritmo

  # Observando predicciones
ggplot() + geom_sf(data = Spat_pred, 
                   aes(color = exp(var1.pred))) +
  scale_color_viridis_c(name = "[Zinc]")

  # Observando varianza
ggplot() + geom_sf(data = Spat_pred, 
                   aes(color = var1.var)) +
  scale_color_viridis_c(name = "Varianza C. Zinc")

# Se observa que esos puntos oscuros corresponden a los puntos en los cuales se hicieron las mediciones y que poseen la menor varianza

  # Predicci?n para todos los modelos ----
Null_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, 
                   model = Abn_fit_null) %>% 
  mutate(Modelo = "Nulo")

Spat_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, 
                   model = Abn_fit_Spat) %>% 
  mutate(Modelo = "Espacial")

Dist_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, 
                   model = Abn_fit_Dist) %>% 
  mutate(Modelo = "Distancia")

Dist_sq_pred <- krige(log(zinc) ~ 1, Meuse, Meuse_Grid, 
                   model = Abn_fit_Dist_sq) %>% 
  mutate(Modelo = "sqrt(Dist)")

  # Juntar modelos predictivos
Pred <- list(Null_pred, Spat_pred, Dist_pred, Dist_sq_pred) %>% 
  purrr::reduce(bind_rows)

  # Observar predicciones en general
ggplot() + geom_sf(data = Pred, aes(color = exp(var1.pred))) +
  scale_color_viridis_c(name = "[Zinc]") + 
  facet_wrap(~Modelo)

  # Observar varianza en general
ggplot() + geom_sf(data = Pred, aes(color = var1.var)) +
  scale_color_viridis_c(name = "Varianza [Zinc]") + 
  facet_wrap(~Modelo)

# 4. ?C?mo elijo el mejor modelo? Cross validation----
  # Se elige mediante el RMSE (Root Mean Square Error)
  # Cross validation: Entrenamos el modelo con algunos datos y dejamos otros.
    #* Vemos el error al predecir los datos faltantes

  # krige.cv(): cross validation
Null_CV <- krige.cv(log(zinc) ~ 1, 
                    Meuse, model = Abn_fit_null,
                    nfold = 5) %>% st_as_sf() %>% mutate(Modelo = "Nulo")

Spat_CV <- krige.cv(log(zinc) ~ 1, 
                    Meuse, model = Abn_fit_Spat,
                    nfold = 5) %>% st_as_sf() %>% mutate(Modelo = "Espacial")

Dist_CV <- krige.cv(log(zinc) ~ 1, 
                    Meuse, model = Abn_fit_Dist,
                    nfold = 5) %>% st_as_sf() %>% mutate(Modelo = "Distancia")

Dist_sq_CV <- krige.cv(log(zinc) ~ 1, 
                       Meuse, model = Abn_fit_Dist_sq,
                       nfold = 5) %>% st_as_sf() %>% mutate(Modelo = "sqrt(Dist)")

  # *nfold: indica en cuantas secciones se subdivide la base de datos (5). Se emplean 4 para construir el modelo y 1 para validarlo

Pred_CV <- list(Null_CV, Spat_CV, Dist_CV, Dist_sq_CV) %>% 
  purrr::reduce(bind_rows)

ggplot() + geom_sf(data = Null_CV, aes(color = factor(fold))) +
  facet_wrap(~factor(fold)) +
  scale_color_viridis_d() +
  theme_bw()

  # Resumen y orden por RMSE ----
Resumen <- Pred_CV %>% as.data.frame() %>% group_by(Modelo) %>% 
  summarize(RMSE = sqrt(sum(residual^2/length(residual)))) %>% arrange(RMSE)

  #* Los valores de RMSE van avriando seg?n la eleccion de datos anteriormente realizada. En este caso nos quedamos con el modelo espacial. 

  # Diagn?sticos ----
ggplot(Null_CV, aes(x = observed, y = var1.pred)) + 
  geom_smooth(method = "lm") +
  geom_point()

  # Estructura espacial ----
Var <- variogram(residual ~ 1, Spat_CV)

ggplot(Var, aes(x = dist, y = gamma)) +
  geom_point() +
  theme_bw() +
  xlab("Distancia metros") +
  ylim(c(0, max(Var$gamma)))

  # *No se genera una estructura espacial clara. La varianza no disminuye a medida que se encuentra m?s proxima a los puntos





