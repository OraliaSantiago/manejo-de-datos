
#
#Proyec: Series de tiempo
#
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
#
#


#El análisis de series de tiempo se centra en el estudio del comportamiento de 
#una única variable, particularmente con propósitos predictivos o pronóstico 

#De esta manera, si se tiene la serie de tiempo de la variable yt donde t denota
#el tiempo, expresada como

#y1,y2,y3,y4,...,yt−1,yt#



# Se analizara la base de Datos de Esparragos, donde se ilustrará 
# el comportamiento de la producción de espárragos entre los años 1993−2019
dat <- read.csv("C:/Users/orali/OneDrive/Escritorio/Trimestre 25-I/Prog_Web/expo/SeriesDeTiempo/Base_Espárragos.csv", sep = ",")
head(dat)


#install.packages("tidyverse")
library(tidyverse)
ggplot(data = dat, mapping = aes(x = Año, y = produción)) +
  geom_point() +
  geom_line(colour = "blue", size = 1) +
  labs(x = "AÑOS", y = "Producción", 
       title = "Producción anual de Espárragos (Tn)")




plot(dat$Año, dat$produción, xlab = "Años", ylab = "Producción", 
     main = "Producción de espárragos (Tn)", type = "l")

#En estos gráficos, la variable Año no está definida como fechas, lo que puede 
#causar problemas. A veces solo se tiene la base de datos y las fechas de inicio 
#y fin, pero no el vector completo de fechas. Por eso, es necesario crear 
#rápidamente un vector de tipo fecha


# Solo tiene la variable Producción
Produccion <- dat$produción
# La serie inicia en 1993 y es de frecuencia anual
dat2 <- ts(Produccion, start = 1993, frequency = 1)

plot(dat2  , xlab = "Años", ylab = "Producción", 
     main = "Producción de espárragos (Tn)", type = "l")





#la diferencia entre graficar con ggplot2 y con funciones base como plot(). 
#En ggplot2 se necesita una variable de fechas explícita, mientras que en plot() 
#se puede usar una serie de tiempo directamente. Para notar estas diferencias, 
#se pueden usar funciones como class() y view().

#Si se cuenta con un data.frame sin una variable de fechas, es posible crearla 
#para usarla con ggplot2.



#La serie inicia inicia en 1993, es anual (12) y
# hasta 2019 hay 27 años.
Fechas <- (seq(as.Date("1993-01-01"), by = "12 months", len = 27))
class(Fechas)




#=====Análisis Clásico=====

#Al analizar una serie temporal, el objetivo principal es poder hacer pronósticos.
#Si se identifica una función matemática que se asemeje al comportamiento de la 
#serie, se puede intentar predecir su evolución. Sin embargo, este enfoque es 
#intuitivo y carece de rigor estadístico, aunque puede ser útil como punto de partida.

#Por ejemplo, al observar la variable Producción, se nota una alta varianza. 
#Para reducirla y facilitar el análisis, se puede aplicar una transformación 
#como el logaritmo natural.




dat3 <- data.frame(Fechas, Produccion)
head(dat3)

dat3 <- data.frame(dat3[1], log(dat3[2]))
head(dat3)

ggplot(data = dat3, mapping = aes(x = Fechas, y = Produccion)) +
  geom_point() +
  geom_line(colour = "red", size = 1) +
  labs(x = "AÑOS", y = "Produccion", 
       title = "Producción anual de Espárragos (Tn)")







#La serie muestra un crecimiento hasta 2011 y luego se estabiliza. Aunque está 
#transformada con logaritmos, entre 1993 y 1996 presenta irregularidades. Aun así, 
#su forma general sugiere una parábola invertida en su primera mitad.

#Se intentará ajustar la serie con una función lineal del tipo Y=a+bXY=a+bX, 
#es decir, con intercepto.







t <- 1:27
mod_lin <- summary(lm(dat3$Produccion ~ t))
# Se observa un $R^2 = 0.91$

# creación del modelo lineal
mod_1 <- coef(mod_lin)[1] + coef(mod_lin)[2]*t
# gráfica
ggplot(data = dat3, mapping = aes(x = Fechas)) +
  geom_line(aes(y = Produccion, colour = "Serie original"), size = 1) +
  geom_line(aes(y = mod_1, colour = "Modelo lineal"), size = 1) +
  labs(x = "AÑOS", y = "Produccion",
       title = "Producción anual de Espárragos (Tn)") + 
  scale_color_manual("", breaks = c("Serie original", "Modelo lineal"),
                     values = c("red", "blue"))




#Aunque el coeficiente de determinación R^2=0,91 sugiere un buen ajuste 
#del modelo lineal, al observar la gráfica se nota que este solo representa bien 
#la tendencia inicial, pero se desvía del comportamiento real en el resto de la 
#serie.

#Por ello, se probarán modelos cuadrático y cúbico para comparar su ajuste a la 
#variable Producción, usando R2 como criterio principal para determinar cuál se 
#adapta mejor.




# modelo cuadrático
t2 <- t*t
mod_cuad <- summary(lm(dat3$Produccion ~ t + t2))
mod_2 <- coef(mod_cuad)[1] + coef(mod_cuad)[2]*t + coef(mod_cuad)[3]*t2

# modelo cúbico
t3 <- t*t*t
mod_cub <- summary(lm(dat3$Produccion ~ t + t2 + t3))
mod_3 <- coef(mod_cub)[1] + coef(mod_cub)[2]*t + coef(mod_cub)[3]*t2 + coef(mod_cub)[4]*t3

# gráfica
ggplot(data = dat3, mapping = aes(x = Fechas)) +
  geom_line(aes(y = Produccion, colour = "Serie original"), size = 1) +
  geom_line(aes(y = mod_1, colour = "Modelo lineal"), size = 1) +
  geom_line(aes(y = mod_2, colour = "Modelo cuadrático"), size = 1) +
  geom_line(aes(y = mod_3, colour = "Modelo cúbico"), size = 1) +
  labs(x = "AÑOS", y = "Produccion",
       title = "Produccion anual de Espárragos (Tn)") + 
  scale_color_manual("", 
                     breaks = c("Serie original", "Modelo lineal",
                                "Modelo cuadrático", "Modelo cúbico"),
                     values = c("red", "blue", "green", "black"))





#Los resultados muestran que el modelo cuadrático tiene un R^2=0.94, superior al 
#modelo lineal, lo que indica un mejor ajuste. El modelo cúbico, con un R2=0.96, 
#ofrece el mejor ajuste de los tres y se confirma visualmente que sigue más 
#fielmente la tendencia de la serie original

#se analizarán los errores de estimación del modelo cúbico para evaluar si 
#presentan un comportamiento normal.

errores <- mod_3 - dat3$Produccion

plotn <- function(x,main="Histograma de frecuencias \n y distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}
plotn(errores,main="Distribución normal")


# Test de normalidad darque Bera
#install.packages("tseries")
library("tseries")
jarque.bera.test(errores)

#===============================================================================


#

