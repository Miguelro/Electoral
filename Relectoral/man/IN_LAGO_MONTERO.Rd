% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Electoral_index.R
\name{IN_LAGO_MONTERO}
\alias{IN_LAGO_MONTERO}
\title{Indice Nacionalizacion de Montero y Lago (2010)}
\usage{
IN_LAGO_MONTERO(Ano, Mes, Ruta, n_escanos = 350, Auto = TRUE, d_votos,
  d_escanos)
}
\arguments{
\item{Ano}{( Sólo para caso automático) Es el valor del año de los datos que se quieren descagar. Este dato puede ser
numérico o de tipo carácter (preferiblemente numérico)}

\item{Mes}{( Sólo para caso automático) Es el mes en el que se ha realizado la elección. Obligatoriamente tiene que ser
de tipo carácter y a dos dígitos, es decir "04" es válido pero "4" no es válido}

\item{Ruta}{( Sólo para caso automático) Se debe indicar una ruta del disco duro local donde se descarga el
fichero del Ministerior del Interior. Una vez finalizada la descarga y de forma automática
el fichero descargado se borra.}

\item{n_escanos}{( Sólo para caso automático) Es el número total de escaños que se deben cubrir. Por defecto tiene
un valor de 350 que son los diputados que se eligen al Congreso de los Diputados en
España.}

\item{Auto}{Contiene un valos lógico que por defecto vale TRUE, indicativo de que se haga
el procedimiento automático. Si se le pasa otro valor entonces el procedimiento sería
manual.}

\item{d_votos}{(Sólo para procedimiento manual). Es un data.frame, conteniendo en la primera
columna la denominación de las regiones o circunscripciones. El resto de las columnas se
debe haber una por cada partido político que contenga los votos que ese partido ha tenido
en cada una de las circunscripciones, y la denominación de esa columna, serán las
siglas de ese partido.}

\item{d_escanos}{(Sólo para procedimiento manual).Es un data.frame con dos columnas.La
primera contiene la denominación de las regiones o circunscripcones, que deben ser
iguales y estar en el mismo orden que están en el data.frame al que
se hace referencia en el parámetro d_votos. La segunda columna debe
contener el número de escaños que corresponde a cada región o circunscripción
electoral}
}
\value{
El valor devuelto es un objeto de tipo list, con tres posiciones. La primera
posición contiene el valor del índice ( se denomina 'V_indice'), la segunda el vector
conteniendo los tantos por uno de los votos respecto al total nacional
( se denomina Porcentaje_votos'), y el tercer componente son los tantos por uno de escaños
en disputa de las circunscripciones donde se presenta el partido político en
cuestión. Su denominación es 'Porcentaje_escanos'.
}
\description{
Este índice de nacionalización, cuyo valor oscila entre 0 y 1, fue propuesto por
Lago y Montero (2010).Este índice se basa en la decisión de entrada de los partidos en la
competencia electoral en todos los distritos o solo en algunos. Para su cálculo se tiene en cuenta
los resultados electorales de los partidos, así como el número de escaños de los distritos
donde se presentan. La fórmula que utiliza es la siguiente:

\deqn{E=\sum_{j=1}^{J}p_{j}^{e}*q_{j}}{E=sum(p_j*q_j;j=1,2,..,J)}

Donde:
p_j es el tanto por uno de los votos obtenidos en todos el territorios sobre el total
q_j es el tanto por uno de escaños ( sobre el total de escaños) de las circunscripciones
en las que la formación política j se presenta.

Se han habilitado dos procedimentos para el cálculo de este indice, a uno se le denominará
automático  y al otro manual. Con el procedimiento automático se conectará con los
datos del Ministerio del Interior español y sin mayor intevención se calculará el indicador
para las elecciones a Cortes Generales que se haya indicado con los parámetros de la
fórmula. Para el procedimiento manual, se deden pasar los datos que se quieren procesar,
tal y como se indica a continuación en el apartado de parámetros
}
\examples{
#Procedimiento automático
s<-IN_LAGO_MONTERO(2019,"04","D:/",n_escanos = 350)
s$V_indice
s$Porcentaje_votos
s$Porcentaje_escanos

#Procedimiento manual
da1 <- data.frame( # Contiene los votos
 Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
 PSOE=c(400,300,0,50,25),
 PP=c(300,200,10,150,2),
 Cs=c(400,0,3,300,45),
 Uno=c(465,23,341,263,0))

da2 <- data.frame( # Contiene los escaños totales de cada provincia
 Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
 escanos=c(2,3,6,5,4))

s2<-IN_LAGO_MONTERO(Auto = FALSE,d_votos = da1,d_escanos = da2)
}
