% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Indelec.R
\name{AgregadosIndi}
\alias{AgregadosIndi}
\title{utilidades. Obtencion indicadores electorales datos agrupados.}
\usage{
AgregadosIndi(Ano = 0, Mes = "", RutaDescarga = "", Auto = T,
  datos = "")
}
\arguments{
\item{Ano}{es el año del proceso electoral que se quiera tratar. Debe ser un valor
numérico de cuatro dígitos( Si se utiliza un procedimiento manual, no hace falta
ningún valor a este parámetro) .}

\item{Mes}{es el mes del proceso electoral, debe ser una cadena con dos carcateres
numéricos, asociados al mes en que se han celebrado las elecciones.( Si se utiliza
un procedimiento manual, no hace faltaningún valor a este parámetro) .}

\item{RutaDescarga}{debe ser una cadena indicativa del camino a seguir para descargar
el fichero proveniente del Ministerio del Interior. Posteriormente y de forma
automática este fichero se borra. Se hace la advertencia de que esta ruta debe
indicar un lugar donde se tenga permiso de lectura y escritura, ya que en caso contrario
no se ompletará el proceso.( Si se utiliza un procedimiento manual, no hace falta
ningún valor a este parámetro) .}

\item{Auto}{Puede tener los valores lógicos TRUE ó FALSE. Por defecto tiene
el valor TRUE, para indicar que se quiere un porceso automático. En el supuesto de
querer un proceso manual, se dará un valor FALSE a este parámetro}

\item{datos}{debe contener un data.frame con tres columnas. La primera columna contiene
el nombre de los partidos, la segunda contiene el número de votos, y la tercera columna
contiene el número de escaños que consigue el partido político en cuestión. (Si se
utiliza un procedimiento automático, no hace falta ningún valor a este parámetro)}
}
\value{
Devuelve un data.frame con cuatro elementos:

1.- Tiene por denominación 'dat' y es un data.frame con los votos y escaños de
cada candidatura, en términos absolutos y en porcentajes, también la distribución
acumulada de esos valores y una última columna conteniendo un indicador de
desproporcionalidad electoral que se calcula como la diferencia entre el porcentaje
de votos y el porcentaje de escaños obtenidos

2.- Tiene la identificación 'grafico' y es un objeto de tipo ggpplot donde se representa
mediante un diagrama de barras, la diferencia entre el porcentaje de votos y el
porcentaje de escaños.

3.- Tiene la identificación 'In_despro' y es un data.fame conteniendo los valores de los
índices de desproporcionalidad indicados anteriormente.

4.- Tiene la identificación 'In_dimen' y contiene un data.frame con los valores de los
indicadores de dimensión de voto indicados anteriormente. Se facilita la versión
electoral y la parlamentaria
}
\description{
Con esta función se pueden obtener para datos agregados  los siguientes
indicadores de desproporcionalidad electoral:
\itemize{
\item Saint Lagüe (SL)
\item RAE (R)
\item RAE Corregido (Rco)
\item Loosemores y Hanby (LH)
\item Cuadrados Mínimos de Gallagher (Gcm)
\item Índice de máxima desviación (Lmax)
\item Índice de Cox y Shugart (Cs)
\item Índice de Cox y Shugart corregido(CS_correg)
\item Línea de Tukey (LT)
}

Todos los valores de estos indicadores están en un data.frame de salida donde los nombres de las columnas
coinciden con la identificación de estos indicadores que está entre paréntesis anteriormente.

Igualmente se obtienen los siguientes indicadores de dimensión de voto:
\itemize{
\item Fragmentación Electoral y Parlamentaria (F)
\item Número efectivo de partidos (N)
\item Índice de Hiperfraccionamiento ( Hiper)
\item Número efectivo de partidos Molinar (NP)
\item Concentración de voto (Con)
\item Competitividad entre patidos (Comp)
}

Se calculan para estos indicadores de dimensión de voto la versión electoral y la
parlamentaria. Sus valores están contenidos en la salida de esta función dentro de
un data.frame con el nombre de las columnas es el indicado entre paréntesis anteriormente
y al mismo se añade el sufijo '_electoral' para indicar se trata de la versión electoral
o el sufijo '_parlamen' para indicar que se trata de la versión parlamentaria.

\strong{Nota importante}. Con esta función se pueden sacar los resultados de una forma
automática o manual. Con la forma automática se descargan los datos del Ministerior
del Interior. Con la forma manual, hay que introducir los datos con un data.frame
con la estructura que se indicará posteriormente.
}
\examples{
d<-AgregadosIndi(2019,"04",RutaDescarga = "D:/")


}
