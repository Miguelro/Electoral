#En este fichero desarrollamos todos los indices electrorales

#'@title Indice de RAE-1971 ("R")
#'
#'@describeIn Indicadores de desproporcionalidad
#'@section Indicadores
#'
#'@description Este índice mide la desproprocionalidad electoral con el resultado obtenido en unas
#'elecciones. Tiene el inconveniente de que está muy afectado por el número de partidos pequeños
#'que concurren a las elecciones. La fórmula uilizada es la siguiente:
#'\deqn{R=\frac{\sum_{i=1}^{n}|E_{i}-V_{i}|}{n}}{R = (1/n)*sum(|Vi-Ei|)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#'Para calcular su valor hay que introducir como parámetros de la fórmula los vectores de números
#'enteros que se corresponden con los votos obtenidos por cada partido y los escaños conseguidos.
#'El código R se encargará de calcular los porcentajes correspondientes, para la obtención del índice.
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve el valor obtenido para el índice
#'
#'@examples
#'Rae(c(1200, 30, 4000),c(10,6,8))
#'
#'@export
Rae <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  n <- length(votes)
  res <- sum(abs(p_votes - p_seats))/n
  return(res)
}

Rae(c(1200, 30, 4000),c(10,6,8))


#'@title Indice de RAE corregido ("Rco")
#'
#'@description Fue el propio Rae el que se dio cuenta de los problemas que se generaban en el
#'cálculo del índice al tener en cuenta todos los partidos. Por este motivo hizo una revisión
#'del mismo y decidió excluir del cálculo todos partidos que no lleguen al 0.5 por ciento de los
#'votos. En la función creada para calcular este índice por defecto toma este valor corrector, aunque
#'se permite introducir otro valor, como se puede ver en el ejemplo que se presenta líneas abajo.
#'
#'Para calcular su valor hay que introducir como parámetros de la fórmula los vectores de números
#'enteros que se corresponden con los votos obtenidos por cada partido y los escaños conseguidos.
#'El código R se encargará de calcular los porcentajes correspondientes, para la obtención del índice.
#'
#'@param votes Es un vector de números enteros, conteniendo TODOS los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo TODOS los escaños obtenidos por las candidaturas
#'@param correc Es un valor decimal qu indica el porcentaje de votos que sirve de corte
#'
#'@return Devuelve el valor obtenido para el índice
#'
#'@examples
#'Rae_corregido(c(1200, 30, 4000),c(10,6,8), correc = 1)
#'Rae_corregido(c(1200, 30, 4000),c(10,6,8))
#'
#'@export
Rae_corregido <- function(votes, seats, correc = 0.5){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  lim <- sum(votes)*correc*10^-2
  seats <- seats[votes>lim]
  votes <- votes[votes>lim]

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  n <- length(votes)
  res <- sum(abs(p_votes - p_seats))/n
  return(res)
}

Rae_corregido(c(1200, 30, 4000),c(10,6,8), correc = 1)
Rae_corregido(c(1200, 30, 4000),c(10,6,8))

#' @title Indice de Loosemore y Hanby-1971 ("LH")
#'
#' @description El índice de Loosemore y Hanby ( 1971 ) pretende solventar las dificultades encontradas
#' con el índice de RAE. Para calclar este índice de desproporcionalidad, lo que se hace es sumar los
#' valores absolutos de las diferencias entre votos y escaños y dividir el resultado entre dos. La
#' fórmula concreta que se utiliza es la siguiente:
#' '\deqn{LH=\frac{\sum_{i=1}^{n}|E_{i}-V_{i}|}{2}}{LH = (1/2)*sum(|Vi-Ei|)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#' @param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#' @param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#' @return Devuelve el valor obtenido para el índice
#'
#' @examples
#' Loos_Hanby(c(1200, 30, 4000),c(10,6,8))
#'
#' @export
Loos_Hanby <-  function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- sum(abs(p_votes - p_seats))/2
  return(res)
}

Loos_Hanby(c(1200, 30, 4000),c(10,6,8))

#' @title Indice de los cuadrados minimos de Gallagher-1991 (Gcm)
#'
#' @description El índice de desproporcionalidad electoral de los cuadrados mínimos de Gallagher, utiliza
#' una fórmula algo más elaborada que los índices anteriores (R, LH) para ello calcula las diferencias
#' al cuadrado entre los votos y escaños, los suma y el resultado lo divide entre dos y después
#' calcula su raiz cuadrada. Por consiguiente pondera adecuadamente las distorsiones de la
#' desproporcionalidad. Su fórmula matemática es la siguiente:
#'
#'\deqn{Gcm=\sqrt{\frac{\sum(V_{i}-E_{i})^{2}}{2}}}{Gcm =sqrt(sum(Vi-Ei)^2*(1/2)) }
#'for \eqn{i =  1, 2, \ldots,n}
#'
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve el valor obtenido para el índice
#'
#' @examples
#'
#' Rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#' Gallagher(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'
#'@export
Gallagher <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- sqrt(0.5*sum((p_votes-p_seats)^2))
  return(res)
}

Rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
Gallagher(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#'@title Indice de desproporcionalidad de Saint Lague ( SL )
#'
#'@description Este índice está ideado para estudiar la desporprocionalidad de aquellos sistemas electorales
#'que utilizan el sistema de repartos de escaños que tiene el mismo nombre (método de Saint Lagüe).Es de
#'destacar que este indicador tiene en cuenta la diferencia relativa entre los escaños-votos de cada
#'partido, así como la del conjunto del sistema. La fórmula empleada para su cálculo es la siguiente:
#'
#'\deqn{SL=\sqrt{\sum\frac{(E_{i}-V_{i})^{2}}{V_{i}}}}{SL =sqrt(sum((Vi-Ei)^2*(1/Vi)) }
#'for \eqn{i =  1, 2, \ldots,n vi>0}
#'
#'
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve el valor obtenido para el índice
#'
#'@examples
#'
#'Sainte_Lague(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
Sainte_Lague <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  seats <- seats[votes>0]
  votes <- votes[votes>0]

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- sum(((p_seats - p_votes)^2)/p_votes)
  return(res)
}

Sainte_Lague(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#'@title Indice de desproporcionalidad de maxima desviacion (Lmax)
#'
#'@description Indicador de desproporcionalidad adpatado para aquellos sistemas que utilizan la Ley
#'D'Hondt en el reparto de sus escaños, como es el casod de España ( salvo en las elecciones al Senado),
#'presenta la desventaja de que tan sólo tiene en cuenta la diferencia entre escaños y votos sólo
#'para la fuerza política más votada. La fórmula que se utiliza para su cálculo es la siguiente:
#'\deqn{Lmax=max[|V_i-E_i]}{Lmax=max[|Vi-Ei|]}
#'for \eqn{i =  1, 2, \ldots,n }
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve el valor obtenido para el índice
#'
#'@examples
#'
#'L_max(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export

L_max <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  res <- max(abs(p_seats - p_votes))
  return(res)
}

L_max(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#'@title  Indice de desproporcionalidad de Cox_Shugart-1991 ( CS )
#'
#'@description El índice de desproporcionalidad de Cox_Shugart (1991) mide la desproporción en el
#'reparto de escaños mediante una línea de regresión entre el porcentaje de escaños y el porcentaje
#'de votos. Si el reparto fuera aproximadamente proporcional la pendiente de la recta de regresión
#'sería uno. Si se aleja de ese valor existirá desproporcionalidad en el reparto de escaños.
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve el valor obtenido para el índice
#'
#'@examples
#'
#'Cox_Shugart(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))
#'
#'@export
Cox_Shugart <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  model <- lm(p_seats ~ p_votes)
  res <- model$coefficients[2]
  #browser()
  return(as.numeric(res))
}

Cox_Shugart(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))

#'@title  Indice de desproporcionalidad de Cox_Shugart corregido ( CS_correg )
#'
#'@description Este índice viene a corregir el defecto que presenta el índice de Cox_Shugart, ya
#'que el mismo es muy sensible a la presencia de partidos políticos pequeños. Para corregir esa deficiencia
#'lo que se hace para calcular este indicador es no tener en cuenta los partiddos que no obtienen
#'representación parlamentaria. De hecho, cunado se calcula su valor lo único que se hace es
#'eliminar los votos y los escaños a los partidos que no tienen escaños y esos resultados se les pasa
#'a la función "Cox_Shugart()".
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve el valor obtenido para el índice
#'
#'@examples
#'
#'Cox_Shugart_correg(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))
#'
#'@export
Cox_Shugart_correg<- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")
  # Nos quedamos con los partidos con representación
  rep<- seats>0
  vot<- votes[rep]
  sea<- seats[rep]
  return(Cox_Shugart(vot,sea))
}

Cox_Shugart_correg(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))

#'@title Indice de desproporcionalidad Linea de Tuckey (LT)
#'
#'@description Este indicador también se denomina índice de sesgo robustos, y el resultado se obtiene
#'mediante la utilización de la pendiente de la conocida "Linea de Tuckey" y su utilización queda recomendada
#'cuando exista un "pequeño" grupo de partidos que tienen un comportamiento significativamente diferente
#'al resto. Este  procedimiento se califica como "robusto" pues la presencia de comportamientos de
#'ciertos partidos diferentes al resto no altera de forma sustancial la recta obtenida, a diferencia
#'de lo que ocurría cuando se utilizaba en críterio de mínimos cuadrados para hacer el ajuste
#'de regresión.
#'
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve el valor obtenido para el índice
#'
#'@examples
#'
#'L_Tukey(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))
#'
#'@export
L_Tukey <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  p_votes <- (votes/sum(votes)) * 100
  p_seats <- (seats/sum(seats)) * 100

  model <- line(p_seats ~ p_votes)
  res <- model$coefficients[2]
  return(res)
}

L_Tukey(c(3947,3189,1971,466,345,82),c(184,99,44,10,0,0))

#### Dimension del voto del sistemas de partidos ###

#'@title Indice Dmension de voto de fragmentacion ( electoral y parlamentaria) de Rae ( F )
#'
#'@description Este indicador pretende resumir en un valor el nivel de dispersión o concentración de
#'poder político, es decir y de forma resumida si se encuentra concentrado o no los escaños o votos
#'recibidos en una serie de partidos polítcos. Los valores del índice varían entre 0 y 1, de forma que
#'un valor cero indica no hay ninguna fragmentación ( todos los votos van a un sólo partido), mientras
#'que un valor dercano a uno indica fuerte fragmentación electoral
#'
#'  La fórmula que se utiliza para su cálculo es la
#'siguiente:
#'
#'\deqn{1-\sum_{i}q_{i}^{2}}{1-sum(qi^2)]}
#'for \eqn{i =  1, 2, \ldots,n }
#'
#'Donde qi son las proporciones (en tanto por uno) del número de escaños o votos ( indicador electoral
#'o parlamentario), de los votos obtenidos por cada candidatura.
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve un dataframe, con las columnas "electoral" y "parlamentario" para albergar
#'respectivamente el valor del índice de fagmentación electoral o el parlamentario
#'
#'@examples
#'
#'fragmentacion_rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export

fragmentacion_rae <- function(votes, seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")

  p_votes2 <- (votes/sum(votes))^2
  p_seats2 <- (seats/sum(seats))^2

  res_electoral <- 1-sum(p_votes2)
  res_parlamento <- 1-sum(p_seats2)

  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

fragmentacion_rae(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))


### Numero efectivo de partidos ####
#'@title Indice Dimension de voto. Numero efectivo de partidos (electoral y parlamentario ) ( N)
#'
#'@description Este indicador ( Laakso y Taagepera 1979) es complementario del indicador de fragmentación electoral, y su objetivo
#'es medir el número de partidos que realmente compiten en un proceso electoral. Este indicador, normalmente
#'oscila entre el -1,+1 del número de partidos que obtienen mas de un 10 por ciento de los votos.La
#'fórmula matemática que se emplea para calcular este indicador es la sigiente:
#'
#'\deqn{N=\frac{1}{\sum p_{i}^{2}}}{N=1/sum(pi^2)}
#'for \eqn{i = 1, 2, \ldots,n } y pi los porcentajes ( en tanto por uno) de votos o escaños
#'del partido i
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve un dataframe, con las columnas "electoral" y "parlamentario" para albergar
#'respectivamente el valor del índice del número efectivo de partidos  electoral o el parlamentario
#'
#'@examples
#'
#'nep(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
nep <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")
  p_votes2 <- (votes/sum(votes))^2
  p_seats2 <- (seats/sum(seats))^2

  res_electoral <- 1/sum(p_votes2)
  res_parlamento <- 1/sum(p_seats2)

  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

nep(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

####  Indice hiperfraccionamiento. ###
# Esta fubción es auxiliar
R_hiperfragmentacion2 <- function(datos){
  # d es un vetor conteniendo los datos
  # Lo pasamos a proporciones
  x_por <- datos/sum(datos)
  x_por1 <- x_por[x_por>0]
  x_por2 <- log(x_por1)
  return(exp(-sum(x_por1*x_por2)))
}

#'@title Indice Dimension de voto. Indice de hiperfraccionamiento (electoral y parlamentario ) (I)
#'
#'@description El índice de hiperfraccionamiento, propuesto por kesselman (1996) y
#'Wilden ( 1991),es otro indicador que se utiliza para medir el
#'número de partidos que son relevantes en cualquier tipo de elección. Este índice de
#'hiperfraccionamiento es muy sensible a la presencia de partidos pequeños y les otorga más
#'relevancia de la que realmente tienen. La fórmula matemática utilizada para calcular el valor
#'de este índice es la siguiente:
#'
#'\deqn{I=exp(-\sum p_{i}ln(p_{i}))}{I=exp(-sum(pi*ln(pi)))}
#'for \eqn{i = 1, 2, \ldots,n } y pi>0 los porcentajes ( en tanto por uno) de votos o escaños
#'del partido i
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve un dataframe, con las columnas "electoral" y "parlamentario" para albergar
#'respectivamente el valor del índice de hiperfraccionamiento  electoral o el parlamentario
#'
#'@examples
#'
#'hiper(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
hiper <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")
  res_electoral <- R_hiperfragmentacion2(votes)
  res_parlamento <- R_hiperfragmentacion2(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

hiper(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

#### Numero efectivo partidos Molinar ###
# No documento estas funciones porque son auxiliares y no las quiero hacer visibles
R_nepMolinar2 <- function(datos){
  # x es un vetor conteniendo los datos
  # Lo pasamos a proporciones
  x_por <- datos/sum(datos)
  N <- 1/sum(x_por^2) # Número efecivo de partidos
  return(1+(N^2)*sum((x_por[2:length(datos)])^2))
}

#'@title Indice Dimension de voto. Indice de Numero partidos de Molinar (electoral y parlamentario ) (NP)
#'
#'@description Este indicador propuesto por Molinar ( 1991), intenta evaluar el número relevante
#'de partidos que realmente existe en un proceso electoral. Cabe decir de este índice que presenta un
#'mejor comportamiento, que los índices del número de partidos e hiperfraccionamiento,tanto en la
#'ponderación que hace del partido ganador como en la diferencia que hay entre el primero y el
#'segundo partido, así como del grado de concentración de los partidos minoritarios.La formula empleada
#'para el cálculo de este índice es la siguiente:
#'
#'\deqn{NP=1+N^{2}\sum_{i=2}p_{i}^{2}}{NP=1+N^2*sum(pi^2, for i>=2)}
#'for \eqn{i =  2, \ldots,n } y pi los porcentajes ( en tanto por uno) de votos o escaños
#'del partido i
#'
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve un dataframe, con las columnas "electoral" y "parlamentario" para albergar
#'respectivamente el valor del índice del Numero de partidos de Molinar  electoral o el parlamentario
#'
#'@examples
#'
#'nepMolinar(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
nepMolinar <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")
  res_electoral <- R_nepMolinar2(votes)
  res_parlamento <- R_nepMolinar2(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

#nepMolinar(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

##### Indice concentración #####
# No lo documento pues no quiero sea transparente a usuario final
R_concentracion <- function(datos){
  # x es un vetor conteniendo los datos
  # Lo pasamos a proporciones
  x_por <- datos/sum(datos)
  # odenamos en orden descendente
  x_por_ord <- x_por[order(-x_por)]
  return(x_por_ord[1]+x_por_ord[2])
}

#'@title Indice Dimension de voto.Concentracion del voto ( electoral y parlamentario)
#'
#'@description Otro indicador muy importante del sistema de partidos es el de concentracón del voto,
#'que indica qué porcentaje de votos o escaños se llevan los dos partidos que tienen el mayor
#'número de votos. La fórmula utilizada para calcular este indicador es la siguiente:
#'
#'\deqn{concentración=p_1+p_2}{concentración= p1+p2}
#'
#'Este indicador como mucho vale 1, de tal manera que cuanto más cercano a 1 se encuentre,
#'mayor concentración el voto se tiene.
#'
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve un dataframe, con las columnas "electoral" y "parlamentario" para albergar
#'respectivamente el valor del índice de concentración de voto electoral o el parlamentario
#'
#'@examples
#'
#'concentracion(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
concentracion <- function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")
  res_electoral <- R_concentracion(votes)
  res_parlamento <- R_concentracion(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

concentracion(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

### Competitividad ###
# Función auxiliar no la documento
R_competitividad <- function(datos){
  # x es un vetor conteniendo los datos
  # Lo pasamos a procentajes
  x_por <- datos/sum(datos)
  # odenamos en orden descendente
  x_por_ord <- x_por[order(-x_por)]
  return(1-(x_por_ord[1]-x_por_ord[2]))
}

#'@title Indice Dimension de voto. Competitividad ( electoral, parlamentaria).
#'
#'@description Con este índice se va a medir el nivel de rivalidad electoral que existe en un
#'determinado sistema de partidos entre el primer y el segundo ganador de unas elecciones. Este indicador
#'por lo tano va a poner de manifiesto cuánta rivalidad hay entre los dos partidos que mayor número
#'de votos han obtenido. Para medir este fenómeno se uutiliza la aproximación o lejanía de los resultados
#'de las dos formaciones políticas más votadas. El calculo de este indicador se hace mediante la
#'formula siguiente:
#'
#'\deqn{competitividad=1-(p_1-p_2)=}{competitividad=1-(p1-p2)}
#'
#'Donde p1 y p2 son los porcentajes de votos ( escaños), en tanto por uno, de las dos formaciones
#'políticas más votadas. Un valor cercano a 1 ( p1=p2) indica fuerte competividad, mientras que un valor
#'de  cero (p1=1 p2=0) indicará competitividad nula.
#'@param votes Es un vector de números enteros, conteniendo los votos de todas las candidaturas.
#'@param seats Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas
#'
#'@return Devuelve un dataframe, con las columnas "electoral" y "parlamentario" para albergar
#'respectivamente el valor del índice de competitividad electoral o el parlamentario
#'
#'@examples
#'
#'competitividad(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))
#'
#'@export
competitividad <-function(votes,seats){
  if( length(votes) != length(seats)) stop("ERROR: La longitud de votos y escaños debe ser la misma")
  if(sum(votes<0) != 0) stop("ERROR: No puede haber un número de votos negativo")
  if(sum(seats<0) != 0) stop("ERROR: No puede haber un número de escaños negativo")
  res_electoral <- R_competitividad(votes)
  res_parlamento <- R_competitividad(seats)
  return(data.frame(electoral = res_electoral, parlamentaria = res_parlamento))
}

competitividad(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))





### Indice de nacionalización del partido (INP) ####
#'@title Indice de nacionalizacion de un partido (INP)
#'
#'@description Jones y Mainwaring (2003), propusieron el cálculo de este índice basado en
#'el índice de concentración de la renta de Gini. Este valor va a tomar valores entre 0 y 1.
#'Un valor cercano a 1 indica que el partido está muy nacionalizado, y un valor cercano a
#'cero indicará todo lo contrario.
#'Este índice mide el nivel de homogeneidad de los votos que recibe un partido en todas las
#'circunscripciones donde se compite electoralmente. De forma muy resumida, la fórmula de
#'cálculo de este indicador es la siguiente.
#'
#'\deqn{IPN=1- Coeficient de Gini}{IPN=1- Coeficiente de Gini}
#'
#'@param dat Vector conteniendo las proporciones ( tanto por uno ) de voto del partido en
#'           en cada circunscripción
#'
#'@return Un número real conteniendo el valor del indicador
#'
#'@examples
#'
#'INP(c(0.15,0.22,0.24,0.26,0.27,0.27,0.27,0.32,0.33,0.34))
#'
#'INP(c(0.3,0.1,0.2))
#'
#'@export
INP<-function(dat){
  #dat son los tantos por uno del voto del partido en cada circunscripción
  # porcentajes en cada circunscripción ordenado
  porcen1<-dat[order(dat)]

  # Obtenemos los datos que se pasan en el vector
  Ndatos<-length(dat)
  # representación sobre resto de unidades
  c<-rep(1/Ndatos,Ndatos)
  # Calculo los porcentajes de porcen1
  porcen2<-porcen1/sum(porcen1)

  # Suma acumulada del voto
  Pc <- cumsum(porcen2)
  # reprresentación sobre el resto acumulado
  Qc <- cumsum(c)
  # El minuendo
  s1<-sum(Pc[2:(Ndatos)]*Qc[1:(Ndatos-1)])


  # El sustraendo
  r1<-sum(Pc[1:(Ndatos-1)]*Qc[2:Ndatos])
  #browser()
  return(1-s1+r1)
}


#### Calculo del Indice nacionalización sistema de partidos ###

#'@title Indice Nacionalizacion del Sistema de Partidos (INSP)
#'
#'
#'@description Este índice se basa en el índice de nacionalización de cada partido. Su cálculo
#'se basa en el agregado de puntuaciones de nacionalización de los partidos ponderado con
#'el valor del peso electoral ( número de votos obtenidos).Al igual que en el índice de
#'Nacionalización de partidos, unos valores cercanos a 1 indicará una fuerte nacionalización
#'y valores cercanos a cero indicarán lo contrario. La fórmula empleada para este indicador
#'es la siguiente:
#'
#'\deqn{INSP=sum(INPi ∗ pi)}{INSP=sum(INPi ∗ pi)}
#'
#'Donde INPi es el índice de Nacionalización del partido i, y pi el tanto por uno
#'de votos que recibe el partido i-ésimo.
#'
#'@param datos Es un dataframe con la primera columna conteniendo el nombre de la circunscripción
#'electoral, el restos de las columnas, se corresponden con los partidos presentados y cada
#'columna contendrá el número de votos que cada partido ha obtenido en la circunscrición
#'electoral correspondiente.
#'
#'@return Un número real conteniendo el valor del indicador
#'
#'@examples
#'
#'a <- data.frame(prov=c("Alava","Albacete","Valladolid"),PP=c(20,30,10),PSOE=c(34,12,45),   stringsAsFactors = FALSE)
#'
#'INSP(a)
#'
#'
#'b<-data.frame(prov=c("1","2"),A=c(400,190),B=c(200,1000),stringsAsFactors = FALSE)
#'INSP(b)
#'
#'@export
INSP <- function(datos){
  # Las datos deben ser un dataframe, primera columna con los nombres de las provincias
  # Una columna para cada partido
  # calculamos los porcentajes por filas
  # primero la suma por filas, quito la primera columna para sumar
  sum_filas <- apply(datos[,-1],1,sum)
  #datos_porcen va a contener los porcentajes por filas
  datos_porcen <-datos
  for ( i in 2:ncol(datos)){
    datos_porcen[,i] <- datos[,i]/sum_filas
  }

  # Calculamos el INP de cada partido
  INPi <- rep(NA,ncol(datos)-1)
  for (i in 1:length(INPi)){
    INPi[i] <-INP(datos_porcen[,i+1])
  }

  # Total votos por partido
  Tvot<-colSums(datos[,-1])
  # Proporcion votos cada artido nivel nacional
  Pj <- Tvot/sum(Tvot)

  return(sum(Pj*INPi))
}

### Indice nacionalizacion lago y montero

#'@title Indice Nacionalizacion de Montero y Lago (2010)
#'
#'@description Este índice de nacionalización, cuyo valor oscila entre 0 y 1, fue propuesto por
#'Lago y Montero (2010).Este índice se basa en la decisión de entrada de los partidos en la
#'competencia electoral en todos los distritos o solo en algunos. Para su cáculo tiene en cuenta
#'los resultados electorales de los partidos, así como el número de escaños de los distritos
#'donde se presentan. La fórmula que utiliza es la siguiente:
#'
#'\deqn{E=\sum_{j=1}^{J}p_{j}^{e}*q_{j}}{E=sum(p_j*q_j;j=1,2,..,J)}
#'
#'Donde:
#'p_j es el tanto por uno de los votos obtenidos en todos el territorio sobre el total
#'q_j es el tanto por uno de escaños ( sobre el total de escaños) de las circunscripciones
#'en las que la formación política j se presenta.
#'
#'@param Ano Es el valor del año de los datos que se quieren descagar. Este dato puede ser
#'numérico o de tipo carácter ( mejor numérico)
#'@param Mes Es el mes en el que se ha realizado la elección. Obligatoriamente tiene que ser
#'de tipo carácter y a dos dígitos, es decir "04" es válido pero "4" no es válido
#'@param Ruta Se debe indicar una ruta del disco duro local donde se descarga el
#'fichero del Ministerior del Interior. Una vez finalizada la descarga de forma automática
#'el fichero descargado se borra.
#'@param n_escanos Es el número total de escaños que se deben cubrir. Por defectos tiene
#'un valor de 350 que son los diputados que se eligen al Congreso de los Diputados en
#'España.
#'
#'@return El valor devuelto es un objeto de tipo list, con tres posiciones. La primera
#'posición contiene el valor del índice ( se denomina 'V_indice'), la segunda el vector
#'conteniendo los tantos por uno de los votos respecto al total nacional
#'( se denomina Porcentaje_votos'), y el tercer componente son los tantos por uno de escaños
#'en disputa de las circunscripciones donde se presenta el partido político en
#'cuestión. Su denominación es 'Porcentaje_escanos'.
#'
#'@examples
#'s<-IN_LAGO_MONTERO(2019,"04","D:/",n_escanos = 350)
#'s$V_indice
#'s$Porcentaje_votos
#'s$Porcentaje_escanos
#'
#'
#'@export

IN_LAGO_MONTERO<-function(Ano,Mes,Ruta,n_escanos=350){
  if( class(Mes) != "character") stop( " El mes debe ser de tipo carácter")
  if(nchar(Mes) != 2) stop("El mes debe contener dos caracteres")
  if(is.null(Ruta)) stop("Se debe facilitar algún valor para 'Ruta'")
  # Indice de nacionalización de Lago y Montero

  datos_MIR<-Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta,Borrar=T)
  datos_MIR<-datos_MIR[1:52,]
  #Me quedo con la columna de los votos
  # Mediante expresión regular selecciono las columnas que tienen el número de votos
  columnas_Votos<-grep("V_",colnames(datos_MIR))
  # Me quedo solo con las columna de votos
  datos_MIR_Votos<-as.data.frame(datos_MIR[,columnas_Votos])
  # Transformo los datos a numéricos
  datos_MIR_Votos <- as.data.frame(apply(datos_MIR_Votos,2,as.integer))
  #calculo los totales
  totales<-colSums(datos_MIR_Votos)
  #Tantos por uno
  totales_por<-totales/sum(totales)
  #Calculo para cada provincia los escaños que tiene

  # Mediante expresión regular selecciono las columnas que tienen el número de escanos
  columnas_escanos<-grep("D_",colnames(datos_MIR))
  #extraigo las columnas con datos de escaños
  datos_MIR_escanos <- as.data.frame(datos_MIR[,columnas_escanos])
  #transformo los datos a numericos
  datos_MIR_escanos <- as.data.frame(apply(datos_MIR_escanos,2,as.integer))
  #Ahora saco los escaños que tiene cada provncia
  escanos_prov<-rowSums(datos_MIR_escanos)

  #Detectamos en que provincia se han presentado los partidos mediante
  # el número de votos, si es cero no se presentan y en caso contrario si
  # Se hace mediante na matriz de valores lógicos

  presentado<-datos_MIR_Votos>0
  # Calculo lo escaños de la provincias donde se presentan
  n<-ncol(datos_MIR_escanos)
  esca_presentado<-rep(NA,n)
  for(i in 1:n){
    esca_presentado[i]<-sum(presentado[,i]*escanos_prov)

  }
  esca_presentado<-esca_presentado/n_escanos
  re<-list()
  re[[1]]<-sum(esca_presentado*totales_por)
  re[[2]]<-as.numeric(totales_por)
  re[[3]]<-as.numeric(esca_presentado)
  names(re)<-c("V_indice","Porcentaje_votos","Porcentaje_escanos")
  re$Porcentaje_votos
  re$Porcentaje_escanos
  return(re)
}

#s<-IN_LAGO_MONTERO(2019,"04","D:/",n_escanos = 350)
#s$V_indice
#s$Porcentaje_votos
#s$Porcentaje_escanos


### Injusticia matemática (IM) ####

#'@title Indice Dimension de voto.Injusticia matematica ( IM )
#'
#'@description Este indicador fue propuesto por Edward V.Huntington y lo que mide es el nivel
#'de "injusticia" que se produce en un sistema electoral, al traducir los votos conseguidos
#'por las formaciones políticas en escaños. En este sentido Edward V.Huntington, definió
#'esta injusticia matemática entre dos partidos que compiten en un proceso electoral como
#'la diferencia en valor absoluto entre los cocientes de escaños y votos obtenidos por
#'esos dos partidos políticos. Es decir para cada partido político se obtiene el cociente
#'entre escaños y votos, y la injusticia matemática será la diferencia en valor absoluto de
#'esos cocientes.
#'
#'La fórmula matemática utilizada es la siguiente:
#'
#'\deqn{IM_{ij}=\left|\frac{e_{i}}{v_{i}}-\frac{e_{j}}{v_{j}}\right|}{IM_ij=abs((ei/vi)-(ej/vj))}
#'for \eqn{i =1,  2, \ldots,n } y vi los votos del partido i y ei sus escaños
#'
#'@param dates Es un objeto de tipo data.frame que contiene la información de la siguiente
#'manera: La primera columna está reservada para el nombre de los partidos políticos. La
#'segunda columna contiene los votos obtenidos, y la tercera sirve para anotar los escaños
#'que obtiene ese partido político
#'
#'@return Como resultado se obtiene un objeto de tipo matrix, de dimensión n x n, con n igual
#'al número de partidos que se pasan en el data.frame de entrada. Los nombres de las filas
#'y las columnas coinciden con los nombres de los partidos pasados en el data.frame de
#'entrada en la primera columna.
#'
#'@examples
#'a<- data.frame(par=c('A','B','C','D'),
#'               vot=c(200,300,100,24),sea=c(3,4,1,0), stringsAsFactors = FALSE)
#'
#'InjusticiaM(a)
#'@export
InjusticiaM <-function(dates){
  #dates es un dataframe con la primera columna conteniendo los partidos
  #La segunda columna conteniendo los votos y la tercera conteniendo los escaños
  if(class(dates) != "data.frame") stop("ERROR:Se debe pasar como parámetro un objeto de tipo data.frame" )
  # Si la primera columna no es de tipo character genero un error

  if(class(dates[,1]) != "character") stop("ERROR:la primera columna debe contener cadena caracteres")


  n <- nrow(dates)
  #Defino la matriz donde se almacenan los resultados
  resul <-matrix(NA,nrow = n,ncol = n)
  for(i  in 1:n){
    for(j in 1:n){
      resul[i,j]<-abs((dates[i,3]/dates[i,2])-(dates[j,3]/dates[j,2]))
    }
  }
  colnames(resul) <- dates[,1]
  rownames(resul) <- dates[,1]
  return(resul)
}

#a<- data.frame(par=c('A','B','C','D'),
#               vot=c(200,300,100,24),sea=c(3,4,1,0), stringsAsFactors = F)

#InjusticiaM(a)
