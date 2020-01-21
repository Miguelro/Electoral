# Calculo de la polaridad
# se necesita un data.frame. Primera columna nombre partido;
# segunda:ubicación ideologica;
# tercera columna: nº votos o escaños
# Tipo=1 se utilizan desviaciones de la media al cuadrado
# Tipo=2 se utilizan desviaciones de la media en valor absoluto

#'@title Dimension del voto. Indice polarizacion. ( PP )
#'
#'@description Esta dimensión es una de las más importantes en los sistemas democráticos, ya que explica
#'parte de los problemas de estabilidad y quiebra de las democracias.El índice que se calcula con esta
#'función necesita los datos en el parámetro "escala" que son difíciles de conseguir y su significado
#'se puede ver en el apartado de los parámetros que se expondrá más adelante. Si el indicador toma
#'un valor cercano a cero, significará no habrá polarización, y la misma se incrementará a medida
#'que el indicador tome valores más altos.
#'En este apartado se calcula el índice de polarización ponderado, y para su calculo se empleará
#'las siguientes modalidades de fórmulas:
#'
#'\deqn{Pp=\sum_{i=1}^{n}p_{i}(x_{i}-\overline{x}_{p})^{2}}{Pp = sum(p_j*(x_j-mean(x_p))^2)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#'\deqn{Pp2=\sum_{i=1}^{n}p_{i}\left|x_{i}-\overline{x}_{p}\right|}{Pp2 = sum(p_j*|x_j-mean(x_p)|)}
#'for \eqn{i = 1, 2, \ldots,n}
#'
#'@param datos Es un data.frame con tres columnas: la primera contiene el nombre de los
#'partidos. La segunda contiene el valor de la ubicación ideológica ( escala de 1 a 10),
#'y la tercera el número de votos obtenidos ( polarización electoral), o el número
#'de escaños obtenidos ( polarización parlamentaria).
#'@param Tipo Puede tomar los valores 1 ó 2. 1 Para indicar que se obtenga la fórmula
#'ponderada pero calculando las distancia a la media ponderada al cuadrado. Si se pasa el
#'valor de 2, se utilizará la fórmula que obtiene el valor absoluto de las diferencias
#'a la media ponderada
#'
#'@return Devuelve el valor numérico de este indicador
#'
#'@examples
#'d <- data.frame(partidos=c("RN","PDC","PS","PPD","UDI","PRSC","otros"),
#'  ubicacion=c(6.36,5.31,2.73,4.13,7.04,4.00,5.33),
#'  c(19,20,15,21,33,7,5))
#'polarizacion(d,Tipo = 2)
#'
#'@export

polarizacion <- function(datos,Tipo=1){
  if(class(datos) != "data.frame") stop("Se debe introducir un data.frame")
  if(ncol(datos) != 3) stop("El data.frame de entrada debe tener tres columnas")
  if(Tipo !=1 & Tipo != 2 ) stop("El parámetro Tipo debe valer 1 ó 2 ")

  #Asignamos nombre a las columnas
  colnames(datos)<-c("partidos","ubicacion","vot_escanos")
  # calculo la ubicacion ideológica media ponderada
  media_pon <- weighted.mean(datos$ubicacion,datos$vot_escanos)
  # Si Tipo= 1
  if(Tipo == 1){
    dif_cuadrado <- (datos$ubicacion-media_pon)^2
    res <- weighted.mean(dif_cuadrado,datos$vot_escanos)
  } else if(Tipo == 2){
    dif_abs <- abs(datos$ubicacion-media_pon)
    res <- weighted.mean(dif_abs,datos$vot_escanos)
  }

  return(res)
}

#d <- data.frame(partidos=c("RN","PDC","PS","PPD","UDI","PRSC","otros"),
#                ubicacion=c(6.36,5.31,2.73,4.13,7.04,4.00,5.33),
#                c(19,20,15,21,33,7,5))
#polarizacion(d,Tipo = 2)

# Polarizacion de Dalton
# se necesita un data.frame. Primera columna nombre partido;
# segunda:ubicación ideologica;
# tercera columna: porcentaje de votos


#'@title Dimension del voto. Polarizacion ponderada adaptada Dalton. ( Pd )
#'
#'@description Con esta función se calcula el índice de polarización ponderada adaptado
#'de Dalton ( 2008). Este índice pondera las posiciones ideolóicas de los partidos por su
#'resultado electoral. Este indicador oscila entre 0 ( en el caso hipotético de que todos
#'los partidos ocupen la misma posición ideológica) y 10 si los partidos se encuentran en los
#'extremos de la posición de la escala. La fórmula de cálculo es la siguiente:

#'\deqn{Pd=\sqrt{\sum_{i=1}^{n}p_{i}\left[\frac{\overline{x}_{i}-\overline{x}_{p}}{4.5}\right]^{2}}}
#'{Pd = sqrt(sum(p_j*(x_j-mean(x_p))/4.5)^2)}
#'for \eqn{i = 1, 2, \ldots,n}
#'

#'
#'@param datos Es un data.frame con tres columnas: la primera contiene el nombre de los
#'partidos. La segunda contiene el valor de la ubicación ideológica ( escala de 1 a 10),
#'y la tercera el porcentaje votos obtenidos respecto de todos los partidos presentados,
#'se tengan en cuenta o no para calcular la fórmula.
#'
#'
#'@return Devuelve el valor numérico de este indicador
#'
#'@examples
#'d2<- data.frame(partidos=c("RN","PDC","PS","PPD","UDI","PRSC"),
#'  ubicacion=c(6.36,5.31,2.73,4.13,7.04,4.00),
#'  c(14.12,20.76,10.05,15.42,22.36,3.54))
#'
#'polarizacion_Dalton(d2)
#'
#'
#'@export

polarizacion_Dalton <- function(datos){
  if(class(datos) != "data.frame") stop("Se debe introducir un data.frame")
  if(ncol(datos) != 3) stop("El data.frame de entrada debe tener tres columnas")
  if(sum(datos[,3])>100) stop("La suma de la tercera solumna no puede ser superior a 100")
  if(sum(datos[,3])<=1) stop("La suma de la tercera columna no puede ser menor que 1")
  #Asignamos nombre a las columnas
  colnames(datos)<-c("partidos","ubicacion","votos")
  # calculo la ubicacion ideológica media ponderada
  media_pon <- weighted.mean(datos$ubicacion,datos$votos)
  a <- datos$votos*((datos$ubicacion-media_pon)/4.5)^2
  res <- sqrt(sum(a))
  return(res)
}

# d2<- data.frame(partidos=c("RN","PDC","PS","PPD","UDI","PRSC"),
#                 ubicacion=c(6.36,5.31,2.73,4.13,7.04,4.00),
#                 c(14.12,20.76,10.05,15.42,22.36,3.54))
#
# polarizacion_Dalton(d2)


