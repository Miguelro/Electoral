
#if (!require("ggforce")) install.packages("ggforce");require("ggforce")
#if (!require("ggplot2")) install.packages("ggplot2"); require("ggplot2")
#'@title Graficos. Representacion Arco parlamentario
#'
#'@description con esta función se facilita la representación de la distribución de escaños
#'obtenidos por cada uno de los partidos que les corresponde alguna representación parlamentaria.
#'Se puede introducir un título para el gráfico, los colores a utilizar para cada partido, así como
#'elegir si se hace una distribución de los escaños en terminos obsolutos o proporcionales.
#'
#'@param Partidos es un vector de string's conteniendo los nombres de los partidos.
#'@param Escanos es un vector de números naturales conteniendo los escaños que han correspondido
#'a cada partido
#'@param cols (Optativo) es un vector conteniendo los nombres de los colores a usar para dibujar
#'cada zona del arco parlamentario
#'@param repr Debe contener la expresión "absolute" o "Proportion" para representar números
#'absolutos o relativos
#'@param titulo Para indicar el título a colocar en el gráfico
#'
#'@import ggplot2
#'@import ggforce
#'
#'
#'@return devuelve un objeto de la clase ggplot, conteniendo la representación
#'del arco parlamentario obtenido.
#'
#'@examples
#'
#'bt <- data.frame(parties = c("PP", "CSU", "SPD", "AfD", "FDP", "UPyD", "CC", "Verdes"),
#'seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
#'cols    = c("black", "blue", "red", "lightblue", "yellow", "purple", "lavenderblush", "grey"),
#'stringsAsFactors = FALSE)
#'
#'Arc_Parlamentario(bt$parties, bt$seats, cols = bt$cols)
#'
#'@export
Arc_Parlamentario <- function(Partidos, Escanos, cols = NULL, repr=c("absolute", "proportion"),
                              titulo="Poner un titulo") {
  repr = match.arg(repr)
  stopifnot(length(Partidos) == length(Escanos))
  if (repr == "proportion") {
    if(sum(Escanos) != 1) stop("La suma de esca\u00F1os debe ser 1")

  }
  if (!is.null(cols)) {
    names(cols) <- Partidos
  }

  # Hago el reparto de las areas propocionales a los valores.
  # Comienzo por -pi/2 y termino en pi/2. Realmente voy acumulando
  # las medidas de los ángulos
  cc <- cumsum(c(-pi/2, switch(repr, "absolute" = (Escanos / sum(Escanos)) * pi, "proportion" = Escanos * pi)))

   cc[length(cc)] <- pi/2

  # obtener el punto medio de cada ángulo
  # para poder poner después el texto
  meanAngles <- colMeans(rbind(cc[2:length(cc)], cc[1:length(cc)-1]))

  # circulo unidad para calcular lugar donde poner los textos
  labelX <- sin(meanAngles)
  labelY <- cos(meanAngles)

  # previniendo bounding box y<y=0
  labelY <- ifelse(labelY < 0.015, 0.015, labelY)

  p <- ggplot() + theme_no_axes() + coord_fixed() +
    expand_limits(x = c(-1.3, 1.3), y = c(0, 1.3)) +
    theme(panel.border = element_blank(),plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none") +
    ggtitle(titulo)+

    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.6, r = 1,
                     start = cc[1:length(Escanos)],
                     end = c(cc[2:length(Escanos)], pi/2), fill = Partidos)) +

    switch(is.null(cols)+1, scale_fill_manual(values = cols), NULL) +

    # for label and line positions, just scale sin & cos to get in and out of arc
    #pongo las líneas de las etiquetas
    geom_path(aes(x = c(0.9 * labelX, 1.15 * labelX), y = c(0.9 * labelY, 1.15 * labelY),
                  group = rep(1:length(Escanos), 2)), colour = "white", size = 2) +
    geom_path(aes(x = c(0.9 * labelX, 1.15 * labelX), y = c(0.9 * labelY, 1.15 * labelY),
                  group = rep(1:length(Escanos), 2)), size = 1) +

    #Pongo los partidos + los votos
    geom_label(aes(x = 1.15 * labelX, y = 1.15 * labelY,
                   label = switch(repr,
                                  "absolute" = sprintf("%s\n%i", Partidos, Escanos),
                                  "proportion" = sprintf("%s\n%i%%", Partidos, round(Escanos*100)))), fontface = "italic",
               label.padding = unit(1.2, "points"),size=3) +

    # pongo los puntos finales de la linea de etiquetas
    geom_point(aes(x = 0.9 * labelX, y = 0.9 * labelY), colour = "red", size = 2) +
    #geom_point(aes(x = 0.9 * labelX, y = 0.9 * labelY)) +
    # Pongo el texto de abajo
    geom_text(aes(x = 0, y = 0, label = switch(repr,
                               "absolute" = (sprintf("Total: %i Esca\u00F1os", sum(Escanos))),
                               "proportion" = "")),
              fontface = "italic", size = 6)

  return(p)
}

# bt <- data.frame(parties = c("PP", "CSU", "SPD", "AfD", "FDP", "UPyD", "CC", "Verdes"),
#                  seats   = c(200, 46, 153, 92, 80, 69, 67, 2),
#                  cols    = c("black", "blue", "red", "lightblue", "yellow", "purple", "lavenderblush", "grey"),
#                  stringsAsFactors = FALSE)
#
# Arc_Parlamentario(bt$parties, bt$seats, cols = bt$cols)

#'@title Graficos. Grafico comparativo de Arcos parlamentarios
#'
#'@description Con este gráfico se obtendrán dos arcos parlamentarios con la finalidad de
#'poder comparar la estructura de los mismos, es decir los resultados obtenidos en dos
#'procesos electorales diferentes.
#'
#'@param Partidos1 Es un vector de caracteres con los nombres de los partidos del primer proceso
#'@param Partidos2 Es un vector de caracteres con los nombres de los partidos del segundo proceso
#'@param Escanos1  Es un vector de números enteros con los resultados del primer proceso
#'@param Escanos2  Es un vector de números enteros con los resultados del segundo proceso
#'@param cols Es un vector conteniendo los colores a utilizar en la elaboración del gráfico
#'@param repr Para indicar "absolute" si se quiere un gráfico en términos absolutos o
#'"proportion" si se utilizan términos relativos
#'@param titu1 Expresión a utilizar para nombrar el primer proceso
#'@param titu2 Expresión a utilizar para nombrar el segundo proceso
#'@param titulo Para indicar el título a colocar en el gráfico global.
#'
#'@return Se obtiene un objeto del tipo ggplot que contiene dos gráficos representativos
#'de los dos arcos parlamentarios a comparar.
#'
#'@examples
#'
#'bt1 <- data.frame(parties = c("PP", "CSU", "SPD","IU"),
#'                  seats   = c(200, 46, 153,58),
#'                  cols    = c("black", "blue", "red","jjj"),
#'                  stringsAsFactors = FALSE)
#'
#'bt2 <- data.frame(parties = c("PP", "CSU","UP1"),
#'                  seats   = c(100, 146,200),
#'                  cols    = c("green", "blue","GGG"),
#'                  stringsAsFactors = FALSE)
#'
#'Arc_Comparacion(bt1$parties,bt2$parties, bt1$seats,
#'                bt2$seats, cols=c("PP"="blue", "CSU"="red","SPD"="green",
#'                                  "IU"="pink","UP1"="antiquewhite" ),
#'                titu1 = "Abril-2019",titu2 = "Noviembre-2019",
#'                titu = " Comparación de elecciones")
#'
#'@export
Arc_Comparacion <- function(Partidos1,Partidos2, Escanos1, Escanos2,
                            cols = NULL, repr=c("absolute", "proportion"),
                            titu1="Election1",titu2="Election2",
                            titulo="Poner un titulo") {
  repr = match.arg(repr)
  if(length(Partidos1) != length(Escanos1)) stop("N\u00FAmero de Partidos_1 debe ser igual a N. Esca\u00F1os 1")
  if(length(Partidos2) != length(Escanos2)) stop("N\u00FAmero de Partidos_2 debe ser igual a N. Esca\u00F1os 2")
  if (repr == "proportion") {
    if(sum(Escanos1) != 1) stop("La suma de Esca\u0F1os_1 debe ser 1")
    if(sum(Escanos2) != 1) stop("La suma de Esca\u00F1os_2 debe ser 1")
  }
  #if (!is.null(cols1)) {
  #  names(cols1) <- Partidos1
  #}


  # Hago el reparto de las areas propocionales a los valores.
  # Comienzo por -pi/2 y termino en pi/2. Realmente voy acumulando
  # las medidas de los ángulos
  # Inicialmente para los primeros valores
  cc1 <- cumsum(c(-pi/2, switch(repr, "absolute" = (Escanos1 / sum(Escanos1)) * pi, "proportion" = Escanos1 * pi)))
  cc1[length(cc1)] <- pi/2

  # Ahora para los segundos
  cc2 <- cumsum(c(-pi/2, switch(repr, "absolute" = (Escanos2 / sum(Escanos2)) * pi, "proportion" = Escanos2 * pi)))
  cc2[length(cc2)] <- pi/2

  # obtener el punto medio de cada ángulo
  # para poder poner después el texto
  meanAngles1 <- colMeans(rbind(cc1[2:length(cc1)], cc1[1:length(cc1)-1]))
  meanAngles2 <- colMeans(rbind(cc2[2:length(cc2)], cc1[1:length(cc2)-1]))

  # circulo unidad para calcular lugar donde poner los textos
  labelX1 <- sin(meanAngles1)
  labelY1 <- cos(meanAngles1)

  labelX2 <- sin(meanAngles2)
  labelY2 <- cos(meanAngles2)

  # previniendo bounding box y<y=0
  labelY1 <- ifelse(labelY1 < 0.015, 0.015, labelY1)
  labelY2 <- ifelse(labelY2 < 0.015, 0.015, labelY2)


  p <- ggplot() + theme_no_axes() + coord_fixed() +
    expand_limits(x = c(-1.3, 1.3), y = c(0, 1.3)) +
    theme(panel.border = element_blank(),plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none") +
    ggtitle(titulo)+
    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.8, r = 1,
                     start = cc1[1:length(Escanos1)],
                     end = c(cc1[2:length(Escanos1)], pi/2), fill = Partidos1)) +

    geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 0.7,
                     start = cc2[1:length(Escanos2)],
                     end = c(cc2[2:length(Escanos2)], pi/2), fill = Partidos2)) +


    switch(is.null(cols)+1, scale_fill_manual(values =cols), NULL) +

    #Pongo los partidos + los votos
    geom_label(aes(x = labelX1, y =  labelY1,
                   label = switch(repr,
                                  "absolute" = sprintf("%s\n%i", Partidos1, Escanos1),
                                  "proportion" = sprintf("%s\n%i%%", Partidos1, round(Escanos1*100)))), fontface = "italic",
               label.padding = unit(1.2, "points"),size=3) +

  geom_label(aes(x = 0.6*labelX2, y = 0.6* labelY2,
                 label = switch(repr,
                                "absolute" = sprintf("%s\n%i", Partidos2, Escanos2),
                                "proportion" = sprintf("%s\n%i%%", Partidos2, round(Escanos2*100)))), fontface = "italic",
             label.padding = unit(1.2, "points"),size=3)+

# Pongo los título de las elecciones
    geom_text(aes(x = 0, y = 1.2, label = titu1),
              fontface = "italic", size = 5)+
    geom_text(aes(x = 0, y = 0.4, label = titu2),
              fontface = "italic", size = 4)



  return(p)
}



# bt1 <- data.frame(parties = c("PP", "CSU", "SPD","IU"),
#                   seats   = c(200, 46, 153,58),
#                   cols    = c("black", "blue", "red","jjj"),
#                   stringsAsFactors = FALSE)
#
# bt2 <- data.frame(parties = c("PP", "CSU","UP1"),
#                   seats   = c(100, 146,200),
#                   cols    = c("green", "blue","GGG"),
#                   stringsAsFactors = FALSE)
#
# Arc_Comparacion(bt1$parties,bt2$parties, bt1$seats,
#                 bt2$seats, cols=c("PP"="blue", "CSU"="red","SPD"="green",
#                                   "IU"="pink","UP1"="antiquewhite" ),
#                 titu1 = "Abril-2019",titu2 = "Noviembre-2019",
#                 titu = " Comparación de elecciones")

