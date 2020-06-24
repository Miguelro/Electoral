# Script para volatilidad

#'@title Indice Dimension de voto. Volatilidad
#'
#'@description Con esta función se calcula la volatilidad total de Pederson (1983:31 y 32),
#'y la volatilidad entre bloques. Las fórmulas para calcular estos valores se pueden encontrar
#'en el libro de Oñate y Ocaña titulado "Análisis de datos electorales", página 45.
#'Con esta dimensión lo que se hace es comparar el commportamiento del electorado en
#'dos elecciones distintas, para ver el traspase de votos, bientre entre bloques, o
#'entre patidos.
#'
#'@param dat1 Es un dataframe con dos columnas. La primera columna contiene las siglas
#'del partido político y la segunda los votos o escaños conseguidos del primer
#'periodo de tiempo, dependiendo
#'se quiera   calcular la volatilidad electoral o parlamentaria respectivamente.
#'@param dat2 Es un dataframe con dos columnas. La primera columna contiene las siglas
#'del partido político y la segunda los votos o escaños conseguidos del segundo
#'periodo de tiempo, dependiendo
#'se quiera   calcular la volatilidad electoral o parlamentaria respectivamente.
#'
#'@param enlace es un data.frame que sirve para enlazar los partidos, coaliciones o
#'agrupaciones que se quieran comparar ente los dos periodos objeto de estudio.Este
#'data.frame contiene un total de 22 columnas. La primera columna contiene la denominación
#'pertinente. Las 10 columnas siguientes (denominadas p1_i para i=1,2,...,10) sirven para
#'indicar el número de la/las fila/filas del/los partidos del primer periodo electoral que se tengan
#'que agrupar. Las 10 columnas siguientes (denominadas p2_i para i=1,2,...,10) sirven
#'para indicar  el número de la/las fila/filas del/los partidos del segundo periodo electoral que se tengan
#'que agrupar. Cuando no se necesiten las 10 columnas, las sobrantes se rellenan a ceros.
#'Los datos que aparecen en cada fila sirven para lo siguente: Para el primer bloque
#'de 10 columnas se suman los votos/escaños de los partidos que aparecen en ese bloque.
#'Para el segundo boque de 10 columnas, igualmente se suman los votos/escaños que aparecen
#'en ese segundo bloque. Estos dos valores serán los que posteriormente sirvan para
#'hacer la comparación.
#'La última columna de este data.frame ( que tiene por denominación "bloque"), contendrá
#'los valores "D" ó "I" indicativos de "Derecha","Izquierda".
#'Para aclarar todos estos conceptos ejecutar en R *browseVignettes("Relectoral")* ó *vignette("Volatilidad")*. 
#'
#'
#' @return Devuelve una lista con dos objetos. El primero es la volatilidad total, y el
#' segundo la volatilidad entre-bloques.
#'
#'
#'
#'@export
#'@md
volatilidad <- function(dat1,dat2,enlace){
  if(ncol(dat1) != 2) stop("El data.frame dat1 debe tener 2 columnas")
  if(ncol(dat2) != 2) stop("El data.frame dat2 debe tener 2 columnas")
  if(ncol(enlace) != 22) stop("El data.frame enlace debe tener 22 columnas")
  res <- matrix(NaN, ncol = 4,nrow = nrow(enlace))
  colnames(res) <- c("DENO","V1","V2","BLOQUE")
  res <- data.frame(res)
  for(h in 1:nrow(res)){
    # incluimos la sigla del partido
    res[h,1] <- enlace[h,1]
    # incluyo el bolque
    res[h,4] <- enlace[h,ncol(enlace)]
    # calculo los votos del primer proceso electoral
    p1=0
    for(m in 1:10){
      if(enlace[h,1+m]>0){
        p1 = p1+dat1[enlace[h,1+m],2]
      }
    }
    res[h,2]<-p1
    #calculo los votos del segundo proceso electoral
    p2=0
    for(m in 1:10){
      if(enlace[h,11+m]>0){
        p2 = p2+dat2[enlace[h,11+m],2]
      }
    }
    res[h,3] <- p2
  }
  #Calculo las columnas con los porcentajes
  res$V1_p <- (res$V1/sum(res$V1))*100
  res$V2_p <- (res$V2/sum(res$V2))*100
  # calculo el valor absoluto de las diferencias
  res$dif <- abs(res$V2_p-res$V1_p)
  VT= 0.5*sum(res$dif)
  #Volatilidad entre bloques
  Derecha=0
  izquierda=0
  for(m in 1:nrow(enlace)){
    if(enlace[m,ncol(enlace)]=="D"){
      Derecha=Derecha+res[m,"V2_p"]-res[m,"V1_p"]
    }else{
      izquierda=izquierda+res[m,"V2_p"]-res[m,"V1_p"]
    }
  }

  VB=0.5*(abs(Derecha)+abs(izquierda))
  lista=list(Total=VT,Entre=VB)

  return(lista)
}

#s<-volatilidad(dat1,dat2,enlace)
