# Funciones para Reparto de Escaños

# Función para metodo restos mayores
#'@title Metodo restos mayores
#'
#'@family Reparto Diputados
#'@seealso \code{\link{reparto_div}} para reparto mediante divisores
#'@seealso [https://en.wikipedia.org/wiki/Largest_remainder_method](https://en.wikipedia.org/wiki/Largest_remainder_method) en wikipedia
#'
#'
#'
#'@description Esta función sirve para traducir una serie de votos
#'en escaños, siguiendo el criterio denominado "Restos mayores". De forma
#'resumida este método se calcula de la sigueinte forma: se divide el
#'número total de votos entre los escaños totales ( costo de cada escaño ).
#'Después se dividen los votos de cada partido entre el cociente anterior.
#'Se toman las partes enteras de los resultados anteriores que serán
#'los escaños iniciales que corresponde a cada partido. El resto de
#'escaños hasta completar los que hay que repartir, se asigna a los
#'partidos que tengan los restos más altos ( ver https://es.wikipedia.org/wiki/M%C3%A9todo_del_resto_mayor).
#'
#'El costo de cada escaño es lo que diferencia a unos métodos de otros.
#'De esta manera si n es el total de escaños y m la suma de todos los votos,
#'la denominación de los métodos  es la siguiente, dependiendo del
#'cociente que se tome:
#'
#'\enumerate{
#'\item \bold{Hare (Hare)}: q=m/n
#'\item \bold{Droop (Droop)}:q=1+(m/(1+n))
#'\item \bold{Imperiali (Imperiali)}: q=m/(n+2)
#'\item \bold{Imperiali modificado (Mod_Imperiali)}: q=m/(n+3)
#'\item \bold{Hangenbach Bischof (hangenbach-bischo)}: q=m/(n+1)
#'}
#'

#'
#'En todos los caso el valor de q se redondea al entero más proximo
#'
#'
#'@param partidos  Un vector de texto conteniendo el nombre de los partidos
#'@param votos Un vector de números enteros, con los votos de cada partido
#'@param escanos un número entero conteniendo el total de escaños a repartir
#'@param metodo Es el método a utilizar los valores admitidos son: "Hare","Droop","Imperiali",
#' "Mod_Imperiali","hangenbach-bischo".
#' 
#'  El valor por defecto es "Hare"
#'
#'
#'@return Dataframe con los partidos políticos y los votos asignados
#'
#'@import readxl
#'
#'@examples
#'Restos_Mayores(c("A","B","C","D","E","F","G"),
#'  c(391000,311000,184000,73000,27000,12000,2000),
#'  21,metodo = "Imperiali")
#'
#'@seealso \code{\link{reparto_div}}
#'
#'
#'@export
#'@md
Restos_Mayores<-function(partidos,votos,escanos,metodo="Hare"){
  if(length(partidos)!=length(votos)) stop("Dimensi\u00F3n vector partidos, debe ser igual a la dimensi\u00F3n
                                           del vector votos")
  if(escanos<=0) stop("El n\u00FAmero de esa\u00F1os debe ser mayor o igual que 1")

  if(!(metodo %in% c("Hare","Droop","Imperiali","Mod_Imperiali",
                     "hangenbach-bischo")))
  {
    stop("M\u00FAtodo inv\u00E1lido")
  }
  tvotos<-sum(votos) # total de votos
  # Calculo del valor de q
  if(metodo == "Hare"){
    cociente=round(tvotos/escanos,0)
  }else if(metodo =="Droop"){
    cociente=round(1+(tvotos/(1+escanos)),0)
  }else if(metodo == "Imperiali"){
    cociente=round(tvotos/(escanos+2),0)
  }else if(metodo == "Mod_Imperiali"){
    cociente=round(tvotos/(escanos+3),0)
  }else if(metodo == "hangenbach-bischo"){
    cociente=round(tvotos/(escanos+1),0)
  }

  # Tomo la parte entera
  ente<-floor(votos/cociente)
  # tomo la parte decimal
  frac<-(votos/cociente)-ente

  Entera<-data.frame(parti=partidos,esca=ente)
  Fracional<-data.frame(parti=partidos,fra=frac)
  # Calculo resto de escaños quedan a repartir
  #por la parte fraccionaria
  resto_escanos<-escanos-sum(Entera$esca)
  Fracional<-Fracional[order(-Fracional$fra),][1:resto_escanos,]
  # Asigno un escaño partidos con mayor resto
  Fracional$fra<-1
  Total<-merge(Entera,Fracional,all.x=T)
  # Obtengo escaños finales
  Total$escaT<-apply(Total[,2:3],1,sum,na.rm=T)
  return(Total[,c(1,4)])
}

#Restos_Mayores(c("A","B","C","D","E","F","G"),
#               c(391000,311000,184000,73000,27000,12000,2000),
#               21,metodo = "Imperiali")



#### Ley d'Hondt. Esta función no va al paquete
dHont<-function(candidaturas,votos,escanos){
  tmp<-data.frame(
    candi<-rep(candidaturas,each=escanos),
    scor<-as.vector(sapply(votos,function(x) x/1:escanos))
  )
  tmp<-tmp$candi[order(-tmp$scor)][1:escanos]
  return(table(tmp))
}

dHont(c("A","B","C"),c(25000,16000,9000),5)

# esto sí va al paquete
# Repartos por cocientes
# Esta función la utilizo de apoyo
# Sirve para hacer las divisiones, dependiendo del método usado
coci<-function(x,metodo,escanos){
  if(metodo == "dhondt"){
    a<- seq.int(1,escanos)
    #re<-x/seq.int(1,escanos)
  }
  else if(metodo == "saint_lague"){
    # Calculo secuencia divisores y me quedo con tantos
    #como indique la variable escanos
    a<- seq.int(1,4*escanos,by=2)[1:escanos]
    #re<-x/a
  }
  else if(metodo=="saint_lague_Mod"){
    a<-c(1.4,seq.int(3,4*escanos,by=2))[1:escanos]
    re<-x/a
  }else if(metodo=="Danish"){
    a<-seq.int(1,4*escanos,by=3)[1:escanos]
  }
  else if(metodo=="Imperiali"){
    a<-seq.int(2,escanos+1)[1:escanos]
  }
  else if(metodo=="Hill_Huntington"){
    a1<-1:escanos
    a2<-2:(escanos+1)
    a<-sqrt(a1*a2)
  }
  else if(metodo=="Dean"){
    a1<-1:escanos
    a2<-2:(escanos+1)
    a11<-(2*a1)*a2
    a22<-2*a1+1
    a<-a11/a22
  }

  # Calculo el valor de las divisiones
  re<-x/a
  return(re)
}

#Función con los métodos de los divisores
#'@title Reparto mediante metodos de los divisores
#'@family Reparto Diputados
#'@seealso \code{\link{Restos_Mayores}} para reparto restos mayores
#'@seealso [https://en.wikipedia.org/wiki/D\%27Hondt_method](https://en.wikipedia.org/wiki/D\%27Hondt_method) en wikipedia
#'@seealso [https://bit.ly/3aEidM9](https://bit.ly/3aEidM9) en wikipedia.
#'@seealso [https://bit.ly/2Q0a0u0](https://bit.ly/2Q0a0u0) en internet.
#'
#'@description Con esta opción se utiliza diversos métodos que tienen
#'Como carcaterística principal que el número de votos obtenidos por las candidaturas se
#'dividen por una serie de números. El nombre del método usado depende de cómo esté formada
#'esa serie de números. En este sentido los métodos admitidos son los siguientes:
#'
#'\enumerate{
#'\item \bold{dhont}. Es la denominada Ley D'Hondt y es el procedimiento utilizado en España
#'para transformar votos en escaños en el Congreso de los Diputados. La derie de números
#'utilizada como conciente son los números enteros: 1,2,...n , siendo n el número de escaños
#'a repartir.
#'
#'\item \bold{saint_lague}. Es el método de Saint Lagüe, método webster o de divisores impares.
#'El conjunto de divisores estárá formado por los números impares, es decir {1,3,..,2n+1}
#'
#'\item \bold{saint_lague_Mod}. Es el método de Saint Lagüe modificado.Una variante del método
#'de Saint Lagüe, según la cual el cociente inicial es v/1.4 y a pertir de que cada lista
#'tenga un escaño se adopta la fórmula estándar v/(2n+1)
#'
#'\item \bold{Danish}. En este método los divisores van de tres en tres unidades, es decir
#'está formado por los números: 1,4,7,10,13...y el n-ésimo sería 3*n-2
#'
#'\item \bold{Imperiali}. Los divisores son los enteros positivos pero arranacando del
#'número 2 en adelante, es decir son: 2,3,4,....
#'
#'\item \bold{Hill_Huntington}. En este caso los números divisores están formados por
#'la siguiente secuencia: sqrt(2),sqrt(6),sqrt(12),...., sqrt(n*(n+1))
#'
#'\item \bold{Dean}. El conjunto de divisores está formado por los siguientes números
#'4/3, 12/5, 24/7,40/0,....,(2*n)(n+1)/(2*n+1)
#'
#'}
#'En todos los caso el valor de q se redondea al entero más proximo
#'
#'
#'@param candidaturas  Un vector de texto conteniendo el nombre de los partidos
#'@param votos Un vector de números enteros, con los votos de cada partido
#'@param escanos un número entero conteniendo el total de escaños a repartir
#'@param metodo Es el método a utilizar los valores admitidos son
#'  c("dhondt","saint_lague","saint_lague_Mod",
#' "Danish","Imperiali","Hill_Huntington",
#' "Dean")
#'
#'
#'@return Devuelve un Dataframe con los nomnbre de los partidos políticos y los
#'Diputados asignados con el metodo elegido
#'
#'@import readxl
#'
#'@examples
#'
#'reparto_div(c("A","B","C","D","E"),c(340000,280000,160000,60000,15000),7,metodo="dhondt")
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='saint_lague')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='saint_lague_Mod')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Danish')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Imperiali')
#'reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Dean')
#'
#'
#'@export
#'@md
reparto_div<-function(candidaturas,votos,escanos,metodo){
  # Primero compruebo que el método existe
  if(!(metodo %in% c("dhondt","saint_lague","saint_lague_Mod",
                     "Danish","Imperiali","Hill_Huntington",
                     "Dean"))){
    stop("El m\u00E9todo solicitado no est\u00E1 implementado en esta funci\u00F3n")
  }
  scor2<-as.vector(sapply(votos,coci,metodo,escanos))

  tmp<-data.frame(
    candi = rep(candidaturas,each=escanos),
    scor = scor2
  )
  tmp<-tmp$candi[order(-tmp$scor)][1:escanos]
  r <- as.data.frame(table(tmp))
  colnames(r) <- c("Candidatura", "Escanos")
  return(r)

}


# reparto_div(c("A","B","C","D","E"),c(340000,280000,160000,60000,15000),7,metodo="dhondt")
# reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='saint_lague')
# reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='saint_lague_Mod')
# reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Danish')
# reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Imperiali')
# reparto_div(c("A","B","C","D"),c(340000,280000,160000,60000),7,metodo='Dean')
