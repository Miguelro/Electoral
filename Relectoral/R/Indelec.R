#if(!require("Relectoral")) install.packages("Relectoral"); require("Relectoral")
#if(!require("ggplot2")) install.packages("ggplot2"); require("ggplot2")
#if (!require("dplyr")) install.packages("dplyr"); require("dplyr")

#'@title utilidades. Obtencion indicadores electorales datos agrupados.
#'
#'@description Con esta función se pueden obtener para datos agregados  los siguientes
#'indicadores de desproporcionalidad electoral:
#'
#'* Saint Lagüe (SL)
#'* RAE (R)
#'* RAE Corregido (Rco)
#'* Loosemores y Hanby (LH)
#'* Cuadrados Mínimos de Gallagher (Gcm)
#'* Índice de máxima desviación (Lmax)
#'* Índice de Cox y Shugart (Cs)
#'* Índice de Cox y Shugart corregido(CS_correg)
#'* Línea de Tukey (LT)
#'
#'Todos los valores de estos indicadores están en un data.frame de salida donde los nombres de las columnas
#'coinciden con la identificación de estos indicadores que está entre paréntesis anteriormente.
#'
#'Igualmente se obtienen los siguientes indicadores de dimensión de voto:
#'
#'* Fragmentación Electoral y Parlamentaria (F)
#'* Número efectivo de partidos (N)
#'* Índice de Hiperfraccionamiento ( Hiper)
#'* Número efectivo de partidos Molinar (NP)
#'* Concentración de voto (Con)
#'* Competitividad entre patidos (Comp)
#'
#'Se calculan para estos indicadores de dimensión de voto la versión electoral y la
#'parlamentaria. Sus valores están contenidos en la salida de esta función dentro de
#'un data.frame con el nombre de las columnas es el indicado entre paréntesis anteriormente
#'y al mismo se añade el sufijo '_electoral' para indicar se trata de la versión electoral
#'o el sufijo '_parlamen' para indicar que se trata de la versión parlamentaria.
#'
#'**Nota importante**. Con esta función se pueden sacar los resultados de una forma
#'automática o manual. Con la forma automática se descargan los datos del Ministerior
#'del Interior. Con la forma manual, hay que introducir los datos con un data.frame
#'con la estructura que se indicará posteriormente.
#'
#'@import dplyr
#'
#'@param Ano es el año del proceso electoral que se quiera tratar. Debe ser un valor
#'numérico de cuatro dígitos( Si se utiliza un procedimiento manual, no hace falta
#'ningún valor a este parámetro) .
#'@param Mes es el mes del proceso electoral, debe ser una cadena con dos carcateres
#'numéricos, asociados al mes en que se han celebrado las elecciones.( Si se utiliza
#'un procedimiento manual, no hace faltaningún valor a este parámetro) .
#'@param RutaDescarga debe ser una cadena indicativa del camino a seguir para descargar
#'el fichero proveniente del Ministerio del Interior. Posteriormente y de forma
#'automática este fichero se borra. Se hace la advertencia de que esta ruta debe
#'indicar un lugar donde se tenga permiso de lectura y escritura, ya que en caso contrario
#'no se ompletará el proceso.( Si se utiliza un procedimiento manual, no hace falta
#'ningún valor a este parámetro) .
#'@param Auto Puede tener los valores lógicos TRUE ó FALSE. Por defecto tiene
#'el valor TRUE, para indicar que se quiere un porceso automático. En el supuesto de
#'querer un proceso manual, se dará un valor FALSE a este parámetro
#'@param datos debe contener un data.frame con tres columnas. La primera columna contiene
#'el nombre de los partidos, la segunda contiene el número de votos, y la tercera columna
#'contiene el número de escaños que consigue el partido político en cuestión. (Si se
#'utiliza un procedimiento automático, no hace falta ningún valor a este parámetro)
#'
#'@return Devuelve un data.frame con cuatro elementos:
#'
#'1.- Tiene por denominación 'dat' y es un data.frame con los votos y escaños de
#'cada candidatura, en términos absolutos y en porcentajes, también la distribución
#'acumulada de esos valores y una última columna conteniendo un indicador de
#'desproporcionalidad electoral que se calcula como la diferencia entre el porcentaje
#'de votos y el porcentaje de escaños obtenidos
#'
#'2.- Tiene la identificación 'grafico' y es un objeto de tipo ggpplot donde se representa
#'mediante un diagrama de barras, la diferencia entre el porcentaje de votos y el
#'porcentaje de escaños.
#'
#'3.- Tiene la identificación 'In_despro' y es un data.fame conteniendo los valores de los
#'índices de desproporcionalidad indicados anteriormente.
#'
#'4.- Tiene la identificación 'In_dimen' y contiene un data.frame con los valores de los
#'indicadores de dimensión de voto indicados anteriormente. Se facilita la versión
#'electoral y la parlamentaria
#'
#'
#'@examples
#'d<-AgregadosIndi(2019,"04",RutaDescarga = "D:/")
#'
#'
#'@export
#'@md

AgregadosIndi <- function(Ano=0,Mes="",RutaDescarga="",Auto=T,datos=""){
  if(Auto==T){
    if(Ano == 0) stop("Debes de facilitarme un a\u00F1o")
    if(Mes=="") stop("Debes indicar un mes")
    if(RutaDescarga=="") stop("Debes indicar una ruta de descarga")
  }else{
    if(class(datos) != "data.frame") stop("Debes indicar un data.frame de entrada")
    if(ncol(datos) != 3) stop("El data.frame facilitado debe tener tres columnas")
  }
  #Creamos el data.frame que se va a procesar
  if(Auto){
    data <- suppressMessages(Agregado_Prov_MIR(Ano,Mes,Tipo="Congreso",RutaDescarga, Borrar=T))
    # Miro la columna que comienza por "V_"
    start_idx <-  which(substr(colnames(data),1,2)=="V_")[1]
    #Convierto la columna de votos-escaños en numericos
    data[,start_idx:ncol(data)]<-apply(data[,start_idx:ncol(data)],2,as.integer)
    #Me quedo solo con las columnas que tienen votos-escaños
    data <-data[1:52,start_idx:ncol(data)]

    # obtenemos la suma de todas las columnas
    #data2 <- data[nrow(data),start_idx:ncol(data)]
    data2<- as.data.frame(t(colSums(data)))
    # Me quedo solo con las columna que tienen los votos (Lo valores lógicos)
    idx_votos <- substr(colnames(data2),1,2)=="V_"
    #Ahora  me quedo por un lado con las columna de los votos
    #y por otro con la de los escaños
    data3 <- t(data2[,idx_votos])
    data4 <- t(data2[,!idx_votos])
    # Gener el data.frame con las tres columnas que necesito: Nombre partidos,
    # N. votos; y n. escaños
    data5 <- data.frame(Partidos=substr(rownames(data3),3,nchar(rownames(data3))),
                        Votos=data3,Escanos=data4,stringsAsFactors = F)
    # Convierto la columna de votos y escaños a valores numéricos
    data5[,c(2,3)] <- apply(data5[,c(2,3)],2,as.integer)
    # Este data.frame es el que entra al proeso
  }else{
    # Si se facilita un data.frame va a salir un data.frama llamado también
    # data5 y con los mismos nombres de las columnas
    data5 <-datos
    # Llamo a las columnas igual que en el proceso automático
    colnames(data5)<-c("Partidos","Votos","Escanos")
  }

  # Ahora ya genero los resultados
  data5$Porc_votos <- round(data5$Votos/sum(data5$Votos),4)*100
  data5$Porc_escanos <- round(data5$Escanos/sum(data5$Escanos),4)*100
  data5$Acum_porc_votos <- cumsum(data5$Porc_votos)
  data5$Acum_porc_escanos <- cumsum(data5$Porc_escanos)
  data5$Desproporcionalidad <- round(data5$Porc_escanos - data5$Porc_votos,2)
  p<-ggplot(data5,aes(x=Partidos,y=Desproporcionalidad))+
    geom_bar(stat='identity') +
    coord_flip() +
    scale_x_discrete(limits=rev(data5$Partidos))+
    geom_hline(yintercept = 0, color="blue") +
    geom_text(aes(x = Partidos,y = max(Desproporcionalidad) + 0.1,
                  label = Desproporcionalidad),size=2.5)
  #Calculo indicadores de desproporcionalidad
  # Indice Sainte_Lague (SL)
  idx_sainte_lag <- Sainte_Lague(data5$Votos,data5$Escanos)
  # Indice de Rae (R)
  idx_rae <- Rae(data5$Votos,data5$Escanos)
  #Indice Rae corregido (Rco)
  idx_rae_corr <- Rae_corregido(data5$Votos,data5$Escanos,correc=0.5)
  # Indice Loos Hanby (LH)
  idx_loos_hanb <- Loos_Hanby(data5$Votos,data5$Escanos)
  # Índice Gallagher (Gcm)
  idx_Gallagher <- Gallagher(data5$Votos,data5$Escanos)
  # Índice máxima desviación
  idx_L_max <-L_max(data5$Votos,data5$Escanos)
  # Índice de Cox Shugart (CS)
  idx_Cox_Shugart <- Cox_Shugart(data5$Votos,data5$Escanos)
  # Índice Cox Shugart corregido
  idx_Cox_Shugart_corr <- Cox_Shugart_correg(data5$Votos,data5$Escanos)
  # ïndice desproporcionalidad línea Tukey (LT)
  idx_L_Tukey <- L_Tukey(data5$Votos,data5$Escanos)
  # Todos estos indicadores de desproporcionalidad los incluyo en un data.frame
  despr_res=data.frame(SL=idx_sainte_lag)
  despr_res$R<-idx_rae
  despr_res$Rco <- idx_rae_corr
  despr_res$LH <- idx_loos_hanb
  despr_res$Gcm <- idx_Gallagher
  despr_res$Lmax <- idx_L_max
  despr_res$CS <- idx_Cox_Shugart
  despr_res$CS_correg <- idx_Cox_Shugart_corr
  despr_res$LT <- idx_L_Tukey

  # Añado los indicadores de dimension del voto
  #Fragmentación electoral y parlamentario (F)
  idx_F <- fragmentacion_rae(data5$Votos,data5$Escanos)
  # Numero efectivo de partidos
  idx_N <- nep(data5$Votos,data5$Escanos)
  # Indice hiperfraccionamiento.N. efecti par. Kesselman & Wildgen
  idx_Hiper <-hiper(data5$Votos,data5$Escanos)
  #Numero efectivo partidos Molinar (NP)
  idx_NP <-nepMolinar(data5$Votos,data5$Escanos)
  # Concentracion de voto
  idx_Con <- concentracion(data5$Votos,data5$Escanos)
  # Competitividad electoral
  idx_Comp <- competitividad(data5$Votos,data5$Escanos)

  # Todos estos indicadores los incluyo en un data.frame
  dimension <- data.frame(F_electoral=as.numeric(idx_F[1]),
                          F_parlamen=as.numeric(idx_F[2]))
  #Numero efectivo partidos
  dimension$N_electoral <- as.numeric(idx_N[1])
  dimension$N_parlamen <- as.numeric(idx_N[2])
  # Hiperfraccionamiento..N. efecti par. Kesselman & Wildgen
  dimension$Hiper_electoral <- as.numeric(idx_Hiper[1])
  dimension$Hiper_parlamen <- as.numeric(idx_Hiper[2])

  #Numero efectivo partidos Molinar (NP)
  dimension$NP_electoral <- as.numeric(idx_NP[1])
  dimension$NP_parlamen <- as.numeric(idx_NP[2])



  # Concentración voto
  dimension$Con_electoral <- as.numeric(idx_Con[1])
  dimension$Con_parlamen <- as.numeric(idx_Con[2])

  #Competitividad electoral
  dimension$Comp_electoral <- as.numeric(idx_Comp[1])
  dimension$Comp_parlamen <- as.numeric(idx_Comp[2])


  #preparo la salida
  res <- list(dat=data5,grafico=p,In_despro= despr_res,In_dimen=dimension)
  return(res)
}

#Pruebo la función anterior
#d<-AgregadosIndi(2019,"04",RutaDescarga = "D:/")

#'@title utilidades. Obtencion indicadores electorales datos desagrupados.
#'
#'@description Con esta función se van a obtener los mismos resultados que con la
#'función 'AgregadosIndi()', pero para cada una de las regiones que figuren en
#'los datos facilitados. En concreto cuando se descargan datos del Ministerio del Interior
#'se obtendran los datos indicados, tanto para cada Comunidad Autónoma como para
#'cada provincia. El formato de esta salida, se indica más adelante.
#'
#' **NOTA IMPORTANTE:** También en esta ocasión se pueden introducir los datos de
#' forma automática o manual
#'
#'@param Ano es el año del proceso electoral que se quiera tratar. Debe ser un valor
#'numérico de cuatro dógitos( Si se utiliza un procedimiento manual, no hace falta
#'ningún valor a este parámetro) .
#'@param Mes es el mes del proceso electoral, debe ser una cadena con dos carcateres
#'numéricos, asociados al mes en que se han celebrado las elecciones.( Si se utiliza
#'un procedimiento manual, no hace faltaningún valor a este parámetro) .
#'@param RutaDescarga debe ser una cadena indicativa del camino a seguir para descargar
#'el fichero proveniente del Ministerio del Interior. Posteriormente y de forma
#'automática este fichero se borra. Se hace la advertencia de que esta ruta debe
#'indicar un lugar donde se tenga permiso de lectura y escritura, ya que en caso contrario
#'no se ompletará el proceso.( Si se utiliza un procedimiento manual, no hace falta
#'ningún valor a este parámetro) .
#'@param Auto Puede tener los valores lógicos TRUE ó FALSE. Por defecto tiene
#'el valor TRUE, para indicar que se quiere un porceso automático. En el supuesto de
#'querer un proceso manual, se dará un valor FALSE a este parámetro.
#'@param datos_v es un data.frame  con al menos tres columnas. La primera contiene el
#'nombre de la unidad geográfica que agrupa los elementos de la tercera columna,
#'que en el caso de España, inicialmente puede ser el nombre de la Comunidad
#'Autónoma. La segunda columna es de carácter alfabético y contiene el código
#'de la unidad geográfica que aparece en la tecera columnas.En el caso de España,
#'lo normal será el código de provincia INE. La tercera columna será también
#'de tipo carácter y contiene el nombre de la unidad geográfica, en el caso de
#'España normalmente será el nombre de la provincia. La cuarta columna y siguientes
#'hará referencia  a un determinado partido político, y el nombre de la columna
#'se aconseja coincida con las siglas del partido político en cuestion  será de
#'tipo numérico y contrendrá los votos de dicho patido.  (Si se
#'utiliza un procedimiento automático, no hace falta ningún valor a este parámetro)
#'
#'@param datos_d Es un data.frame con la misma estructura que datos_v, lo único que
#'en las columnas de la cuarta en adelante figurarán los diputados obtenidos por cada
#'partido político para cada una de las unidades territoriales contempladas en las
#'filas. **El orden de estas columnas**, debe ser mismo que el utilizado en datos_v
#'para incluir los votos.(Si se
#'utiliza un procedimiento automático, no hace falta ningún valor a este parámetro)
#'
#'@return La salida consiste en un conjunto de listas en dos capas
#'
#'1. Capa de Comunidades Autónomas o unidades geográficas de agrupación. Hay una lista
#'para cada Comunidad Autónoma o unidad de agrupación. El orden de esta lista coincide
#'con el orden en que aparecen las Comunidades Autonomas o unidades de agrupación en
#'el data.frame de entrada. Después, elegida una Comunidad Autónoma o unidad de agrupación
#'se obtiene otra lista con el mismo contenido ( los mismos cuatro elementos) que el explicado para la función
# 'AgregadosIndi()'
#'
#'2.- Capa de provincias o unidades desagrupadas. En este caso también se obtiene una
#'lista para cada provincia o unidad de desagrupación, con el identificador igual
#'al que figura para la provincia o unidad de agrupación en los ficheros de entrada.Finalmente
#'para cada provincia, se obtiene una lista con los cuatro elementos que se tenían con
#'la función 'AgergadosIndi()',
#'
#'@examples
#'d2<-DesAgregadosIndi(2019,"04",RutaDescarga = "D:/")
#'
#'
#'@export
#'@md


# A continuación defino la función para datos desagregados
DesAgregadosIndi <- function(Ano=0,Mes="",RutaDescarga="",Auto=T,datos_v="",datos_d=""){
  if(Auto==T){
    if(Ano == 0) stop("Debes de facilitarme un a\u00F1o")
    if(Mes=="") stop("Debes indicar un mes")
    if(RutaDescarga=="") stop("Debes indicar una ruta de descarga")
  }else{
    if(datos_v=="") stop("Debes facilitar el data.frame con datos de voto")
    if(datos_d=="") stop("Debes facilitar el data.frame con datos de diputados")
    if(class(datos_v) != "data.frame") stop("Debes indicar un data.frame de entrada conteniendo los votos")
    if(class(datos_d) != "data.frame") stop("Debes indicar un data.frame de entrada conteniendo los diputados")
  }

  # Si es automatico genero dos data.frame. El primero conteniendo votos
  # y el segundo los escaños obtenidos
  if(Auto){
    data <- suppressMessages(Agregado_Prov_MIR(Ano,Mes,Tipo="Congreso",RutaDescarga, Borrar=T))
    #Me quedo con las tres primeras columna que contienen las datos de identifi.
    #Provincia y Comunidad Autónoma
    iden <- data[1:50,1:3]
    # Miro la columna que comienza por "V_"
    start_idx <-  which(substr(colnames(data),1,2)=="V_")[1]
    #Me quedo solo con las columnas que tienen votos o diputados
    data22 <- data[1:50,start_idx:ncol(data)]
    #idx_votos vale TRUE si es una columna de votos. FALSE en caso contrario
    idx_votos <- substr(colnames(data22),1,2)=="V_"
    #data33 contiene los valores de los votos
    data33 <- data22[,idx_votos]
    # Añado lod datos de indentificación territorial
    data33 <- cbind(iden,data33)
    # data44 contiene los valores de los diputados obtenido
    data44 <- data22[,!idx_votos]
    # Añado lod datos de indentificación territorial
    data44 <- cbind(iden,data44)
    # Como salidad tengo dos data.frame data33 y data44, ambos tienen en las
    # tres primeras columnas los datos de identificación territorial y luego
    # d33 los datos de los votos  y d44 los datos de los diputados
  }else{ #si no tomo los data frame que se incluyen en los parámetros de la función
    # Para unificar estructura datos obtengo como salida una estructura
    #similar a la obtenida en el caso anterior
    data33 <- datos_v # Contiene los votos
    data44 <- datos_d #contiene los diputados

  }
  # tanto en el caso manual como en el automático obtengo dos ficheros. En ambos
  # la primera columna contiene el nombre de la C. Autónoma. La segunda el código
  # de la provincia y la tercera el nombre de la provincia. El resto de los campos
  # se corresponden con las candidaturas y el data33 contiene los votos y data44
  # contienen los diputados obtenidos en cada provincia

  res_prov <-list() # En esta lista van todas las respuestas
  # Itero ahora sobre cada provincia
  for(h in 1:nrow(data33)){
    cprov <- data33[h,2]
    # Cojo los nombres de los partidos políticos
    d1<-colnames(data33[4:ncol(data33)])
    d1 <- substr(d1,3,nchar(d1))
    #Cojo los votos de esa provincia
    d2<-t(data33[h,4:ncol(data33)])
    #Cojo los diputados de esa provincia
    d3 <- t(data44[h,4:ncol(data33)])
    # Ahora integro todo en un data.frame
    data5 <-data.frame(Partidos=d1,Votos=d2,Escanos=d3,stringsAsFactors = F)
    colnames(data5) <- c("Partidos","Votos","Escanos")
    # Convierto la columna de votos y escaños a valores numéricos
    data5[,c(2,3)] <- apply(data5[,c(2,3)],2,as.integer)
    # Me quedo solo con los partidos que tienen algún voto
    votos <- data5[,2]>0
    data5 <-data5[votos,]
    #browser()
    #Ahora paso estos datos a la función AgregadosInd
    r <- AgregadosIndi(Ano=0,Mes="",RutaDescarga="",Auto = F,datos=data5)
    # El resultado lo incluyo en una lista
    res_prov[[cprov]]<- r
  }
  # Hago lo propio agregando por Comunidades Autónomas
  res_CCAA <- list() # Aquí meto los resultados de las Comunidades Autónoma
  #Quito lo nombres y código de provincia
  data333 <- data33[,-c(2,3)]
  #Cambio el nombre de la primera variable
  colnames(data333)[1]<-"Comunidad"
  # Cambio a valores numéricos los votos
  data333[,2:ncol(data333)] <-apply(data333[,2:ncol(data333)],2,as.integer)
  # Obtengo la suma de votos para cada Comunidad Autónoma
  data333 <-data333 %>% group_by(Comunidad) %>% summarise_all(funs(sum))
  #Hago lo mismo pero para los diputados
  data444 <- data44[,-c(2,3)]
  #Cambio nombre primera variable
  colnames(data444)[1]<-"Comunidad"
  #Cambio a valores numéricos los escaños obtenidos
  data444[,2:ncol(data444)] <-apply(data444[,2:ncol(data444)],2,as.integer)
  # Obtengo la suma de escaños para cada Comunidad Autónoma
  data444 <-data444 %>% group_by(Comunidad) %>% summarise_all(funs(sum))
  for(h in 1:nrow(data333)){
    # Cojo los nombres de los partidos políticos
    d1<-colnames(data333[2:ncol(data333)])
    d1 <- substr(d1,3,nchar(d1))
    #Cojo los votos de esa Comunidad autónoma
    d2<-t(data333[h,2:ncol(data333)])
    #Cojo los diputados de esa Comunidad Autónoma
    d3 <- t(data444[h,2:ncol(data444)])
    # Ahora integro todo en un data.frame
    data5 <-data.frame(Partidos=d1,Votos=d2,Escanos=d3,stringsAsFactors = F)
    colnames(data5) <- c("Partidos","Votos","Escanos")
    # Me quedo solo con los partidos que tienen algún voto
    votos <- data5[,2]>0
    data5 <-data5[votos,]
    #Ahora paso estos datos a la función AgregadosInd
    r <- AgregadosIndi(Ano=0,Mes="",RutaDescarga="",Auto = F,datos=data5)
    # El resultado lo incluyo en una lista
    res_CCAA[[as.character(h)]]<- r

  }
  # Genero la respuesta
res_total <- list(CCAA=res_CCAA,PROV= res_prov)
return(res_total)
}

#Pruebo la función anterior
#d2<-DesAgregadosIndi(2019,"04",RutaDescarga = "D:/")



