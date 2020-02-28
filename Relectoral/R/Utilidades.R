# Este fichero contiene un conjunto de funciones de utilidad.
#'@title utilidades.Obtener datos para metodo Bochsler (2010)
#'
#'@section utilidades
#'
#'@description El índice de nacionalización de partidos estandarizado elaborado por Bochsler (2010)
#'se puede calcular mediante una hoja de cálculo excel, que se puede descargar de la siguiente
#'página web [https://www.bochsler.eu/pns/](https://www.bochsler.eu/pns/). El fichero excel que se puede desccargar
#'contiene una macro para calcular \bold{el índice de nacionalización de partidos}, muy
#'utilizado en la actualidad. El objetivo de esta función es crear una hoja excel que se
#'carga directamente con los datos facilitados por el Ministerior del Interior (MIR) de españa,
#'[http://www.infoelectoral.mir.es/](http://www.infoelectoral.mir.es/). Estos datos así creados están dispuestos de
#'foram tal que tan sólo hay que copiar y pegar en la macro creada por Bochsler. De esta manera
#'se suaviza enormemente el trabajo de cargar la macro con datos.
#'En la hoja excel que se crea, la última línea contiene los totales que son los que hay
#'que colocar en la fila 8 de la macro de Daniel Bochsler
#'
#'@param Ano Es el valor del año de los datos que se quieren descagar. Este dato puede ser
#'numérico o de tipo carácter ( mejor numérico)
#'@param Mes Es el mes en el que se ha realizado la elección. Obligatoriamente tiene que ser
#'de tipo carácter y a dos dígitos, es decir "04" es válido pero "4" no es válido
#'@param RutaDescarga Se debe indicar una ruta del disco duro local donde se descarga el
#'fichero del Ministerior del Interior. Una vez finalizada la descarga de forma automática
#'el fichero descargado se borra.
#'@param RutaSalida Es una ruta del disco duro local donde se depositará la hoja excel generada
#'con los datos para llegar a la macro de Daniel Bochsler. El libro excel generado se
#'denomina  'Bochsler.xlsx'
#'
#'
#'@return Con la presente función se obtiene una hoja excel denominada 'Bochsler.xlsx' colocada
#'en la ruta que se ha indicado con el parámetro 'RutaSalida'
#'
#'@import xlsx
#'
#'@examples
#'
#'Bochsler(2019,"04","D:/","D:/")
#'Bochsler(2016,"06","D:/","D:/")
#'
#'@export
#'@md
#if (!require("xlsx")) install.packages("xlsx"); require("xlsx")
Bochsler <- function(Ano,Mes,RutaDescarga,RutaSalida){
  if( class(Mes) != "character") stop( " El mes debe ser de tipo car\u00E1cter")
  if(nchar(Mes) != 2) stop("El mes debe contener dos caracteres")
  if(is.null(RutaDescarga)) stop("Se debe facilitar alg\u00FAn valor para 'RutaDescarga'")
  if(is.null(RutaSalida)) stop("Se debe facilitar alg\u00FAn valor para 'RutaSalida'")
  # Creo el libro excel

  wb<-createWorkbook(type="xlsx")
  CellStyle(wb, dataFormat=NULL, alignment=NULL,
            border=NULL, fill=NULL, font=NULL)
  sheet <- createSheet(wb, sheetName = "Datos")
  # Descargo los datos del MIR
  datos_MIR<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",RutaDescarga,Borrar=T))
  # Elimino la fila de los totales,
  datos_MIR <-datos_MIR[1:52,]
  # Tengo que sacar la columna de votos válidos
  if(as.integer(Ano) %in% c(1977,1979,1982)){
    column = 8
  } else if(as.integer(Ano) %in% c(1986,1989,1993,1996,2000,2004,2008)){
    column=12
  } else {
    column=13
  }

  valid <- as.data.frame(datos_MIR[,column])
  valid <- as.data.frame(apply(valid,2,as.integer))
  # Mediante expresión regular selecciono las columnas que tienen el número de votos
  columnas_Votos<-grep("V_",colnames(datos_MIR))
  # Me quedo solo con las columna de votos
  datos_MIR_Votos<-as.data.frame(datos_MIR[,columnas_Votos])
  #Uno la columna del total votos
  datos_MIR_Votos<-cbind(valid,datos_MIR_Votos)

  datos_MIR_Votos <- as.data.frame(apply(datos_MIR_Votos,2,as.integer))
  #calculo los totales
  totales<-colSums(datos_MIR_Votos)
  # Añado los totales

  datos_MIR_Votos[53,]<-totales
  #Ordenos las columnas de mas a menos votos
  datos_MIR_Votos<-datos_MIR_Votos[,order(totales,decreasing = T),]

  # Añado la columna del nombre de la provincia
  provin <-datos_MIR[3]
  provin[53,1]<-"Totales"

  datos_MIR_Votos<-cbind(provin,datos_MIR_Votos)

  addDataFrame(datos_MIR_Votos,sheet,startRow=1, startColumn=1)
  saveWorkbook(wb,paste0(RutaSalida,"/Bochsler.xlsx"))
  print("Hoja excel llamada 'Bochsler.xlsx' creada en la ruta solicitada")
}

#Bochsler(2016,"06","D:/","D:/")


### Injusticia Matemática provincias ###
#'@title utilidades. Calculo de Injusticia Matematica, desagregada
#'
#''@section utilidades
#'
#'@description La función de este mismo paquete denominada 'InjusticiaM()', permite obtener
#'la injusticia matemática para el conjunto de datos que se pase. Ahora bien, si queremos
#'obtener la injusticia matemática para diferentes áreas geográficas se requiere un importante
#'esfuerzo y tiempo para preparar y ejecutar los datos. Con esta función, se agiliza enormemente
#'este proceso, si lo que se quiere evaluar son resultados al Congreso de los Diputados en
#'España, ya
#'que gracias a esta función, se puede extraer de forma automática la información del
#'Ministerio de Interior, se procesa de forma directa para cada provincia y los datos nacionales
#'y se devuelve el resultado en un objeto de tipo list() de R, donde cada elemento se
#'corresponde con el resultado obtenido de cada provincia o del Total Nacional.
#'
#'@param Ano Es el valor del año de los datos que se quieren descagar. Este dato puede ser
#'numérico o de tipo carácter ( mejor numérico)
#'@param Mes Es el mes en el que se ha realizado la elección. Obligatoriamente tiene que ser
#'de tipo carácter y a dos dígitos, es decir "04" es válido pero "4" no es válido
#'@param Ruta Se debe indicar una ruta del disco duro local donde se descarga el
#'fichero del Ministerior del Interior. Una vez finalizada la descarga de forma automática
#'el fichero descargado se borra.
#'
#'@return El resultado es una lista con 53 elementos, de manera que cada elemento es una
#'matriz que contiene los datos de las injusticia matemática calculada. Los 'names' de esta
#'lista son los códigos de provincia ( códigos INE), o bien la expresión 'Total' si son los datos
#'de toda España.
#'
#'@examples
#'f<-InjusticiaM_desagregada(2019,"04","D:/")
#'
#'@export
InjusticiaM_desagregada<-function(Ano,Mes,Ruta){
  if( class(Mes) != "character") stop( " El mes debe ser de tipo car\u00E1cter")
  if(nchar(Mes) != 2) stop("El mes debe contener dos caracteres")
  if(is.null(Ruta)) stop("Se debe facilitar alg\u00FAn valor para 'Ruta'")
  datos_MIR<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta,Borrar=T))
  # Me quedo con los datos de cada provincia
  datos_MIR<-datos_MIR[1:52,]
  # Saco los nombre de las provincias
  Cprov<-datos_MIR[,2]
  # Añado en la ultima posición el epigrafe "Total"
  Cprov[53,]<-"Total"
  #me quedo con los votos
  # Mediante expresión regular selecciono las columnas que tienen el número de votos
  columnas_Votos<-grep("V_",colnames(datos_MIR))
  dat_votos<- as.data.frame(datos_MIR[,columnas_Votos])
  # paso los datos a numéricos
  dat_votos<-as.data.frame(apply(dat_votos,2,as.integer))
  #calculo los totales
  totales_voto<-colSums(dat_votos)
  #Añado ahora los votos totales
  dat_votos[53,]<-totales_voto
  #Hago lo mismo que antes pero con los escaños
  columnas_escanos<-grep("D_",colnames(datos_MIR))
  dat_escanos<-as.data.frame(datos_MIR[,columnas_escanos])
  # paso los datos a numéricos
  dat_escanos<-as.data.frame(apply(dat_escanos,2,as.integer))
  #calculo los totales
  totales_escanos<-colSums(dat_escanos)
  #Añado ahora los votos totales
  dat_escanos[53,]<-totales_escanos

  #Creo una lista donde almaceno los resultados.
  res<-list()
  # lo calculo para cada provincia
  for(i in 1:52){
    # Saco vector logico si el partido ha tenido votos o no en esa provincia
    chek<-dat_votos[i,]>0
    dat_votos_prov<-dat_votos[i,chek]
    dat_escanos_prov<-dat_escanos[i,chek]
    dat_in2<-data.frame(
      par=substr(colnames(dat_votos)[chek],3,nchar(colnames(dat_votos))[chek]),
      vot=as.numeric(dat_votos_prov),
      sea=as.numeric(dat_escanos_prov), stringsAsFactors = F
    )

    res[[i]]<-InjusticiaM(dat_in2)*10000 # multiplico por esta cantidad para no sacar cifras muy bajas
  }
  #Ahora lo hago para el total nacional
  dat_in<-data.frame(
    par=substr(colnames(dat_votos),3,nchar(colnames(dat_votos))),
    vot=as.numeric(dat_votos[53,]),
    sea=as.numeric(dat_escanos[53,]),stringsAsFactors = F
  )

  res[[53]]<-InjusticiaM(dat_in)*10000
  #Asigno los nombres de las provincias y el total
  names(res)<-as.data.frame(Cprov)[,1]
  return(res)
}

#f<-InjusticiaM_desagregada(2019,"04","D:/")

### Generar ficheros .bazi ####

#'@title utilidades.Generacion de ficheros *.bazi
#'
#'@description BAZI es un programa Java disponible gratuitamente que implementa varios
#' métodos de distribución de escaños para sistemas de representación proporcional.
#' Ofrece la posibilidad de utilizar método de los divisores, así como métodos de cuota.
#' La descarga del programa se puede hacer en [https://www.math.uni-augsburg.de/htdocs/emeriti/pukelsheim/bazi/](https://www.math.uni-augsburg.de/htdocs/emeriti/pukelsheim/bazi/)
#' Instrucciones sobre su uso se pueden en contrar en la siguiente dirección web:
#' [https://opus.bibliothek.uni-augsburg.de/opus4/frontdoor/index/index/docId/601](https://opus.bibliothek.uni-augsburg.de/opus4/frontdoor/index/index/docId/601).
#' Con esta función lo que se obtienen son fichero de tipo ASCII planos, con extensión '.bazi'
#' que tienen una estructura enfocada a que su información pueda ser entendida por
#' BAZI y obtener los resultados que se pueden entresacar con esta aplicación.
#'@param Ano Es el valor del año de los datos que se quieren descagar. Este dato puede ser
#'numérico o de tipo carácter ( mejor numérico)
#'@param Mes Es el mes en el que se ha realizado la elección. Obligatoriamente tiene que ser
#'de tipo carácter y a dos dígitos, es decir "04" es válido pero "4" no es válido
#'@param camino Se debe indicar una ruta del disco duro local donde se descarga el
#'fichero del Ministerior del Interior ( finalizado el proceso se borra de forma automática),
#'y done se escribirá el fichero con extensión '.bazi' listo para ser usado por BAZI.
#'@param cota Es la barrera eletoral que se quiera utilizar. En el caso del Conreso de
#'los Diputados en España es del 3 por ciento (partidos con menos del 3 por ciento de
#'los votos válidos o entran en el reparto), y es el valor que se le ha asignado por defecto.
#'
#'@return El resultado es un fichero ASCII plano con una estructura de información propia
#'para ser leido por BAZI y que tiene la siguiente denominación genérica 'Congreso_AAAA_MM.bazi',
#'donde AAAA es el año del proceso electoral y MM el mes a dos dígitos.
#'
#'
#' @examples
#' Bazi(Ano = 2019,"04","D:/")
#'
#'@export
#'@md
Bazi<-function(Ano,Mes,camino,cota=3){
  sink(paste0(camino,"Congreso_",Ano,"_",Mes,".bazi"))

  cat(paste("=TITEL=","Elecciones al Congreso, A\u00F1o:",Ano,"Mes:",Mes,"\n"))
  cat("=METHODE= DivAbr\n")
  cat("=AUSGABE= vert./horiz., Div/Quote, kodiert\n")
  cat("=EINGABE= Candidatura, Votos, ---\n")
  cat("=DISTRIKTOPTION= separat\n")
  datos_Mir<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta=camino))
  datos_Mir<-datos_Mir[0:52,]


  cprov<-apply(datos_Mir[,2],2,as.integer)
  datos_Mir<-datos_Mir[order(cprov),]
  Provincias<-apply(datos_Mir[,3],2,as.character)

  # Tengo que sacar la columna de votos válidos para luego aplicar el 3 por ciento
  if(as.integer(Ano) %in% c(1977,1979,1982)){
    column = 8
  } else if(as.integer(Ano) %in% c(1986,1989,1993,1996,2000,2004,2008)){
    column=12
  } else {
    column=13
  }

  valid <- as.data.frame(datos_Mir[,column])
  valid <- as.data.frame(apply(valid,2,as.integer))
  # Mediante expresión regular selecciono las columnas que tienen el número de votos
  columnas_Votos<-grep("^(V_)",colnames(datos_Mir))
  # Me quedo solo con las columna de votos
  datos_MIR_Votos<-as.data.frame(datos_Mir[,columnas_Votos])

  datos_MIR_Votos <- as.data.frame(apply(datos_MIR_Votos,2,as.integer))
  #calculo los totales
  totales<-colSums(datos_MIR_Votos)
  #Ordenos las columnas de mas a menos votos
  datos_MIR_Votos<-datos_MIR_Votos[,order(totales,decreasing = T),]

  #Hago lo mismo que antes pero con los escaños
  columnas_escanos<-grep("^(D_)",colnames(datos_Mir))
  datos_MIR_escanos<-as.data.frame(datos_Mir[,columnas_escanos])
  datos_MIR_escanos <- as.data.frame(apply(datos_MIR_escanos,2,as.integer))

  for(i in 1:nrow(datos_Mir)){
    Dipu <-sum(datos_MIR_escanos[i,])#N. Diputados
    #Me quedo con los partidos que tienen algun voto
    Si_Voto<-datos_MIR_Votos[i,]>0
    datos_voto<-datos_MIR_Votos[i,Si_Voto]
    datos_escanos<-datos_MIR_escanos[i,Si_Voto]
    #Aplico el minimo voto para oder conseguir escaño
    tope<-valid[i,1]*(cota/100)
    corte<-datos_voto[1,]>tope
    #Me quedo con los partidos con votos encima topa
    datos_voto<-datos_voto[,corte]
    datos_escanos<-datos_escanos[,corte]
    cat(paste("=DISTRIKT=",Provincias[i,1],"\n"))
    cat(paste("=MANDATE=",Dipu,"\n"))
    cat("=DATEN=\n")
    a<-colnames(datos_voto)
    siglas<- substr(a,3,nchar(a))
    #Recorro las columnas
    #browser()
    for(j in 1:ncol(datos_escanos)){
      cat(paste0("\"",siglas[j],"\"","\t",datos_voto[1,j]," ",datos_escanos[1,j],"\n"))
    }
  }
  cat("=INFO=\n")
  cat(paste("Elecciones al Congreso, A\u00F1o:",Ano,"Mes:",Mes,"\n"))
  cat("=ENDE=")

  sink()
  message(paste0("Fichero: '",camino,"Congreso_",Ano,"_",Mes,".bazi' generado con \u00E9xito"))
}

#Bazi(Ano = 2019,"04","D:/")



