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
  message("Hoja excel llamada 'Bochsler.xlsx' creada en la ruta solicitada")
}

#Bochsler(2016,"06","D:/","D:/")


### Injusticia Matemática provincias ###
#'@title utilidades. Calculo de Injusticia Matematica, desagregada
#'
#'@section utilidades
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
#'Igualmente, si se tiene otro tipo de elección, se puede utilizar el parámetro Auto=FALSE,
#'y se proporcionarán dos data.frame con las características que se indicarán más adelante
#'y se esta manera se calcularán las injusticias matemáticas para cada región que se indique
#'en esos data.frame
#'
#'@param Ano ( obigatorio si Auto=TRUE) Es el valor del año de los datos que se quieren
#'descagar. Este dato puede ser numérico o de tipo carácter ( mejor numérico)
#'@param Mes ( obigatorio si Auto=TRUE) Es el mes en el que se ha realizado la elección. Obligatoriamente tiene que ser
#'de tipo carácter y a dos dígitos, es decir "04" es válido pero "4" no es válido
#'@param Ruta ( obigatorio si Auto=TRUE) Se debe indicar una ruta del disco duro local donde se descarga el
#'fichero del Ministerior del Interior. Una vez finalizada la descarga de forma automática
#'el fichero descargado se borra.
#'@param Auto Su valor por defecto es TRUE e indica que el procedimiento es automático, con
#'otro valor se ejecutará el proceso no automático
#'@param d_votos (Obligatorio si Auto != TRUE). Es un data.frame con la primera columna que
#'contiene la deniminación de la región, y después una columna para cada partido que se
#'quiera evaluar, y que contendrán los votos obtenidos en cada región contemplada.
#'@param d_escanos (Obligatorio si Auto != TRUE).Es un data.frame con la primera columna que
#'contiene la deniminación de la región, y después una columna para cada partido que se
#'quiera evaluar, y que contendrán los escaños obtenidos en cada región contemplada.
#'
#'@return El resultado es una lista con 53 elementos ( para proceddimiento automático),
#'de manera que cada elemento es una matriz que contiene los datos de las injusticia matemática calculada.
#' Los 'names' de esta lista son las de provincia españolas, o bien la expresión 'Total' si son los datos
#'de toda España.
#'Cuando el procedimiento no se automático, también se obtiene una lista conteniendo un
#'número de elementos igual al número de regiones facilitadas más uno, ya que hay un elemento
#'denominado 'Total' que hace referencia al conjunto de territorios facilitados.
#'
#'@examples
#'f<-InjusticiaM_desagregada(2019,"04","D:/") # Modo agregado
#'
#'da1 <- data.frame( # Contiene los votos
#'    Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
#'      PSOE=c(400,300,0,50,25),
#'      PP=c(300,200,10,150,2),
#'      Cs=c(400,0,3,300,45),
#'      Uno=c(465,23,341,263,0))
#'
#'da2 <- data.frame( #contiene los escaños
#'      Reg=c("Alava","Albacete","Madrid","Barcelona","Valladolid"),
#'      PSOE=c(4,3,0,0,0),
#'      PP=c(2,3,0,1,0),
#'      Cs=c(4,0,0,2,1),
#'       Uno=c(3,0,3,2,0))
#'
#'f2<- InjusticiaM_desagregada(Auto=FALSE,d_votos=da1,d_escanos = da2) #No agregegado
#'@export
InjusticiaM_desagregada<-function(Ano,Mes,Ruta,Auto=TRUE,d_votos,d_escanos){
  #Primero compruebo si Auto es TRUE
  if(Auto == TRUE){
    if( class(Mes) != "character") stop( " El mes debe ser de tipo car\u00E1cter")
    if(nchar(Mes) != 2) stop("El mes debe contener dos caracteres")
    if(is.null(Ruta)) stop("Se debe facilitar alg\u00FAn valor para 'Ruta'")
    datos_MIR<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta,Borrar=TRUE))
    # Me quedo con los datos de cada provincia
    datos_MIR<-datos_MIR[1:52,]
    # Saco los nombre de las provincias
    Cprov<-datos_MIR[,3]
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
  } # Fin de AUTO == TRUE
  else{
    if(missing(d_votos)) stop("Debe facilitarse un data.frame con los datos de votos")
    if(missing(d_escanos)) stop("Debe facilitarse un data.frame con los datos de esca\u00F1s")
    if(class(d_votos) != "data.frame") stop("El par\u00E1metro 'd_votos' debe ser un data.frame")
    if(class(d_escanos) != "data.frame") stop("El par\u00E1metro 'd_escanos' debe ser un data.frame")
    # Compruebo que el numero de columnas de ambos ficheros es el mismo
    if(ncol(d_votos) != ncol(d_escanos)) stop("Deben coincider el n\u00FA de columnas de los
                                              dos data.frame")
    # compruebo que el numero de filas es el mismo
    if(nrow(d_votos) != nrow(d_escanos)) stop("Deben coincider el n\u00FA de filas de los
                                              dos data.frame")


    #Comparo nombre columnas es el mismo en los dos data.frame
    a <- colnames(d_votos) == colnames(d_escanos)

    if(sum(a) != ncol(d_votos)) stop("Los nombres de las columnas de los dos data.frame
                                     deden ser iguales")
    #Comparo que los nombres de las regiones son los mismo
    b <- as.character(d_votos[,1]) == as.character(d_escanos[,1])
    if(sum(b) != nrow(d_votos)) stop("Los nombres de las regiones ('primera columa') deben
                                     ser iguales en los dos data.frame")

    #Hechas las comprobaciones comienzo el cálculo

    #Saco los votos totales
    T_votos <- colSums(d_votos[,-c(1)])
    #Añado la fila de los totales
    d_votos[,1] <- as.character(d_votos[,1])
    d_votos <- rbind(d_votos,c("Totales",T_votos))
    # hago lo mismo para los escaños
    T_escanos <- colSums(d_escanos[,-c(1)])
    #Añado la fila de los totales
    d_escanos[,1] <- as.character(d_escanos[,1])
    d_escanos <- rbind(d_escanos,c("Totales",T_escanos))


    #Creo una lista donde almaceno los resultados.
    res<-list()
    # Ahora calculo las injuticias matemáticas
    for( i in 1:nrow(d_escanos)){
      # Saco vector logico si el partido ha tenido votos o no en esa region
      chek<-d_votos[i,-c(1)]>0
      # Saco los nombres de los partidos con votos
      parti <- colnames(d_votos)[-c(1)][chek]
      #Saco los votos
      voto <- d_votos[i,-c(1)][chek]
      #Saco los escanos
      esc <- d_escanos[i,-c(1)][chek]
      #browser()
      #Genero el data.frame para pasarlo a la función InjusticiaM()
      dat_in2<-data.frame(
        par=parti,
        vot=as.integer(voto),
        sea=as.integer(esc), stringsAsFactors = F
      )

      res[[i]]<-InjusticiaM(dat_in2)*10000 # multiplico por esta cantidad para no sacar cifras muy bajas

    } #fin del for
    names(res) <- d_votos[,1]
    return(res)
  }# Fin del else
}

# InjusticiaM_desagregada<-function(Ano,Mes,Ruta){
#   if( class(Mes) != "character") stop( " El mes debe ser de tipo car\u00E1cter")
#   if(nchar(Mes) != 2) stop("El mes debe contener dos caracteres")
#   if(is.null(Ruta)) stop("Se debe facilitar alg\u00FAn valor para 'Ruta'")
#   datos_MIR<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta,Borrar=T))
#   # Me quedo con los datos de cada provincia
#   datos_MIR<-datos_MIR[1:52,]
#   # Saco los nombre de las provincias
#   Cprov<-datos_MIR[,2]
#   # Añado en la ultima posición el epigrafe "Total"
#   Cprov[53,]<-"Total"
#   #me quedo con los votos
#   # Mediante expresión regular selecciono las columnas que tienen el número de votos
#   columnas_Votos<-grep("V_",colnames(datos_MIR))
#   dat_votos<- as.data.frame(datos_MIR[,columnas_Votos])
#   # paso los datos a numéricos
#   dat_votos<-as.data.frame(apply(dat_votos,2,as.integer))
#   #calculo los totales
#   totales_voto<-colSums(dat_votos)
#   #Añado ahora los votos totales
#   dat_votos[53,]<-totales_voto
#   #Hago lo mismo que antes pero con los escaños
#   columnas_escanos<-grep("D_",colnames(datos_MIR))
#   dat_escanos<-as.data.frame(datos_MIR[,columnas_escanos])
#   # paso los datos a numéricos
#   dat_escanos<-as.data.frame(apply(dat_escanos,2,as.integer))
#   #calculo los totales
#   totales_escanos<-colSums(dat_escanos)
#   #Añado ahora los votos totales
#   dat_escanos[53,]<-totales_escanos
#
#   #Creo una lista donde almaceno los resultados.
#   res<-list()
#   # lo calculo para cada provincia
#   for(i in 1:52){
#     # Saco vector logico si el partido ha tenido votos o no en esa provincia
#     chek<-dat_votos[i,]>0
#     dat_votos_prov<-dat_votos[i,chek]
#     dat_escanos_prov<-dat_escanos[i,chek]
#     dat_in2<-data.frame(
#       par=substr(colnames(dat_votos)[chek],3,nchar(colnames(dat_votos))[chek]),
#       vot=as.numeric(dat_votos_prov),
#       sea=as.numeric(dat_escanos_prov), stringsAsFactors = F
#     )
#
#     res[[i]]<-InjusticiaM(dat_in2)*10000 # multiplico por esta cantidad para no sacar cifras muy bajas
#   }
#   #Ahora lo hago para el total nacional
#   dat_in<-data.frame(
#     par=substr(colnames(dat_votos),3,nchar(colnames(dat_votos))),
#     vot=as.numeric(dat_votos[53,]),
#     sea=as.numeric(dat_escanos[53,]),stringsAsFactors = F
#   )
#
#   res[[53]]<-InjusticiaM(dat_in)*10000
#   #Asigno los nombres de las provincias y el total
#   names(res)<-as.data.frame(Cprov)[,1]
#   return(res)
# }

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
#' Esta función se ha generado para que pueda ser utilizada de forma automática o manual, es decir,
#' trabajará autónomamente si después de darle los parámetros correspondientes se conecta a las
#' bases de datos del Ministerio del Interior español, descarga los resultados y genera el
#' fichero con extensión «.bazi» correspondiente. El formato manual, está pensado para darle
#' los datos adecuados y que la función se encargue de transformarlos al formato que BAZI
#' entiende para poder generar los resultados para los que está programado.
#' Veamos a continuación cómo proceder en cada caso.
#'
#' En primer lugar se debe matizar que el parámetro «camino» es preciso facilitarlo tanto con el
#' procedimiento manual como el automático, y con él lo que se indica es el camino que debe seguir
#' para almacenar el fichero con extensión «.bazi» que se genera. También hay que tener en cuenta
#' que se debe facilitar una ruta con permiso de escritura para R.
#'
#' El procedimiento manual o automática se indica con el parámetro «Auto» de la función Bazi(),
#' de tal manera que si vale «TRUE» ( valor por defecto ), se ejecutará el procedimiento automático
#' y en otro caso el manual.
#'
#'@param Ano . Sólo para procedimiento automático.Es el valor del año de los datos que se quieren descagar. Este dato puede ser
#'numérico o de tipo carácter ( mejor numérico)
#'@param Mes . Sólo para procedimiento automático. Es el mes en el que se ha realizado la elección. Obligatoriamente tiene que ser
#'de tipo carácter y a dos dígitos, es decir "04" es válido pero "4" no es válido
#'@param camino . Para procedimiento manual y automático. Se debe indicar una ruta del disco duro local donde se descarga el
#'fichero del Ministerior del Interior ( finalizado el proceso se borra de forma automática),
#'y done se escribirá el fichero con extensión '.bazi' listo para ser usado por BAZI. Tener en cuenta que esta ruta debe indicar
#'un lugar con permiso de escritura por parte de R.
#'@param cota . Sólo procedimiento automático.Será la barrera eletoral que se quiera utilizar. En el caso del Conreso de
#'los Diputados en España es del 3 por ciento (partidos con menos del 3 por ciento de
#'los votos válidos o entran en el reparto), y es el valor que se le ha asignado por defecto.
#'@param Auto . Procedimiento manual y automático. Si es igual a TRUE se ejecuta el procedimiento
#'automático, en caso contrario será el procedimiento manual el que se lleve a cabo.
#'@param votes . Sólo procedimiento manual. Debe ser un objeto de tipo dataframe con la siguiente estructura
#'por columnas: La primera columna debe contener la denominación de las circunscripciones electorales a tener en cuenta.
#' Y después habrá una columna por cada partido político que se quiera tener en cuenta,
#' de tal manera que una columna estará identificada por la sigla del partido correspondiente,
#' y contendrá los votos de esa formación en cada una de las circunscripciones que se presente.
#' Donde no se presente, o no llegue a superar la barrera electoral que se quiera tener en cuenta,
#' se añadirá un valor de cero, de tal forma que la función Bazi() tiene en cuenta esta circunstancia
#' para no contabilizar estos casos en la generación del fichero final ( el fichero que tiene la extensión ".bazi").
#'@param seats . Sólo procedimiento manual.  Es un dataframe con dos columnas. En la primera se indican las denominaciones de las circunscripciones
#'electorales ( que deben tener la misma denominación y estar en el mismo orden que las que se han indicado en
#'el dataframe "votes" ) y en la segunda columna, se indicarán los diputados a elegir en cada circunscripción electoral.
#'@param Titulo   . Sólo procedimiento manual. Debe ser una cadena de caracteres que son las que encabezan el fichero con extensión ".bazi" que se genera.
#'
#'@return El resultado es un fichero ASCII plano con una estructura de información propia
#'para ser leido por BAZI y que tiene la siguiente denominación genérica 'Congreso_AAAA_MM.bazi',
#'donde AAAA es el año del proceso electoral y MM el mes a dos dígitos. En el caso de utilizar
#'un procedimiento manual el fichero tendrá denominación "Congreso.bazi".
#'
#'
#' @examples
#' Bazi(Ano = 2019,"04","D:/")
#'
#' # Ejemplo manual
#'
#'vo <- data.frame(
#'  circu=c("c1","c2","c3","c4","c5","c6","c7","c8","c9"),
#'  p1=c(200,300,0,0,250,360,145,0,0),
#'  p2=c(0,0,450,467,576,346,234,0,437),
#'  p3=c(243,567,0,0,345,634,456,634,0),
#'  p4=c(0,367,384,134,267,0,0,364,146),
#'  p5=c(345,123,234,254,123,543,342,45,0),
#'  p6=c(23,45,234,0,0,354,254,56,123)
#')
#'
#'se <- data.frame(
#'  circu=c("c1","c2","c3","c4","c5","c6","c7","c8","c9"),
#'  dip=c(3,4,5,3,2,6,2,4,3)
#')
#'
#'Bazi(camino="D:/",Auto=FALSE, votes=vo,seats=se, Titulo="Fichero_prueba")
#'
#'
#'@export
#'@md
Bazi<-function(Ano=0,Mes,camino="",cota=3,Auto=TRUE,votes,seats,Titulo=""){
  if(camino == "") stop("Se debe indicar un camino donde dejar el fichero")
  if(Auto){
    # Comprobaciones
    if(Ano == 0) stop("Hay que facilitar un a\u00F1o en formato de 4 d\u00EDgitos")
    if(nchar(as.character(Ano)) != 4) stop("El a\u00F1o debe tener 4 d\u00EDgitos")
    if(class(Mes) != "character") stop("El mes debe ser de tipo character")
    if(nchar(Mes) != 2 ) stop("El mes debe tener dos d\u00EDgitos")


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
    datos_MIR_Votos<-datos_MIR_Votos[,order(totales,decreasing = T)]

    #Hago lo mismo que antes pero con los escaños
    columnas_escanos<-grep("^(D_)",colnames(datos_Mir))
    datos_MIR_escanos<-as.data.frame(datos_Mir[,columnas_escanos])
    datos_MIR_escanos <- as.data.frame(apply(datos_MIR_escanos,2,as.integer))
    #Ordenos las columnas de mas a menos votos ##### Lo meto nuevo
    datos_MIR_escanos<-datos_MIR_escanos[,order(totales,decreasing = T)]

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
    message(paste0("Fichero: ",camino,"Congreso_",Ano,"_",Mes,".bazi' generado con \u00E9xito"))
  } else{ # Fin si procedimiento automatico. Comienzo procedimiento manual
    # Comprobaciones iniciales
    if(nchar(Titulo) == 0) stop("Debes darme un t\u00EDtulo para este procedimiento manual")
    if(nchar(camino) == 0) stop("Debes indicar un camino para guardar el fichero")
    if(class(votes) != "data.frame") stop("El fichero de votos debe ser un data.frame")
    if(class(seats) != "data.frame") stop("El fichero conteniendo los esca\u00F1os debe ser un data.frame")
    if(nrow(votes) != nrow(seats)) stop("El n\u00FAmero de filas del fichero
        conteniendo los votos no coincide con las filas del fichero de esca\u00F1os")
    if( sum(votes[,1] == seats[,1]) != nrow(votes) ) stop("Las circunscripciones
    electorales del ficehro de votos y de esca\u00F1os no coinciden o no
    est\u00E1n en el mismo orden")
    #abro la conexion para generar el fichero correspondiente
    sink(paste0(camino,"Congreso",".bazi"))
    cat(paste("=TITEL=",Titulo,"\n"))
    cat("=METHODE= DivAbr\n")
    cat("=AUSGABE= vert./horiz., Div/Quote, kodiert\n")
    cat("=EINGABE= Candidatura, Votos, ---\n")
    cat("=DISTRIKTOPTION= separat\n")

    # separo los nombres de las circunscripciones de los votos
    circuns <- votes[,1]
    votes <- votes[,2:ncol(votes)]

    #Voy generando el fichero con extensión .bazi
    # itero para cada circunscripción
    #browser()
    for(i in 1:nrow(votes)){
      cat(paste("=DISTRIKT=",circuns[i],"\n"))
      cat(paste("=MANDATE=",seats[i,2],"\n"))
      cat("=DATEN=\n")
      # Quito las columnas con valores de votos a cero
      ceros <- votes[i,]>0
      votes2 <- votes[i,ceros]
      # Ordeno las columnas de más a menos votos
      votes2 <- votes2[order(votes2,decreasing = T)]

      for(j in 1:ncol(votes2) ){
        cat(paste0("\"",colnames(votes2)[j],"\"","\t",votes2[j],"\n"))
      }
    }
    cat("=INFO=\n")
    cat(paste("Elecciones :",Titulo ,"\n"))
    cat("=ENDE=")

    sink()
    message(paste0("Fichero: ",camino,"Congreso",".bazi' generado con \u00E9xito"))

  } # Fin del else

} # Fin de la función
###################################################
# Bazi<-function(Ano,Mes,camino,cota=3){
#   sink(paste0(camino,"Congreso_",Ano,"_",Mes,".bazi"))
#
#   cat(paste("=TITEL=","Elecciones al Congreso, A\u00F1o:",Ano,"Mes:",Mes,"\n"))
#   cat("=METHODE= DivAbr\n")
#   cat("=AUSGABE= vert./horiz., Div/Quote, kodiert\n")
#   cat("=EINGABE= Candidatura, Votos, ---\n")
#   cat("=DISTRIKTOPTION= separat\n")
#   datos_Mir<-suppressMessages(Agregado_Prov_MIR(Ano,Mes,"Congreso",Ruta=camino))
#   datos_Mir<-datos_Mir[0:52,]
#
#
#   cprov<-apply(datos_Mir[,2],2,as.integer)
#   datos_Mir<-datos_Mir[order(cprov),]
#   Provincias<-apply(datos_Mir[,3],2,as.character)
#
#   # Tengo que sacar la columna de votos válidos para luego aplicar el 3 por ciento
#   if(as.integer(Ano) %in% c(1977,1979,1982)){
#     column = 8
#   } else if(as.integer(Ano) %in% c(1986,1989,1993,1996,2000,2004,2008)){
#     column=12
#   } else {
#     column=13
#   }
#
#   valid <- as.data.frame(datos_Mir[,column])
#   valid <- as.data.frame(apply(valid,2,as.integer))
#   # Mediante expresión regular selecciono las columnas que tienen el número de votos
#   columnas_Votos<-grep("^(V_)",colnames(datos_Mir))
#   # Me quedo solo con las columna de votos
#   datos_MIR_Votos<-as.data.frame(datos_Mir[,columnas_Votos])
#
#   datos_MIR_Votos <- as.data.frame(apply(datos_MIR_Votos,2,as.integer))
#   #calculo los totales
#   totales<-colSums(datos_MIR_Votos)
#   #Ordenos las columnas de mas a menos votos
#   datos_MIR_Votos<-datos_MIR_Votos[,order(totales,decreasing = T),]
#
#   #Hago lo mismo que antes pero con los escaños
#   columnas_escanos<-grep("^(D_)",colnames(datos_Mir))
#   datos_MIR_escanos<-as.data.frame(datos_Mir[,columnas_escanos])
#   datos_MIR_escanos <- as.data.frame(apply(datos_MIR_escanos,2,as.integer))
#
#   for(i in 1:nrow(datos_Mir)){
#     Dipu <-sum(datos_MIR_escanos[i,])#N. Diputados
#     #Me quedo con los partidos que tienen algun voto
#     Si_Voto<-datos_MIR_Votos[i,]>0
#     datos_voto<-datos_MIR_Votos[i,Si_Voto]
#     datos_escanos<-datos_MIR_escanos[i,Si_Voto]
#     #Aplico el minimo voto para oder conseguir escaño
#     tope<-valid[i,1]*(cota/100)
#     corte<-datos_voto[1,]>tope
#     #Me quedo con los partidos con votos encima topa
#     datos_voto<-datos_voto[,corte]
#     datos_escanos<-datos_escanos[,corte]
#     cat(paste("=DISTRIKT=",Provincias[i,1],"\n"))
#     cat(paste("=MANDATE=",Dipu,"\n"))
#     cat("=DATEN=\n")
#     a<-colnames(datos_voto)
#     siglas<- substr(a,3,nchar(a))
#     #Recorro las columnas
#     #browser()
#     for(j in 1:ncol(datos_escanos)){
#       cat(paste0("\"",siglas[j],"\"","\t",datos_voto[1,j]," ",datos_escanos[1,j],"\n"))
#     }
#   }
#   cat("=INFO=\n")
#   cat(paste("Elecciones al Congreso, A\u00F1o:",Ano,"Mes:",Mes,"\n"))
#   cat("=ENDE=")
#
#   sink()
#   message(paste0("Fichero: '",camino,"Congreso_",Ano,"_",Mes,".bazi' generado con \u00E9xito"))
# }



