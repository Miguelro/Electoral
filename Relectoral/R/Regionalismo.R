###########################################
# Creo la función para el paquete Relectoral
########################################3
#if (!require("dplyr")) install.packages("dplyr"); require("dplyr")
# Función para generar el csv. No ofrecida en Relectoral
generar_CSV <- function(data,fichero){
  #Data es un dataframe con primera columna las siglas de los partidos
  # Fichero es el nombre y patch del fichero que se quiere crear
  ##### Genero las restricciones de acceso correspondientes

  if(file.access(names = fichero,mode = 2) == 0) stop("El fichero csv que se quiere generar, no se puede crear
                                    porque no hay permisos de escritura o bien ya existe")
  colnames(data)[1] <- c("PARTIDOS")
  a <- data.frame(PARTIDOS="PARTIDOS")
  data$PARTIDOS <- substring(data$PARTIDOS,3)
  data <- rbind(a,data)

  data$PARTIDOS <- paste0(data$PARTIDOS,",")
  write.table(data[,c(1)],fichero,quote = F,row.names=F,col.names = F)
  message(paste0("\u00A1 \u00A1 \u00A1 Generado el fichero de tipo csv: ",fichero,"\u0021 \u0021 \u0021 "))
}

generar_VRta <- function(datos,panes){
  # datos es un dataframes. primera columna nivel medio (CCAA), segunda columna niveles
  #inferiores (provincias). Despues una columna para cada partido, con nombre el logo del
  #partido y conteniendo los votos a ese partido en cada nivel inferior
  #panes es el camino de fichero de tipo csv contenendo información sobre panes y no panes



  panes <- read.csv(panes,header=T,stringsAsFactors = F)
  #### Genero las restricciones correspondientes
  if(ncol(panes) !=2 ) stop("El fichero conteniendo el regionalismo ( PANES ) no tiene dos columnas")
  colnames(panes) <- c("PARTIDOS","PANE")
  panes$PANE <- as.logical(panes$PANE)
  suma_total <- rowSums(datos[,3:ncol(datos)])
  a <- datos[,3:ncol(datos)]
  suma_panes <- rowSums(a[,panes$PANE])  #Sumo quedandome solo con las columnas que son PANE
  #Añadimos al resultado final el nombre de provincia
  porc_panes_prov <- data.frame(cbind(datos[,c(2)],(suma_panes/suma_total)*100))
  colnames(porc_panes_prov)<-c("C_PROv","VRta")

  # Para Comunidades Autónomas
  votos_CCAA <- datos[,-c(2)] %>% group_by_at(1) %>% summarise_all(list(sum))
  suma_total <- rowSums(votos_CCAA[,-c(1)])
  a <- votos_CCAA[,-c(1)]
  suma_panes <- rowSums(a[,panes$PANE])
  porc_panes_CCAA <- data.frame(cbind(votos_CCAA[,c(1)]),(suma_panes/suma_total)*100)
  colnames(porc_panes_CCAA) <- c("CCAA","VRta_CCAA")

  # Calculo ahora el VRtaD
  # Tengo que introducir el VRta de la C.A en cada provincia, lo hago ccon un merge
  tem <- merge(datos[,c(1,2)],porc_panes_prov,by.x='Nombre de Provincia',by.y = 'C_PROv')
  resul<-merge(tem,porc_panes_CCAA,by.x = 'Nombre de Comunidad',by.y='CCAA')
  resul$VRta <- as.numeric(as.character(resul$VRta))

  resul$VRtaD <- resul$VRta-resul$VRta_CCAA
  resul <- resul[,c(2,5)]

  #Calculamos ahora VRtaD para CCAA
  # Calculamos el apoyo panes a nivel nacional
  # Total votos por partidos
  T_vot_part <- sum(colSums(datos[,3:ncol(datos)]))
  T_vot_part_PANES <-sum(colSums(datos[,3:ncol(datos)][panes$PANE]))
  # VRtad nacional
  POR_PANES_NACIONAL <-(T_vot_part_PANES/T_vot_part)*100
  # PAra cada C.A cada porcentaje PANES le resto  POR_PANES_NACIONAL
  VRtaD_CCAA <- porc_panes_CCAA[2]-
    matrix(POR_PANES_NACIONAL,ncol = nrow(porc_panes_CCAA))
  VRtaD_CCAA <- cbind(porc_panes_CCAA[1],VRtaD_CCAA)

  resul_Total <-list(VRta_Inferior=porc_panes_prov,
                     VRta_Medio=porc_panes_CCAA,
                     VRtaD_inferior=resul,
                     VRtaD_Medio=VRtaD_CCAA)
  return(resul_Total)
}

#'@title Indice Dimension de voto. Regionalismo
#'
#'
#'@description Con esta función calculamos los índice de regionalismo descritos en el libro
#'"Análisis de datos electorales" ( Pablo Oñate y Francisco A. Ocaña, pag: 48). Se remite al lector
#'a esta publicación para comprender el significado de los mismos. En concreto se calcularán el índice
#'de voto regionalista ( VRta ), el índice de voto regionalista difereneciado ( VRtaD), y el índice del
#'regional diferenciado ( VRD).
#'
#'@import dplyr
#'
#'@param Ano es el año del proceso electoral que se quiera tratar. Debe ser un valor
#'numérico de cuatro dígitos( Si se utiliza un procedimiento manual, no hace falta
#'ningún valor a este parámetro) .
#'
#'@param Mes es el mes del proceso electoral, debe ser **una cadena con dos caracteres**
#'numéricos, asociados al mes en que se han celebrado las elecciones.( Si se utiliza
#'un procedimiento manual, no hace falta ningún valor a este parámetro) .
#'@param RutaDescarga debe ser una cadena indicativa del camino a seguir para descargar
#'el fichero proveniente del Ministerio del Interior. Posteriormente y de forma
#'automática este fichero se borra. Se hace la advertencia de que esta ruta debe
#'indicar un lugar donde se tenga permiso de lectura y escritura, ya que en caso contrario
#'no se completará el proceso.( Si se utiliza un procedimiento manual, no hace falta
#'ningún valor a este parámetro) .
#'@param Auto Puede tener los valores lógicos TRUE ó FALSE. Por defecto tiene
#'el valor TRUE, para indicar que se quiere un proceso automático. En el supuesto de
#'querer un proceso manual, se dará un valor FALSE a este parámetro.
#'@param datos Son los valores que serán procesados si se indica un procesamiento manual, es decir «Auto=FALSE».
#'Debe ser un dataframe con la siguiente estructura. La primera columna debe contener el nombre
#'de la unidad intermedia ( en el caso de España la comunidad autónoma), en la segunda
#'columna se debe incluir el nombre de las unidades inferiores ( en el caso de España, el nombre
#'de la provincia), y después se debe tener una columna por cada partido político que contenga
#'los votos obtenidos por ese partido en cada unidad inferior ( en España en cada provincia). El
#'valor del nombre para cada una de esta columnas debe ser las siglas que identifiquen a
#'cada partido político.
#'@param PANES Deber ser el camino completo del fichero de tipo csv que contiene para
#'cada partido la información sobre si dicho partido es o no de tipo regionalista.
#'1 será si el partido es regionalista o nacionalista y 0 en caso contrario.
#'Estos valores figurarán en la segunda columna de ese fichero csv.
#'@param Generate_PANES es un parámetro de tipo boolean, por defecto tienen el valor de FALSE. En
#'el caso de que sea TRUE se generará el fichero de tipo csv que se indique en el parámetro
#'"PANES". La primera línea de este fichero de texto será la siguiente: "PARTIDOS,PANE", que será
#'el nombre de las variables con las que se trabajará internamente en la función.
#'
#'En este caso el fichero de tipo csv generado contendrá una columna con las siglas de los partidos, y a continuación y de
#'forma manual se anotará 1 si el partido es PANE (partido nacionalista o regionalista) y 0 si es
#'NO PANE ( partido no nacionalista o no regionalista). Es decir con el parámetro
#'Generate_PANES = TRUE, se consigue un sistema de ayuda para obtener el fichero que se debe pasar
#'con el parámetro  "PANES". Pero este fichero csv, también se puede generar de forma totalmente
#'manual, si el usuario así lo desea.
#'
#'
#'
#'@return Devuelve una lista con todos los indicadores que se obtienen con esta función.Esta
#'lista inicial, contiene a su vez otras dos listas.
#'
#'1. VRtaD. Contiene otras tres listas, cada una conteniendo un data.frame con la siguiente
#'información: **$VRta_Provincias**: contiene los valores de indicador de VRta al nivel más
#'inferior ( en el caso de España, las provincias). **$VRta_CCAA** contiene el indicador
#'Vrta a nivel de comunidad autónoma. **VRtaD** Contiene el indicador VRtaD al nivel
#'inferior ( en el caso de España a un nivel provincial)
#'
#'2. VRD. Contiene a su vez tres listas, cada una conteniendo un data.frame con la siguiente
#'información. **$inferior_medio** Contiene el valor de VRD del nivel inferior (provincias)
#'respecto del nivel medio ( comunidades autónomas).  **$inferior_superior** Contiene el
#'valor de VRD para el nivel inferior, respecto del superior ( España). **$medio_superior**
#'Contiene información del valor de VRD del nivel medio respecto del superior.
#'
#'@examples
#'r <- Regionalismo(Ano=2019,Mes = "11",RutaDescarga = "D:/",
#'  PANES = system.file("extdata", "Regionalismo.csv", package = "Relectoral"))
#'
#'@export
#'@md

Regionalismo <- function(Ano=0,Mes="",RutaDescarga="",Auto=TRUE,datos="",PANES="",Generate_PANES=FALSE){
  # Ano es el año a cuatro dígitos. Mes es numero de mes a dos dítos.
  #RutaDescarga es donde se descarga el zip proveniente del MIR. Auto es para proceso autom. (descarga MIR) o no
  #PANES es el nombre , con el pacth,de fichero CSV que contiene información sobre PANES
  #Generate_PANES boolean para indicar si se hay que generar fichero csv con las siglas de los partidos o no
  # datos es un data.frame con una estructura similar a la que se descarga del MIR
  if(Auto==TRUE & Ano==0) stop("Como auto=TRUE, debe facilitarse un a\u00F1 a cuatro d\u00EDgitos")
  if(Auto==TRUE & ((Mes=="") || (nchar(Mes) !=2) || (class(Mes) != "character")))
    stop("Como auto=TRUE debe facilitarse el mes en formato character y a dos d\u00EDgitos")
  if(Generate_PANES==FALSE & file.exists(PANES)==FALSE) stop("No se ha indicado ning\u00FAn fichero de tipo csv
                                               o el indicado  con el par\u00E1metro no se encuentra ")

  if(Auto == TRUE){
    data <- suppressMessages(Agregado_Prov_MIR(Ano,Mes,Tipo="Congreso",RutaDescarga, Borrar=T))
    # Miro la columna que comienza por "V_"
    V_idx <-  which(substr(colnames(data),1,2)=="V_")
    V_idx <- c(1,3,V_idx) #Añado las columnas 1 y 2
    datos <- data[1:52,V_idx]  #Selecciono esas columnas y elimino las filas con el total

  } else {datos = datos}
  datos=as.data.frame(datos)
  # Para unificar doy esta denominación al fichero de entrada
  colnames(datos)[c(1,2)]<-c("Nombre de Comunidad","Nombre de Provincia")
  # Si queremos crear el fichero de tipo csv con información de PANES y NO PANES
  if(Generate_PANES == TRUE){
    # Genero restricciones para la generación del fichero CSV

    if(class(datos) !="data.frame") stop( "Para generar el fichero csv se tiene que facilitar el data.frame
              ,con la estructura adecuada, conteniendo informaci\u00F3n sobre los partidos y votos ")
    a <- as.data.frame(colnames(datos)[3:ncol(datos)])
    colnames(a) <- "PARTIDOS"
    generar_CSV(a,PANES)
    return()
  }



  #############################
  # Calculo del VRD
  ##################################



  colnames(datos)[3:ncol(datos)] <- substring(colnames(datos)[3:ncol(datos)],3)


  #Convierto la columna de votos-escaños en numericos
  datos[,3:ncol(datos)]<-apply(datos[,3:ncol(datos)],2,as.integer)
  sumrows <- rowSums(datos[,3:ncol(datos)])
  votos_porc <- (datos[,3:ncol(datos)]/sumrows)*100
  votos_porc <- cbind(datos[,c(1,2)],votos_porc)
  colnames(votos_porc)[c(1,2)] <- c("CCAA","Prov")
  votos_porc_ccaa <- votos_porc[,-c(2)] %>% group_by(CCAA) %>% summarise_all(list(mean))
  votos_porc_ccaa <- merge(votos_porc[,c(1,2)],votos_porc_ccaa, by="CCAA")

  votos_porc <- votos_porc[order(votos_porc$Prov),]
  votos_porc_ccaa <- votos_porc_ccaa[order(votos_porc_ccaa$Prov),]

  resul_parc <- abs(votos_porc[,3:ncol(datos)] - votos_porc_ccaa[,3:ncol(datos)])
  resul <- rowSums(resul_parc)*0.5
  #Añadimos al resultado final el nombre de provincia
  resul <- data.frame(cbind(votos_porc_ccaa[,c(2)],resul))
  colnames(resul)<-c("Nivel_Inferior","VRD")

  # Calcullamos VRD de nivel inferior (provincias) frente al superior (Estado)
  # Calculamos la suma por coulumna
  col_sum <-colSums(datos[,3:ncol(datos)])
  # Calculamos los porcentajes para cada partido
  col_sum_porcen <- (col_sum/sum(col_sum))*100
  # para poder hacer la resta repetimos esta fila 52 veces
  col_sum_porcen <- as.data.frame(do.call("rbind",replicate(nrow(votos_porc),col_sum_porcen,simplify=FALSE)))
  resul_prov_Total <- abs(votos_porc[,3:ncol(datos)]-col_sum_porcen)
  resul_prov_Total <- rowSums(resul_prov_Total)*0.5
  # Añado las provincias
  resul_prov_Total<-data.frame(cbind(votos_porc[,c(2)],resul_prov_Total))
  colnames(resul_prov_Total)<-c("Provinc.","VRD")

  # Calculamos lo anterior pero para el nivel intermedio, es decir CCAA frente al superior

  votos_porc_ccaa <- votos_porc[,-c(2)] %>% group_by(CCAA) %>% summarise_all(list(mean))
  # Calculamos la suma por coulumna
  col_sum <-colSums(datos[,3:ncol(datos)])
  # Calculamos los porcentajes para cada partido
  col_sum_porcen <- (col_sum/sum(col_sum))*100
  # para poder hacer la resta repetimos esta fila 52 veces
  col_sum_porcen <- as.data.frame(do.call("rbind",replicate(nrow(votos_porc_ccaa),col_sum_porcen,simplify=FALSE)))
  resul_CCAA_TOTAL <- abs(votos_porc_ccaa[,-c(1)]-col_sum_porcen)
  resul_CCAA_TOTAL <- rowSums(resul_CCAA_TOTAL)*0.5
  # Añadimos ahora el nombre de la CCAA
  resul_CCAA_TOTAL <- data.frame(cbind(votos_porc_ccaa[,c(1)],resul_CCAA_TOTAL))
  colnames(resul_CCAA_TOTAL) <- c("CCAA","VRD")




  #Devuelvo los resultados de VRD
  resulVRD <-list(inferior_medio = resul, inferior_superior = resul_prov_Total,
                  medio_superior= resul_CCAA_TOTAL)

  ##############################
  # Calculos con PANES, NOPANES
  ###############################
  res_PANES<- generar_VRta(datos,PANES)

  # Devuelvo los resultados totales
  devolver <- list(VRtaD=res_PANES,VRD=resulVRD)
  return(devolver)
}

#r <-Regionalismo(Ano=2019,Mes = "11",RutaDescarga = "D:/")
#r <- Regionalismo(Ano=2019,Mes = "11",RutaDescarga = "D:/" ,
#                 PANES = "/data_raw/Regionalismo/Regionalismo.csv")



