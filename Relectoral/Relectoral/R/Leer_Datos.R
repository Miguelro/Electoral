### Aquí van las funciones a usar para leer datos del MIR (Ministerio Interior)
##if (!require("readxl")) install.packages("readxl");require("readxl")

### Lectura datos agregados nivel Municipal ####

#'@title Download. Datos Agregado a nivel Municipal obtenido del MIR (Ministerior del Interior)
#'
#'
#'@description Esta función sirve para descargar los datos en excel a nivel municipal y
#'carga un data frame con esa información. ¡¡¡Observación!!!: Los campos devueltos son todos
#'de tipo character, por lo que si es necesario hacer operaciones, habrá que convertir
#'los campos necsarios a numéricos
#'
#'@param Ano El año de la eleccion cuatro dígito (YYYY). Puede ser numérico o texto
#'@param Mes El mes de la elección. Tiene que se como texto. (Por ejemplo "06", correspondiente a junio)
#'@param Tipo El tipo de fichero a descargar: "Congreso" o "Europeas"
#'@param Ruta Es la ruta donde se descargarán los ficheros tipo zip del MIR
#'@param Borrar Es de tipo Boolean e indica si se borran o no los ficheros después de obtener el data.frame
#'
#'@return Objeto de tipo tbl_df con los datos del voto a nivel municipal
#'
#'@import readxl
#'
#'@examples
#'c<-Agregado_Mun_MIR(1989,"06",Tipo = "Europeas","F:/")
#'
#'
#'@export
Agregado_Mun_MIR<-function(Ano,Mes,Tipo,Ruta,Borrar=T){
  temp<-paste0(Ruta,"temp.zip")
  url1 <- "http://www.infoelectoral.mir.es/infoelectoral/docxl/"
  if(Tipo=="Congreso"){
    url <- paste0(url1,"02_",as.character(Ano),Mes,"_1.zip")
    fic<-paste0(Ruta,"02_",as.character(Ano),Mes,"_1.xlsx")
  }else if(Tipo=="Europeas"){
    url <- paste0(url1,"07_",as.character(Ano),Mes,"_1.zip")
    fic<-paste0(Ruta,"07_",as.character(Ano),Mes,"_1.xlsx")
  }else{
    stop("el Tipo no es correcto")
  }

  download.file(url,temp)
  unzip(zipfile=temp, exdir=Ruta)
  dat <- read_xlsx(fic,col_names = T,skip = 5)
  if(Borrar){
    unlink(temp)
    unlink(fic)
  }
  return(dat)
}
# Suprimo los mensajes de esta función
#suppressMessages(Agregado_Mun_MIR())

#c<-Agregado_Mun_MIR(1989,"06",Tipo = "Europeas","F:/",Borrar = T)

#### Lectura datos agregados nivel Provincial #######

#'@title Download. Datos Agregado a nivel Provincial obtenidos del MIR (Ministerio Interior)
#'
#'
#'@description Esta función sirve para descargar los datos en excel a nivel Provincial y
#'carga un data frame con esa información.¡¡¡Observación!!!: Los campos devueltos son todos
#'de tipo character, por lo que si es necesario hacer operaciones, habrá que convertir
#'los campos necsarios a numéricos
#'
#'@param Ano El año de la eleccion cuatro dígito (YYYY). Puede ser numérico o texto
#'@param Mes El mes de la elección. Tiene que ser como texto. (Por ejemplo "06", correspondiente a junio)
#'@param Tipo El tipo de fichero a descargar: "Congreso" o "Europeas"
#'@param Ruta Es la ruta donde se descargarán los ficheros tipo zip del MIR
#'@param Borrar Es de tipo Boolean e indica si se borran o no los ficheros después de obtener el data.frame
#'
#'@return Objeto de tipo tbl_df con los datos del voto a nivel Provincial
#'
#'
#'@examples
#'
#'c<-Agregado_Prov_MIR(2019,"05",Tipo = "Europeas","G:/")
#'
#'
#'@export

Agregado_Prov_MIR<-function(Ano,Mes,Tipo,Ruta,Borrar=T){
  temp<-paste0(Ruta,"temp.zip")
  url1 <- "http://www.infoelectoral.mir.es/infoelectoral/docxl/"
  if(Tipo=="Congreso"){
    url <- paste0(url1,"PROV_02_",as.character(Ano),Mes,"_1.zip")
    fic<-paste0(Ruta,"PROV_02_",as.character(Ano),Mes,"_1.xlsx")
  }else if(Tipo=="Europeas"){
    url <- paste0(url1,"PROV_07_",as.character(Ano),Mes,"_1.zip")
    fic<-paste0(Ruta,"PROV_07_",as.character(Ano),Mes,"_1.xlsx")
  }else{
    stop("el Tipo no es correcto")
  }

  download.file(url,temp)
  unzip(zipfile=temp, exdir=Ruta)
  #En el año 2015 los datos comienzan una linea más abajo.POr eso incluyo esto
  #Generaba un error
  if(as.integer(Ano)==2015){
    dat <- read_xlsx(fic,col_names = F,skip = 5)
  } else{
    dat <- read_xlsx(fic,col_names = F,skip = 4)
  }

  if(Tipo %in% c("Congreso","Europeas" )){
    n<-ncol(dat)
    for(i in 1:n){
      if(trimws(dat[2,i])=='Votos'){
        dat[2,i]=paste0("V_",dat[1,i])
      }else if(trimws(dat[2,i])=='Diputados'){
        dat[2,i]=paste0("D_",dat[1,i-1])
      }
    }
    # Pongo nombres a las columnas
    colnames(dat)<-dat[2,]
    #Quito las dos primeras filas
    dat<-dat[-c(1,2),]
  }

  if(Borrar){
    unlink(temp)
    unlink(fic)
  }
  return(dat)
}
# Suprimo los mensajes de esta función
#suppressMessages(Agregado_Prov_MIR())
#c<-Agregado_Prov_MIR(2019,"05",Tipo = "Europeas","G:/")


#### Lectura datos desagregados #####

#'@title Download. Datos desagregados a nivel mesa electoral obtenidos del MIR (Ministerio Interior)
#'
#'
#'@description Esta función sirve para descargar los datos en excel a nivel de mesa electoral y
#'despues carga un data frame con esa información.¡¡¡Observación!!!: Los campos devueltos son todos
#'de tipo character, por lo que si es necesario hacer operaciones, habrá que convertir
#'los campos necsarios a numéricos
#'
#'@param Ano El año de la eleccion cuatro dígito (YYYY). Puede ser numérico o texto
#'@param Mes El mes de la elección. Tiene que ser como texto. (Por ejemplo "06", correspondiente a junio)
#'@param Tipo El tipo de fichero a descargar: "Cogreso" o "Europeas"
#'@param Ruta Es la ruta donde se descargarán los ficheros tipo zip del MIR
#'@param Borrar Es de tipo Boolean e indica si se borran o no los ficheros después de obtener el data.frame
#'
#'@return objeto de tipo tbl_df con los datos del voto a nivel de mesa electoral
#'
#'
#'@examples
#'
#'c2<-Desagregados_Mesa_Mir(2019,"04",Tipo = "Congreso", Ruta = "G:/",Borrar = T)
#'
#'
#'@export
Desagregados_Mesa_Mir<-function(Ano,Mes,Tipo,Ruta,Borrar=T ){
  temp<-paste0(Ruta,"temp.zip")
  url1 <- "http://www.infoelectoral.mir.es/infoelectoral/docxl/apliextr/"
  if(Tipo=="Congreso"){
    url <- paste0(url1,"02",as.character(Ano),Mes,"_MESA.zip")
    #fic<-paste0(Ruta,"PROV_02_",as.character(Ano),Mes,"_1.xlsx")
  }else if(Tipo=="Europeas"){
    url <- paste0(url1,"07",as.character(Ano),Mes,"_MESA.zip")
    #fic<-paste0(Ruta,"PROV_07_",as.character(Ano),Mes,"_1.xlsx")
  }else{
    stop("el Tipo no es correcto")
  }

  download.file(url,temp)
  if(Tipo == "Congreso"){
    ti<-"02"
  }else if(Tipo == "Europeas"){
    ti<-"07"
  }
  print("Generando fichero dat01")
  dat01<-read.fwf(unz(temp,paste0("01",ti,substr(Ano,3,4),Mes,".DAT")),
                widths = c(2,4,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                col.names = c(
                  "Tipo","Ano","Mes","Nvuelta","F01","F02","F03",
                  "F04","F05","F06","F07","F08","F09","F10",
                  "F11","F12","F0510","F0610","F0710","F0810"
                ))
  print("Generando fichero dat02")

  dat02<-read.fwf(unz(temp,paste0("02",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,1,2,2,2,4,5,5,5,5),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Tambito",
                    "Ambito","Dia","Mes2","Ano2","HoraAp",
                    "HoraC","HoraAvan1","HoraAvan2"
                  ))
  print("Generando fichero dat03")

  dat03<-read.fwf(unz(temp,paste0("03",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,6,50,150,6,6,6),
                  col.names = c("Tipo","Ano","Mes","CodCan",
                                "Sigla","Denominacion","Cod_Prov",
                                "Cod_CCAA","Cod_Nacion"
                  ))
  print("Generando fichero dat04")

  dat04<-read.fwf(unz(temp,paste0("04",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,2,1,3,6,3,1,25,25,25,1,2,2,4,10,1),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Cprov","Cdist",
                    "Cmun","CodCan","Norden","Tipo","Nombre","Ape1","Ape2",
                    "Sexo","NaciDia","NaciMes","NaciAno","Dni","Elegido"
                  ))
  print("Generando fichero dat05")

  dat05<-read.fwf(unz(temp,paste0("05",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,2,2,3,2,100,1,3,3,3,8,5,8,
                             8,8,8,8,8,8,8,8,3,8,8,1),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Cccaa","Cprov","Cmun",
                    "Cdist","Cnombre","Cdist2","Cpj","Cdipu","Ccomarca",
                    "Poblacion","Nmesas","CensoINE","CensoEscrutinio",
                    "CERE","TvCERE","Vavan1","Vavan2","Vblanco","Vnulos",
                    "Vcandidaturas","Nescanos","Vsi","Vno","Doficiales"
                  ))
  print("Generando fichero dat06")

  dat06<-read.fwf(unz(temp,paste0("06",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,2,3,2,6,8,3),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Cprov","Cmun",
                    "Cdist","CodCan","VotosCand","Ncandi"
                  ))
  print("Generando fichero dat07")

  dat07<-read.fwf(unz(temp,paste0("07",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,2,2,1,50,8,5,8,8,8,8,8,8,8,8,8,
                             6,8,8,1),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Cccaa","Cprov",
                    "Cdist","NomAmbito",
                    "Poblacion","Nmesas","CensoINE","CensoEscrutinio",
                    "CERE","TvCERE","Vavan1","Vavan2","Vblanco","Vnulos",
                    "Vcandidaturas","Nescanos","Vsi","Vno","Doficiales"
                  ))
  print("Generando fichero dat08")

  dat08<-read.fwf(unz(temp,paste0("08",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,2,2,1,6,8,5),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Cccaa",
                    "Cprov","Cdist","CodCan","Votos","Ncand"
                  ))
  print("Generando fichero dat09")

  dat09<-read.fwf(unz(temp,paste0("09",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,2,2,3,2,4,1,7,7,7,7,7,7,7,7,
                             7,7,7,1),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Cccaa",
                    "Cprov","Cmun","Cdist","Csecc","Cmesa","Tcenso",
                    "CERA","CERE","VotantesCERE","Vavan1","Vavan2",
                    "Vblanco","Vnulos","Vcandidaturas","Vsi","Vno","Oficiales"
                  ))

  print("Generando fichero dat10 (tarda un poco)")

  dat10<-read.fwf(unz(temp,paste0("10",ti,substr(Ano,3,4),Mes,".DAT")),
                  widths = c(2,4,2,1,2,2,3,2,4,1,6,7),
                  col.names = c(
                    "Tipo","Ano","Mes","Nvuelta","Cccaa",
                    "Cprov","Cmun","Cdist","Csecc","Cmesa","CodCan","Tvotos"
                  ))

  final<-list("dat01"=dat01,"dat02"=dat02,"dat03"=dat03,"dat04"=dat04,
              "dat05"=dat05,"dat06"=dat06,"dat07"=dat07,"dat08"=dat08,
              "dat09"=dat09,"dat10"=dat10)
  return(final)
  if(Borrar){
    # Borro ficheros
    unlink(temp)
  }
}
# Suprimo los mensajes de esta función
#suppressMessages(Desagregados_Mesa_Mir())

#c2<-Desagregados_Mesa_Mir(2019,"04",Tipo = "Congreso", Ruta = "G:/",Borrar = T)

