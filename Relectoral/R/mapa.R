#Este fichero se utilizará para hacer representaciones de mapas
#if (!require("sf")) install.packages("sf");require("sf")
#if (!require("ggplot2")) install.packages("ggplot2"); require("ggplot2")

#' @title Graficos.Representacion en mapas. choropleth map
#'
#' @description Con esta función se podrá hacer mapas de tipo "choropleth map",
#' para hacer representaciones de los datos sobre un mapa. Inicialmente el mapa
#' puede ser de España, pero se podría utilizar cualquier otro de tipo "shapefile".
#' **NOTA:** El fichero de typo shapefile que se deberá utilizar se puede bajar de la siguiente
#' dirección [https://github.com/Miguelro/Electoral/tree/master/Mapa](https://github.com/Miguelro/Electoral/tree/master/Mapa) y se colcarán todos los
#' ficheros en una carpeta, que servirá para marcar el path a indicar en el
#' parámetro camino
#'
#' @param dat es un data.frame con dos columnas. La primera contiene el código INE de
#' la provincia, y la segunda el valor de la variable que se quiera representar.
#' **NOTA IMPORTANTE:** Se debe tener cuidado de que la primera columna no sea de
#' tipo factor. Se recomienda sea de tipo character, nunca pasar un factor.
#' @param camino Sera un valor de tipo carácter para indicar donde se encuentran los
#' ficheros descargados de github conteniendo los datos del mapa que se ha indicado
#' anteriormente. El camino **NO** debe finalizar con el carácter "/".
#' @param titulo Aquí se indicará el título que se desee mostrar en el mapa.
#' @param size_letra Es el tamaño de la letra que se utilizará para mostrar los valores
#' numéricos de los datos en el mapa.
#' @param color_text Sirve para indicar el color del texto que se utilizará para mostrar
#' los datos en el mapa
#' @param ver_text Debe ser un valor lógico. Por defecto vale TRUE, para indcar que se
#' muestren los datos en el mapa. Si se asigna el valor FALSE, los valores de los datos
#' no se mostrarán.
#'
#' @return Un objeto de tipo ggplot2 con el mapa a representar
#'
#' @import sf
#' @import ggplot2
#'
#' @examples
#'
#' d <-data.frame(
#'cod=c(1,2,5,9,15,23,43,50),
#'da = c(3,5,7,10,1,3,80,4)
#')
#'suppressWarnings(mapa(d,system.file("mapa", package = "Relectoral") ,
#'                      titulo="Un mapa de ejemplo, y valores NA",ver_text = TRUE))
#'
#'
#'@export
#'@md

mapa <-function(dat,camino,titulo,size_letra=3,color_text="brown",ver_text=T){
  if(class(dat) != "data.frame") stop("dat debe ser de la clase data.frame")
  if(ncol(dat) != 2) stop("dat debe tener dos columnas")
  colnames(dat) <- c("Codigo","variable")
  # Lo formateo para tener un cero a la izquierda si hay sólo un dígito
  dat$Codigo <- sprintf("%02d",as.integer(dat$Codigo))

  nc<-st_read(paste0(camino,"/Provincias_ETRS89_30N.shx"))
  #Entresacamos los datos de las Islas Canarias
  canarias<-nc[as.character(nc$Cod_CCAA)=='05',]
  #Entresacamos los datos de la península
  Peninsula<-nc[!as.character(nc$Cod_CCAA)=='05',]

  nccanarias<-st_geometry(canarias)
  nccanarias<-nccanarias+c(1700000,700000)
  canarias$geometry<-nccanarias
  st_crs(canarias)<-st_crs(Peninsula)
  esp<-rbind(Peninsula,canarias)
  # Codigo es un factor lo paso a tipo character
  esp$Codigo <- as.character(esp$Codigo)
  esp <-merge(esp,dat,by=c("Codigo"),all.x=T)


  p <- ggplot(data=esp)+geom_sf(aes(fill = variable))+
    ggtitle(titulo)+
    annotate(geom = "text", x = 950000, y = 4050000, label = "Islas Canarias",
             fontface = "italic", color = "grey22", size = size_letra)+
    coord_sf(xlim = c(-14094.75, 1184136.44),
             ylim = c(3832131.07, 4859239.69), expand = FALSE)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        )+
    theme_void()+
    scale_fill_gradient(low = "#56B1F7", high = "#132B43",na.value="white")+ labs(fill = "")+
    theme(plot.title = element_text(hjust = 0.5),
          plot.margin = unit(c(0.3,0.3,0.5,0.3), "cm"))
  if(ver_text){
    p<-p+geom_sf_text(data=st_centroid(esp),aes(label=variable),size=size_letra,col=color_text)
  }
  #imprimo
  return(p)
}

# d <-data.frame(
#   cod=c(1,2,5,9,15,23,43,50),
#   da = c(3,5,7,10,1,3,80,4)
# )
#
# suppressWarnings(mapa(d,"F:/Elecciones/TFM/Mapa/",
#                       titulo="Un mapa de ejemplo, y valores NA",ver_text = T))

