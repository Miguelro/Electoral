#########################
# C\u00F3digos de Comunidades Aut\u00F3nomas
# Son los c\u00F3digos que utiliza el MIR
##############################
#'@title utilidades. Codigos de Comunidad Autonoma.
#'
#'@description Con esta función se pueden obtener los códigos de las Comunidades Autónomas de España
#'que utiliza el MIR para codificar sus datos. Esta codificación se puede
#'[ver igualmente en este enlace](https://github.com/Miguelro/Electoral/blob/master/Diseno_Registros_MIR/Diseno_Registros.pdf).
#'Es de hacer notar que este sistema de codificación no coinciden en su totalidad con los [códigos de Comunidad Autónoma del
#'Instituto Nacional de Estadística](https://www.ine.es/daco/daco42/codmun/cod_ccaa.htm).Esta función
#'no tiene parámetros.
#'
#'@return Devuelve un dataframe con dos columnas. La primera columna contiene el código de la
#'Comunidad Autónoma y el segundo campo la denominación de esa Comunidad.
#'
#' @examples
#' get_CCAA()
#'
#'
#'
#'@export
#'@md

get_CCAA <- function(){
  codigos <- data.frame(
    cod=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19"),
    nombre=c(
      "Andaluc\u00EDa",
      "Arag\u00F3n",
      "Asturias",
      "Baleares",
      "Canarias",
      "Cantabria",
      "Castilla-La Mancha",
      "Castilla y Le\u00F3n",
      "Catalu\u0169a",
      "Extremadura",
      "Galicia",
      "Madrid",
      "Navarra",
      "Pa\u00EDs Vasco",
      "Regi\u00F3n de Murcia",
      "La Rioja",
      "Comunidad Valenciana",
      "Ceuta",
      "Melilla"
    )
  )
  return(codigos)
}
#get_CCAA()

#'@title utilidades. Codigos de las provincias de España.
#'
#'@description Con esta función se obtiene la denominación de todas las provincias españolas, junto
#'a sus códigos, que coinciden en su integridad con los que facilita el Instituto Nacional de Estadística
#'[en esta página web](https://www.ine.es/daco/daco42/codmun/cod_provincia.htm). Esta función no tiene parámetros.
#'
#'@return Devuelve un dataframe con dos columnas. La primera contiene el código de la
#'provincia y el segundo campo la denominación de esa provincia.
#'
#' @examples
#' get_Provincias()
#'
#'
#'
#'@export
#'@md


get_Provincias <- function(){
  codigos <- data.frame(
  cod=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19",
        "20","21","22","23","24","25","26","27","28","29",
        "30","31","32","33","34","35","36","37","38","39",
        "40","41","42","43","44","45","46","47","48","49",
        "50","51","52"),
  nombre=c("\u00C1lava","Albacete","Alicante","Almer\u00EDa",
           "\u00C1vila","Badajoz","Baleares, Illes","Barcelona",
           "Burgos","C\u00E1ceres","C\u00E1diz",
           "Castell\u00F3n","Ciudad Real","C\u00F3rdoba","Coru\u0169a, A",
           "Cuenca","Girona","Granada","Guadalajara","Gipuzkoa",
           "Huelva","Huesca","Ja\u00E9n","Le\u00F3n","Lleida","Rioja, La","Lugo",
           "Madrid","M\u00E1laga","Murcia","Navarra","Ourense","Asturias",
           "Palencia","Palmas, Las","Pontevedra",
           "Salamanca","San. Cr. Tenerife","Catabria",
           "Segovia","Sevilla","Soria","Tarragona",
           "Teruel","Toledo","Valencia","Valladolid",
           "Bizkaia","Zamora","Zaragoza",
           "Ceuta","Melilla")
  )
  return(codigos)
}

get_Provincias()

