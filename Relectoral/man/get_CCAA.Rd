% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Codigos.R
\name{get_CCAA}
\alias{get_CCAA}
\title{utilidades. Codigos de Comunidad Autonoma.}
\usage{
get_CCAA()
}
\value{
Devuelve un dataframe con dos columnas. La primera columna contiene el código de la
Comunidad Autónoma y el segundo campo la denominación de esa Comunidad.
}
\description{
Con esta función se pueden obtener los códigos de las Comunidades Autónomas de España
que utiliza el MIR para codificar sus datos. Esta codificación se puede
\href{https://github.com/Miguelro/Electoral/blob/master/Diseno_Registros_MIR/Diseno_Registros.pdf}{ver igualmente en este enlace}.
Es de hacer notar que este sistema de codificación no coinciden en su totalidad con los \href{https://www.ine.es/daco/daco42/codmun/cod_ccaa.htm}{códigos de Comunidad Autónoma del Instituto Nacional de Estadística}.Esta función
no tiene parámetros.
}
\examples{
get_CCAA()



}
