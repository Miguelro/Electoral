% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Electoral_index.R
\name{Rae}
\alias{Rae}
\title{Indice de desproporcionalidad de RAE-1971 ("R")}
\usage{
Rae(votes, seats)
}
\arguments{
\item{votes}{Es un vector de números enteros, conteniendo los votos de todas las candidaturas.}

\item{seats}{Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas}
}
\value{
Devuelve el valor obtenido para el índice
}
\description{
Este índice mide la desproprocionalidad electoral con el resultado obtenido en unas
elecciones. Tiene el inconveniente de que está muy afectado por el número de partidos pequeños
que concurren a las elecciones. La fórmula uilizada es la siguiente:
\deqn{R=\frac{\sum_{i=1}^{n}|E_{i}-V_{i}|}{n}}{R = (1/n)*sum(|Vi-Ei|)}
for \eqn{i = 1, 2, \ldots,n}

Para calcular su valor hay que introducir como parámetros de la fórmula los vectores de números
enteros que se corresponden con los votos obtenidos por cada partido y los escaños conseguidos.
El código R se encargará de calcular los porcentajes correspondientes para la obtención del índice.
}
\section{Functions}{
\itemize{
\item \code{Rae}: de desproporcionalidad
}}

\section{Indicadores}{
NA
}

\examples{
Rae(c(1200, 30, 4000),c(10,6,8))

}
