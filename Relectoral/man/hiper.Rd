% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Electoral_index.R
\name{hiper}
\alias{hiper}
\title{Indice Dimension de voto. Indice de hiperfraccionamiento (electoral y parlamentario ) (I)}
\usage{
hiper(votes, seats)
}
\arguments{
\item{votes}{Es un vector de números enteros, conteniendo los votos de todas las candidaturas.}

\item{seats}{Es un vector de números enteros, conteniendo los escaños obtenidos por las candidaturas}
}
\value{
Devuelve un dataframe, con las columnas "electoral" y "parlamentario" para albergar
respectivamente el valor del índice de hiperfraccionamiento  electoral o el parlamentario
}
\description{
El índice de hiperfraccionamiento, propuesto por kesselman (1996) y
Wilden ( 1991),es otro indicador que se utiliza para medir el
número de partidos que son relevantes en cualquier tipo de elección. Este índice de
hiperfraccionamiento es muy sensible a la presencia de partidos pequeños y les otorga más
relevancia de la que realmente tienen. La fórmula matemática utilizada para calcular el valor
de este índice es la siguiente:

\deqn{I=exp(-\sum p_{i}ln(p_{i}))}{I=exp(-sum(pi*ln(pi)))}
for \eqn{i = 1, 2, \ldots,n } y pi>0 los porcentajes ( en tanto por uno) de votos o escaños
del partido i
}
\examples{

hiper(c(3947,3189,1971,466,345,82),c(184,99,44,10,1,0))

}
