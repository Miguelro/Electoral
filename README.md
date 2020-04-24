# Electoral
En este repositorio se van a incluir diferentes archivos que pueden servir para calcular
 muchos indicadores que permitan estudiar muchos aspectos relacionados con procesos
 electorales.
 En concreto, el producto más interesante se puede en esta colección de ficheros, es el
 paquete desarrollado en el entorno estadístico R y cuya fuente se encuentra en la 
 carpeta Relectoral.
 
 Igualmente en la carpeta base, se puede encontrar el fichero "Relectoral_0.1.0.tar.gz", que 
 se puede decargar a ordenador local de cada usuario, y utilizar el mismo para su carga en R, 
 y poder de esta manera utilizar el amplio elenco de funciones que tiene para obtener
 una ingente cantidad de indicadores electorales.
 
## Utilidades del paquete Relectoral

Las herramientas que se ponen a disposición del usuario de este producto van a permitir calcular indicadores electorales de dos tipos:

1.- Indicadores de desproporcionalidad.

Dentro de estos indicadores se pueden distinguir los siguiente:

* Índice de desproporcionalidad de RAE
* Índice de desproporcionalidad de RAE corregido.
* Índice de desproporcionalidad de Loosemore y Hanby.
* índice de desproporcionalidad de Saint Laguë.
* Índice de desproporcionalidad de los cuadrados mínimos de Gallagher.
* Índice de la máxima desviación de Lijphart.
* Índice de sesgo en la distribución de escaños de Cox y Shugart
* Índice de sesgo robusto ( en varias versiones)

2.- Cálculo de las dimensiones del voto. 
 
 Las funciones desarrolladas en este apartado, permiten calcular los siguientes
 indicadores:
 
* Índices de fragmentación electoral y parlamentaria de RAE.
* Índices del número efectivo de partidos, electoral y parlamentario.
* Índices de hiperfragmentación ( electoral y parlamentaria) de Kesselman y Wildgen
* Índices del número de partidos de Molinar ( electoral y parlamentaria)
* Indices de concentración electoral y parlamentaria.
* Índices de competitividad electoral y parlamentaria.
* Índice de polarización de Sartori.
* Índices de polarización ponderada ( electoral y parlamentaria).
* Índice de polarización ponderada. Adaptado de Dalton (2008)
* Índices de volatilidad tanto electoral como ponderada, en sus versiones de total,
	entre bloques e intrabloques.
* Fórmula para calcular la injusticia matemática entre dos partidos.
* Índice de Nacionalización del Partido (INP).

# Otras utilidades.

Además de todos los indicadores expuestos anteriormente, se han desarrollado herramientas
informáticas que faciliten la utilización de recursos gratuitos existentes en 
Internet, como pueden ser los siguientes:

* Programa [Bazi](https://www.math.uni-augsburg.de/htdocs/emeriti/pukelsheim/bazi/). Gracias
al cual se pueden hacer diferentes formas de reparto de escaños, mediante las gran
cantidad de fórmulas que dispone. Con Relectoral, se va a poder obtener ficheros en 
el formato que necesita esta aplicación para obtener sus resultados.

* Macro escrita en excell de [Bochsler](https://www.bochsler.eu/pns/), que permite
calcular el índice de nacionalización de los partidos. Con el paquete Relectoral, se puede
obtener la información ordenada, para facilitar la cumplimentación de dicha macro.