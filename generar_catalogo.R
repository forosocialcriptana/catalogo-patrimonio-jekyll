# Script para crear crear el catálogo de patrimonio de Campo de Criptana
# Author: Alfredo Sánchez Alberca (asalber@gmail.com)

# URL de la hoja de cálculo con los datos
url.data <- "https://docs.google.com/spreadsheets/d/1hWyDiPwVU1oUflqVSC6iSveP-NigMpZhh9e7RKXZpJk/pub?output=csv"
library(RCurl)
library(rmarkdown)
library(yaml)
library(dplyr)


# Carga de los datos
data <- read.csv(text = getURL(url.data, .encoding = "UTF-8"), encoding = "UTF-8", header = T, stringsAsFactors = F)
data <- data %>% arrange(Título)
headers <- gsub(".", " ", names(data), fixed=T)


#' Title
#' Función que crea un fichero con el contenido de una ficha de un lugar en formato markdown. 
#' El nombre del fichero en formato Rmarkdown se toma del segundo campo que se supone es el título de ficha.
#' @param x Vector con los campos de la ficha.
#'
#' @return None
#' @export
#'
#' @examples
render.record <- function(x){
  # Primero eliminar tildes, espacios y pasar a minuscula
  name <- gsub(" ", "-", tolower(iconv(x[2], to='ASCII//TRANSLIT')))
  name <- gsub("/", "-", name)
  file.name <- paste("lugares/", name, ".md", sep="")
  file.create(file.name)
  # Descargar fotos
  url.photos <- trimws(unlist(strsplit(gsub("open\\?", "uc?export=download&", x[,9]), ",")))
  photos <- NULL
  if (length(url.photos)>0) {
    for (i in 1:length(url.photos)) {
      photos[i] <- paste("img/", name, "-", i, ".jpg", sep="")
      download.file(url.photos[i], photos[i], method="wget", mode="w")
    }
    banner = photos[1]
  } else {
    banner="img/fondo-azul.png"
  }
  yamlheader <- as.yaml(list(layout="page", title=as.character(x[2]), "header-img"=paste("/", banner, sep=""), category=as.character(x[7]), comments=as.character("true")))
  write(paste("---\n", yamlheader,"---\n\n", sep=""), file=file.name, append=T)
  write(unlist(x[6]), file=file.name, append=T)
  if (length(photos)>0) {
    write('\n<div id="myCarousel" class="carousel slide" data-ride="carousel">
  <!-- Indicators -->
  <ol class="carousel-indicators">
    <li data-target="#myCarousel" data-slide-to="0" class="active"></li>',
    file=file.name, append=T)
    for (i in seq_along(photos[-1])) {
      write(paste('    <li data-target="#myCarousel" data-slide-to="', i, '"></li>', sep=""), file = file.name, append = T)
    }
    write('  </ol>
  <!-- Wrapper for slides -->
  <div class="carousel-inner" role="listbox">
    <div class="item active">',
    file = file.name, append=T)
    write(paste('      <img src="{{ site.github.url }}/', photos[1], '" alt="', x[2], '">', sep=""), file = file.name, append = T)
    write('    </div>', file= file.name, append = T)
    for (i in photos[-1]) {
      write('    <div class="item">', file= file.name, append = T)
      write(paste('      <img src="{{ site.github.url }}/', i, '" alt="', x[2], '">', sep=""), file = file.name, append = T)
      write('    </div>', file= file.name, append = T)    
    }
    write('  <!-- Left and right controls -->
  <a class="left carousel-control" href="#myCarousel" role="button" data-slide="prev">
    <span class="glyphicon glyphicon-chevron-left" aria-hidden="true"></span>
    <span class="sr-only">Previous</span>
  </a>
  <a class="right carousel-control" href="#myCarousel" role="button" data-slide="next">
    <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>
    <span class="sr-only">Next</span>
  </a>
</div>', file = file.name, append=T)
  }
  return(paste(name, ".md", sep=""))
}


#' Función que genera todas las fichas de los lugares en formato markdown.
#'
#' @param data Data frame con los registros de las fichas de los lugares.
#'
#' @return None
#' @export
#'
#' @examples
render.all.records <- function(data){
  # Generar el índice
  file.name = "lugares/index.md"
  file.create(file.name)
  yamlheader <- "---
layout: page
title: Índice de lugares
header-img: /img/sierra-de-los-molinos-1.jpg
---\n\n"
  write(yamlheader, file=file.name, append=T)
  write('## Índice por categorías\n
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#ambiental">Ambiental</a> &nbsp;&nbsp;&nbsp;
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#artístico">Artístico</a> &nbsp;&nbsp;&nbsp;
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#etnográfico">Etnográfico</a> &nbsp;&nbsp;&nbsp;
<i class="fa fa-tag"></i> &nbsp;<a href="{{ site.github.url }}/lugares/categorias/index.html#histórico">Histórico</a> &nbsp;&nbsp;&nbsp;

## Índice alfabético\n', file=file.name, append=T)
  write(unlist(lapply(data[,2], function(x) paste("- [", x, "](", gsub(" ", "-", tolower(iconv(x, to='ASCII//TRANSLIT'))), "/index.html)", sep=""))), file=file.name, append=T)
  
  # Generar estadísticas
  
  write("\n## Estadísticas de lugares\n", file = file.name, append = T)
  write(paste("Número de lugares catalogados: <b>", nrow(data), "</b>\n", sep=""), file = file.name, append = T)
  write('<img src="estadisticas.png" alt="Estadística de lugares catalogados">', file= file.name, append = T)
  table <- table(data$Tipo.de.valor)
  labels <- paste(names(table), "\n", table, sep="")
  png("lugares/estadisticas.png")
  pie(table, labels = labels, main="Lugares catalogados por categorías", col=c("#00FF00FF", "#FFFF00FF", "#FF0000FF", "#0000FFFF"))
  dev.off()
  # Generar las fichas
  lapply(1:nrow(data), function(i) render.record(data[i,]))
}

render.all.records(data)
