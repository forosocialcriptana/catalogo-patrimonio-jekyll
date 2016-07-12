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
  yamlheader <- as.yaml(list(layout="page", title=as.character(x[2]), "header-img"=banner, categories=as.character(x[7]), comments=as.character("true")))
  write(paste("---\n", yamlheader,"---\n\n", sep=""), file=file.name, append=T)
  write(unlist(x[5]), file=file.name, append=T)
  write("\n<div class=\"photos\">", file=file.name, append=T)
  lapply(photos, function(y) write(paste("<img src=\"/", y, "\" alt=\"", x[2], "\">", sep=""), file=file.name, append=T))
  write("</div>", file=file.name, append=T)
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
header-img: img/fondo-azul.png
---\n\n"
  write(yamlheader, file=file.name, append=T)
  write("## Índice alfabético\n", file=file.name, append=T)
  write(unlist(lapply(data[,2], function(x) paste("- [", x, "](", gsub(" ", "-", tolower(iconv(x, to='ASCII//TRANSLIT'))), "/index.html)\n", sep=""))), file=file.name, append=T)
  # Generar las fichas
  lapply(1:nrow(data), function(i) render.record(data[i,]))
}

render.all.records(data)
