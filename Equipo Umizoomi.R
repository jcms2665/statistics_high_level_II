
library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)
library(dplyr)
library(stringr)
library(stringi)


# Pelotita azul y Set Working Directory

datos=read_excel("Equipo Umizoomi.xlsx", sheet = "BASE DE DATOS")

nombre_columna <- "Motivos"  

# Separar y limpiar categorías únicas
categorias_transporte <- unique(unlist(strsplit(datos[[nombre_columna]], ", ")))
categorias_transporte <- categorias_transporte[!is.na(categorias_transporte)]

# Convertir a minúsculas, eliminar comas, puntos y acentos, y reemplazar espacios por guiones bajos
categorias_transporte <- categorias_transporte %>%
  tolower() %>%
  gsub("[.,]", "", .) %>%  # Elimina comas y puntos
  stri_trans_general("Latin-ASCII") %>%  # Elimina acentos
  str_replace_all(" ", "_") %>%
  trimws()

# Asegurar que la columna sea de tipo carácter y limpiar espacios, comas, puntos y acentos en cada entrada
datos <- datos %>%
  mutate(across(all_of(nombre_columna), ~ tolower(trimws(gsub("[.,]", "", stri_trans_general(as.character(.), "Latin-ASCII"))))))  

# Crear variables 
for (categoria in categorias_transporte) {
  datos[[categoria]] <- as.numeric(str_detect(datos[[nombre_columna]], fixed(categoria, ignore_case = TRUE)))
}

# Ver el resultado final
View(datos)
