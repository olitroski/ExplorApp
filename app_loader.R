# Cargar la app para pruebas
rm(list=ls())
mainfolder <- "D:/GoogleDrive/ExplorApp"
setwd(mainfolder)


# Cargar la app
library(openxlsx)
library(dplyr)
library(stringr)
library(lubridate)


# | -- Cargar funciones al Gloval Env --------------------------------------- #
# Cargar el folder
cat("---Cargando funciones---\n")
setwd(file.path(mainfolder, "funciones"))
funciones <- dir()[grep("[.Rr]$", dir())]

# Cargar
for (fun in funciones) {
    print(paste("Loading function", fun))
    source(fun)
}    

# Limpiar
rm(fun, funciones)
setwd(mainfolder)



# | -- Proceso de Data Management ------------------------------------------- #
# | ---- Leer los datos
archivo_excel <- "excel_input.xlsx" 
data01 <- data01_leerExcel(archivo_excel)

# | ---- Checar el tipo de cada variable
if (data01[[1]] == "error al cargar"){
    cat("algo sali� mal, revisar el archivo \n")
} else {
    datos <- data01[["datos"]]
    print(dim(data01[["datos"]]))
    data02_DetectNumeric(datos, "Nombre")
}


