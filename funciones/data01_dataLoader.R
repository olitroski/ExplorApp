# rm(list = ls())
# library(haven)
# library(glue)
# library(markdown)
# 
# setwd("D:/GoogleDrive/R/ExplorApp")
# dir()
# dataFile <- 'auto.13.dta'


dataLoader <- function(dataFile){
    
    # Chequeos varios
    if (file.exists(dataFile) == FALSE){
        stop('El archivo no existe')
        
    } else {
        extension = strsplit(dataFile, '\\.')
        extension = extension[[1]]
        extension = extension[length(extension)]
    }
    
    # Cargar datos
    if (extension == 'xlsx'){
        return(dataExcel(dataFile))
        
    } else if (extension == 'dta'){
        return(dataStata(dataFile))
        
    } else {
        stop('No es posible leer archivo de datos')
    }
    
    
}







