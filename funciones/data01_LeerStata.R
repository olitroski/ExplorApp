# --------------------------------------------------------------------------- #
# ----- Leer un stata con HAVEN --------------------------------------------- #
# ----- Para explorApp, 08.06.2021 (en Fase2, Valdivia) Oliver.Rojas -------- #
# --------------------------------------------------------------------------- #
# rm(list = ls())
library(haven)
library(glue)
library(markdown)

# setwd("D:/GoogleDrive/R/ExplorApp")
# dir()
# stataFile <- 'auto13.dta'
# data <- DM01_leerStata(stataFile)

# Data management 01 - Leer archivo de Excel
DM01_leerStata <- function(stataFile){
    
    # Intentar leer el archivo
    data <- tryCatch(
        # Try part
        {
            data <- read_dta(stataFile)    
            vars <- names(data)
            for (v in vars){    # Recastear los factores
                classVar <- class(data[[v]])
                if (classVar[1] == 'haven_labelled'){
                    data[[v]] <- as_factor(data[[v]])
                }
            }
            data <- as.data.frame(data)
        },

        # Catch part
        warning = function(w){
            cat(paste0("> Warning al intentar leer DTA ", stataFile, '\n'))
            message(w)
        },
        error = function(e){
            cat(paste0("> Error al intentar leer DTA <", stataFile, '>\n'))
            message(e)
        }
    )

    # Tirar pa fuera en caso de warning o error
    if (class(data) != "data.frame"){
        stop("Archivo no es un data.frame")
        # return(list(error = "Archivo no es un data.frame"))
        
        # Tirar la lista con variables y cosas
    } else {
        # Filas y columnas variables
        f <- nrow(data)
        c <- ncol(data)
        vars <- names(data)
        vars2 <- paste(vars, collapse = ', ')
            
        # Reporte en markdown
        resumen <- glue('# Resumen inpección archivo "{stataFile}" \n',
                        'El archivo se pudo leer de manera correcta, las principales caracteristicas son: \n\n',
                        '- Número de filas: {f} \n',
                        '- Número de columnas: {c} \n',
                        '- Nombres de variable: {vars2} \n')
        resumen <- markdownToHTML(text = as.character(resumen), 
                                  fragment.only = TRUE, 
                                  stylesheet = css)
        
        # Resultado
        return(list(filas = f, columnas = c, variables = vars, header = resumen, datos = data))
    }
}