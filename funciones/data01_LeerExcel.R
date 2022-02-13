# ----------------------------------------------------------------------------------------------- #
# ------ data01_excel -> Lectura del Excel inicial ---------------------------------------------- #
# ----------------------------------------------------------------------------------------------- #
# library(openxlsx)
# library(glue)
# library(markdown)
# 
# setwd("D:/GoogleDrive/R/ExplorApp")
# dir()
# excelFile <- 'excel_input.xlsx'
# rm(list=ls())
# data <- DM01_leerExcel(excelFile)

# Data management 01 - Leer archivo de Excel
dataExcel <- function(excelFile){
    
    # Intentar leer el archivo
    data <- tryCatch(
        # Try part
        data <- read.xlsx(excelFile, 
                          detectDates = TRUE, 
                          skipEmptyRows = TRUE, 
                          skipEmptyCols = TRUE,
                          check.names = TRUE),
        # Catch part
        warning = function(w){
            cat(paste0("> Warning al intentar leer Excel ", excelFile, '\n'))
            message(w)
        },
        error = function(e){
            cat(paste0("> Error al intentar leer Excel <", excelFile, '>\n'))
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
        
        file.info(excelFile)
        
        # Reporte
        resumen <- glue('# Resumen inpección archivo "{excelFile}" \n',
                       'El archivo se pudo leer de manera correcta, las principales caracteristicas son: \n\n',
                       '- Número de filas: {f} \n',
                       '- Número de columnas: {c} \n',
                       '- Nombres de variable: {vars2} \n')
        resumen <- markdownToHTML(text = as.character(resumen), 
                                  fragment.only = TRUE, 
                                  stylesheet = css)
        
        # css <- 'https://gist.githubusercontent.com/andyferra/2554919/raw/10ce87fe71b23216e3075d5648b8b9e56f7758e1/github.css'
        # html <- glue('<!DOCTYPE html>',
        #              '<html>',
        #              '<head>',
        #              '<title>Data Management</title>',
        #              '<link rel="stylesheet" href={css}>',
        #              '</head>',
        #              '<body>',
        #              resumen,
        #              '</body>',
        #              '</html>' )
        # writeLines(html, 'test.html')            
        
        # Resultado
        return(list(filas = f, columnas = c, variables = vars, header = resumen, datos = data))
    }
}

