# ---------------------------------------------------------------- # 
# ---- Cargar funciones desde github.com/olitroski --------------- #
# ---------------------------------------------------------------- #
explorAppLoader <- function(){
    
    # ---- Cargar Librerias --------------------------------------------
    packlist <- c('devtools', 'openxlsx', 'haven', 'glue', 'markdown',"lubridate",
                  'dplyr')
    new.packages <- packlist[!(packlist %in% installed.packages()[,"Package"])]
    
    # Instalar si no estan
    if (length(new.packages) > 0) {
        install.packages(new.packages)
    }
    # Cargar
    for (lib in packlist) {
        eval(parse(text = paste0("library(",lib,")")))
    }
    
    
    # ---- Cargar funciones -------------------------------------------
    
    # setwd('D:/GoogleDrive/R/ExplorApp/funciones')
    # funcs <- dir()
    # funcs <- funcs[grep("[^rR]", funcs)]
    # funcs <- paste0("'", funcs, "'")
    # funcs <- paste(funcs, collapse = ', ')
    # funcs <- paste('funcs <- c(', funcs, ')')
    # writeClipboard(funcs)
    
    # Cargar fnciones
    funcs <- c( 'data01_dataLoader.R', 'data01_LeerExcel.R', 'data01_LeerStata.R', 'data02_DetectFactor.R', 'data02_DetectMissing.R', 'data02_DetectNumeric.R', 'data02_DetectOutlier.R' )
    url <- 'https://raw.githubusercontent.com/olitroski/ExplorApp/main/funciones/'
    for (f in funcs){
        print(paste('Cargando: ', f))
        devtools::source_url(paste0(url, f))
    }
    
    
}






