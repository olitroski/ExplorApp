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
    url <- 'https://raw.githubusercontent.com/olitroski/ExplorApp/main/funciones/'
    
    # setwd('D:/GoogleDrive/R/ExplorApp/funciones')
    # funcs <- dir()
    # funcs <- funcs(grep("[^rR]", funcs))
    
    for (f in funcs){
        print(paste('Cargando: ', f))
        devtools::source_url(paste0(url, f))
    }
    
    
}






