# ---------------------------------------------------------------- # 
# ---- Cargar funciones desde github.com/olitroski --------------- #
# ---------------------------------------------------------------- #
explorAppLoader <- function(local = FALSE, ruta = 'D:/GoogleDrive/R/ExplorApp/funciones'){
    
    # ---- Cargar Librerias --------------------------------------------
    packlist <- c('devtools', 'openxlsx', 'haven', 'glue', 'markdown',"lubridate",
                  'data.table', 'ggplot2')
    new.packages <- packlist[!(packlist %in% installed.packages()[,"Package"])]
    
    options(datatable.print.nrows=10)
    options(datatable.print.trunc.cols = TRUE)
    
    # Instalar si no estan
    if (length(new.packages) > 0) {
        install.packages(new.packages)
    }
    # Cargar
    for (lib in packlist) {
        eval(parse(text = paste0("library(",lib,")")))
    }

    # ---- Cargar funciones de Github -----------------------------
    
    if (local == TRUE){
        setwd(ruta)
        funcs <- dir()
        funcs <- funcs[grep("[rR$]", funcs)]
        
        for (f in funcs){
            print(paste('Cargando: ', f))
            setwd(ruta)
            source(f)
        }
        
    } else {
        # Vector de funciones
        setwd(ruta)
        funcs <- dir()
        funcs <- funcs[grep("[^rR]", funcs)]
        
        # Cargar fnciones
        url <- 'https://raw.githubusercontent.com/olitroski/ExplorApp/main/funciones/'
        for (f in funcs){
            print(paste('Cargando: ', f))
            devtools::source_url(paste0(url, f))
        }
    }
}
