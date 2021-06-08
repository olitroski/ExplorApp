# ---------------------------------------------------------------------------------- #
# ----- Deteccion de outliers por sd y pct - 12.2019 - O.Rojas --------------------- #
# ---------------------------------------------------------------------------------- #
# Toma un 2 variables, detecta primero si es fac o num, y en virtud de la combinación
# crea un informe de outliers y las dos variables con outliers en NA. O se a una var.

dmDetectOutlier <- function(v1, v2 = NULL, df, sd = 3, pct = 0.05){
    library(data.table)
    
    # ---- Una variable ----
    if (class(v2) == "NULL"){
        # Data a procesar
        data <- as.data.table(df)
        data <- data[, ..v1]
        names(data) <- "var"
            
            
        # ==== I-1. Si es numerica ======================================================
        if (class(data[, var]) == "numeric"){
            # Normalizar        
            data[, z := scale(var)]
            data[, out := fifelse(abs(z) > sd, 1, 0)]
            
            # Crear tabla con outliers
            data[, indx := row.names(data)]
            out <- data[out == 1]
            out <- out[, !"out"][order(z)]
            out[, z := round(z, 3)]
            names(out) <- c(v1, "valor-Z", "index")
            
            # variable sin outliers
            data[, newvar := ifelse(out == 1, NA, var)]
            
            # Resultado
            r <- list(outlier = as.data.frame(out), new.var = data[, newvar])
            names(r)[2] <- v1
            detach("package:data.table", unload=TRUE) 
            return(r)
            
            
        # ==== I-2. Si es categórica ====================================================
        } else if (class(data[, var]) == "factor"){
            # Tabla outliers
            tab <- data[, .(count = .N), by = var]
            tab[, percent := count/sum(count)]
            # tab[3, percent := 0.049]
            tab <- tab[percent < pct]
            
            # NO hay outliers            
            if (nrow(tab) == 0){
                # Reporte
                out <- data.frame(Outliers = "No se encontraron valores extremos")
                newvar <- data[, var]
                
                # Resultado
                r <- list(outlier = as.data.frame(out), new.var = newvar)
                names(r)[2] <- v1
                detach("package:data.table", unload=TRUE) 
                return(r)
            
            # Hay outliers
            } else {                
                # Reporte
                out <- tab[, percent := percent * 100]
                
                # Variable sin outliers
                cats <- out[, as.character(var)]
                data[, newvar := var]
                data[var %in% cats, newvar := NA]
                newvar <- data[, newvar]
                names(out) <- c(v1, "Conteo", "Porcentaje")
                
                # Resultado
                r <- list(outlier = as.data.frame(out), new.var = newvar)
                names(r)[2] <- v1
                detach("package:data.table", unload=TRUE) 
                return(r)
            }
            
            
        # 3. Una variable pero de tipo equivocado
        } else {
            detach("package:data.table", unload=TRUE) 
            stop("Solo variables numericas o factor")
        }

        
    # ---- Dos variables -------------------
    } else {
        # Data a procesar
        data <- as.data.table(df)
        vars <- c(v1, v2)
        data <- data[, ..vars]
        names(data) <- c("var1", "var2")
            
          
        # ==== II-1. Si el par es num-num ===============================================
        if (class(data[, var1]) == "numeric" & class(data[, var2]) == "numeric"){
            # Zetas y outliers
            data[, c("z1", "z2") := .(scale(var1), scale(var2))]
            data[, out1 := fifelse(abs(z1) > sd, 1, 0)]
            data[, out2 := fifelse(abs(z2) > sd, 1, 0)]
            data[, index := row.names(data)]
            data[, newvar1 := ifelse(out1 == 0, var1, NA)]
            data[, newvar2 := ifelse(out2 == 0, var2, NA)]
            
            # Reportes
            out1 <- data[out1 == 1, .(var1, z1, index)]
            names(out1) <- c(v1, "Z-Score", "Index")
            out2 <- data[out2 == 1, .(var2, z2, index)]
            names(out2) <- c(v2, "Z-Score", "Index")
            
            # Resultado
            lnames <- c(paste0("out.", v1), paste0("var.", v1), 
                        paste0("out.", v2), paste0("var.", v2))
            outlier <- list(as.data.frame(out1), data[, newvar1],
                            as.data.frame(out2), data[, newvar2])
            names(outlier) <- lnames
            
            detach("package:data.table", unload=TRUE) 
            return(outlier)
            
            
        # ==== II-2. Si el par es num-fac ===============================================
        } else if ((class(data[, var1]) == "numeric" & class(data[, var2]) == "factor") |
                   (class(data[, var1]) == "factor" & class(data[, var2]) == "numeric")){
                
            # Buscar cual es cual
            if (class(data[, var1]) == "numeric"){
                names(data) <- c("num", "fac")
                num.name <- v1
                fac.name <- v2
            } else {
                names(data) <- c("fac", "num")
                num.name <- v2
                fac.name <- v1
            }
            
            # <<<<< Procesar el factor >>>>>
            # Adaptar todo
            facdata <- data[, .(fac)]
            facdata <- rename(facdata, var = fac)
            
            # Tabla outliers
            tab <- facdata[, .(count = .N), by = var]
            tab[, percent := count/sum(count)]
            # tab[3, percent := 0.049]
            tab <- tab[percent < pct]
            
            # NO hay outliers            
            if (nrow(tab) == 0){
                # Reporte
                out <- as.data.frame(tab)
                out[1, 1:3] <- NA
                newvar <- facdata[, var]
                
                # Resultado factor
                lnames <- c(paste0("out.", fac.name), paste0("var.", fac.name))
                list.fac <- list(outlier = as.data.frame(out), new.var = newvar)
                names(list.fac) <- lnames
                
            # Hay outliers
            } else {                
                # Reporte
                out <- tab[, percent := percent * 100]
                
                # Variable sin outliers
                cats <- out[, as.character(var)]
                catdata <- data[, .(newvar = fac)]
                catdata[newvar %in% cats, newvar := NA]
                newvar <- catdata[, newvar]
                names(out) <- c(fac.name, "Conteo", "Porcentaje")
                
                # Resultado factor
                lnames <- c(paste0("out.", fac.name), paste0("var.", fac.name))
                list.fac <- list(outlier = as.data.frame(out), new.var = newvar)
                names(list.fac) <- lnames
            }
            
            # <<<<< Procesar el numeric >>>>> 
            # Normalizar   
            numdata <- data[, .(var = num)]
            numdata[, z := scale(var)]
            numdata[, out := fifelse(abs(z) > sd, 1, 0)]
            
            # Crear tabla con outliers
            numdata[, indx := row.names(numdata)]
            out <- numdata[out == 1]
            out <- out[, !"out"][order(z)]
            out[, z := round(z, 3)]
            names(out) <- c(num.name, "valor-Z", "index")
            
            # variable sin outliers
            numdata[, newvar := ifelse(out == 1, NA, var)]
            
            # Resultado numerico
            lnames <- c(paste0("out.", num.name), paste0("var.", num.name))
            list.num <- list(outlier = as.data.frame(out), new.var = numdata[, newvar])
            names(list.num) <- lnames
            
            
            # <<<<< Procesar ambos >>>>>
            # Pendiente 23.02.2020
            
            
            # <<<<< Resultado combinación num|fac >>>>> 
            detach("package:data.table", unload=TRUE) 
            return(c(list.fac, list.num))
            
            
        # ==== II-3. Si el par es fac-fac ===============================================
        } else if (class(data[, var1]) == "factor" & class(data[, var2]) == "factor"){
            
            # ---- Variable 1 fac ---------------------------------------------
            f1data <- data[, .(var = var1)]
            
            # Tabla outliers
            tab <- f1data[, .(count = .N), by = var]
            tab[, percent := count/sum(count)]
            # tab[3, percent := 0.049]
            tab <- tab[percent < pct]
            
            # NO hay outliers            
            if (nrow(tab) == 0){
                # Reporte
                out <- as.data.frame(tab)
                out[1, 1:3] <- NA
                newvar <- f1data[, var]
                
                # Resultado factor
                lnames <- c(paste0("out.", v1), paste0("var.", v1))
                list.fac1 <- list(outlier = as.data.frame(out), new.var = newvar)
                names(list.fac1) <- lnames
                
            # Hay outliers
            } else {                
                # Reporte
                out <- tab[, percent := percent * 100]
                
                # Variable sin outliers
                cats <- out[, as.character(var)]
                catdata <- f1data[, .(newvar = var)]
                catdata[newvar %in% cats, newvar := NA]
                newvar <- catdata[, newvar]
                
                # Resultado factor
                names(out) <- c(v1, "Conteo", "Porcentaje")
                lnames <- c(paste0("out.", v1), paste0("var.", v1))
                list.fac1 <- list(outlier = as.data.frame(out), new.var = newvar)
                names(list.fac1) <- lnames
            }
            rm(tab, lnames, catdata, newvar, out)
            
            # ---- Variable 2 fac ---------------------------------------------
            f2data <- data[, .(var = var2)]
            
            # Tabla outliers
            tab <- f2data[, .(count = .N), by = var]
            tab[, percent := count/sum(count)]
            # tab[3, percent := 0.049]
            tab <- tab[percent < pct]
            
            # NO hay outliers            
            if (nrow(tab) == 0){
                # Reporte
                out <- as.data.frame(tab)
                out[1, 1:3] <- NA
                newvar <- f2data[, var]
                
                # Resultado factor
                lnames <- c(paste0("out.", v2), paste0("var.", v2))
                list.fac2 <- list(outlier = as.data.frame(out), new.var = newvar)
                names(list.fac2) <- lnames
                
            # Hay outliers
            } else {                
                # Reporte
                out <- tab[, percent := percent * 100]
                
                # Variable sin outliers
                cats <- out[, as.character(var)]
                catdata <- f2data[, .(newvar = var)]
                catdata[newvar %in% cats, newvar := NA]
                newvar <- catdata[, newvar]
                
                # Resultado factor
                names(out) <- c(v2, "Conteo", "Porcentaje")
                lnames <- c(paste0("out.", v2), paste0("var.", v2))
                list.fac2 <- list(outlier = as.data.frame(out), new.var = newvar)
                names(list.fac2) <- lnames
            }
            
            
            # ---- Celdas -----------------------------------------------------
            celldata <- table(data$var1, data$var2)
            celldata <- as.data.frame.matrix(celldata)
            
            menos5 <- NULL
            for (r in 1:nrow(celldata)){
                menos5 <- c(menos5, celldata[r, ] < 5)
            }
            
            if (sum(menos5) > 0){
                celldata <- bind_cols(data.frame(var = row.names(celldata)),
                                      celldata)
                names(celldata)[1] <- v1
            } else {
                celldata <- (data.frame(Outlier = "No se detectan celdas N < 5"))
            }
            
            
            # Resultado
            detach("package:data.table", unload=TRUE) 
            return(c(list.fac1, list.fac2, list(celdas = celldata)))
            
            
        # ==== III-1. Si fuera otro tipo ================================================
        } else {
            detach("package:data.table", unload=TRUE) 
            cat("Las variables debieran ser num | fac \n")
            stop("La base debe pasarse primero por el CheckData()")
        }
    }
    
    # Sacar esta cosa porque hace conflicto con todo lo más viejo
    detach("package:data.table", unload=TRUE) 
}

