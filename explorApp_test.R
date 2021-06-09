# --------------------------------------------------------------------------- #
# ----- Flujo de trabajo en ExplorApp --------------------------------------- #
# ----- Para Caro, 08.06.2021 (en Fase2, Valdivia) Oliver.Rojas ------------- #
# --------------------------------------------------------------------------- #
# La idea es que este sea el flujo de trabajo, se cargan las funciones y se 
# comienza trabajar los datos. 

# Setear directorio de trabajo y cargar funciones desde GitHub
setwd('D:/GoogleDrive/R/ExplorApp')
source('gitLoader.R')
explorAppLoader()


# Leer los datos
datos <- dataLoader(file.path(getwd(), "/testData/excel_input.xlsx"))
datos <- datos[['header']]





