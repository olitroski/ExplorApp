#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    
    
    sidebarLayout(
        # Izquierda
        sidebarPanel(
            # Cargar 
            fileInput("fileToLoad", "cargar archvio"),
            hr(),
            
            # http://www.open-meta.org/technology/one-observer-for-all-buttons-in-shiny-using-javascriptjquery/
            # El script para capturar le nobmre del boton clicado
            tags$script(
                "$(document).on('click', '.btn.btn-default.action-button.olakase.shiny-bound-input', 'button', function(e) {
                    e.stopPropagation()
                    Shiny.onInputChange('js.button_clicked', e.target.id);
                });"
            ),

            # Los botones
            div(style="display:flex; flex-direction:column;",
                uiOutput("listaVariables")
            )
        ),
        
        # Main
        mainPanel(
            br(),
            actionButton("olakase", "olakase"),
            uiOutput("nombreVariable"),
            hr(),
            verbatimTextOutput("loadedFileInfo")
        )
        
    )
))
