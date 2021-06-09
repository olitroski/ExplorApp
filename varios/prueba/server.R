#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)
library(glue)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Cargar el archvio
    xls <- reactive({
        if (is.null(input$fileToLoad)){
            NULL
        } else {
            read.xlsx(input$fileToLoad[["datapath"]][1])
        }
        
    })

    # cargar las variables
    output$listaVariables <- renderUI({
        listaBotones <- list()
        var <- names(xls())
        
        for (v in var){
            listaBotones[[v]] <- glue( "div(class='btnvariable', style='padding-bottom:15px',
                                            actionButton('{v}', lab = '{v}', width = '100%', class='olakase'))" )
        }
        
        lapply(listaBotones, function(x) eval(parse(text=x)))
    })
    
    # El nombre de la variable clicada
    # observeEvent(input$js.button_clicked, {
    #     uid = str_split(input$js.button_clicked, "_")
    #     button = uid[[1]][1]
    #     n = uid[[1]][2]
    #     # for debugging...
    #     
    #     print(paste0(button, " clicked on row ", n))
    #     
    #     switch(button,
    #            "view" = {...},
    #            ...
    #     )
    
    # Mostarr base
    output$loadedFileInfo <- renderPrint(
        # xls()
        input$js.button_clicked
    )

})
