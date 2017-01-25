# server.R
uca_admit <- UCBAdmissions 
shinyServer(function(input, output) {
     output$graph_vars <- renderText(paste("Admit ~", input$x))
    output$plotgraphs <- renderPlot(
        plot(as.formula(paste("Admit ~",input$x)),data=uca_admit))
})
