library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram

shinyServer(function(input, output) {
  output$plots <- renderUI({
    listaUI = list()
    for (i in 8:length(notas)) {
      listaUI = list(listaUI, h3(paste("Pregunta", names(notas)[i])),
                     selectInput(paste("option",i,sep = ""), label = "Seleccione un filtro", c("Ninguno",names(notas)[6:7])),
                     selectInput(paste("type",i,sep = ""), label = "Seleccione un tipo de grafico", c("Histograma","Cajas")),
                     plotOutput(paste("plot",i,sep = "")),
                     verbatimTextOutput(paste("summary",i,sep = "")))
      local({
        column = i
        output[[paste("plot",column,sep = "")]] <- renderPlot({
          if (input[[paste("type",column,sep = "")]] == "Histograma") {
            p = ggplot(notas, aes(notas[[names(notas)[column]]])) + geom_histogram()
            if (input[[paste("option",column,sep = "")]] == "Profesor") {
              p = p + facet_grid(~Profesor)
            }
            if (input[[paste("option",column,sep = "")]] == "Programa") {
              p = p + facet_grid(~Programa)
            }
          }
          if (input[[paste("type",column,sep = "")]] == "Cajas") {
            p = ggplot(notas, aes(y = notas[[names(notas)[column]]],x = notas$ID)) + geom_boxplot()
            if (input[[paste("option",column,sep = "")]] == "Profesor") {
              p = ggplot(notas, aes(y = notas[[names(notas)[column]]],x = factor(notas$Profesor))) + geom_boxplot()
            }
            if (input[[paste("option",column,sep = "")]] == "Programa") {
              p = ggplot(notas, aes(y = notas[[names(notas)[column]]],x = factor(notas$Programa))) + geom_boxplot()
            }
          } 
          return(p)
        })
        output[[paste("summary",column,sep = "")]] <- renderPrint({
          summary(notas[[names(notas)[column]]])
        })
      })
    }
    return(listaUI)
  })
})