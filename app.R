#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Librerias utilizadas
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(aws.s3)

#Lectura de bases de datos de usuarios y cursos
profesores <- read_xlsx("www/input/Usuarios y Cursos.xlsx", sheet = "Informacion profesores" ) 
asistentes <- read_xlsx("www/input/Usuarios y Cursos.xlsx", sheet = "Asistentes" )
cursos <- read_xlsx("www/input/Usuarios y Cursos.xlsx", sheet = "Cursos" )

#Definición de áreas de cursos
cursos_general <- filter(cursos, Área == 'General')
cursos_invops <- filter(cursos, Área == 'Investigación de Operaciones')
cursos_prolog <- filter(cursos, Área == 'Producción y Logística')
cursos_gestorg <- filter(cursos, Área == 'Gestión de Organizaciones')
cursos_ecofin <- filter(cursos, Área == 'Economía y Finanzas')

#Lectura de bases de datos de notas para indicadores
notas <- read_xlsx("www/input/data.xlsx",
                "clean",
                col_types = c("numeric", 
                              "numeric",
                              "text",
                              "text",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric",
                              "numeric"
                              )
)

# Estadísticos-------------------------------------------------------------------
lista <- notas %>% filter(semestre == "201610" & curso == "IIND2401" & nombre_curso == "Adec") %>% select(K) %>% unlist()
descriptivos <- function(lista){
  datos <- lista %>% na.omit()
  resumen <- summary(datos) %>% unlist()
  varianza <- var(datos)
  desviacion <- sd(datos)
  grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
  intervalos <- table(grupos) %>% as.data.frame()
  datosplot <- data.frame(notas = datos)
  grafico <- ggplot(datosplot, aes(x = notas)) + 
    geom_histogram(aes(y=..density..), bins = ceiling(sqrt(length(datos)))) + 
    geom_density(size= 1.0, colour = "red", alpha = 1) + 
    scale_y_continuous(name = "Densidad", limits = c(0, 1), sec.axis = sec_axis(~.*nrow(datosplot), name = "Cantidad de observaciones", breaks = c(seq(0,nrow(datosplot),by = 100),nrow(datosplot)))) +
    xlab("Calificaciones")+
    theme_minimal()
  
  retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
  return(list(numericas = retorno, intervalos = intervalos, grafico = grafico))
}

# ggplot(datosplot, aes(x = notas)) + 
#   geom_histogram(aes(y=..density..), bins = ceiling(sqrt(length(datos)))) + 
#   geom_density(size= 1.0, colour = "red", alpha = 1) + 
#   scale_y_continuous(name = "Densidad", limits = c(0, 1), sec.axis = sec_axis(~.*nrow(datosplot), name = "Cantidad de observaciones", breaks = c(seq(0,nrow(datosplot),by = 100),nrow(datosplot)))) +
#   xlab("Calificaciones")+
#   theme_minimal()

retornoprueba <- descriptivos(lista)
retornoprueba$grafico

# Esquema de la Heramienta
ui <- ui <- dashboardPage(
  dashboardHeader(title = "HeMCo"),
  # dashboardHeader(title = tags$a(href='http://mycompanyishere.com',
  #                                tags$img(src='images/logo.png'))),
  # dashboardHeader(title = "My Dashboard",
  #                 tags$li(a(href = 'http://shinyapps.company.com',
  #                           icon("power-off"),
  #                           title = "Back to Apps Home"),
  #                         class = "dropdown"),
  #                 tags$li(a(href = 'http://www.company.com',
  #                           img(src = 'images/logo.png',
  #                               title = "Company Home", height = "30px"),
  #                           style = "padding-top:10px; padding-bottom:10px;"),
  #                         class = "dropdown")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("window-restore")),
      menuItem("Información", tabName = "informacion", icon = icon("fal fa-info-circle")),
      menuItem("Resultados", 
               tabName = "resultados", 
               icon = icon("folder-open"),
               menuSubItem('Cursos',
                           tabName = 'resultados_cursos',
                           icon = icon('line-chart')),
               menuSubItem('Outcomes',
                           tabName = 'resultados_outcomes',
                           icon = icon('line-chart')),
               menuSubItem('Departamento',
                           tabName = 'resultados_departamento',
                           icon = icon('line-chart'))
               ),
      menuItem("Reportes", tabName = "reportes", icon = icon("fas fa-book")),
      menuItem("Registro", tabName = "registro", icon = icon("edit"),
               menuSubItem('Datos',
                           tabName = 'registro-datos',
                           icon = icon('list-ol')),
               menuSubItem('Usuarios',
                           tabName = 'registro-usuarios',
                           icon = icon('address-card'))
               ),
      menuItem("Encuestas", tabName = "encuestas", icon = icon("far fa-comment"),
               menuSubItem('Estudiantes',
                           tabName = 'encuestas_estudiantes',
                           icon = icon('fas fa-id-badge')),
               menuSubItem('Egresados',
                           tabName = 'encuestas_egresados',
                           icon = icon('fas fa-graduation-cap'))
               ),
      menuItem("GMC", tabName = "gmc", icon = icon("fas fa-check-circle"),
               menuSubItem('Actas',
                           tabName = 'actas',
                           icon = icon('far fa-file')),
               menuSubItem('Día ABET',
                           tabName = 'dia_ABET',
                           icon = icon('far fa-calendar')),
               menuSubItem('Talleres',
                           tabName = 'talleres',
                           icon = icon('fas fa-users'))
      ),
      menuItem("Ayuda", tabName = "ayuda", icon = icon("question-circle"))
    )
  ),
  # Body --------------------------------------------------------------------
  dashboardBody(
    tabItems(
    # Reportes---------------------------------------------------------------  
      tabItem(tabName = "reportes",
              fluidRow(
                  tabBox(title = "Áreas",
                         id = "tabset_rareas",
                         width = 12,
                         tabPanel("General", 
                                  uiOutput("cajas_rgeneral")
                         ),
                         tabPanel("Investigación de Operaciones", 
                                  uiOutput("cajas_rinvops")
                         ),
                         tabPanel("Producción y Logística", 
                                  uiOutput("cajas_rprolog")
                         ),
                         tabPanel("Gestión de Organizaciones",
                                  uiOutput("cajas_rgestorg")
                         ),
                         tabPanel("Economía y Finanzas",
                                  uiOutput("cajas_recofin")
                         )
                  )
                )
      ),
    # Inicio --------------------------------------------------------------
      tabItem(tabName = "inicio",
              imageOutput("imagenInicio", height = 300)
      ),
    
    # Información --------------------------------------------------------------
    tabItem(tabName = "informacion",
            imageOutput("imagenInformacion", height = 300)
      ),
    
    # Resultados Cursos (Estructura) --------------------------------------------------------------
      tabItem(tabName = "resultados_cursos",
              h2("Resultados por área"),
              fluidRow(
                tabBox(title = "Áreas",
                       id = "tabset_areas",
                       width = 12,
                       tabPanel("General", 
                                # dataTableOutput('table_prueba'),
                                uiOutput("cajas_general")
                        ),
                       tabPanel("Investigación de Operaciones", 
                                uiOutput("cajas_invops")
                        ),
                       tabPanel("Producción y Logística", 
                                uiOutput("cajas_prolog")
                        ),
                       tabPanel("Gestión de Organizaciones",
                                uiOutput("cajas_gestorg")
                        ),
                       tabPanel("Economía y Finanzas",
                                uiOutput("cajas_ecofin")
                        )
                )
              )
      ),
    
    # Resultados Outcomes (Estructura) --------------------------------------------------------------
      tabItem(tabName = "resultados_outcomes",
              h2("Resutados por Outcome"),
              fluidRow(
                tabBox(title = "Outcomes",
                       id = "tabset_outcomes",
                       width = 12,
                       tabPanel("A", uiOutput("cajas_outcome_a")),
                       tabPanel("B", uiOutput("cajas_outcome_b")),
                       tabPanel("C", uiOutput("cajas_outcome_c")),
                       tabPanel("D", uiOutput("cajas_outcome_d")),
                       tabPanel("E", uiOutput("cajas_outcome_e")),
                       tabPanel("F", uiOutput("cajas_outcome_f")),
                       tabPanel("G", uiOutput("cajas_outcome_g")),
                       tabPanel("H", uiOutput("cajas_outcome_h")),
                       tabPanel("I", uiOutput("cajas_outcome_i")),
                       tabPanel("J", uiOutput("cajas_outcome_j")),
                       tabPanel("K", uiOutput("cajas_outcome_k"))

                )
              )
      ),
    
    # Registro ----------------------------------------------------------------------
      tabItem(tabName = "registro-datos",
              h2("Registro de datos"),
              selectInput("registro-datos-semestre",
                          label = "Semestre",
                          choices = c("201610","201620","201710")),
              selectInput("registro-datos-curso",
                          label = "Curso",
                          choices = c("IIND2401",
                                      "IIND3400",
                                      "IIND2400",
                                      "IIND2104",
                                      "IIND2103",
                                      "IIND2107",
                                      "IIND3113",
                                      "IIND2106",
                                      "IIND3221",
                                      "IIND2201",
                                      "IIND2202",
                                      "IIND1000",
                                      "IIND3311",
                                      "IIND2301",
                                      "IIND2302")),
              selectInput("registro-datos-area",
                          label = "Área",
                          choices = c("Área 1",
                                      "Área 2",
                                      "Área 3")),
              fileInput("input_datos", 
                        "Escoja el archivo a cargar",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".xlsx",".xlsm")),
              tableOutput("contents")
      ),

      tabItem(tabName = "registro-usuarios",
              h2("Registro de usuario"),
              textInput("registro-usuario-nombre",
                        label = "Nombres y apellidos"),
              textInput("registro-usuario-codigo",
                        label = "Código"),
              textInput("registro-usuario-login",
                        label = "Login uniandes (sin @uniandes.edu.co)"),
              selectInput("registro-usuario-perfil",
                        label = "Perfil",
                        choices = c("Asistente","Profesor","Administrador")),
              actionButton("registro-usuario-perfil",
                           label = "Crear usuario")
      ),
    
    # Encuestas -------------------------------------------------------------------------------------  
      tabItem(tabName = "encuestas_estudiantes",
            imageOutput("imagenEncuestasEs", height = 300)
      ),
      tabItem(tabName = "encuestas_egresados",
            imageOutput("imagenEncuestasEg", height = 300)
      ),
    # GMC -------------------------------------------------------------------------------------  
      tabItem(tabName = "actas",
              fluidPage(
                fluidRow(
                  column(width = 7,
                         box(width = 12,
                             h1("Archivos cargados")
                         )
                  ),
                  column(width = 5,
                         box(width = 12, 
                             background = "light-blue", 
                             h2("Carga un archivo"),
                             fileInput("file1", "Escoja archivo a cargar",
                                       multiple = TRUE,
                                       accept = c(".pdf",
                                                  ".doc",
                                                  ".docx",
                                                  ".xls",
                                                  ".xlsx"))
                             )
                         )
                )
              )
      ),
    
    tabItem(tabName = "ayuda",
            h2("Ayuda"),
            fluidRow(
              column(width = 12,
                     box(title = "Status summary 1",
                         width = 12,
                         background = "red"),
                     box(title = "Status summary 2",
                         width = 12,
                         background = "red"),
                     box(title = "Status summary 3",
                         width = 12,
                         background = "red"),
                     box(title = "Status summary 4",
                         width = 12,
                         background = "red"),
                     box(title = "Status summary 5",
                         width = 12,
                         background = "red"),
                     box(title = "Status summary 6",
                         width = 12,
                         background = "red"),
                     box(title = "Status summary 7",
                         width = 12,
                         background = "red")
              )
            )
      )
    )
  )
)

#Publicación visual en HeMCo--------------------------------------------------------------------
server <- function(input, output) {
  output$contents <- renderTable(
    {
      req(input$input_datos)
      
      df <- read_xlsx("www/input/data.xlsx",
                      "clean",
                      col_types = c("numeric", 
                                    "numeric",
                                    "text",
                                    "text",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric"
                      )
      )
      return(head(df))
    }
  )

  # Pestaña Inicio-------------------------------------------
  output$imagenInicio <- renderImage({
    return(list(
      src = "www/images/inicio.png",
      contentType = "image/png",
      alt = "info1"
    ))
  }, deleteFile = FALSE)

  # Pestaña Información--------------------------------------
  output$imagenInformacion <- renderImage({
    return(list(
      src = "www/images/infor.png",
      contentType = "image/png",
      alt = "info2"
    ))
  }, deleteFile = FALSE)
  
  # Pestaña Encuestas ---------------------------------------
  output$imagenEncuestas <- renderImage({
    return(list(
      src = "www/images/infografía3.png",
      contentType = "image/png",
      alt = "info3"
    ))
  }, deleteFile = FALSE)
  
  # Resultados Área general -----------------------------------------
  output$cajas_general <- renderUI({
    numcajas <- as.integer(nrow(cursos_general))
    lapply(1:numcajas, function(i) 
    {
      local
      ({
        output[[paste0("plot",cursos_general[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_general[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_general[i,'Código curso'])]] <- renderTable(
          colnames = FALSE, striped = TRUE, align = "c", width = "auto", {
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_general[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          t(retornoprueba$numericas)
        })
        output[[paste0("Intervalos",cursos_general[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_general[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_general", i), 
          title = paste0(cursos_general[i,'Código curso'],"-",cursos_general[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_general[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_general[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_general[i,'Código curso']))
        )
      })
    })
    
  })

  # Resultados Área Investigación de Operaciones -----------------------------------------
  output$cajas_invops <- renderUI({
    numcajas <- as.integer(nrow(cursos_invops))
    lapply(1:numcajas, function(i) 
    {
      local
      ({
        output[[paste0("plot",cursos_invops[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_invops[i,'Código curso'])]] <- renderTable(
          colnames = FALSE, striped = TRUE, align = "c", width = "auto", {
            lista <- notas %>% filter(semestre == "201710" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
            retornoprueba <- descriptivos(lista)
            t(retornoprueba$numericas)
          })
        output[[paste0("Intervalos",cursos_invops[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_invops", i), 
          title = paste0(cursos_invops[i,'Código curso'],"-",cursos_invops[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_invops[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_invops[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_invops[i,'Código curso']))
        )
      })
      # local({
      # output[[paste0("plot",cursos_invops[i,'Código curso'])]] <- renderPlot({
      #   lista <- notas %>% filter(semestre == "201610" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
      #   descriptivos <- function(lista){
      #     datos <- lista %>% na.omit()
      #     resumen <- summary(datos) %>% unlist()
      #     varianza <- var(datos)
      #     desviacion <- sd(datos)
      #     grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
      #     intervalos <- table(grupos) %>% as.data.frame()
      #     datosplot <- data.frame(notas = datos)
      #     grafico <- ggplot(datosplot, aes(x = notas)) + geom_histogram(aes(y=..density..), bins = ceiling(sqrt(length(datos)))) + geom_density()
      #     retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
      #     return(list(numericas = retorno, intervalos = intervalos, grafico = grafico))
      #   }
      #   
      #   retornoprueba <- descriptivos(lista)
      #   retornoprueba$numericas
      #   retornoprueba$intervalos
      #   retornoprueba$grafico
      #   
      # })
      
      # output[[paste0("Estadísticas",cursos_invops[i,'Código curso'])]] <- renderTable({
      #   lista <- notas %>% filter(semestre == "201610" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
      #   descriptivos <- function(lista){
      #     datos <- lista %>% na.omit()
      #     resumen <- summary(datos) %>% unlist()
      #     varianza <- var(datos)
      #     desviacion <- sd(datos)
      #     grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
      #     intervalos <- table(grupos) %>% as.data.frame()
      #     datosplot <- data.frame(notas = datos)
      #     retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
      #     return(list(numericas = retorno, intervalos = intervalos))
      #   }
      #   
      #   retornoprueba <- descriptivos(lista)
      #   t(retornoprueba$numericas)
      # },
      # colnames = FALSE, striped = TRUE)
      # 
      # output[[paste0("Intervalos",cursos_invops[i,'Código curso'])]] <- renderTable({
      #   lista <- notas %>% filter(semestre == "201610" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
      #   descriptivos <- function(lista){
      #     datos <- lista %>% na.omit()
      #     resumen <- summary(datos) %>% unlist()
      #     varianza <- var(datos)
      #     desviacion <- sd(datos)
      #     grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
      #     intervalos <- table(grupos) %>% as.data.frame()
      #     datosplot <- data.frame(notas = datos)
      #     retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
      #     return(list(numericas = retorno, intervalos = intervalos))
      #   }
      #   
      #   retornoprueba <- descriptivos(lista)
      #   retornoprueba$intervalos
      #   
      # })
      # 
      # })
      # box(inputId = paste0("caja_invops", i), 
      #     title = paste0(cursos_invops[i,'Código curso'],"-",cursos_invops[i,'Curso']),
      #     collapsible = TRUE, collapsed = TRUE,
      #     plotOutput(paste0("plot", cursos_invops[i,'Código curso'])),
      #     tableOutput(paste0("Estadísticas", cursos_invops[i,'Código curso'])),
      #     tableOutput(paste0("Intervalos", cursos_invops[i,'Código curso']))
      # )
    
    })
    
  })
  
  # Resultados Área Producción y Logística -----------------------------------------
  output$cajas_prolog <- renderUI({
    numcajas <- as.integer(nrow(cursos_prolog))
    lapply(1:numcajas, function(i) 
    {
      local
      ({
        output[[paste0("plot",cursos_prolog[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_prolog[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_prolog[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_prolog[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
        })
        output[[paste0("Intervalos",cursos_prolog[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_prolog[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_prolog", i), 
          title = paste0(cursos_prolog[i,'Código curso'],"-",cursos_prolog[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_prolog[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_prolog[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_prolog[i,'Código curso']))
        )
      })
    })
    
  })
  
  # Resultados Área Gestión Organizacional -----------------------------------------
  output$cajas_gestorg <- renderUI({
    numcajas <- as.integer(nrow(cursos_gestorg))
    lapply(1:numcajas, function(i) 
      {
      local
      ({
        output[[paste0("plot",cursos_gestorg[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_gestorg[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_gestorg[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_gestorg[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
        })
        output[[paste0("Intervalos",cursos_gestorg[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_gestorg[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_gestorg", i), 
          title = paste0(cursos_gestorg[i,'Código curso'],"-",cursos_gestorg[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_gestorg[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_gestorg[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_gestorg[i,'Código curso']))
        )
      })
    })
    
  })
  
  # Resultados Área Economía y Finanzas -----------------------------------------
  output$cajas_ecofin <- renderUI({
    numcajas <- as.integer(nrow(cursos_ecofin))
    lapply(1:numcajas, function(i) 
    {
      local({
        output[[paste0("plot",cursos_ecofin[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_ecofin[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_ecofin[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_ecofin[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
        })
        output[[paste0("Intervalos",cursos_ecofin[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_ecofin[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_ecofin", i), 
          title = paste0(cursos_ecofin[i,'Código curso'],"-",cursos_ecofin[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_ecofin[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_ecofin[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_ecofin[i,'Código curso']))
        )
      }) 
    })
    
  })
  
  # Resultados Outcomes-----------------------------------------------
  # A-----------------------------------------------------------------
  output$cajas_outcome_a <- renderUI({
    local({
      output[[paste0("plot","outcome_a")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(A) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_a")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(A) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_a")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(A) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "a"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_a")),
        tableOutput(paste0("Estadísticas", "outcome_a")),
        tableOutput(paste0("Intervalos", "outcome_a"))
      )
    })
  })

  # B-----------------------------------------------------------------
  output$cajas_outcome_b <- renderUI({
    local({
      output[[paste0("plot","outcome_b")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(B) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_b")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(B) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_b")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(B) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "b"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_b")),
        tableOutput(paste0("Estadísticas", "outcome_b")),
        tableOutput(paste0("Intervalos", "outcome_b"))
      )
    })
  })
  
  # C-----------------------------------------------------------------
  output$cajas_outcome_c <- renderUI({
    local({
      output[[paste0("plot","outcome_c")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(C) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_c")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(C) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_c")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(C) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "c"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_c")),
        tableOutput(paste0("Estadísticas", "outcome_c")),
        tableOutput(paste0("Intervalos", "outcome_c"))
      )
    })
  })
  
  # D-----------------------------------------------------------------
  output$cajas_outcome_d <- renderUI({
    local({
      output[[paste0("plot","outcome_d")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(D) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_d")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(D) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_d")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(D) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "d"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_d")),
        tableOutput(paste0("Estadísticas", "outcome_d")),
        tableOutput(paste0("Intervalos", "outcome_d"))
      )
    })
  })
  
  # E-----------------------------------------------------------------
  output$cajas_outcome_e <- renderUI({
    local({
      output[[paste0("plot","outcome_e")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(E) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_e")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(E) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_e")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(E) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "e"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_e")),
        tableOutput(paste0("Estadísticas", "outcome_e")),
        tableOutput(paste0("Intervalos", "outcome_e"))
      )
    })
  })
  
  # F-----------------------------------------------------------------
  output$cajas_outcome_f <- renderUI({
    local({
      output[[paste0("plot","outcome_f")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(F) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_f")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(F) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_f")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(F) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "f"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_f")),
        tableOutput(paste0("Estadísticas", "outcome_f")),
        tableOutput(paste0("Intervalos", "outcome_f"))
      )
    })
  })
  
  # G-----------------------------------------------------------------
  output$cajas_outcome_g <- renderUI({
    local({
      output[[paste0("plot","outcome_g")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(G) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_g")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(G) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_g")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(G) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "g"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_g")),
        tableOutput(paste0("Estadísticas", "outcome_g")),
        tableOutput(paste0("Intervalos", "outcome_g"))
      )
    })
  })
  
  # H-----------------------------------------------------------------
  output$cajas_outcome_h <- renderUI({
    local({
      output[[paste0("plot","outcome_h")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(H) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_h")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(H) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_h")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(H) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "h"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_h")),
        tableOutput(paste0("Estadísticas", "outcome_h")),
        tableOutput(paste0("Intervalos", "outcome_h"))
      )
    })
  })
  
  # I-----------------------------------------------------------------
  output$cajas_outcome_i <- renderUI({
    local({
      output[[paste0("plot","outcome_i")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(I) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_i")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(I) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_i")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(I) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "i"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_i")),
        tableOutput(paste0("Estadísticas", "outcome_i")),
        tableOutput(paste0("Intervalos", "outcome_i"))
      )
    })
  })
  
  # J-----------------------------------------------------------------
  output$cajas_outcome_j <- renderUI({
    local({
      output[[paste0("plot","outcome_j")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(J) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_j")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(J) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_j")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(J) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "j"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_j")),
        tableOutput(paste0("Estadísticas", "outcome_j")),
        tableOutput(paste0("Intervalos", "outcome_j"))
      )
    })
  })
  
  # K-----------------------------------------------------------------
  output$cajas_outcome_k <- renderUI({
    local({
      output[[paste0("plot","outcome_k")]] <- renderPlot({
        lista <- notas %>%
          filter(semestre == "201710") %>%
          select(K) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$grafico
      })
      output[[paste0("Estadísticas","outcome_k")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(K) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$numericas
      })
      output[[paste0("Intervalos","outcome_k")]] <- renderTable({
        lista <- notas %>% filter(semestre == "201710") %>%
          select(K) %>%
          unlist()
        retornoprueba <- descriptivos(lista)
        retornoprueba$intervalos
      })
      box(
        inputId = paste0("caja_outcome_", "k"),
        title = paste0("Principal"),
        collapsible = TRUE, collapsed = FALSE,
        plotOutput(paste0("plot", "outcome_k")),
        tableOutput(paste0("Estadísticas", "outcome_k")),
        tableOutput(paste0("Intervalos", "outcome_k"))
      )
    })
  })
  
  # output[["plotIIND 2104"]] <- renderPlot({
  #   lista <- notas %>% filter(semestre == "201610" & curso == "IIND2104" & nombre_curso == "Modelos Probabilísticos") %>% select(A) %>% unlist()
  #   descriptivos <- function(lista){
  #     datos <- lista %>% na.omit()
  #     resumen <- summary(datos) %>% unlist()
  #     varianza <- var(datos)
  #     desviacion <- sd(datos)
  #     grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
  #     intervalos <- table(grupos) %>% as.data.frame()
  #     datosplot <- data.frame(notas = datos)
  #     grafico <- ggplot(datosplot, aes(x = notas)) + geom_histogram(aes(y=..density..), bins = 24) + geom_density()
  #     retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
  #     return(list(numericas = retorno, intervalos = intervalos, grafico = grafico))
  #   }
  #   
  #   retornoprueba <- descriptivos(lista)
  #   retornoprueba$numericas
  #   retornoprueba$intervalos
  #   retornoprueba$grafico
  #   
  # })
  # 
  # output[["EstadísticasIIND 2104"]] <- renderTable({
  #   lista <- notas %>% filter(semestre == "201610" & curso == "IIND2104" & nombre_curso == "Modelos Probabilísticos") %>% select(A) %>% unlist()
  #   descriptivos <- function(lista){
  #     datos <- lista %>% na.omit()
  #     resumen <- summary(datos) %>% unlist()
  #     varianza <- var(datos)
  #     desviacion <- sd(datos)
  #     grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
  #     intervalos <- table(grupos) %>% as.data.frame()
  #     datosplot <- data.frame(notas = datos)
  #     retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
  #     return(list(numericas = retorno, intervalos = intervalos))
  #   }
  #   
  #   retornoprueba <- descriptivos(lista)
  #   retornoprueba$numericas
  #   
  # })
  

# Cajas Reportes ----------------------------------------------------------

  output$cajas_rgeneral <- renderUI({
    numcajas <- as.integer(nrow(cursos_general))
    lapply(1:numcajas, function(i) 
    {
      local({
        output[[paste0("plot",cursos_general[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_general[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_general[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_general[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
        })
        output[[paste0("Intervalos",cursos_general[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_general[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_rgeneral", i), 
          title = paste0(cursos_general[i,'Código curso'],"-",cursos_general[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_general[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_general[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_general[i,'Código curso']))
        )
      })
    })
    
  })
  
  output$cajas_rinvops <- renderUI({
    numcajas <- as.integer(nrow(cursos_invops))
    lapply(1:numcajas, function(i) {
      local({
        output[[paste0("plot",cursos_invops[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% filter(semestre == "201610" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          descriptivos <- function(lista){
            datos <- lista %>% na.omit()
            resumen <- summary(datos) %>% unlist()
            varianza <- var(datos)
            desviacion <- sd(datos)
            grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
            intervalos <- table(grupos) %>% as.data.frame()
            datosplot <- data.frame(notas = datos)
            grafico <- ggplot(datosplot, aes(x = notas)) + geom_histogram(aes(y=..density..), bins = ceiling(sqrt(length(datos)))) + geom_density()
            retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
            return(list(numericas = retorno, intervalos = intervalos, grafico = grafico))
          }
          
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
          retornoprueba$intervalos
          retornoprueba$grafico
          
        })
        
        output[[paste0("Estadísticas",cursos_invops[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201610" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          descriptivos <- function(lista){
            datos <- lista %>% na.omit()
            resumen <- summary(datos) %>% unlist()
            varianza <- var(datos)
            desviacion <- sd(datos)
            grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
            intervalos <- table(grupos) %>% as.data.frame()
            datosplot <- data.frame(notas = datos)
            retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
            return(list(numericas = retorno, intervalos = intervalos))
          }
          
          retornoprueba <- descriptivos(lista)
          t(retornoprueba$numericas)
        },
        colnames = FALSE, striped = TRUE)
        
        output[[paste0("Intervalos",cursos_invops[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201610" & curso == cursos_invops[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          descriptivos <- function(lista){
            datos <- lista %>% na.omit()
            resumen <- summary(datos) %>% unlist()
            varianza <- var(datos)
            desviacion <- sd(datos)
            grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
            intervalos <- table(grupos) %>% as.data.frame()
            datosplot <- data.frame(notas = datos)
            retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
            return(list(numericas = retorno, intervalos = intervalos))
          }
          
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
          
        })
        
      })
      box(inputId = paste0("caja_invops", i), 
          title = paste0(cursos_invops[i,'Código curso'],"-",cursos_invops[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_invops[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_invops[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_invops[i,'Código curso']))
      )
      
    })
    
  })
  output$cajas_rprolog <- renderUI({
    numcajas <- as.integer(nrow(cursos_prolog))
    lapply(1:numcajas, function(i) 
    {
      local({
        output[[paste0("plot",cursos_prolog[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_prolog[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_prolog[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_prolog[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
        })
        output[[paste0("Intervalos",cursos_prolog[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_prolog[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_prolog", i), 
          title = paste0(cursos_prolog[i,'Código curso'],"-",cursos_prolog[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_prolog[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_prolog[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_prolog[i,'Código curso']))
        )
      })
    })
    
  })
  output$cajas_rgestorg <- renderUI({
    numcajas <- as.integer(nrow(cursos_gestorg))
    lapply(1:numcajas, function(i) 
    {
      local({
        output[[paste0("plot",cursos_gestorg[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_gestorg[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_gestorg[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_gestorg[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
        })
        output[[paste0("Intervalos",cursos_gestorg[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_gestorg[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_gestorg", i), 
          title = paste0(cursos_gestorg[i,'Código curso'],"-",cursos_gestorg[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_gestorg[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_gestorg[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_gestorg[i,'Código curso']))
        )
      })
    })
    
  })
  output$cajas_recofin <- renderUI({
    numcajas <- as.integer(nrow(cursos_ecofin))
    lapply(1:numcajas, function(i) 
    {
      local({
        output[[paste0("plot",cursos_ecofin[i,'Código curso'])]] <- renderPlot({
          lista <- notas %>% 
            filter(semestre == "201710" & curso == cursos_ecofin[i,'Código curso'][["Código curso"]]) %>% 
            select(A) %>% 
            unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$grafico
        })
        output[[paste0("Estadísticas",cursos_ecofin[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_ecofin[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$numericas
        })
        output[[paste0("Intervalos",cursos_ecofin[i,'Código curso'])]] <- renderTable({
          lista <- notas %>% filter(semestre == "201710" & curso == cursos_ecofin[i,'Código curso'][["Código curso"]]) %>% select(A) %>% unlist()
          retornoprueba <- descriptivos(lista)
          retornoprueba$intervalos
        })
        box(
          inputId = paste0("caja_ecofin", i), 
          title = paste0(cursos_ecofin[i,'Código curso'],"-",cursos_ecofin[i,'Curso']),
          collapsible = TRUE, collapsed = TRUE,
          plotOutput(paste0("plot", cursos_ecofin[i,'Código curso'])),
          tableOutput(paste0("Estadísticas", cursos_ecofin[i,'Código curso'])),
          tableOutput(paste0("Intervalos", cursos_ecofin[i,'Código curso']))
        )
      }) 
    })
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

