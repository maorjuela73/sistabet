library(readxl)
library(dplyr)
library(ggplot2)

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
# Cambiar semestre, curso,  nombre curso y outcome acá
lista <- notas %>% filter(semestre == "201610" & curso == "IIND2104" & nombre_curso == "Modelos Probabilísticos") %>% select(A) %>% unlist()
lista <- notas %>% 
  filter(semestre == "201710") %>% 
  select(B) %>% 
  unlist()
lista  <- data.frame(notas = lista)


ggplot(lista, aes(x = notas)) +
  geom_histogram(aes(y=..density..), bins = ceiling(sqrt(length(datos)))) +
  geom_density(size= 1.0, colour = "red", alpha = 1) +
  scale_y_continuous(name = "Densidad", limits = c(0, 1), sec.axis = sec_axis(~.*nrow(datosplot), name = "Cantidad de observaciones", breaks = c(seq(0,nrow(datosplot),by = 100),nrow(datosplot)))) +
  xlab("Calificaciones")+
  theme_minimal()


descriptivos <- function(lista){
  datos <- lista %>% na.omit()
  resumen <- summary(datos) %>% unlist()
  varianza <- var(datos)
  desviacion <- sd(datos)
  grupos <- cut(datos, breaks = ceiling(sqrt(length(datos))))
  intervalos <- table(grupos) %>% as.data.frame()
  datosplot <- data.frame(notas = datos)
  grafico <- ggplot(datosplot, aes(x = notas)) + geom_histogram(aes(y=..density..), bins = 24) + geom_density()
  retorno <- data.frame( dato = c(names(resumen),"Var","Sd"), valor = c(resumen,varianza,desviacion))
  return(list(numericas = retorno, intervalos = intervalos, grafico = grafico))
}

retornoprueba <- descriptivos(lista)
retornoprueba$numericas
retornoprueba$intervalos
retornoprueba$grafico
