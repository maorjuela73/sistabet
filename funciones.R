library(scales)

notas <- read_xlsx(path = "www/input/data.xlsx",
                   col_types = c("text",
                                  "text",
                                  "text",
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
                                  "numeric"))
notas

# Recibe una tibble de notas y entrega la misma filtrada por variables entregadas como parámetro
# Input: inotas: base de datos de notas, icurso: curso a filtrar, isemestre: semestre a filtrar, ivar: variable a filtrar (outcome o nota_final)
# output: tibble de una sola columna con los datos filtrados. NA en caso de no encontrar datos
retriever <- function(inotas, iarea = "Total", icurso = "Total", isemestre = "Total", ivar){
  notasfiltradas <- inotas 
  if(iarea != "Total")
  {
    notasfiltradas <- notasfiltradas %>% filter(area == iarea)
  }
  if(icurso != "Total")
  {
    notasfiltradas <- notasfiltradas %>% filter(curso == icurso)
  }
  if(isemestre != "Total")
  {
    notasfiltradas <- notasfiltradas %>% filter(semestre == isemestre)
  }  
  notasfiltradas <- notasfiltradas %>% select(calificacion = ivar) %>% 
    na.omit()
  return(notasfiltradas)
}

prueba <- retriever(inotas = notas, isemestre = "201710", ivar = "H")

ggplot(prueba, aes(x = calificacion)) + 
  geom_histogram(aes(y=..density..), bins = ceiling(sqrt(length(datos)))) + 
  geom_density(size= 1.0, colour = "red", alpha = 1) + 
  scale_y_continuous(name = "Densidad", limits = c(0, 1), sec.axis = sec_axis(~.*nrow(datosplot), name = "Cantidad de observaciones", breaks = c(seq(0,nrow(datosplot),by = 100),nrow(datosplot)))) +
  xlab("Calificaciones")+
  theme_minimal()

ggplot(prueba, aes(x = calificacion)) +
  geom_histogram(aes(y = ..density..), bins = ceiling(2*(nrow(prueba)^(1/3)))) +
  geom_density(size= 1.0, colour = "red") +
  scale_y_continuous(limits = c(0, 2), labels=scales::percent) +
  scale_x_continuous(limits = c(0, 5))

plot <- ggplot(prueba, aes(x = calificacion)) +
  geom_histogram(aes(y = ..count..), bins = ceiling(2*(nrow(prueba)^(1/3))))

plot

h<-hist(prueba$calificacion, breaks = 25)

ggplot(notas, aes(x = A)) +
  geom_histogram(aes(y = ..density..), bins = ceiling(2*(nrow(prueba)^(1/3)))) +
  geom_density(size= 1.0, colour = "red") +
  scale_y_continuous(limits = c(0, 1), labels=scales::percent) +
  scale_x_continuous(limits = c(0, 5)) +
  facet_grid(semestre ~ curso)

plot <- ggplot(prueba, aes(x = calificacion)) +
  geom_histogram(aes(y = ..count..), bins = ceiling(2*(nrow(prueba)^(1/3))))

plot <- ggplot(notas, aes(x = A)) +
  geom_histogram(aes(y = ..density..), bins = ceiling(2*(nrow(prueba)^(1/3))))+
  geom_density(size= 1.0, colour = "red") 

plot

summary(prueba)
var(prueba$calificacion)
sd(prueba$calificacion)  

ggplot(notas, aes(x = A)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(size= 1.0, colour = "red") +
  facet_grid(semestre ~ curso)


