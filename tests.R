x <- c(17,19,20,22,23,25,26,27,28,29,31,33,34,37,40,42,44,48,50,58)
x
# Medidas de tendencia central
mean(x)
median(x)
t <- table(x)
t
max(x)-min(x)
var(x)*19/20
sd(x)
sd(x)/mean(x)
boxplot(x)
quantile(x,c(0.25,0.50,0.75))
choose(3,2)
a <- c('c1','c2','x1','x2','x3')
combn(a,3)
