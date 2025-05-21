# Para regressão vamos trabalhar com o conjunto Iris
head(iris)


plot(iris$Sepal.Length, iris$Petal.Length)

modelo <- lm(iris$Petal.Length ~ iris$Sepal.Length)
modelo <- lm(Petal.Length ~ Sepal.Length, data = iris)
plot(modelo)
residuo <- modelo$residuals
residuo
summary(iris)
mediana <- median(residuo)
mediana
menor <- residuo[residuo < mediana]
maior <- residuo[residuo > mediana]

var.test(menor, maior)

plot(modelo)
shapiro.test(residuo)

summary(modelo)


# Gráfico
pacman::p_load(tidyverse, ggpubr)

ggscatter(
  iris,  # conjunto de dados
  y = "Petal.Length", # variável y
  x = "Sepal.Length",  # variável x
  xlab = "Comprimento de Sépala (cm)",
  ylab = "Comprimento de Pétala (cm)",
  color = "Species"
  )

names(iris)

modelo2 <- lm(Petal.Length ~ 
              Sepal.Length*Sepal.Width*Species , 
              data = iris)
summary(modelo2)
step(modelo2)



