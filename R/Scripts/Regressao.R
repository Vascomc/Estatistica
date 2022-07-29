################################################################
################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: REGRESSÃO
################################################################
################################################################

# Correlaçao e Regrressao
# A correlaçao mede a relaçao entre duas variáveis quantitativas e
# ou ordinais. Ela varia de -1 a +1, sendo que valores:
#   0 a 0,5: Correlaçao fraca
#   0,5 a 0,7: Média
#   0,7 a 0.9: Forte
#   0,9 a 1,0: Perfeita.
#   OBS.: Independe do sinal, se - ou +.

# Para variáveis com distribuiçao normal usamos a correlaçao de
# Pearson, dados nao normais, Spearman ou Kendall.
### -------------------------------------
library(tidyverse)

data("mtcars")
# Dados de Carros fabricados nos EUA, par mais detalhes ?mtcars

attach(mtcars)

# Correlação usando a função cor()
# Temos variáveis Qualitativas no conjunto de dados?
glimpse(mtcars)

cor(mtcars)
plot(cyl, mpg)


# Um pouco mais bonitinho
library(GGally)

p <- ggpairs(mtcars[, c(1:5)]) + theme_bw()
# Loop through each plot changing relevant scales
for (i in 1:p$nrow) {
  for (j in 1:p$ncol) {
    p[i, j] <- p[i, j]
  }
}
p
### -------------------------------------------
# Regressao linear simples
# Na regressao linear simples queremos ver se a variável explicativa
# explica o padrao observado na variável resposta.
# Teste de hipótese:
#   H0: O valor de b = 0;
#       Onde: Y = a +- b*X
#              a = inserçao da reta, ou valor de Y quando b = 0
#              b = inclinaçao da reta

library(ggpubr)
# Temos que avaliar no nosso conjunto de dados...
#   1. Qual a variável resposta (Y): MPG
#   2. Qual a variável explicativa (x): hp

# Vamnos avaliar por meio de um gráfico de disperssão.
ggscatter(mtcars,
  x = "hp",
  y = "mpg",
  add = "reg.line",
  conf.int = TRUE,
  add.params = list(
    color = "blue",
    fill = "lightgray"
  )
)

# O que podemos dizer sobtre esse gráfico?
# E sobre a reta?

modelo <- lm(mpg ~ hp)

# Normalidade dos Resíduos
ggdensity(modelo$residuals, fill = "lightgray")
ggqqplot(modelo$residuals)

shapiro.test(modelo$residuals) # Há problemas de normalidade

# Homogeneidade de variâncias
# Fazemos isso comparando os resíduos padronizados contra os resíduos
# observados

df <- data.frame(modelo$fitted, modelo$residuals)
ggscatter(df,
  x = "modelo.fitted",
  y = "modelo.residuals"
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  )

# podemos testar:
#   1. Vamos utilizar a mediana para separar o conjunto de
#      dados.
medina <- median(mtcars$hp) # mediana da variável X

#   2. Separamos os resíduos com base na mediana
a <- modelo$residuals[mtcars$hp > medina]
# Valores dos resíduos quando x > que a mediana
b <- modelo$residuals[mtcars$hp < medina]
# Valores dos resíduos quando x < que a mediana

#   3. Testamos usando a funçao var.test
#      H0: Variâncias sao iguais.
var.test(a, b)
# Qual o resultado????
# P > 0.05; ACEITO HO

# RESULTADO DO MODELO
modelo.log <- lm(log1p(mpg) ~ hp)
shapiro.test(modelo.log$residuals) # REJEITA H0
summary(modelo.log) # Para dados da equação da reta
summary(aov(modelo.log)) # Resultado "como uma ANOVA" usado para reportar o resultado

# Equação da RETA
# Y = 3.4877797 - 0.0032557*hp
3.4877797 - 0.0032557 * 400
3.4877797 - 0.0032557 * 40

summary(modelo)
30.09886 - 0.06823 * 400
30.09886 - 0.06823 * 40
### -------------------------------------------------------
# Regressao Multipla
# Adicionamos pelo menos uma variavel explicativa ao nosso modelo anterior

library(plotly)

plot_ly(
  type = "scatter3d",
  x = hp,
  y = qsec,
  z = mpg # Z é a nossa variável resposta
)


modelo <- lm(mpg ~ hp * qsec)

# Normalidade
ggdensity(modelo$residuals, fill = "lightgray")
ggqqplot(modelo$residuals)

shapiro.test(modelo$residuals)

# Homogeneidade
df2 <- data.frame(modelo$fitted, modelo$residuals)
ggscatter(df2,
  x = "modelo.fitted",
  y = "modelo.residuals"
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red"
  )

summary(modelo)
# Equaçao
# 8.524612 + 0.235879*hp + 1.476832*qsec - 0.019492*hp*qsec

# R2: Coeficiente de Determinação ( 0 até 1): 0.76
#     Explicação da Variável X sobre a variação em Y.
### --------------------------------------------
# Regressao logística
# Quando a variável resposta é dicotômica, isto é 0 ou 1.

# Vamos criar nosso conjunto de dados
y <- rep(0:1, each = 100) # Cria a variável y com 0 e 1, cada com 100 valores
x <- vector(length = 200) # Cria variável x, um vetor vazio com 200 valores
x
# Cria uma variável x1 com 100 valores a partir de uma distribuiçao normal
x1 <- rnorm(100,
  mean = 20, # media de x1
  sd = 5 # DP de x1
)

x2 <- x1 * 3 # cria uma variável x2 que é o culbo de x1

x[0:100] <- x1 # Preenche com x1
x[101:200] <- x2 # Preenche com x2
x

dados <- data.frame(y, x)
summary(dados)
dim(dados)
head(dados)

ggscatter(dados,
  x = "x",
  y = "y"
)

ggscatter(dados,
  x = "x",
  y = "y"
) +
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial")
  )
# Vejam que temos uma curva em forma de S
# Temos uma relaçao que nao é linear. Vamos usar a funçao glm()

modelo <- glm(y ~ x,
  data = dados,
  family = "binomial"
) # A distribuiçao da variável y é binomial

# por isso, acrescentamos esse argumento (family = "binomial"). Sempre que
# trabalharmos com glm, precisamos indicar a family dos dados.


summary(modelo)
# E ai?

# Como interpretar?
# install.packages("mfx")

library(mfx)
logitor(y ~ x, data = dados)

# Odds Ratio:
#     OddsRatio Std.Err.  z     P>|z|
# x  1.377542  0.094579 4.6652 3.083e-06 ***

# Aqui queremos o odds ratio, que é a chance de classificaçao. O valor para a
# nossa variável é 1.3775; isso quer dizer que cada aumento de uma unidade
# em x aumenta a probabilidade da resposta em 37,75%.
# EX.: Suponha que y = teve cancer de pulmao e x = anos fumando. O resultado
#      diz que cada ano a mais fumando aumnta a chance de ter cancer em
#      37,75%.

########### --------------------------------------
# Regressão Polinomial
# Aqui vamos realizar uma abordagem diferente.

library(tidyverse)
library(caret) # Talvez seja necessário instalar

data("Boston", package = "MASS")
glimpse(Boston)

Boston <- Boston |>
  mutate_if(is.double, as.numeric)

# Primeiro, vamos definir nossas variáveis.
# Resposta: medv
# Preditora: lstat

# Gráfico de disperssão
library(ggpubr)

a <- ggscatter(Boston,
  x = "lstat",
  y = "medv",
  add = "reg.line"
)

b <- ggscatter(Boston,
  x = "lstat",
  y = "medv"
) +
  stat_smooth()

ggarrange(a, b)
# Qual aparece o melhor ajuste visual?

### ----------------------
# Vamos para a análise
set.seed(123)

# Vamos dividir o conjunto de dados em duas partes
#  1. Treino
#  2. Teste
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Primeiro Um modelo Linear
# Construindo o modelo
model.linear <- lm(medv ~ lstat, data = train.data)

# Fazendo Prediçoes
predictions <- model.linear %>% predict(test.data)
# Checar a performance do modelo
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)
# Aqui queremos valores de RMSE Baixo e R2 Auto



# Como funciona um modelo Polinomial
lm(medv ~
  poly(lstat,
    2, # Ordem do polinômio; Y = a + b1X + b2Xˆ2 + erro
    raw = TRUE
  ),
data = train.data
) |>
  summary()

# Modelo Polinomial
# Construindo o modelo
model.poli <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Fazendo Prediçoes
predictions <- model.poli %>% predict(test.data)
# Checar a performance do modelo
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)

# Qual o melhor modelo?????

# Podemos ainda, quando observamos relaçoes não lineares, é realizar uma transformação logarítimica.

# Construindo o modelo
model.log <- lm(medv ~ log(lstat), data = train.data)
# Fazendo Prediçoes
predictions <- model.log %>% predict(test.data)
# Checar a performance do modelo
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)

# Lienarizando a relação entre as variáveis
Boston$lstat2 <- log(Boston$lstat)
a <- ggscatter(Boston,
  x = "lstat",
  y = "medv",
  add = "reg.line"
)

b <- ggscatter(Boston,
  x = "lstat2",
  y = "medv",
  add = "reg.line"
)

ggarrange(a, b)


# Ficamos com o Modelo Polinomial de grau 5
model.poli
summary(model.poli)

# Equação
#  6.832 - 1.215X + 1.292X^2 - 6.937e-02X^3 + 1.756e-03X^4 - 1.663e-05X^5
######################### This is the end! ################################
step(model.poli)
