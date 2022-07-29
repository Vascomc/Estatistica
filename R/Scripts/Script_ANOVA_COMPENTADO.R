################################################################
################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: FAMÍLIA ANOVA
# ##############################################################
################################################################
# Os assuntos que serão tratados aqui serão:
# Introdução ao teste de hipóteses
#         - Modelos Lineares 1
#           * ANOVA de um fator
#           * ANOVA de um fator em Blocos
#           * ANOVA de dois fatores
#         - Esses serão via videos externo no Youtube
#           * ANOVA de medidas repetidas
#           * ANCOVA
#           * ANOVA com parcelas subdivididas (ANINHADO)
# -------------------------------------
# Análise de Variância - ANOVA
# A ANOVA é uma espançao do teste t. Nela queremos ver se há diferenças entre
# três ou mais grupos. Os pressupostos sao os mesmos do teste t. Contudo,
# devemos ver a distribuiçao dos resíduos da análise e nao da variável
# resposta como no caso do teste t.
#### ---------------------------------------------------
# ANOVA de um fator #
# Utilizada quando uma variável preditora. Usaremos os dados Iris

library(tidyverse)
library(ggpubr)

data(iris) # Carregar os dados iris a aprtir do R
names(iris) # Ver os nomes das variáveis
attach(iris) # Faz o R reconhecer os nomes das variáveis

# Usaremos como variável resposta largura da Sépala (Sepal.Width).
# Passos:
#   1. Testes de PRESSUPOSTOS,
#   2. Realizar a análise,
#   3. Temos que checar a distribuição dos resíduos
#      resíduo = diferença entre o observado e o ajustado(média)
ggerrorplot(iris,
  x = "Species",
  y = "Sepal.Width"
  #  add = "jitter"
)

#   4. Interpretar o resultado
#### -------------------------------
# Passo 1
library(rstatix)
attach(iris)

# Há outliers nos nossos dados; isto é, há valores extremos que podem influenciar os resultados?
a <- iris |>
  group_by(Species) |>
  identify_outliers(Sepal.Width)
a[, c(1, 6, 7)]

ggboxplot(iris,
  x = "Species",
  y = "Sepal.Width"
  #  add = "jitter"
)

# Há homogeneidade de variâncias???
iris |> levene_test(Sepal.Width ~ Species)
# H0: Homogeneidades IGUAIS   <- Isso que queremos
# P = 0.55 > 0.05; Logo ACEITAMOS H0.
# H1: Homogeneidades DIFERENTES

# Há normalidade nos Dados????
iris |>
  group_by(Species) |>
  shapiro_test(Sepal.Width)
# H0: Dados (Y) com Distribuição NORMAL <- Isso que queremos
# Todos os valores de P > 0.05. ACEITAMOS H0.
# H1: Dados (Y) sem Distribuição NORMAL
### ---------------------------------
# Passo 2. Rodamos o modelo
modelo <- aov(Sepal.Width ~ Species)

# Passo 3.
# Na ANOVA precisamos mesmo é ver se os RESÍDUOS do modelo tem distribuição normal
shapiro.test(modelo$residuals)
# H0: Resíduos com Distribuição Normal  <- ISSO que queremos
# P > 0.05, ACEITAMOS H0

# Outra forma de avaliarmos
plot(modelo)
# 1° Homogeneidade de Variâncias
# 2° Normalidade dos dados

# podemos ainda ver assim. Essa Diagnose é para vermos o "ajuste" do nosso modelo.
# Homogeneidade de Variâncias
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

# Normalidade dos Resíduos
ggdensity(modelo$residuals, fill = "lightgray")
ggqqplot(modelo$residuals)
## -----------------------
# Passo 4. Vemos o resultado
summary(modelo)
#              Df Sum Sq Mean Sq F value Pr(>F)
# Species       2  11.35   5.672   49.16 <2e-16 ***
# Residuals   147  16.96   0.115
dim(iris)
# [1] 150   5; tenho 150 linhas x 5 colunas

# Numa linguagem da AULA TEÓRICA
#                    Df Sum Sq Mean Sq F value Pr(>F)
# Entre grupos       2  11.35   5.672   49.16 <2e-16 ***
# Dentro grupos    147  16.96   0.115

# INTERPRETAÇÃO
# REJEITAMOS H0, de que as médias para largura de Sépala são iguais, pois o valor de P (2e-16)) < 0.05.
# PELO MENOS UMA MÉDIA difere das outras.


ggerrorplot(iris,
  x = "Species",
  y = "Sepal.Width"
)

# O resultado mostra que há diferença entre os grupos, contudo nao mostra onde
# essa diferença está. assim temos que realizar uma análise a posteriori.
# Basicamente temos dois para pensar. Tukey e Dunnett. No Tukey, testamos as
# médias de todos os tratamentos entre si; já no Dunnett testamos as médias
# contra um grupo de referência.

# Aqui o indicado seria o Tukey, mas vamos fazer o Dunnett também.

# Tukey
resu.tukey <- TukeyHSD(modelo)
resu.tukey
plot(resu.tukey)

######### Gráfico mais "bunitinho" ##########
tukey <- TukeyHSD(modelo, conf.level = 0.95) # Salvando o resultado do Tukey
tky <- as.data.frame(tukey$Species) # Pegando as informações e salvando como data frame
head(tky) # observando as primeiras linhas
tky$pair <- rownames(tky) # Renomeando as linhas com base nas comparações

a <- ggplot(
  tky, # Nossa data frame
  aes(colour = cut(`p adj`, # Aqui é para gerar as corres para Significativo e
    # náo significativo
    c(0, 0.01, 0.05, 1),
    label = c(
      "p<0.01",
      "p<0.05",
      "Non-Sig"
    )
  ))
) +
  geom_hline(
    yintercept = 0, # Linha no valor 0
    lty = "11", # Tipo de linha
    colour = "grey30"
  ) + # Cor da linha
  geom_errorbar(aes(pair, # Aqui é para as barras de erro
    ymin = lwr,
    ymax = upr
  ),
  width = 0.2
  ) +
  geom_point(aes(pair, diff)) + # Para adicionar o ponto que é a Média
  labs(colour = "") +
  xlab("Comparações Multiplas") + # Título do eixo x
  ylab("Diferenças na média") + # Título do eixo y
  coord_flip() + # Para inverter os eixos
  theme_bw() + # Tema de fundo do gráfico, experimente, theme_minimal()
  theme(text = element_text(
    family = "Times New Roman", # Para ser Times no título dos eixos
    size = 14 # Tamanho da fonte
  )) +
  scale_colour_brewer(palette = "Dark2") # Padrão de cores par os valores,
# tente 'BrBG'
a

# Salvar o gráfico do Tukey
ggsave("gráfico_tukey.Tiff", # Nome do arquivo na pasta
  plot = a, # Qual gráfico estamos salvando
  height = 15, # Altura do gráfico
  width = 20, # Largura do gráfico
  units = "cm", # unidade das medidas de altura e largura
  dpi = 300 # resolução da imagem final
)



#############################################
# Dunnett
install.packages("multcomp")
library(multcomp)

# Usemos como nosso Nivel(tratamento) de referência a espécie 'setosa'.
levels(Species)

resultado <- glht(modelo, # Resultado da ANOVA
  linfct = mcp(
    Species = # Variável explicativa
      c(
        "setosa - versicolor = 0", # setosa serve de referencia
        "setosa - virginica = 0"
      )
  )
)

summary(resultado)
plot(resultado)

# Gráfico

ggerrorplot(iris,
  x = "Species",
  y = "Sepal.Width",
  xlab = "Espécies de flores estudadas",
  ylab = "Largura da sépala (cm)"
)

### ------------------------------
# ANOVA de um fator em Blocos #
# Os blocos sao um recurso experimental que tem por finalidade controlar
# fatores externos ao experimento. Como inclinaçao, sombreamento, etc.

names(ANOVA)
attach(ANOVA)

# library(rstatix)
# outliers
a <- ANOVA |>
  group_by(Tratamento) |>
  identify_outliers(abundancia)
a[, c(1, 7, 8)]

# Há homogeneidade de variâncias???
ANOVA |> levene_test(abundancia ~ Tratamento)
# ACEITAR H0, P > 0.05

# Há normalidade nos Dados????
ANOVA |>
  group_by(Tratamento) |>
  shapiro_test(abundancia)
# Para o tratamento fino, P < 0.05
# Para o tratamento grosso, P < 0.05
# Quando acontece temos duas opções.
#   1. Usar um equivalente não paramétrico
#   2. Padronizar a variável resposta (Y) para que temnha distribuição normal. Aqui geralmente usamos logarítimica.
##### ------------
# Apenas para EXEMPLIFICAR vou continuar.

modelo <- aov(abundancia ~ Tratamento + Bloco)
# Criamos um modelo, onde quero avaliar o efeito dos tratamento sobre a abundância de individuos. Nesse modelo a variaçào dos tratamentos é controlada pelo '+ Bloco'.

shapiro.test(modelo$residuals)
# P < 0.05; REJEITO H0 de resíduos terem distribuição normal.
# Alternativas
#   1. transformar abundancia com log1p()
#   2. usar o teste não paramétrico de Friedman
#   APENAS POR DIDÁTICA VAMOS SEGUIR NORMALMENTES
#  modelo <- aov(log1p(abundancia) ~ Tratamento + Bloco)
hist(abundancia)
hist(log1p(abundancia))
hist(sqrt(abundancia))

modelo <- aov(sqrt(abundancia) ~ Tratamento + Bloco)

shapiro.test(modelo$residuals)
# Com Raiz quadrado os dados ajustaram à distribuição nomral.

summary(modelo)
# P > 0.05; ACEITO H0 de que não há diferença entre os
# tratamentos. Ou eu nao tenho evidências o suficiente para
# rejeitar H0.

# IMPORTANTE: Nao interpretamos o Bloco.

# Como o tratamento nao foi significativo, nao realizamos o teste a posteriori

#### COMO REPORTAR ESSE RESULTADO  ###
# As mádias pada abundância entre os tratamentos utilizados foram similares (F(3,75): 2.42, P = 0.09)
#######

# SE e SOEMNTE SE o tratamento fosse significativo P =< 0.05
# plot(TukeyHSD(modelo))
### ------------------------------
# ANOVA fatorial: dois fatores #
# A ANOVa fatorial é usada quando temos dois fatores como variável explicativa
# e pressumos interaçao entre eles.

data("ToothGrowth") # Crescimento dentário
str(ToothGrowth)
attach(ToothGrowth)
?ToothGrowth

# Vamos transformar Dose em fator
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
str(ToothGrowth)

attach(ToothGrowth)

# library(rstatix)
# outliers
ToothGrowth |>
  group_by(supp, dose) |>
  identify_outliers(len)

# Há homogeneidade de variâncias???
ToothGrowth |> levene_test(len ~ supp) # ACEITA H0
ToothGrowth |> levene_test(len ~ dose) # ACEITA H0

# Há normalidade nos Dados????
ToothGrowth |>
  group_by(supp, dose) |>
  shapiro_test(len)
# Não temos nenhum valor  de P < 0.05; logo todas as combinações possuem distribuição nomral.

# Criando nosso MODELO
modelo <- aov(len ~ dose * supp) # Veja que usei * e não o +.
shapiro.test(modelo$residuals)
# Resíduos tem Distirbuição normal (ACEITA H0)

summary(modelo)
# COmo avaliar o resultado
#   1. A interação é significativa (Fator_1:Fator_2)
#      1.1. Se P < 0.05; não podemos olhar cada fator de forma isolada
#      1.2. se P > 0.05; ai olhamos os fatores de forma isolada.
# AQUI a interação é significativa dose:supp, P < 0.05

plot(TukeyHSD(modelo))

######### Gráfico mais "bunitinho" ##########
tukey <- TukeyHSD(modelo, conf.level = 0.95) # Salvando o resultado do Tukey
tky <- as.data.frame(tukey$`dose:supp`) # Pegando as informações e salvando como data frame
head(tky) # observando as primeiras linhas
tky$pair <- rownames(tky) # Renomeando as linhas com base nas comparações

a <- ggplot(
  tky, # Nossa data frame
  aes(colour = cut(`p adj`, # Aqui é para gerar as corres para Significativo e
    # náo significativo
    c(0, 0.01, 0.05, 1),
    label = c(
      "p<0.01",
      "p<0.05",
      "Non-Sig"
    )
  ))
) +
  geom_hline(
    yintercept = 0, # Linha no valor 0
    lty = "11", # Tipo de linha
    colour = "grey30"
  ) + # Cor da linha
  geom_errorbar(aes(pair, # Aqui é para as barras de erro
    ymin = lwr,
    ymax = upr
  ),
  width = 0.2
  ) +
  geom_point(aes(pair, diff)) + # Para adicionar o ponto que é a Média
  labs(colour = "") +
  xlab("Comparações Multiplas") + # Título do eixo x
  ylab("Diferenças na média") + # Título do eixo y
  coord_flip() + # Para inverter os eixos
  theme_bw() + # Tema de fundo do gráfico, experimente, theme_minimal()
  theme(text = element_text(
    family = "Times New Roman", # Para ser Times no título dos eixos
    size = 14 # Tamanho da fonte
  )) +
  scale_colour_brewer(palette = "Dark2") # Padrão de cores par os valores,
# tente 'BrBG'
a

ggerrorplot(
  ToothGrowth, # Conjunto de dados
  x = "dose", # Variável X1
  y = "len", # Variável resposta
  color = "supp", # Variável X2
  xlab = "Doses (mg)",
  ylab = "Comprimento do dente (cm)"
)

# Invertendo as variáveis
ggerrorplot(
  ToothGrowth, # Conjunto de dados
  x = "supp", # Variável X1
  y = "len", # Variável resposta
  color = "dose", # Variável X2
  xlab = "Tipo de suplemento",
  ylab = "Comprimento do dente (cm)"
)

###### REPORTANDO O RESULTADO #########
# O crescimento dentário é dependente do tipo de  suplemento utilizado, bem como das doses emq ue são usados (F (2,54): 4.107, P = 0.021). O suplemento OJ tende a ser melhor que o sumplemento VC, contudo, quando utilizamos 1mg de OJ não há diferenças para VC ou OJ com doses de 2 mg.
summary(modelo)

###########################################################################
###########################################################################
# ANOVA de Medidas, ANCOVA e ANOVA com parcelas subdivididas

# Para não ficar muito longo, deixarei videos separados sobre cada um dos assuntos com os respectivos scripts.
#############################################################################
######################### This is the end! ##################################
#############################################################################
