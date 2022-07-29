#########################################################################
#########################################################################
# Universidade Estadual do Rio Grande do Sul
# Disciplina de Estatística e Experimentação
# Prof. Dr. Márloin de Castro Vasconcelos
# Unidade em Três Passos
# ASSUTO: Gráficos com o pacote ggpubr
#########################################################################
#########################################################################
# Mostrarei aqui três tipos mais usados de gráficos. 
#   1. Barras
#   2. Box Plot
#   3. Disperssão
# Muito dos arguemntos servem para vários tipos de gráficos.
# Mais informações em:
#   1. <http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/>
#   2. Para o pacote GGplot2 <https://www.r-graph-gallery.com>
#########################################################################
# Pacotes
library(ggpubr)
# Já carrega o ggplot2
library(RColorBrewer) # Para Padrões de cores
library(tidyverse) # Para Sintaxe
#########################################################################
# Dados 
# Usaremos os dados IRIS, uma vez que já conhecem esses dados

# Relembrando
glimpse(iris) # Estrutura das variáveis
names(iris) # Nome das variáveis 
summary(iris) # Mostra um resumo das variáeis

# Estrutura básica 
# TipoDeGráfico (
#  dados ,
#  x = "variável do eixo X" ,
#  y = "variável do eixo Y" ,
# )
######------------------
# Gráficos de Histogramas e Barras.

# Histograma
# Básico
gghistogram(
    iris,   # Dados
    x = "Sepal.Length", # Variável de interesse
    y = "..density..", # para adicionar as frequências
    bins = 10 
    )
# bins = 10; Número de Classes, Padrão é 30. Brinquem com esse valor pois pode induzir se os dados podem assumir ou não distribuição normal. Sugiro estipular o número de classes via raiz do tamanho da amostra, que é o número de linhas que o conjunto de dados possui.
# Ex.: iris = 150 linhas; 
sqrt(150)
# [1] 12.24745;  podemos pensar em umas 15 classes. 
gghistogram(
    iris,   # Dados
    x = "Sepal.Length", # Variável de interesse
    y = "..density..", # para adicionar as frequências
    bins = 15 
)
# Fica um pouco menos subjetivo. 
# OBS.: Lembrem-se que aqui ainda há uma variável qualitativa. Por enquanto estamos olhando o padrão GERAL para a variável comprimento de Sépala.

# Adicionando cor nas barras
gghistogram(
    iris,   
    x = "Sepal.Length", 
    y = "..density..", 
    bins = 15,
    fill = "gray" # Usamos o argumento fill
    )

# Linha de densidade
gghistogram(
    iris,   
    x = "Sepal.Length", 
    y = "..density..", 
    bins = 15,
    fill = "gray",
    add_density = TRUE
)

# Olhando por Espécie
# Aqui vamos adicionar a variável fatorial
gghistogram(
    iris,   
    x = "Sepal.Length", 
    y = "..density..", 
    bins = 15,
    fill = "Species", # Substitui "gray" pelo nome da variável fator que temos.
    add_density = TRUE
)
# O que o histograma mostra em relação a:
#   1. distribuição da variável Comprimento de Sépala
#   2. essa distribição em relação aos níveis (espécies de flores) da variável Espécies?

# Adicinando nomes aos eixos
# Olhando por Espécie
# Aqui vamos adicionar a variável fatorial
gghistogram(
    iris,   
    x = "Sepal.Length", 
    y = "..density..", 
    bins = 15,
    fill = "Species",
    add_density = TRUE,
    xlab = "Comprimento da Sépala (cm)",
    ylab = "Frequências observadas"
)

# Mudando a cor das barras usando o pacote RColorBrewer

# Quadro de Cores que o Pacote possui
display.brewer.all(n=NULL, 
                   type="all", 
                   select=NULL, 
                   exact.n=TRUE, 
                   colorblindFriendly=T)

# Usarei no Exemplo o padrão BrBG, o último que aparece
# Mostra todas os "tipos" de cores
display.brewer.pal(11, # Número de cores que aparecem
                   'BrBG') # Padrão de Cor que vc quer.
brewer.pal.info
# Mostra quantas cores tem em cada padrão de cor.

# Código das cores observadas na área do PLOT
brewer.pal(11,'BrBG')
# Esses códigos que usaremos. No Iris temos 3 espécies. Assim, precisamos de 3 cores. POdemos fazer isso de duas formas
#  Forma 1 - Padrão
gghistogram(
    iris,   
    x = "Sepal.Length", 
    y = "..density..", 
    bins = 15,
    fill = "Species",
    add_density = TRUE,
    xlab = "Comprimento da Sépala (cm)",
    ylab = "Frequências observadas",
    palette = "BrBG" # Usamos o argumento Palette
)

#  Forma 2 - Escolhendo com os códigos
gghistogram(
    iris,   
    x = "Sepal.Length", 
    y = "..density..", 
    bins = 15,
    fill = "Species",
    add_density = TRUE,
    xlab = "Comprimento da Sépala (cm)",
    ylab = "Frequências observadas",
    palette = c("#543005", "#F6E8C3", "#003C30")
)
# Aqui peguei uma mais do início, mais no meio e mais no final
# Brinquem com isso..... com outros Padrões.
# Para mais <https://www.datanovia.com/en/?s=great+colors&search-type=default>

########_------------------------
# Gráfico de Barras
# Básico
# 1. Precisamos organizar os dados
library(rstatix)
names(iris)
df <- iris |>
    group_by(Species) |>
    get_summary_stats(Sepal.Length, type = "common")

df
# Usando a média
ggbarplot(
    df, 
    x = "Species",
    y = "mean",
    fill = "Species",
    position = position_dodge()
)

# Usando o Maior Valor
ggbarplot(
    df, 
    x = "Species",
    y = "max",
    fill = "Species",
    position = position_dodge()
)

# Adicionando barras de erro.
ggbarplot(iris, 
          x = "Species", 
          y = "Sepal.Length",
          fill = "gray",
          add = c("mean_se") # Média e Erro Padrão
          )
ggbarplot(iris, 
          x = "Species", 
          y = "Sepal.Length",
          fill = "gray",
          add = c("mean_ci") # Média e Intervalo de Confiaça
)
########################## This is The End ! ############################