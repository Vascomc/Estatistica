#########################################################################
################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: GRÁFICOS COM O GGPUBR
################################################################
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
# Usaremos os dados ToothGrowth, uma vez que já conhecem esses dados

# Relembrando
glimpse(ToothGrowth) # Estrutura das variáveis
names(ToothGrowth) # Nome das variáveis
summary(ToothGrowth) # Mostra um resumo das variáeis

# Estrutura básica
# TipoDeGráfico (
#  dados ,
#  x = "variável do eixo X" ,
#  y = "variável do eixo Y" ,
# )
###### ------------------
# Box Plot

# Para Um Fator
names(ToothGrowth)
ggboxplot(
  ToothGrowth,
  x = "dose",
  y = "len",
  fill = "gray",
  xlab = "Doses de Vitamina C (mg)",
  ylab = "Comprimento mensurado (cm)"
)


# Para DOIS Fatores
# Usamos o argumento Fill, para adicionar o segundo Fator.
# Experimento inverter o x = "supp" e fill = "dose".
#   obs.: Essa mudança dará ênfase a fatores diferentes. Dependerá de qual fator é mais "importante" no seu estudo.
names(ToothGrowth)
ggboxplot(
  ToothGrowth,
  x = "dose",
  y = "len",
  fill = "supp",
  xlab = "Doses de Vitamina C (mg)",
  ylab = "Comprimento mensurado (cm)"
)

# Mudando as cores das caixas
ggboxplot(
  ToothGrowth,
  x = "dose",
  y = "len",
  fill = "supp",
  xlab = "Doses de Vitamina C (mg)",
  ylab = "Comprimento mensurado (cm)",
  palette = "Dark2" # Outro Padrão, pacote RColorBrewer
)

# Mudando as sequencias das doses no eixo x.
# Geralmente será plotado em ordem alfabética ou numérica. Na grande maioria dos casos não iremos querer isso. Daí podemos ordenar o eixo X conforme queremos.
ggboxplot(
  ToothGrowth,
  x = "dose",
  y = "len",
  fill = "supp",
  xlab = "Doses de Vitamina C (mg)",
  ylab = "Comprimento mensurado (cm)",
  palette = "Dark2",
  order = c("2", "1", "0.5")
)

names(iris)
levels(iris$Species)
ggboxplot(
  iris,
  x = "Species",
  y = "Sepal.Length",
  fill = "gray",
  xlab = "Espeécies estudadas",
  ylab = "Comprimento da Sépala (cm)",
  palette = "Dark2",
  order = c("virginica", "versicolor", "setosa")
)

# Mudando a legenda
ggboxplot(
  ToothGrowth,
  x = "dose",
  y = "len",
  fill = "supp",
  xlab = "Doses de Vitamina C (mg)",
  ylab = "Comprimento mensurado (cm)"
) + # o sianl de + adicionará outra camada/função ao gráfico
    # SEMPRE DEPOIS que fechar o paratenses da função do gráfico.
    
  theme(legend.position = "bottom") + # posição: "left", "right", "top"
  scale_fill_manual(breaks = c("VC", "OJ"), # Ordena a sequência
                    values = c("gray25", "gray75") # Cores dos boxplot
                    # ,labels = c ("trat1", "trat2") 
                    )
# Como a segunda variável é definida no argumento fill, modificamos a ordem na lagenda por meio da função scale_fill_manual()

# Mudando os nomes dos Niveis no Eixo X
ggboxplot(
  iris,
  x = "Species",
  y = "Sepal.Length",
  fill = "gray",
  xlab = "Espeécies estudadas",
  ylab = "Comprimento da Sépala (cm)",
  palette = "Dark2"
) + 
  scale_x_discrete(labels = c(
    "setosa" = "Setosa",
    "versicolor" = "Versicolor",
    "virginica" = "Virginica"
  ))
# A função scale_x_discrete, usamos para várias coisas no eixo X, como ordenar os níveis e tal (mais façil com o argumento 'order').
# Na mudança dos nomes dos níveis, 1° vem como nos dados, depois do = o nome que vc quer.

# Mudando os nomes dos Níveis no Eixo X
ggboxplot(
  iris,
  x = "Species",
  y = "Sepal.Length",
  fill = "gray",
  xlab = "Espeécies estudadas",
  ylab = "Comprimento da Sépala (cm)",
  palette = "Dark2"
) + # o sianl de + adicionará outra camada/função ao gráfico
  scale_x_discrete(labels = c(
    "setosa" = "Sp.1",
    "versicolor" = "Sp.2",
    "virginica" = "Sp.3"
  ))

# Gráfico de Média + ERRO.
# O gráfico de Media + EP é usado quando os dados atendem os pressupostos da ANOVA

ggerrorplot(ToothGrowth,
  x = "dose",
  y = "len",
  desc_stat = "mean_se",
  color = "black",
  add = "violin", # Adicionar o violino
  add.params = list(color = "darkgray") # Cor da linha do violino
)
# A forma e a amplitude do violino mostram:
#   1. Amplitude dos dados, minimo e máximo
#   2. Forma: As "barriguinhas" a concentração dos dados

ggerrorplot(ToothGrowth,
  x = "dose",
  y = "len",
  desc_stat = "mean_se",
  color = "black",
  add = "boxplot", # Ao box plot aqui junto
  add.params = list(color = "darkgray")
)

# Cores no gráfico de Erros
# SOMENTE SE DOIS FATORES
ggerrorplot(ToothGrowth,
  x = "dose",
  y = "len",
  desc_stat = "mean_se", # "mean_sd", "mean_ci"
  color = "supp", # Usamos o argumento color, e não fill
  add = "violin",
  add.params = list(color = "black"),
  palette = "Dark2"
)

# OBS.: Notem que os argumentos se repetem em relação ao histograma e barras.

########################## This is The End ! ############################
