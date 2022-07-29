#########################################################################
#########################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: GRÁFICOS COM O GGPUBR
########################################################################
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

glimpse(mtcars)
names(mtcars)
summary(mtcars)

# Gráfico de Disperssão. Usado para representar a relação de duas variáveis quantitativas

# Usaremos mpg como resposta e hp como explicativa

ggscatter(mtcars,
              x = "hp",
              y = "mpg",
          xlab = "Potência mensurada",
          ylab = "Consumo de combustível (Km.gal)")
# No geral está pronto.

# Adicionando modelo de regressão
ggscatter(mtcars,
          x = "hp",
          y = "mpg",
          xlab = "Potência mensurada",
          ylab = "Consumo de combustível (Km.gal)",
          add = "reg.line", # Adiciona um modelo linear
          conf.int = TRUE, # Adiciona intervalo de confiança
          add.params = list(color = "blue", # cor da linha
                            fill = "lightgray") # cor do intervalo
          ) +
# Adicionar a correlação entre hp e mpg
    stat_cor(method = "pearson", # método
             label.x = 200, # Posição do texto no eixo x
             label.y = 30) # Posição do texto no eixo y
# Brinque com esses valores de posição


# Regressões separadas por variável categórica
df <- mtcars
df$cyl <- as.factor(df$cyl)
ggscatter(df,
          x = "hp",
          y = "mpg",
          xlab = "Potência mensurada",
          ylab = "Consumo de combustível (Km.gal)",
          add = "reg.line", # Adiciona um modelo linear
          conf.int = TRUE,
          color = "cyl", # Variável categórica
          palette = "jco", # Padrão de cor
          shape = "cyl" # Tipo de forma nos "pontos"
) +
    stat_cor(aes(color = cyl), # Correlação pelos níveis de cyl
             label.x = 200)



########################## This is The End ! ############################
