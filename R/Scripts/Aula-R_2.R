##################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: Estatistica Descritiva
##################################################################
# Tópicos que serão abordados
# 1. Tabelas de frequências
# 2. Resumo estatístico
# 3. Exportando tabelas
# 4. Gráfico: Histograma, Barras e Ogiva
#    4.1. plots nativos
#    4.2. ggpubr
#    4.3. exportando
## -----------------------------
# 1. Tabelas de Frequências

# Tabela de frequência Simples

head(iris)

sepala <- iris[, 1]
sepala

tabela <- data.frame(t(table(sepala)))[, -1]
tabela$sepala <- as.numeric(levels(tabela$sepala))
tabela <- tabela |>
  mutate(
    Fr = 100 * Freq / sum(Freq),
    Fac = cumsum(Freq),
    Xi.Fi = sepala * Freq
)
tabela

# Tabela de frequência Agrupada
library(fdth) # Pacote necessário

summary(sepala) # Para saber o mínimo e máximo

tabela_f <- fdt(sepala,
  start = 4, # limite inferior da 1° classe
  end = 8, # limiete superior da ultima classe
  h = 1 # amplitude de classe
) 
tabela_f

## -----------------------------
# 2. Resumo estatístico - Estatística Descritiva

# Média
mean(sepala)
mean(sepala, trim = 0.1) # Sem os 10% maiores e menores valores

# Variância
var(sepala)

# Desvio Padrão
sd(sepala)

# Mediana
median(sepala)

# Tudo de uma vez
library(tidyverse)
library(rstatix)

summary(sepala)

resumo <- iris |> get_summary_stats(Sepal.Length, type = "common")
resumo

resumo_2 <- iris |> group_by(Species) |> get_summary_stats(Sepal.Length, type = "common")
resumo_2

## -----------------------------
# 3. Gráficos

# Histograma: Usado para dados em classes e variáveis contínuas.
hist(sepala,
     xlab = "Limite das classes",
     ylab = "Frequência observada")
# Por padrão usa a regra de Sturges

plot(tabela_f) # Tabela de frq. construida anteriormente

# Baras: Gráfico recomendado para dados discretos, ou seja, números inteiros ou tabela de freq. simples. 
resumo_2
df_2 <- cbind(resumo_2$Species, resumo_2$mean)

barplot(df_2[,2] ~ df_2[,1],
        xlab = "Especies",
        ylab = "Valores")

# Ogiva: Mostra as frequências acumuladas
plot(tabela_f,
     xlab = "Limite das classes",
     ylab = "Frequência acumulada", 
     type='cfpp', # Tipo de informação que queremos 
     col = "black") # Cor da linha

#####
# Usando o ggpubr
library(ggpubr)

# Histograma
names(iris)
a <- gghistogram(iris,
            x = "Sepal.Length",
            bins = 10,
            fill = "gray",
            xlab = "Classes",
            ylab = "Frequência absoluta")

# Gráfico de Barras
b <- ggbarplot(resumo_2, 
          x = "Species", 
          y = "mean",
          xlab = "Espécies",
          ylab = "Média observada",
          color = "black", 
          fill = "lightgray")

# Gráfico do tipo Ogiva
df <- as.data.frame(tabela_f)
c <- ggline(df, 
       x = "table.Class.limits",
       y = "table.cf...",
       xlab = "Classes",
       ylab = "Frequência acumulada") +
 scale_x_discrete(labels = c(4,5,6,7))

# Exportando
ggsave(
 "ogiva.tiff",
 plot = c,
 path = "~/Desktop",
 units = "cm",
 height = 15,
 width = 20,
 dpi = 300
)

####################### THIS IS THE END ##########################