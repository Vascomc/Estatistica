##################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: Introdução ao R
##################################################################
# Visão Geral sobre o R Studio
# 01. Janelas;
# 02. Global Options;
# 03. Pacotes;
# 04. Objetos;
# 05. Vetores;
# 06. Matrizes;
# 07. Operações matemáticas
# 08. Fatores;
# 09. Manipulação de dados;
# 10. Gráficos.
## -----------------------------
# 3. Pacotes

# Apresentam funções voltadas para necessidades específicas como
# manipulação de dados, análises não paramétricas, aprendizagem de
# maquinas, etc.

# Instalando Pacotes
# 1. Painel "Packages"
# 2. Função install.packages()
install.packages(c("tidyverse", "rstatix", "agricolae", "car", "ggpubr", "WRS2", "multcomp", "vegan", "pacman", "fdth"))
# Rodar esse comando.

# carregando pacotes
library(tidyverse) # Jeito 1
require(tidyverse) # Jeito 2
pacman::p_load(tidyverse, ggpubr) # Jeito 3

## ---------------------------
# 4. Objetos

# Um objeto é uma forma de gravarmos dados, resultados, gráficos
# no R.
# Eemplo.:

df <- 3 # Estou criando um objeto de nome df, e gravando nele o número 3.
df # Chamando o objeto

# ----------------------------
# 5. Vetores

# Vetores são conjuntos de dados com uma coluna e várias linhas/Unidades Amostrais.

a <- c(3, 4, 5, 2, 5, 3, 1, 5, 4, 2)
a

b <- seq(1:10)
b

# --------------------------------
# 6. Matrizes

# Matriz é conjunto de dados númericos formada por i linhas e j colunas
# Notação.: S(i, j)
# ex.: S(5,10);
#      onde:
#      i = 5 linhas
#      j = 10 colunas

c <- matrix(
  nrow = 5, # n° de linhas
  ncol = 10 # n° de colunas
)
c
# Vejam que não há valores nas células da matriz NA = não disponível
set.seed(123) # Ao usarmos a função rnorm abaixo ela criará númedos aleatórios. Ao usarmos set.seed() comn valores iguais dentro do parêntesses para todos os números gerados serão os mesmos
c <- matrix(
  nrow = 10, # n° de linhas
  ncol = 5, # n° de colunas
  rnorm(50, # n° de valores linhas * colunas
    mean = 20, # média dos valores criados
    sd = 5
  ) # Desvio padrão dos valores criados
)
c

# Data Frames: São bases de dados com variáveis de diferetnes formas.
set.seed(123)
d <- data.frame(list(
  V1 = rnorm(10, mean = 15, sd = 2),
  V2 = rpois(10, 2),
  V3 = gl(2, 5, labels = c("Preservado", "Impactado"))
))

d # ------------------------------------
# 7. Operações matemáticas

# Básicas
2 + 3
2 - 3
2 * 3
2 / 3

# Demais
2^2 # potênica
sqrt(9) # Raiz quadrada
9^(1 / 2) # Raiz quadrada
9^(1 / 3) # Raiz cúbica
9^(1 / 4) # Raiz quarta
log(1) # logarítmo

# Operação com vetores
a * 2
a + 2
log(a)
a^(1 / 5)

# Operação com matrizes
c * 2
c + 2
log(c)
c^(1 / 5)

# ------------------------------
# 8. Fatores

# Fatores são variáveis que representam nomes, são variáveis nominais, isto é, não apresentam ordem de grandeza entre si. O que temos são níveis desse fator de estudo.
# e.: Adubo: Controle, NPK, Organico1, Organico2
#
#  O fator de estudo é tipo de adubo, os níveis são:
#  Controle,
#  NPK,
#  Organico1,
#  Organico2

adubo <- gl(4, # n° de fatores
  10, # n° de unidades amostrais de cada fator
  labels = c(    # Nomes dos fatores
    "Controle",
    "NPK",
    "Organico1",
    "Organico2"
  )
)
adubo

# --------------------------------
# 9. Manipulação de dados

head(iris) # Usaremos os conjunto iris

# Uso do pipe |> 

# Retirando variáveis
sepal.length <- iris$Sepal.Length
f <- iris |> select(Sepal.Length)
g <- iris |> select(Sepal.Length, Species)

# Renomeando
h <- g |> rename(compr.sepala = Sepal.Length,
                 especies = Species)
names(h)

# Agrupando
i <- iris |> group_by(Species) |> summarise(n = n())
i

j <- iris |> group_by(Species) |> summarise(valor = sum(Sepal.Length))
j

#-------------------------------
# 9. Gráficos

# Usaremos o pacote ggpubr para criar gráficos.
# Básico

# Gráfico de caixas - box plot
ggboxplot(iris, x = "Species", y = "Sepal.Length",
          fill = "gray", # Cor da caixa
          xlab = "Espécies avaliadas",
          ylab = "Comprimento de Sépalas (cm)")

# Gráfico de erros - error plot
ggerrorplot(iris, x = "Species", y = "Sepal.Length",
          xlab = "Espécies avaliadas",
          ylab = "Comprimento de Sépalas (cm)")

# Gráfico de dispersão - scatter plot
ggscatter(iris, x = "Sepal.Width", y = "Sepal.Length",
            xlab = "Largura de Sépala (cm)",
            ylab = "Comprimento de Sépalas (cm)")

ggscatter(iris, x = "Sepal.Width", y = "Sepal.Length",
          xlab = "Largura de Sépala (cm)",
          ylab = "Comprimento de Sépalas (cm)",
          color = "Species")

##################### This is the End #####################
