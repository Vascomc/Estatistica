#########################################################################
#########################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: TESTE T
########################################################################
########################################################################
# Teste t
# O teste t avalia a diferença observada entre dois grupos. Aqui
# testamos se essa diferença é uma difernça real ou fruto do acaso.
# Usaremos os dados IRIS para essa análise


# Teste t para UMA amostra
# Vamos criar um conjunto de valores por meio de um vetor.
valor <- c(7, 2, 7, 9, 2, 3, 9, 4, 7, 7)
valor

# Teste de pressupostos #
# Vamos tesstar a normalidade dos dados.
library(car)
shapiro.test(valor) # É um tesde de ADERÊNCIA
# H0: Meus dadose seguem uma ditribuição teórica - H0: distribui(valor) = distribuiçao Normal
# Resultado
# W = 0.86; P = 0.08
# Decissão:
#   ACEITAR  H0: P > 0.05
#   REJEITAR H0: P < ou = 0.05
#   0.08 > 0.05; Logo ACEITAMOS H0.
#   Nossos dados POSSUEM Distribuição Normal

# Teste t na prática
t.test(valor, mu = 8) # Teste Bicaudal
# H0: Média de Valor = 8
# H1: Media de Valor difere de 8
# Decissão:
#   ACEITAR  H0: P > 0.05
#   REJEITAR H0: P < ou = 0.05
# Rejeito H0
# Probibilidade Condicional: P(H1 | H0): Qual a chance de obter a diferença observada (~2.3) entre a média da minha amostra em relação ao valor de referência, ADIMITINDO que a H0 É VERDADEIRA. O Valor de P foi 0.02.
# O valor de 0.02 obtido, quer dizer que, a chance de observarmos a diferença de 2.3 entre a média da amostra (5.7) e o valor de referencia que 8, ADMITINDO UMA REALIDADE ONDE H0 É VERDADEIRA, é de apenas 2% (0.02). Logo, a diferença observada não foi obtida ao acaso.
# O Valor de P do resultado é : Qual a chance de eu REJEITAR H0, sabendo que ela é VERDADEIRA.

t.test(valor, mu = 8, alternative = "less") # unilateral à equerda. Eu estou adimitindo que a média da amostra É MENOR que o valor de referência.
# Compara o valor CALCULADO vs TABELADO
# Decissão:
#   ACEITAR  H0: P > 0.05
#   REJEITAR H0: P < ou = 0.05
#   REJEITO H0; P < 0.05
t.test(valor, mu = 8, alternative = "greater") # unilateral à direita. Eu estou adimitindo que a média da amostra É MAIOR que o valor de referência.
# Decissão:
#   ACEITAR  H0: P > 0.05
#   REJEITAR H0: P < ou = 0.05
#   ACEITO H0; pois P > 0.05.
##### -------------------------------
# Teste t para DUAS amostras
# Carregando os dados
data(iris)
str(iris)
# Temos a variável Species com 3 níveis. Vamos utilizar apenas dois
# desses níveis.
attach(iris)
library(tidyverse) # Para usar a função filter()
# Preparando os dados.
df <- iris %>%
  filter(Species == "setosa" | Species == "virginica")
# Estamos filtrando os dados Iris pela variável Specie. Usando a
# barra verrtical | introduzimos um argumento lógico. Assim temos
# "Quero criar um objeto df, a partir dos dados Iris, sendo que
# quero filtrar os dados pela variável Specie tendo os níveis
# setosa OU virginica
names(df)
detach(iris)
attach(df)

# Vamos fazer um box plot para ver se há diferença entre as espécis
# de flores.
library(ggpubr) # Para usar a função ggboxplot()
ggboxplot(df,
  x = "Species",
  y = "Sepal.Width"
)
# Parece que sim. Vamos proceder a análise.
# Passos:
#   1. Temos que checar a homogeneidade das variâncias
#   2. Temos que checar a distribuição da variável resposta
#   3. Temos que decidir se o nosso teste será bi ou unicaudal
#   4. Realizar a análise
#   5. Interpretar o resultado

# Passo 1
# É preciso testar nos dois níveis da variável Species
# Hipóteses testadas
#   H0: As variâncias são iguais
# Nesse caso queremos ACEITAR H0 de que as variâncias entre os grupos são iguais. Assim queremos valores de P > 0,05.
# library(car) # Carregar se não foi ainda
leveneTest(Sepal.Width ~ Species)
# Decissão ACEITAR HO: P > 0.05
#  O P do teste foi 0.32 > 0.05; Logo ACEITAMOS H0.
#  As variâncias são iguais.


# Passo 2
# Normalidade da variável.
# Hipótese a ser testada
#   H0: A variável resposta possui distribuição normal
# Nesse caso queremos aceita H0 de que a distribuição é normal.
# Mas temos que testar nos dois niveis da variável explicativa.

shapiro.test(Sepal.Width[Species == "setosa"]) # ACEITO H0
shapiro.test(Sepal.Width[Species == "virginica"]) # ACEITO H0


# Passo 3
# Realizar o teste
resultado <- t.test(Sepal.Width ~ Species) # bicaudal

# Passo 5
resultado
# P = 4.5e-09 -> 0,0000000045
# 4.5e-9 < 0.05
# REJEITA H0, de que as médias entre largura de sépala entre as especies virginica e setosa são iguais.



# Faremos agora um teste unicaudal
t.test(Sepal.Width ~ Species,
  alternative = "greater"
) # Pensando para o lado direito

# Gráfico
gráfico.teste_t <- ggerrorplot(df,
  x = "Species",
  y = "Sepal.Width",
  xlab = "Espécies de flores estudadas",
  ylab = "Largura da sépala (cm)"
)
# Usamos um gráfico de médias, pois a distribuição é normal.
gráfico.teste_t
# Exportando o gráfico

ggsave("gráfico_teste_t.tiff", # Nome do gráfico
  dpi = 400, # Resolução
  plot = gráfico.teste_t, # objeto onde salvou o gráfico
  # path = "~/Desktop", # Caminho onde o gráfico será salvo
  unit = "cm", # unidade
  height = 15, # Altura do gráfico
  width = 25 # Largura do gráfico
)
#### --------------------------------------
# Teste t para DUAS amostras PAREADAS
# O teste t pareado é quando a mesma unidade amostral recebe os
# dois tratamentos. Usaremos os dados Pareado

# Os passos são similares ao teste t.
# Mas trabalharemos com as diferenças entre os ramos cobertos e
# descobertos.

attach(Pareado)
str(Pareado)
glimpse(Pareado)

diferenca <- Ramo_desoberto - Ramo_coberto

# Agora submeteremos a diferença ao teste de normalidade.

shapiro.test(diferenca) # P > 0.05; ACEITO H0

resultado <- t.test(Ramo_desoberto, Ramo_coberto,
  paired = T, # Informa que é pareado
  alternative = "greater"
)
resultado
# Resultado: P < 0.05; REJEITO H0 de que a diferença média entre os ramos cobertos e descobertos seja igual a 0.

head(Pareado)

# Gráfico
# Vamos organizar os dados
# library(tidyverse) # CCarregar o pacote se não tiver carregado.
grafico_pareado <- Pareado %>%
  gather(
    key = "Flores",
    value = "Frutos",
    Ramo_coberto, Ramo_desoberto
  )
head(grafico_pareado)
tail(grafico_pareado)
# library(ggpurb)
pareado <- ggpaired(
  grafico_pareado,
  x = "Flores",
  y = "Frutos",
  ylab = "N° de frutos",
  xlab = "Tratamentos utilizados",
  legend = "", # Para retirar a legenda
  line.color = "gray", # Linhas entre as UA's
  line.size = 0.4 # Tamanho da linha
)
pareado


ggsave("gráfico_teste_t_Pareado.tiff", # Nome do gráfico
  dpi = 400, # Resolução
  plot = pareado, # objeto onde salvou o gráfico
  # path = "~/Desktop", # Caminho onde o gráfico será salvo
  unit = "cm", # unidade
  height = 15, # Altura do gráfico
  width = 25 # Largura do gráfico
)
# Reportando um resultado no meu resumo, TCC, etc....
# O número de frutos diferiu entre os tratamentos com ramos cobertos em relacão aos ramos descobertos (t = 16.405, gl = 26, P = 1.5e-15). Essa diferença foi em média ce 10.4 frutos a mais observados nos ramos descobertos (figura 1 "O gráfico para o teste t").

######################### This is the end! ##################################
