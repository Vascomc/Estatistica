################################################################
################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: QUI-QUADRADO
################################################################
################################################################
# Teste de qui-quadrado
# O teste de qui-quadrado é utilizado para avaliar a associação
# entre variáveis qualitativas. Ele é utilizado de 3 formas:
#   1. Aderência: Quando comparamos os nossos dados com um valor
# teórico esperado.
#   2. Heterogeneidade: Quando queremos ver se há diferenças numa
# porpoção observada em uma variável em relação a outra.
#   3. Associação: Seria como se fosse uma correlação entre
# variáveis qualitativas.
## ------------------------------------
# Aderência

# Aqui queremos ver se um dado conjunto de alelos de um gene se
# segregam como a primeira lei de mendel. Logo para o cruzamento
# de dois heterozigotos esperamos as propoções: 25, 50 e 25.
attach(qui_quadrado_aderencia)
observado <- c(6,15,3)
observado

esperado <- c(6, 12, 6) # Esperado pela 1° lei de Mendel
esperado

X2 <- sum((observado - esperado)^2 / esperado) # A estatística
X2 # Valor Calculado de qui-quadrado

gl <- length(observado) - 1 # Graus de liberdade


resultado <- pchisq(X2, df = gl, lower.tail = F) # Valor de P.
# H0: Valores Observado e Esperado são iguais; Isto é, possui a
#     distribuição teórica esperada. P > 0.05
# H1: Valores Observado e Esperado são diferentes. P < ou = 0.05
resultado
# Aceitamos ou Rejeitamos H0?
# O Resultado é o Valor de P; Logo 0.32 > 0.05; ACEITAMOS H0
# Logo os valores observados seguem a 1° Lei de Mendel

# Para o gráfico usaremos o pacote ggpubr. Ele é baseado no GGplot2. 
# Muito popular paracriar gráficos.

library(ggpubr)
observado <- c(6, 15, 3)
alelo <- c("R", "RS", "S")

dados <- data.frame(observado, esperado, alelo)
dados

library(tidyverse)

df <- dados %>%
  gather(
    key = "Variavel_X", # nova variável
    value = "Variavel_Y", # nova variável para os valores
    observado, esperado
  )
head(df)
str(df)

ggbarplot(df, # Conjunto de dados
  x = "Variavel_X", # Variável x
  y = "Variavel_Y", # Variável y
  fill = "alelo", # cor do gráfico
  xlab = "Antígenos", # Título eixo x
  ylab = "Frequência observada" # Título eixo y
)
### -------------------------------------
# Heterogeneidade
attach(qui_quadrado_hetero)
names(qui_quadrado_hetero)

tabela <- table(Cidade, Cor) # Cria uma tabela de contingência
tabela

resultado <- chisq.test(tabela)
# H0: Valores Observado e Esperado são iguais: P > 0.05
# H1: Valores Observado e Esperado são diferentes: P < ou = 0.05
#
resultado
# Aceitamos ou Rejeitamos H0?
# REJEITAR a H0; P = 0.013 < 0.05
# Conclusão é que há diferença nas proporcóes observadas em relação ao que seria 
# esperado (X2 = 12.51, gl: 4, P = 0.014)

# Teste a posteriori
resultado$stdres
resultado$stdres # Resíduos padronizados
# Há valores maiores que (>) 1.96?
# A difernça só é observada para Besouros escuros em São Leopoldo.

df <- data.frame(tabela)
df
ggbarplot(df,
  x = "Cidade",
  y = "Freq",
  fill = "Cor",
  xlab = "Cidades estudadas",
  ylab = "Frequência observada",
  position = position_dodge(0.8)
)
### ---------------------------------------
# Associação

attach(dados)

tabela <- table(doenca, eusinofilo)
tabela

resultado <- chisq.test(tabela)
# H0: Valores Observado e Esperado são iguais
# H1: Valores Observado e Esperado são diferentes
#
resultado
# Aceitamos ou Rejeitamos H0?
# REJEITA a H0, e observamos que há associação entre a doença pulmnoar estudada e a presença de eosinóficlo no escarro (X2: 28.5, gl: 3, P: 2.9e-06).

resultado$stdres
# Há valores maiores que (>) 1.96?
# Se o Valor do resíduo padronizado for = ou > que 1.96
# há assossiação.
# Nossa conclusão é que a presença do Eosinóficlo só ocorre para Asma.

df <- data.frame(tabela)
ggbarplot(df,
  x = "eusinofilo",
  y = "Freq",
  fill = "doenca",
  xlab = "Presença de Eusinófilo",
  ylab = "Frequência observada",
  position = position_dodge(0.8)
) +
  scale_x_discrete(labels = c("nao" = "Não",
                              "sim" = "Sim"))
######################### This is the end! ##################################