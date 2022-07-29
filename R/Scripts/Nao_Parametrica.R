################################################################
################################################################
# Universidade Estadual do Rio Grande do Sul - Uergs
# Unidade em Três Passos
# Disciplina: Estatística e Experimentação Argícola
# Prof. Dr. Márlon de Castro Vasconcelos
# Assunto: NÃO PARAMÉTRICAS
################################################################
################################################################
# Assuntos que serão tratados:
#       1. Wilkoxon - Para Os testes t.
#       2. Kruskall-Wallis - ANOVA de um fator
#       3. Friedman - ANOVA de um fator em Blocos
#       4. ANOVA fatorial
################################################################
################################################################
# Os testes não paramétricos são baseados em postos/ranques
# São utilizados quando os pressupostos de um testes paramétrico não é atendido,
# quer sejam:
#   1. Homogeneidade de Variância
#   2. Normalidade da variável resposta
#
# OBS.: Qui-quadrado é um teste não paramétrico
################################################################
# Teste para UMA amostra

library(tidyverse)
library(rstatix)
library(ggpubr)
library(datarium) # Instalar é bem rápido install.packages("datarium")

# Dados
data(mice, package = "datarium")
head(mice, 3)

# Resumo estatistico
mice %>% get_summary_stats(weight, type = "median_iqr")

# Vamos dar uma olhada na distribuição dos dados
bxp <- ggboxplot(
  mice$weight,
  width = 0.5, add = c("mean", "jitter"),
  ylab = "Weight (g)", xlab = ""
)
bxp
# Aparentemente não temos outliers

# 1. Os dados devem ter uma distribuição mais ou menos simétrica em tornoda Mediana
gghistogram(mice,
  x = "weight", y = "..density..",
  fill = "steelblue", bins = 4, add_density = TRUE,
  xlab = "Peso", ylab = "Frequencia"
)
# Tá ok, ainda mais para somente 10 observaçoes (UA's)

# Vamos ao teste
# Queremos saber se o peso mediano dos camundongos difere de 25g (teste bicaudal)?

stat.test <- mice %>% wilcox_test(weight ~ 1, mu = 25)
# Veja que a sintaxe é parecida com o teste t para UMA amostra.
# Estamos comparado contra o valor de mu = 25

# H0: A mediana dos dados é igual ao valor de referência
# SE P < ou = 0.05 = REJEITAR H0
# SE P > 0.05 = ACEITA H0

# Resultado
stat.test
# P = 0.00195 < 0.05; REJEITAMOS H0
# Há diferença entre a mediana do peso dos camundongos em relação ao nosso valor de referência.
####### -----------------------------------
# Teste para DUAS amostra

library(tidyverse)
library(rstatix)
library(ggpubr)
library(datarium)

# Dados
data("genderweight", package = "datarium")

# Observando por grupo
set.seed(123)
genderweight %>% sample_n_by(group, size = 5)


# Estatistica descritiva
genderweight %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "median_iqr")

# Vizaulizaçào
bxp <- ggboxplot(
  genderweight,
  x = "group", y = "weight",
  ylab = "Weight", xlab = "Groups", add = "jitter"
)
bxp

# 1. Os dados devem ter uma distribuição mais ou menos simétrica em tornoda Mediana
gghistogram(genderweight,
  x = "weight", y = "..density..",
  fill = "group", bins = 4, add_density = TRUE,
  xlab = "Peso", ylab = "Frequencia"
  #   facet.by ="group"
)


# Vamos ao teste
# Vamos teste se a mediana entre os grupos são diferetnes
stat.test <- wilcox_test(genderweight, weight ~ group) %>%
  add_significance()

# H0: Soma dos postos do G1 = G2
stat.test
# P = 1.45e-11 (0.0000000000145) < 0.05
# Há diferença nas medianas dos pesos entre os grupos masculino e feminino.
###### -----------------------------------
# Teste para amostra PAREADAS

library(tidyverse)
library(rstatix)
library(ggpubr)
library(datarium)

# Dados
data("mice2", package = "datarium")
head(mice2, 3)

# Aqui vamos formatar nossa tabela para deixar os dados no jeito.
mice2.long <- mice2 %>%
  gather(key = "group", value = "weight", before, after)

mice2.long %>% sample_n_by(group, size = 5)

# Estatistica descritiva

mice2.long %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "median_iqr")

# Uma olhada
bxp <- ggpaired(mice2.long,
  x = "group", y = "weight",
  order = c("before", "after"),
  ylab = "Peso", xlab = "Grupos"
)
bxp

# Pressupostos

# 1. As diferenças devem ter uma distribuição mais ou menos simétrica em tornoda Mediana
mice2 <- mice2 %>% mutate(differences = after - before)
gghistogram(mice2,
  x = "differences", y = "..density..",
  fill = "steelblue", bins = 5, add_density = TRUE
)


# Vamos ao teste
stat.test <- wilcox_test(mice2.long, weight ~ group, paired = TRUE) %>%
  add_significance()

# H0: A mediana das diferenças(Peso Depois - Antes) é iguala a ZEDO

stat.test
# P = 0.00195 < 0.05; REJEITAMOS H0
# A mediana das diferenças no pelo Após em relação a antes difere de zero, logo há efeito da intervenção sobre o ganho de peso dos camundongos.

################################################################
################################################################
# Teste de Kruskall-Wallis (ANOVA um fator não paramétrico)

library(tidyverse)
library(ggpubr)
library(rstatix)

# Dados
# Já disponível no R, não precisa chamar
set.seed(1234)
PlantGrowth %>% sample_n_by(group, size = 3)
# SObre os dados
# Temos a resposta: Peso
# Temos a explicativa: Grupo com 3 níveis
#                       1. Ctr1
#                       2. trt1
#                       3. trt2

# Vamos reordenar os níveis
PlantGrowth <- PlantGrowth %>%
  reorder_levels(group, order = c("ctrl", "trt1", "trt2"))

# Estatística Descritiva
PlantGrowth %>%
  group_by(group) %>%
  get_summary_stats(weight, type = "common") # troque para "full"

# Visualizar os nossos dados por meio de um Box Plot
ggboxplot(PlantGrowth, x = "group", y = "weight")

# Vamos ao teste
res.kruskal <- kruskal_test(PlantGrowth, weight ~ group)

# H0: A mediana dos grupos são iguais.

res.kruskal
# P = 0.0184 < 0.05; REJEITAMOS H0
# Tenho que as medias são diferentes.

# Comparação post hoc ou a posteriori

pwc <- PlantGrowth %>%
  dunn_test(weight ~ group,
    p.adjust.method = "bonferroni"
  )
# "bonferroni" é um ajuste no valor de P, é bem rigoroso

pwc
# Tenho que as medias são diferentes (x2 = 7.99, df = 2, P = 0.0184). E que a mediana do tratamento 1 difereiu do tratamento 2 (P = 0.0150), sendo que as demais medianas não difereiram entre si.




################################################################
################################################################
# Teste de Friedman (ANOVA de um fator em blocos)


library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

# Carregando os dados
data("selfesteem", package = "datarium")
head(selfesteem, 3)

# Organizando os dados
selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)
head(selfesteem, 3)

# Estatistica Descritiva
selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "common")

# Bora dar uma olhada
ggboxplot(selfesteem, x = "time", y = "score", add = "jitter")

# Vamos ao teste
res.fried <- friedman_test(selfesteem, score ~ time | id)
# Aqui temos o Score modelado por/pelo/pela (~) tempo. o uso do |id diz que
# a modelagem deve levar em consideração a variável id que identifica o indivíduo
# que são as UA's.

res.fried
# H0: é que as medianas para o Score são iguais ao longo do tempo
# P = 0.000112 < 0.05; Logo REJEITAMOS H0.
# Temos que pelo menos uma mediana para o score difere ao longo do tempo(x2 = 18.2, df = 2, P = 0.000112).

# Comparação post hoc ou a posteriori
pwc2 <- pairwise_wilcox_test(selfesteem,
  score ~ time,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

pwc2
# As medianas para o score difere ao longo do tempo(x2 = 18.2, df = 2, P = 0.000112), sendo que os tempos diferem entre si (tabela x).

#####################################################################
######################### This is the End!! #########################
#####################################################################
