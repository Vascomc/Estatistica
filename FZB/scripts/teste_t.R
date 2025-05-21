# Cabeçalho
# Informações gerais sobre o Script
# Aula teste t
#--------------------
# primeira coisa: Pacotes
pacman::p_load(tidyverse, rstatix, ggpubr)

# Carregar os dados
glimpse(dados)
# Análises descritivas
resumo <- dados |> group_by(Colheita) |> 
  get_summary_stats(type = "common")
resumo
# Pressupostos
# 1. Outliers - Valores extremos
dados |> group_by(Colheita) |> identify_outliers(Riqueza)
# Tá ok, sem outliers extremos

# 2. Homogeneidade de variâncias
# Testa via teste de Levene
# H0: As variâncias são iguais
#     - Se P for maior que 0.05 ACEITO H0
dados |> levene_test(Riqueza ~ Colheita)
# Problema de Variâncias

# 3. Normalidade dos dados
# a. Por meio de teste - Teste de Shapiro-Wilk
# H0: Existe normalidade nos dados
#     - Se P for maior que 0.05 ACEITO H0
dados |> group_by(Colheita) |> shapiro_test(Riqueza)
# Dados não possuem normalidade.

# b. Visual
ggqqplot(dados, "Riqueza", facet.by = "Colheita") # Assim que é para fazer
ggqqplot(dados, "Riqueza")

dados$log <- log(dados$Riqueza)

#---- 
# Teste com Log
dados |> t_test(log ~ Colheita,
                var.equal = F)
dados |> t_test(Riqueza ~ Colheita,
                var.equal = F)
# Como comparar REJEITO H0 se P < ou = 0.05
# Conclusão: Existe diferença na Riqueza entre a Safra e a Safrina
# (t: 3.76, gl: 236, P = 0.0002)

# gráfico
ggerrorplot(dados, y = "log", x = "Colheita")
ggerrorplot(dados, y = "Riqueza", x = "Colheita")

# Teste não paramétrico
dados |> wilcox_test(Riqueza ~ Colheita)
# Como comparar REJEITO H0 se P < ou = 0.05

dados |> wilcox_effsize(Riqueza ~ Colheita)

#------------------------------------------
# ANOVA
# Um fator
dados |> group_by(Ontogenia) |> identify_outliers(Riqueza)
# Sem valores extremos

dados |> levene_test(Riqueza ~ Ontogenia)
# Problemas de homogeneidade de variâncias

ggqqplot(dados, "Riqueza", facet.by = "Ontogenia")
dados|> group_by(Ontogenia) |> shapiro_test(Riqueza)
# tem problema de normalidade

# temos duas opções:
#   1. Log da riqueza
#   2. Kruskall-Wallis

# 1. 
dados |> anova_test(log ~ Ontogenia)
# Conclusão: Existe efeito da ontogenia sobre a Riqueza de pragas
# Onde está a diferença?
dados |> tukey_hsd(log ~ Ontogenia)
# Existe acumulo de pragas ao longo do desenvolvimento ontogenético da 
# planta de soja (F: 24.116(2,235), P < 0.001). As diferenças observadas
# referem-se da fase de maturação da vagem em relação às outras duas.

ggerrorplot(dados, y = "Riqueza", x = "Ontogenia") +
  scale_x_discrete(limits = c("Vegetativa", "Reprodutiva", "Maturação"),
                   labels = c("Veg", "Rep", "Mat"))

# 2
dados |> kruskal_test(Riqueza ~ Ontogenia)
dados |> dunn_test(Riqueza ~ Ontogenia)
#-------------------
# Anova em Blocos
modelo <- aov(Riqueza ~ Ontogenia + Colheita, data = dados)
plot(modelo)
shapiro.test(modelo$residuals)
modelo <- aov(log(Riqueza) ~ Ontogenia + Colheita, data = dados)
summary(modelo)
TukeyHSD(modelo)

#------------------
# Anova Fatorial
dados |> group_by(Ontogenia, Colheita) |> identify_outliers(Riqueza)
# nao tem valores extremos
dados |> levene_test(Riqueza ~ Ontogenia * Colheita)
# não tem homogeneidade de variâncias
ggqqplot(dados, "Riqueza") +
  facet_grid(Ontogenia ~ Colheita)
# Problemas de normalidade

dados |> anova_test(log ~ Ontogenia*Colheita)
# Interação Ontogenia:Colheita   Significativa. 
# Não pode analisar a Ontogenia sem a época de colheita.

pos <- dados |> tukey_hsd(log ~ Ontogenia*Colheita)
write.csv(pos, "resultado_tukey_fatorial.csv")

ggerrorplot(dados, y = "log",
            x = "Ontogenia",
            col = "Colheita") +
  scale_x_discrete(limits = c("Vegetativa", "Reprodutiva", "Maturação"),
                   labels = c("Veg", "Rep", "Mat"))
########## por hoje é isso pessoal! #################

