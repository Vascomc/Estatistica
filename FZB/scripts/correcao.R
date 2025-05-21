# Atividades 1 
# Olá Pessoas, tudo bem?

# O que fazer. 
# Pegar um conjunto de dados que contenha cerca de 5 variáveis. O que se atentar: 
# Uma variável qualitativa nominal e 4 quantitativas.

# o que fazer?
#  - Indicar a variável resposta.
# 1. Estatística resumo;
# 2. Um modelo ANOVA;
# 3. Um modelo Regressao linear simples;
# 4. Um modelo Regressao linear multipla;
# - Usar a função step() para selecionar o modelo mais parcimonioso, lembrando que escolhemos o 
#   MENOR valor de AIC.

# o que deverão me enviar.
# Arquivo no word com os passos e comandos utilizados. 
# Escrevam as concusões para os teste de pressupostos e avaliação dos modelos.


# OBS.: QUEM NÃO TIVER CONKUNDO DE DADOS USAR O ABAIXO:

# para informações do conjunto de dados
help(mtcars)

pacman::p_load(tidyverse, rstatix, ggpubr, lmtest, QuantPsyc, psych, car)

########
# ANOVA

# Variável resposta é mpg 
summary(mtcars)

dados <- mtcars |> convert_as_factor(am, vs, carb, cyl)

dados |> group_by(carb) |> get_summary_stats(type = "mean_se")  |> filter (variable == "mpg")
dados |> group_by(cyl) |> get_summary_stats(type = "mean_se") |> filter (variable == "mpg")
dados |> group_by(cyl) |> get_summary_stats(type = "common") |> filter (variable == "mpg")

# Modelo de ANOVA
dados |> group_by(cyl) |> identify_outliers(mpg) # valores extremos
ggqqplot(dados, x = "mpg") + facet_grid(cols = vars(cyl)) # normalidade
dados |> group_by(cyl) |> shapiro_test(mpg) # normalidade
dados |> levene_test(mpg ~ cyl) # homogeneidade
# temos problemas. Alternativa é o teste de Kruskall-Wallis

modelo <- dados |> anova_test(mpg ~ cyl)
pwc <- dados |> tukey_hsd(mpg ~ cyl)

pwc <- pwc %>% add_xy_position(x = "cyl")

ggerrorplot(dados, x = "cyl", y = "mpg",
            xlab = "Cilindradas",
            ylab = "milhas por galão") +

  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(modelo, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

###########
# Linear Simples

ggscatter(dados, x = "wt", y = "mpg", color = "cyl")

modelo <- lm(mpg ~ wt*cyl, data = dados)
plot(modelo)

rownames(mtcars)

summary(modelo)

37.2851 - 5.3445*0
39.571 -5.647*5 + 3.455*8

# linear multipla
# 4 variáveis
modelo <- lm (mpg ~ disp*hp*wt*qsec, data = mtcars)
attach(mtcars)
correlacao <- cor(mtcars)

par(mfrow = c(2, 2))

plot(modelo)

par(mfrow = c(1, 1))
vif(modelo)

summary(aov(modelo))


step(modelo)
