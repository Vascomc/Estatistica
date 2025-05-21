# Análises multivariáveis
#
# Dois modos onde avaliamos: Q: Unidades Amostrais
#                            R: Descritores/variáeis

# Natureza daos dados determina a necessidade de transformações escalares ou vetoriais
#  - Escalares: transformações que visam colocar os dados na mesma escala de medida
#               dados de natureza de medidas diferentes. Ex.: mg, mg.l, g, etc.
#  - Vetoriais: normalmente em dados de comunidade. log, raiz quadrada, hellinger

# obs.: para dados com diferentes TIPOS de dados não transformamos os dados, e nesse caso,
#       usamos como medida de semelhança o índice de gower.
# --------------------------------------

## ORDENAÇÃO

# Pacotes
pacman::p_load(
  tidyverse, # para manipulação dos dados
  ggpubr, # gráficos
  rstatix, # resumos estatísticos
  vegan, # para transformações, distâncias, ordenações
  factoextra, # vizualisar as ordenações
  FactoMineR # realizar as ordenações
)

# Análise de Componentes Principais - PCA #

# 1. Dados
# dados <- readxl::read_xlsx("caminho no computador")

dados <- readxl::read_xlsx("/Users/marlonvasconcelos/Analise_de_dados/Alunos_Graduacao/Ana_Paula/TCC/dados_TTC_ana.xlsx")

head(dados)

# pegando os dados que nos interassa
(dados.amb <- dados |> select(Argila:Sat_Base)) # podemos usar o : pq as variáveis estão em sequência

# Temos variáveis com unidades de medida diferentes, precisamos transformar os dados


# 2. Transformando os dados
help("decostand")
transformados <- decostand(dados.amb, "standardize") # padrão por coluna
# essa transformação centra em média 0 e DP = 1

# 3. Realizando a PCA
res.pca <- PCA(transformados,
  scale.unit = F, # O scale.unit faz o mesmo que o "standardize"
  graph = F # não chamar o gráfico altomaticamente
)

res.pca2 <- PCA(dados.amb,
  scale.unit = T, # O scale.unit faz o mesmo que o "standardize"
  graph = F # não chamar o gráfico altomaticamente
)
# só para fins de exemplo
plot(res.pca)
plot(res.pca2)

# Vamos ver alguns resultados da PCA
fviz_eig(res.pca, addlabels = TRUE)

var <- get_pca_var(res.pca)

# Qualidade da representação
head(var$cos2, 4)

# Correlação com os eixos
head(var$cor, 4)

# COntribuição da variável com a variância de um dado eixo
head(var$contrib, 4)

# Biplot

fviz_pca_biplot(res.pca,
  col.ind = dados$manejo,
  palette = "jco",
  label = "var",
  col.var = "black",
  repel = TRUE,
  legend.title = "Tipos de Manejo"
)

# Por definição a PCA é baseada em Distâncai Euclidiana.


## ANALISE DE COORDENADAS PRINCIPAIS ##

# Usando dados mistos
names(dados)
dados.amb2 <- data.frame(dados |> select(Épocas:Sat_Base))

# distância de Gower
dist.gow <- FD::gowdis(dados.amb2)

# PCoA
res.pcoa3 <- ape::pcoa(dist.gow, correction = "cailliez")

## Porcentagem de explicação do Eixo 1
100 * (res.pcoa3$values[, 1] / res.pcoa3$trace)[1]

## Porcentagem de explicação dos Eixo 2
100 * (res.pcoa3$values[, 1] / res.pcoa3$trace)[2]

# Retirar os eixos
eixos <- data.frame(res.pcoa3$vectors[, 1:2])


# Agora usemos a função cmdscale que tbm faz a PCoA para pegar os dados das
# variáveis


res.pcoa4 <- cmdscale(dist.gow)


(spe.b.pcoa.env <- envfit(res.pcoa4, dados.amb2))
# Plot significant variables with a user-selected colour
plot(spe.b.pcoa.env, p.max = 0.05, col = 4)

??envfit

variaveis <- data.frame(scores(spe.b.pcoa.env, "vectors"))

names(eixos)
names(variaveis)

library(ggrepel)

ggplot() +
  geom_point(eixos,
    mapping = aes(
      x = Axis.1, y = Axis.2
        , shape = dados$manejo
      , fill = dados$manejo,
      col = dados$Épocas,
      #   alpha = 0.5
    ),
    size = 3
  ) +
  geom_vline(xintercept = 0, alpha = 0.9, linetype = "dashed") +
  geom_hline(yintercept = 0, alpha = 0.9, linetype = "dashed") +
  theme(
    text = element_text(family = "Times", size = 14),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_line(colour = NULL),
    panel.grid.major = element_line(colour = NULL),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(fill = NA, colour = "black"),
    axis.text.x = element_text(color = "black", size = 12, angle = 0),
    axis.text.y = element_text(color = "black", size = 12, angle = 0)
  ) +
  geom_segment(
    data = variaveis,
    aes(
      x = 0, y = 0,
      xend = Dim1,
      yend = Dim2
    ),
    size = 1,
    arrow = arrow(
      length = unit(0.2, "cm"),
      type = "closed",
      angle = 15
    ),
    color = "black"
    #   ,alpha = 0.3
    , linewidth = 0.2
  ) +
  geom_text_repel(
    data = variaveis,
    mapping = aes(
      x = Dim1,
      y = Dim2,
      label = rownames(variaveis)
    ),
    color = "black",
    size = 4,
    fontface = "bold.italic",
    segment.color = "grey",
    alpha = 0.8,
    segment.size = 0.1,
    box.padding = unit(0.3, "lines")
  ) +
#  scale_color_manual(values = c("#D95F02", "#7570B3", "#A6761D")) +
  xlab("Dim 1 (70.56%)") +
  theme(axis.title.x = element_text(angle = 0, size = 13)) +
ylab("Dim 2 (19.76%)") +
  theme(axis.title.y = element_text(angle = 90, size = 13))
