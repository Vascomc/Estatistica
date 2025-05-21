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

##### -------------------------------------------------------
# ANÁLISE DE AGRUPAMENTO #

# 1. Dados de composição de espécies dados "mite"
# 2. Distância por bray-curtis
# 3. Agrupamento por ligaçào simplesa e variância mínima
# 4. Correlação cofonética: Avalia a qualidade do agrupamento
# -------------------------------------------------
# Pacotes
pacman::p_load(
  tidyverse, # para manipulação dos dados
  ggpubr, # gráficos
  rstatix, # resumos estatísticos
  vegan # para transformações, distâncias, ordenações
)

# 1. Dados
devtools::install_github("paternogbc/ecodados") # instalar uma vez só

sp_compos <- ecodados::bocaina
sp_compos <- t(sp_compos) # Aqui transportamos os dados para que possamos compara as unidades amostrais.

head(sp_compos)

# Transformando os dados para presença e ausência
com_pa <- decostand(sp_compos, "pa") # transformar dados para presença/ausência

head(com_pa)

# 2. medida de distância
comp_dist <- vegdist(com_pa, "bray") # bray curtis em dados de P/A funciona como sorensen

# 3. agrupamento
com_clust <- hclust(comp_dist, "single") # ligação simples
com_clust_2 <- hclust(comp_dist, "ward.D2") # variância mínima

par(mfrow = c(1, 2)) # divide a janela do gráfico "mfrow = c(i,j)"
# i = linha
# j = coluna

plot(com_clust)
plot(com_clust_2)

par(mfrow = c(1, 1)) # para retornar ao normal

# Veja que o mesmo conjunto de dados, com a mesma distância produziu
# dendogramas diferentes.


# 4. Correlação cofonética
# Avalia a "qualidade" do agrupamento

cofresult <- cophenetic(com_clust)
cor(cofresult, comp_dist)

cofresult <- cophenetic(com_clust_2)
cor(cofresult, comp_dist)

# Valores acima de 0.7 são desejáveis, quanto mais próximo de 1 melhor.
#  - A correlação cofonética mensura a correlação dos valores de agrupamento com a matriz de
#    similaridade.

# mudando a medida de similaridade
comp_dist <- vegdist(com_pa, "jaccard")
com_clust <- hclust(comp_dist, "single") # ligação simples
com_clust_2 <- hclust(comp_dist, "ward.D2") # variância mínima
par(mfrow = c(1, 2))
plot(com_clust)
plot(com_clust_2)
par(mfrow = c(1, 1)) # para retornar ao normal

cofresult <- cophenetic(com_clust)
cor(cofresult, comp_dist)
cofresult <- cophenetic(com_clust_2)
cor(cofresult, comp_dist)
