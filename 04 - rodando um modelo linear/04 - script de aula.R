# carregando o conjunto de dados
dados <- read.table("04 - rodando um modelo linear/dados/ilhas.txt", header = TRUE)
dados # objeto contedo os dados das ilhas

# vamos observar apenas as primeiras linhas desta tabela
head(x = dados)

## hipóteses e teorias que podem explicar a variação na riqueza de espécies
# relação espécie-área: area
# biogeografia de ilhas: area, distancia
# especies x energia: produtividade, temperatura, precipitacao
# heterogeneidade de habitat: habitat, montanha
# disturbio: populacao

# analise exploratoria
plot(riqueza ~ area, data = dados)
plot(riqueza ~ ilha, data = dados)
plot(log(riqueza) ~ log(area), data = dados)
plot(log(area) ~ log(produtividade), data = dados)
pairs(dados)

# que tipo de distribuicao e transformacao melhor se adequa à riqueza de espécies
hist(dados$riqueza) # histograma
qqnorm(dados$riqueza);qqline(dados$riqueza) # qqplot
shapiro.test(dados$riqueza) # teste de shapiro-wilk
library(fitdistrplus) # outro modo visual
descdist(data = dados$riqueza, boot = 1000)
descdist(data = log(dados$riqueza), boot = 1000)
descdist(data = sqrt(dados$riqueza), boot = 1000)
qqnorm(log(dados$riqueza));qqline(log(dados$riqueza))

# vamos usar a transformacao em log para a riqueza de especies

## criação de modelos
### GLMs

# estabelecendo um modelo nulo
modelo_nulo <- glm(log(riqueza) ~ 1, dados, family = gaussian())
summary(modelo_nulo)
# o que são:
## o estimate?
## o Std. Error?
## Deviance
## AIC

# criando outros modelos
modelo1 <- glm(log(riqueza) ~ log(area), dados, family = gaussian())
modelo2 <- glm(log(riqueza) ~ populacao, dados, family = gaussian())
modelo3 <- glm(log(riqueza) ~ ilha, dados, family = gaussian())
modelo4 <- glm(log(riqueza) ~ log(area) * ilha, dados, family = gaussian())
modelo5 <- glm(log(riqueza) ~ log(area) + ilha, dados, family = gaussian())

# comparando os modelos
## via anova
anova(modelo_nulo, modelo1, test = "F")
anova(modelo_nulo, modelo2, test = "F")
anova(modelo_nulo, modelo1, modelo5, test = "F")
anova(modelo5, modelo4, test = "F")
anova(modelo1, modelo2, test = "F")

## via AIC
AIC(modelo1, modelo2)

# vamos observar os resultados detalhados do modelo
summary(modelo4)

# onde esta o efeito da ilha costeira?
summary(glm(log(riqueza) ~ log(area) + populacao, dados, family = gaussian()))

### SELEÇÃO DE MODELOS, ESTILO BALA DE SAL
options(na.action = na.fail())



### DEPOIS DO ALMOÇO:
# 1. SELEÇÃO DE MODELOS
# 2. vALIDAÇÃO DE MODELOS
# 3. TESTES DE SIGNIFICANCIA
# 4. POS-TESTE
# 5. MODELOS MISTOS

