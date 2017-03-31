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
summary(glm(log(riqueza) ~ 0 + log(area) + ilha, dados, family = gaussian()))

### SELEÇÃO DE MODELOS, ESTILO BALA DE SAL
library(MuMIn)
options(na.action = na.fail)
modelo_cheio <- glm(log(riqueza) ~ ilha + arquipelago + log(area) + log(produtividade) +
                      populacao + habitat + montanha + temperatura + precipitacao,
                    data = dados, family = gaussian())
summary(modelo_cheio)
# tabela de selecao de modelos
selecao <- dredge(global.model = modelo_cheio)
head(selecao)

# importancia das variaveis
importance(x = selecao)

# o que incluir?
modelo_final <- glm(log(riqueza) ~ ilha + arquipelago + log(area), data = dados, family = gaussian())
summary(modelo_final)

# teste de normalidade dos residuos
vetor_residuos <- residuals(modelo_final)
hist(vetor_residuos)
qqnorm(vetor_residuos);qqline(vetor_residuos)

plot(vetor_residuos ~ log(riqueza), data = dados)
plot(predict(modelo_final) ~ log(riqueza), data = dados)

## teste de significancia
anova(modelo_final, test = "F")

# anova com soma dos quadrados tipo 2 e/ou 3
library(car)
Anova(modelo_final, type = "II", test.statistic = "F")

# pos testes
summary(modelo_final)
library(lsmeans)
teste <- lsmeans(modelo_final, ~ilha + arquipelago)
teste
contrast(teste, "pairwise")

# exemplo via distribuicao de poisson
exemplo_pois <- glm(riqueza ~ area + ilha + arquipelago, data = dados, family = poisson())
summary(exemplo_pois)

### modelos mistos
library(lme4) # ou nlme
# LMM e GLMM

# modelos-exemplo
modelo6 <- lmer(log(riqueza) ~ log(area) + ilha + (1|arquipelago), data = dados, REML = FALSE)
summary(modelo6)
qqnorm(resid(modelo6));qqline(resid(modelo6))

modelo7 <- lmer(log(riqueza) ~ (1|arquipelago), data = dados, REML = FALSE)
summary(modelo7)

# comparacao entre modelos
anova(modelo6, modelo7)

## teste de significancia
library(lmerTest)
lmerTest::anova(modelo6, type = 2)

# GLMM
## FIQUEM LONGE DISSO
### PERIGO
## FUJA
glmer(riqueza ~ log(area) + ilha + (1|arquipelago), data = dados, family = poisson())
