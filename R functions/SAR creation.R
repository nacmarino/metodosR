# limpar memória
rm(list=ls(all=TRUE))

# carregar pacotes
library(ggplot2)
library(dplyr)

# carregar função para criar o SAR
source("R functions/SARcalc.R")

# quando crio os datasets das ilhas, já aproveito para adicionar uma variável binária que diz se existe algum pico na ilha ou não
# esse pico pode ser uma montanha ou uma parede, tanto faz...é só para mostrar como trabalhar com uma GLM

# criando ilhas oceanias e costeiras
oceanica <- SARcalc(seed = 27, size = 40, meanlog = 0, sdlog = 0.3, z = 0.25, c = 1.43, noise = 0.8, ID = "oceanica")
oceanica$montanha <- ifelse(log10(oceanica$area * 1000) > 0.5, "sim", "nao")

costeira <- SARcalc(seed = 21, size = 40, meanlog = 0.35, sdlog = 0.1, z = 0.19, c = 1.8, noise = 0.8, ID = "costeira")
costeira$montanha <- ifelse(log10(costeira$area * 1000) > 1.5, "sim", "nao")

# juntando os dois tipos de ilha no mesmo data frame
ilhas <- rbind(oceanica, costeira)

# aumentando valor da área
ilhas$area <- ilhas$area*1000

# usando o modelo com base no que sabemos para ver os resíduos
modelo <- lm(log10(riqueza) ~ log10(area)*ilha, data = ilhas)

# resíduo do modelo contra valores ajustados
plot(resid(modelo) ~ fitted(modelo))

# salvando resíduos para continuarmos a trabalhar e criar mais variáveis
residuo <- resid(modelo)
cut(residuo, breaks = 6)
# eu cortei em 6 fatias e vou separar grosseiramente elas, para que as coisas não fiquem perfeitas e continue existindo um bocado de resíduo para mais variáveis

# adicionando uma variável com o tamanho do arquipélago
ilhas$arquipelago <- ifelse(residuo < -0.1, "pequeno", ifelse(residuo > 0.1, "grande", "medio"))

# atualizando o modelo
modelo <- lm(log10(riqueza) ~ log10(area) * ilha + arquipelago, data = ilhas)
summary(modelo)
residuo <- resid(modelo)

# vendo os resíduos
plot(residuo ~ fitted(modelo))

# adicionando um ruído aos resíduos
residuo <- residuo + rnorm(n = 80, sd = 0.15)
residuo <- residuo * 1000
residuo <- residuo + abs(min(residuo))

# e adicionando ele aos dados
ilhas$populacao <- residuo

# atualizando o modelo
modelo <- lm(log10(riqueza) ~ log10(area) * ilha + arquipelago + populacao, data = ilhas)
summary(modelo)
residuo <- resid(modelo)

# vamos começar a adicionar variáveis redundantes e inúteis à tabela de dados

# primeiro, vamos adicionar uma variável com a produtividade da ilha, que vai ter relação próxima com a area
set.seed(875)
ilhas$produtividade <- ilhas$area^(0.75 + rnorm(n = 80, sd = 0.3))

# diversidade de habitats
# usando os residuos da relação entre area e riqueza (que estão livres do que é explicado pela riqueza) para gerar a nova variável de diversidade
# de habitats, que se relaciona com a area, mas não com a riqueza
ilhas$habitat <- round(resid(lm(log10(area) ~ log10(riqueza), data = ilhas)) + abs(min(resid(lm(log10(area) ~ log10(riqueza), data = ilhas)))))

# temperatura
set.seed(754)
ilhas$temperatura <- rnorm(n = 80, mean = 20, sd = 6)

# precipitacao
set.seed(64)
ilhas$precipitacao <- rnorm(n = 80, mean = 1600, sd = 600)

# criando um ID aleatorio para as ilhas
ilhas$ID <- sample(paste0("ilha_",seq(1:80)), size = 80, replace = FALSE)

# organizando tabela
ilhas <- select(ilhas, ID, ilha, arquipelago, riqueza, area, produtividade, populacao, habitat, montanha, temperatura, precipitacao) %>% 
  mutate(area = round(area, digits = 4)) %>% 
  arrange(ilha, area, arquipelago)

# uma variável com distribuição de poisson
# vou criar uma variável que tem uma média baixa, que vai ser a quantidade de mamiferos para ilhas oceanicas
set.seed(174)
(x <- sort(rpois(n = 40, lambda = 3)))
# e a mesma variável só que com média mais alta, que vai ser a quantidade de mamíferos para ilhas costeiras
set.seed(174)
(y <- sort(rpois(n = 40, lambda = 8)))

# adicionando essa variável à tabela
ilhas$mamiferos <- c(x,y)

# reorganizando planilha
ilhas <- select(ilhas, ID, ilha, arquipelago, riqueza, area, produtividade, populacao, habitat, montanha, mamiferos, temperatura, precipitacao) %>% 
  arrange(ID)

# salvando arquivo criado para a aula
write.table(x = ilhas, file = "Dia 1/dados/ilhas.txt", row.names = FALSE, sep = "\t")