island<-read.table("isolation.txt",header=T)
attach(island)
names(island)

model1<-glm(incidence~area*isolation,binomial)

model2<-glm(incidence~area+isolation,binomial)

anova(model1,model2,test="Chi")

library(MuMIn)

AICc(model1)

AICc(model2)

summary(model2)

modela<-glm(incidence~area,binomial)
modeli<-glm(incidence~isolation,binomial)

AICc(modela)

AICc(modeli)