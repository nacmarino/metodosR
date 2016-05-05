numbers<-read.table("sexratio.txt",header=T)
numbers
attach(numbers)
par(mfrow=c(2,2))

p<-males/(males+females)
plot(density,p,ylab="Proportion male")
plot(log(density),p,ylab="Proportion male") 

y<-cbind(males,females)
model.dens<-glm(y~density,binomial)
summary(model.dens)

model.logdens<-glm(y~log(density),binomial)
summary(model.logdens)

plot(p, log(density), cex=1.5, lwd=2, ylab="Probabilidade de Extinção", xlab="Isolamento (m)")
