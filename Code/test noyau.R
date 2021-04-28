library(sm)
library(ape)
library(geiger)

# Estimation de la densité par la méthode des noyaux (Test)

## Chi 2 à 6 ddl

seq <- seq(0,30, length.out = 60)
ychi2 <- dchisq(seq,6)
plot(seq, ychi2, type = "l", col = "red")

rand <-rchisq(100,6)
hist(rand, breaks = 60, freq = F, xlim=c(0, max(rand)+3))
lines(density(rand, kernel =  "epanechnikov", bw = 1), col ="black") # bw = h
lines(density(rand),col="blue")
lines(density(rand, kernel =  "rectangular"), col ="purple")
lines(density(rand, kernel =  "triangular"), col ="green")
lines(seq, ychi2, col = "red")
legend("topright",c("gaussien", "epanechnikov", "rectangulaire", "triangulaire","chi2"),lty=1, col = c("blue", "black","purple","green","red"))
rug(rand,col="darkred")
#sm.density(rand, kernel="gaussian", col="yellow")

# sm.density.compare(time, genre ou famille)
# display="se" pour l'IC 
# model = "Normal" 

## Noyau de densité avec dataset 

### test avec bird families

#### arbre
data("bird.families")
op <- par()
par(cex = 0.3)
plot(bird.families, type = "c")
par(cex = op$cex)

#### noyau gaussien

d<-bird.families$edge.length
#attach(d)
vec <- pretty(0:(max(d)+3),((max(d)+3)*2))
hist(d, breaks = vec, freq = F)
lines(density(d), col = "red") # bw = 1.82

plot(density(d, kernel= "gaussian", window = "gaussian", bw=1), col = "red", bty = "n")
polygon(density(d, kernel= "gaussian"),col=2,border = "blue")
rug(d, col= "red")

### test avec chelonia (turtules)

#### arbre
data(chelonia)
op <- par()
par(cex = 0.3)
plot(chelonia$phy)
par(cex = op$cex)
plotTreeTime(chelonia$phy,chelonia$dat)

#### noyau gaussien
d<-chelonia$phy$edge.length
d1<-chelonia$dat
vec <- pretty(0:(max(d)+3),((max(d)+3)*2))
hist(d, breaks= vec,freq = F, ylim = c(0,0.8))
lines(density(d), col = "red") # bw = 0.201
lines(density(1.4^d1), col="blue")

lines(density(d, kernel= "gaussian"), col = "red", bty = "n")
polygon(density(d, kernel= "gaussian"),col=2,border = "blue")
rug(d, col= "red")

#str(density(d)) pour le bw
#hyp: 1.4^dat \sim edge.length
