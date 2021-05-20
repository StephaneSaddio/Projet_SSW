seque <- seq(0,30, length.out = 60)
ychi2 <- dchisq(seque, 6)
plot(seque, ychi2, type = "l", col = "red")

rand <-rchisq(100, 6)
hist(rand, breaks = 60, freq = F, xlim=c(0, max(rand)+3))

#      hat_f <- function(x, X, bandwith, kern) { # X est l'échantillon, x de \hat f(x)
#        for (i in 1:length(x)){
#          hatf$vect[i] <- sum(kern((X-x[i])/bandwith)) / (bandwith*length(X)) # formule noyau
#        }
#        
#        return(hatf)
#      }
      
dens <- function(sample, bandwith, kernel){

      X <- sample
      n <- length(X)
      x <- seq.int(min(X), max(X), length.out = n)
      if (missing(kernel)) {kernel = "gaussian"}
      kern <- switch(kernel,
                  gaussian = function(u){return(dnorm(u))},
                  rectangular = function(u){
                    ifelse(abs(u) < 1, .5, 0) },
                  triangular = function(u) {
                    return(ifelse(abs(u) < 1, (1 - abs(u)), 0)) },
                  epanechnikov = function(u){
                   return(ifelse(abs(u) < 1, 3/4*(1 - u^2), 0)) },
                  biweight = function(u){ 
                    return(ifelse(abs(u) < 1, 15/16*(1 - u^2)^2, 0)) }) # noyau caractère en fonction
      

      
    if (missing(bandwith)){
        
      bandwith <- seq(0,5,0.1)
      s <- 0
      for (i in 1:n){ 
      s<- s + sum(kern( (X[i]-X[-i])/bandwith)) 
      }
      
      #n2hat_f <- norm(hat_f,2)^2  # ne fonctionne pas, doit être un vecteur
      #n2hat_f <- hat_f(x, X, bandwith)$norm22
      n2hat_f <- integrate(hat_f^2)
      
      R <- n2hat_f - 2/(n(n-1))* s /bandwith
      #gradient ?
      bandwith <- optim(bandwith, R)
      #bandwith <- bandwith[which.max(R)]
    }
      hatfvect <- NULL
      for (i in 1:n){
        som = 0
        for (j in 1:n){
          som = som + kern((X[j]-x[i])/bandwith)
        }
        hatfvect[i] <- som / (bandwith*n) # formule noyau
      }
      return(hatfvect)
      
  
}
plot(rand)
rand[rand=a$y]

test = dens(rand, bandwith = 1.1)
plot(seq(min(rand), max(rand), length.out =  100), type="l", test)
lines(dchisq(0:21, 6)) 
lines(density(rand, kernel = "gaussian"), col = "red")
a=density(rand)
str(a)

Risk <- function(sample, bandwith, kernel){
  
  X  <- sample
  n <- length(X)
  x <- seq.int(min(X), max(X), length.out = n)
  if (missing(kernel)) {kernel = "gaussian"}
  kern <- switch(kernel,
                 gaussian = function(u){return(dnorm(u))},
                 rectangular = function(u){
                   ifelse(abs(u) < 1, .5, 0) },
                 triangular = function(u) {
                   return(ifelse(abs(u) < 1, (1 - abs(u)), 0)) },
                 epanechnikov = function(u){
                   return(ifelse(abs(u) < 1, 3/4*(1 - u^2), 0)) },
                 biweight = function(u){ 
                   return(ifelse(abs(u) < 1, 15/16*(1 - u^2)^2, 0)) }) # noyau caractère en fonction
  
  hatf <- function(x, sample, h, kernel){
    n <- length(sample)
    som <- 0
    for (i in 1:n){
     som <- som + kern((sample[i]-x)/h) 
    }
    return(som/(n*h))
  } 
   
  s <- 0
  for (i in 1:n){ 
    s <- s + sum(kern( (X[i]-X[-i])/bandwith)) 
  }
  

  n2hat_f <- integrate(function(x) hatf(x, sample = X, h = bandwith, kernel = kern), min(X)-50, max(X)+50)$value ^2

  R <- n2hat_f - 2/(n*(n-1))* s /bandwith

  return(R)
}

Risk(rand, bandwith = 0.025)

x <- seq(0.03, 1, by = 0.01)
risvec <- NULL
j=1
for (i in x){risvec[j]=Risk(rand,i); j=j+1}

plot(x, risvec)





MSE <- function(m, o){
  mean((m - o)^2)
}
id <- function(x){x+1}
id(2)
 