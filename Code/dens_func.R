seque <- seq(0,30, length.out = 60)
ychi2 <- dchisq(seque,6)
plot(seque, ychi2, type = "l", col = "red")

rand <-rnorm(100 ,15,1)
hist(rand, breaks = 60, freq = F, xlim=c(0, max(rand)+3))

#      hat_f <- function(x, X, bandwith, kern) { # X est l'échantillon, x de \hat f(x)
#        for (i in 1:length(x)){
#          hatf$vect[i] <- sum(kern((X-x[i])/bandwith)) / (bandwith*length(X)) # formule noyau
#        }
#        
#        return(hatf)
#      }
      
dens <- function(X, bandwith, kernel){

  
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
      bw
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



test = dens(rand, bandwith = "ucv", "rectangular")
plot(seq(min(rand), max(rand), length.out =  1000), test)
lines(dchisq(0:100,6)) 
lines(density(rand, kernel = "rectangular", bw="ucv"), col = "red")
a=density(rand)
str(a)
