

# My AR(1) model

ar<-arima.sim(list(ar=0.9),n=1024)
plot(ar)

acf(ar)

generate_ar <- function(initial = 0, n=1024, phi, error_sd = 1){
  sim = c(initial)
  for (i in 1:1024){
    sim[i+1] = sim[i]*phi+rnorm(n=1,mean=0,sd=error_sd)
  }
  return(sim)
}
 

# ACF

sim = generate_ar(phi=0.2)
plot(sim,type = "l")

get_acf<- function(sim,max_lag=30){
  sim_mean <- mean(sim)
  n = length(sim)-1
  ACF <- c()
  for(i in 1:max_lag){
    ACF[i]=(1/n)*(sum((sim[i:(n)]-sim_mean)*(sim[1:(n-i+1)]-sim_mean)))
  }
  ACF=ACF/ACF[1]
  return(ACF)
}

acf <- get_acf(sim)
plot(acf,type='h')+abline(h=2/sqrt(length(sim)-1),col='blue')+abline(h=-2/sqrt(length(sim)-1),col='blue')


# Check for validity 



