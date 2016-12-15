

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

check_valid <- function(n,acf){
  criteria <- 2/sqrt(n)
  j=1
  while(abs(acf[j])>criteria){
    j=j+1
    if(j>=length(acf)){
      return(j)
    }
  }
  return(j)
}


v<-c()
test_seq <- seq(-1,1,0.05)
for (phi in test_seq){
  valid <- c()
  for (k in 1:10){
    sim<- generate_ar(phi=phi)
    acf <- get_acf(sim)
    valid <- c(valid,check_valid(length(sim)-1,acf))
  }
  mean_valid <- mean(valid)
  v <- c(v,mean_valid)
}

plot(x=test_seq,y=v,type='h')







