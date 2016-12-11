

# My AR(1) model

ar<-arima.sim(list(ar=0.9),n=100)
plot(ar)

acf(ar)
 

# ACF
sim_mean <-mean(sim)
ACF <- c()

n=100
x=ar

for(i in 1:20){
  ACF[i]=(1/100)*(sum((x[i:(n)]-sim_mean)*(x[1:(n-i+1)]-sim_mean)))
}

ACF=ACF/ACF[1]

plot(ACF,type = "h")

