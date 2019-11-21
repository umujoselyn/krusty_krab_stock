#See the book for simulation on page 440

#goal: write a function that simulates the S(t): the stock price

#Sigma_sq:
#S_0: initial price of the stock
#t: how many days we want to simulate

s<- function(mu=.05, sigma_sq=.0025,s_0=100,t=10){
  out<- rep(NA,t)
  z<- rnorm(1)
  out[1]<- s_0* exp(mu-1/2*sigma_sq+sqrt(sigma_sq)*z)
  for (i in 2:t){
    z<- rnorm(1)
    out[i]<-out[i-1]* exp(mu-1/2*sigma_sq+sqrt(sigma_sq)*z)
  }
 return(out)
}



#We are finding the values

#mu                                         #0.05                0.01             0.01
#var                                        #0.0025              0.0025           0.01
#Estimate of Es(100)                      149.227559             2.72             2.6711
#95%CI for Es(100)                 [149.212076, 149.243042]     [2.72,2.705]      2.67,2.671
#Probability that s(100)>s(0)                  100%              95.78%           69.53
#95%CI  for Probability that s(100)>s(0)    (1,1)              (95.38,96.17)      (68.6,70.4)
nsim<- 10000
barnacle_boy<-list()
for(j in 1:nsim){
  barnacle_boy[[j]]<- s(mu=0.05,sigma_sq = 0.0025,s_0=1,t=100)
}
#Q1: Estimate the Es(100)
barn_mat<-do.call(rbind,barnacle_boy)

#To take the average of the columns I will do this 

barn_mat_mean<- apply(barn_mat,2,mean) # the last number in this matrix is the Es(t)= 149.227559


#Q2: Estimate the 95% CI of Es(100)
# To get the 95% interval, we will use the wald : mean+/- Z_(alpha/2)*se(mean)
   #note that the var[mean]= var(x)/n... the var(x) is the variance od the  columns of barn_mat because that is the S at time t

barn_mat_var<- apply (barn_mat,2,var)
LB<-barn_mat_mean+ -1*qnorm(.975)*sqrt(barn_mat_var)/nsim
UB<-barn_mat_mean+ 1*qnorm(.975)*sqrt(barn_mat_var)/nsim
CI<-cbind(LB,UB) #149.212076 149.243042 this is the CI 

#Q3: Estimate the prob that I made money:  Probability that s(100)>s(0) 
prob_boy<-apply(barn_mat>1,2,mean) # barnmat>1 tell me wwhere i made money(true and false, and then take the average of the columns where I made money)
plot(prob_boy)

#Q4: 95% CI of the prob that I made money:  Probability that s(100)>s(0)
      #phat+/- crit*se(phat)
      #se(phat)=sqrt(phat(1-phat)/n)
prob_boy_se<-sqrt( prob_boy*(1-prob_boy)/nsim)
lb<- prob_boy-1*qnorm(0.975)*prob_boy_se
ub<- prob_boy+1*qnorm(0.975)*prob_boy_se
cbind(lb,ub)

#for other values, you repeat the above codes




