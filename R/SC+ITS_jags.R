SC_ITS_model<-function(burnin,
                       samples,
                       thin,
                       chains,
                       n_full,
                       n_modeling,
                       y_modeling,
                       offset,
                       x,
                       z
                       ){

##################################################################
#Statistical Model 
##################################################################
model_string<-"

model{

#Modeling Prior to Intervention (Intervention Occurs at n_pre + 1)
for(t in 1:n_pre){
   
   Y[t] ~ dnegbin(p[t], 
                  r)
   p[t] <- r/(r + lambda[t]) 
   
   log(lambda[t]) <- offset[t] +
                     beta0 +
                     beta1*(t/n) +
                     z[t,]%*%delta + 
                     x[t,]%*%gamma 

   log(lambda_counter[t]) <- offset[t] +
                             beta0 +
                             beta1*(t/n) +
                             z[t,]%*%delta + 
                             x[t,]%*%gamma 

   rr[t] <- lambda[t]/lambda_counter[t]
                  
   }

#Modeling At and One year After Intervention
for(t in (n_pre + 1):(n_pre + 12)){
    
   Y[t] ~ dnegbin(p[t], 
                  r)
   p[t] <- r/(r + lambda[t]) 

   log(lambda[t]) <- offset[t] +
                     beta0 +
                     alpha0 + 
                     beta1*(t/n) +
                     alpha1*((t - (n_pre + 1))/n) + 
                     z[t,]%*%delta + 
                     x[t,]%*%gamma 
                     
   log(lambda_counter[t]) <- offset[t] +
                             beta0 +
                             beta1*(t/n) +
                             z[t,]%*%delta + 
                             x[t,]%*%gamma 

   rr[t] <- lambda[t]/lambda_counter[t]
                     
   }

#Modeling After Intervention
for(t in (n_pre + 13):n){
    
   Y[t] ~ dnegbin(p[t], 
                  r)
   p[t] <- r/(r + lambda[t]) 

   log(lambda[t]) <- offset[t] +
                     beta0 +
                     alpha0 + 
                     eta0 + 
                     beta1*(t/n) +
                     alpha1*((t - (n_pre + 1))/n) + 
                     eta1*((t - (n_pre + 13))/n) + 
                     z[t,]%*%delta + 
                     x[t,]%*%gamma 
                     
   log(lambda_counter[t]) <- offset[t] +
                             beta0 +
                             beta1*(t/n) +
                             z[t,]%*%delta + 
                             x[t,]%*%gamma 

   rr[t] <- lambda[t]/lambda_counter[t]
                     
   }

#Prior Distributions
#delta Priors
for(j in 1:p_z){
   delta[j] ~ dnorm(0.00,
                   0.0001)
}


#gamma Priors: Ridge Regression
for(j in 1:p_x){
  gamma[j] ~ dnorm(0.00, 
                   tau2_inv) 
 }

beta0 ~ dnorm(0.00,
              0.0001)
alpha0 ~ dnorm(0.00,
               0.0001)
eta0 ~ dnorm(0.00,
               0.0001)
beta1 ~ dnorm(0.00,
              0.0001)
alpha1 ~ dnorm(0.00,
               0.0001)
eta1 ~ dnorm(0.00,
               0.0001)
tau2_inv ~ dgamma(0.01,
                  0.01)

r ~ dgamma(0.10,
           0.10)

}
"
####################################################################################################
#Model Fitting
####################################################################################################
model_jags<-jags.model(textConnection(model_string),
                       data = list('n' = n_full,
                                   'n_pre' = n_modeling,
                                   'Y' = y_modeling,
                                   'offset' = offset,
                                   'x' = x,
                                   'z' = z,
                                   'p_x' = ncol(x),
                                   'p_z' = ncol(z)), 
                       n.chains = chains,
                       n.adapt = burnin)  

#
#update(model_jags,
#n.iter = 10000)
posterior_samples<-coda.samples(model_jags, 
                                variable.names = c("lambda",
                                                   "lambda_counter",
                                                   "alpha0",
                                                   "alpha1",
                                                   "eta0",
                                                   "eta1",
                                                   "gamma",
                                                   "rr"),
                                thin = thin,
                                n.iter = samples)

return(posterior_samples)
}
