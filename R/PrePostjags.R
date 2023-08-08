reference<-function(burnin,
                    samples,
                    thin,
                    chains,
                    n_full,
                    y,
                    n_modeling,
                    offset,
                    z){
  
  ##############
  #Packages
  ##############
  require(rjags)
  
  #################################################
  #Statistical Model 
  #################################################
  model_string<-"

model{

for(t in 1:n_full){

   #Likelihood
   Y[t] ~ dnegbin(p[t], 
                  r)
   p[t] <- r/(r + lambda[t]) 

   log(lambda[t]) <- offset[t] +
                     z[t,]%*%beta 
                     
   }

#beta Priors
for(j in 1:p_z){
   beta[j] ~ dnorm(0.00,
                   0.0001)
   }


#Hyper-Prior Distributions
r ~ dgamma(0.01,
           0.01)
#Posterior Predictive Samples
for(t in (n_modeling + 1):n_full){
   Y_pred[t - n_modeling] <- Y[t]
   }

}
"

####################################################################
#Model Fitting
####################################################################
model_jags<-jags.model(textConnection(model_string),
                       data=list('n_full' = n_full,
                                 'Y' = y, 
                                 'n_modeling' = n_modeling,
                                 'offset' = offset,
                                 'z' = z,
                                 'p_z' = ncol(z)),
                       n.chains = chains,
                       n.adapt = burnin)  

posterior_samples<-coda.samples(model_jags, 
                                variable.names = c("Y_pred",
                                                   "beta",
                                                   "Y",
                                                   "r"),
                                thin = thin,
                                n.iter = samples)

return(posterior_samples)

}
