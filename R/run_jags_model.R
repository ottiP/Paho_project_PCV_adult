run_jags_model <- function(df,model,country,data_prep_model,burnin,samples,intervention_date,post_period,eval_period,month1,month2,outcome_name){
  
  ## Model 3: SC+ITS with ridge variable selection 
  ## Run the model over all age groups
  n_cores<-detectCores()-1
  ds.age.spl<-split(df, df$agec)
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, {
    library(lubridate, quietly = TRUE)
    library(HDInterval, quietly = TRUE)
    library(dplyr, quietly = TRUE)
    library(zoo, quietly = TRUE)
    library(rjags, quietly = TRUE)
  })
  clusterExport(cl, c('burnin','samples','intervention_date','post_period','eval_period','month1','month2','outcome_name'), environment())
  mod1<-pblapply(cl = cl,X=ds.age.spl,FUN=data_prep_model)
  stopCluster(cl)
  
  ## Save results
  saveRDS(mod1,file=paste0("./Results/",model,"_",country,".RDS"))
  return(mod1)
  
}