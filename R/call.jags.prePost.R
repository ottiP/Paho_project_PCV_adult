### Data manipulation script

data_prep_model <- function(df){
  df1 <- df%>%
    arrange(monthdate) %>%
    mutate( monthn = lubridate::month(monthdate),
            year=year(monthdate),
            intercept = rep(1, length.out=n()),
            monthdate2= year + monthn/12 -1/12,
            vax0 = if_else(monthdate2>=month1 & monthdate2<month2,1,0),
            vax1 = if_else(monthdate2>=month2,1,0),
            index = row_number()/n())%>%
    dplyr::select( -monthn,-year)
  Xmat <- df1 %>% 
    dplyr::select(-vax0, -vax1,-agec,-monthdate2,-monthdate)
  covars <-   Xmat %>%
    ungroup() %>%
    dplyr::select(-J12_J18_any, -index,-intercept,-acm_noj_nodiarr_prim,-J13_prim, -J12_J18_prim, -J09_J18_prim , -J00_J99_prim,-J20_J22)
  
  #covars.smooth <- apply(covars,2, function(x)  rollapply(x, align='center',FUN=mean, width=12,  partial=T) )
  covars.log <- apply(covars,2, function(x) log(x+0.5))
  covars.log <- covars.log[, colSums(covars.log)>0]
  
  ## Define model input
  df.pre <- df1 %>%
    filter(monthdate<intervention_date)
  n_pre <- nrow(df.pre)
  n <- nrow(df1)
  ## Offset is smoothed and on log scale
  offset <- covars.log[,"acm_noj_prim"]##x.scale1[,"acm_noj_prim"] #log(a2$acm_noj_prim+0.5)
  # Create monthly dummy variables to control for seasonality
  months <- month(df1$monthdate)
  month.mat <- dummies::dummy(months)
  month.mat <- month.mat[,-12]
  month.mat <- cbind(rep(1, nrow(month.mat)), month.mat) # Add an intercept
  colnames(month.mat) <- c("Intercept","s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11")
  z<-month.mat[,1:12] ### be aware of intercept
  df2 <- df1
  df1$J12_J18_prim[df1$monthdate>=post_period[1]]<-NA 
  y <- df1$J12_J18_prim
  
  ### Call model in jags
  source('./R/PrePostjags.R')
  
  mod_test<-reference(burnin = burnin, samples=samples,thin=1,chains=1,n_full=n,n_modeling=n_pre,y=y,offset=offset,z=z)
  
  ### Post-processing
  posterior_samples.all<-do.call(rbind,mod_test)
  y_pred <- posterior_samples.all[,grep('Y_pred', dimnames(posterior_samples.all)[[2]])]
  y_fitted<-posterior_samples.all[,substr(colnames(posterior_samples.all), 1, 1) == "Y"]
  post_means<-apply(posterior_samples.all, 2, median)
  sample.labs<-names(post_means)
  ci<-t(hdi(posterior_samples.all, credMass = 0.95))
  row.names(ci)<-sample.labs
  names(post_means)<-sample.labs
  post.combo<-cbind(post_means,ci)
  n_modeling<-nrow(df.pre)
  post.combo.y<-post.combo[grep('Y_pred', dimnames(post.combo)[[1]]),]
  log.rr.pointwise<- log((df1[(nrow(df.pre)+1):nrow(df1),outcome_name]+0.5)/(post.combo.y+0.5))
  length.rollout<-round(as.numeric((eval_period[1]-post_period[1])/30.3))
  post.samples<-posterior_samples.all[,-c(1:length.rollout)]
  post.samples.y<- post.samples[,grep('Y_pred', dimnames(post.samples)[[2]])]
  post.samples.sum<-apply(post.samples.y,1,sum)
  obs.post.sum<- sum(df2[,outcome_name][df2[,"monthdate"]>=post_period[1]][-c(1:length.rollout)])
  rr.agg<-obs.post.sum/post.samples.sum
  
  ### RRs
  rr.q<-quantile(rr.agg, probs=c(0.025, 0.5, 0.975))
  
  ### Print output
  output.list<-list('rr.samples'=rr.agg,'rr.q'=rr.q,"y.pred"=y_pred,"y_fitted"=y_fitted,'log.rr.pointwise.q'=log.rr.pointwise)
  
  return(output.list)
}