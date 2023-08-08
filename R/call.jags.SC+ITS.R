### Data manipulation script

data_prep_model_SC_ITS <- function(df){
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
  
  #Should be already scaled, but doesn't hurt...
  x.scale1<-apply(covars.log,2, function(z) scale(z)) 
  
  ## Define model input
  df.pre <- df1 %>%
    filter(monthdate<intervention_date)
  n_pre <- nrow(df.pre)
  n <- nrow(df1)
  ## Offset is smoothed and on log scale
  offset <- covars.log[,"acm_noj_prim"]##x.scale1[,"acm_noj_prim"] #log(a2$acm_noj_prim+0.5)
  x.preds <- x.scale1[,2:ncol(x.scale1)]
  # Create monthly dummy variables to control for seasonality
  months <- month(df1$monthdate)
  month.mat <- dummies::dummy(months)
  month.mat <- month.mat[,-12]
  colnames(month.mat) <- c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11")
  z<-month.mat ### be aware of intercept
  y <- df1$J12_J18_prim
  
  ### Call model in jags
  source('./R/SC+ITS_jags.R')
  posterior_samples<-SC_ITS_model(burnin = burnin, samples=samples,thin=1,chains=1,n_full=n,n_modeling=n_pre,y=y,offset=offset,x=x.preds,z=z)
  
  ### Post-processing
  rr<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 2) == "rr"]
  lambda<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 7) == "lambda["]
  lambda_counter<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 7) == "lambda_"]
  alpha0<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 6) == "alpha0"]
  alpha1<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 6) == "alpha1"]
  eta0<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 4) == "eta0"]
  eta1<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 4) == "eta1"]
  eta1<-posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 4) == "eta1"]
  gamma <- posterior_samples[[1]][,substr(colnames(posterior_samples[[1]]), 1, 5) == "gamma"]
  
  rr_combined<-rep(NA,
                   times = nrow(lambda))
  for(j in 1:nrow(lambda)){
    rr_combined[j]<-sum(lambda[j, (n_pre + 13):(n)])/sum(lambda_counter[j, (n_pre + 13):(n)])
  }
  
  rr.q <- c(median(rr_combined), hdi(rr_combined, credMass = 0.95))
  ### Print output
  output.list<-list('rr.samples'=rr_combined,'rr.q'=rr.q,"alpha0"=alpha0,"alpha1"=alpha1,"eta0"=eta0,"eta1"=eta1,"lambda_counter"=lambda_counter,"lambda"=lambda,"gamma"=gamma)
  
  return(output.list)
}