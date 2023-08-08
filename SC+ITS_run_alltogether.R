### 
library(coda) 
library(rjags)
library(dplyr)
library(tidyverse)
library(HDInterval)
library(parallel)
library(pbapply)
library(lubridate)

#Argentina
# Load the monthly dataset
rm(list = ls(all = TRUE))
setwd("~/Desktop/Kayoko/")

## Include useful functions
source("./R/call.jags.SC+ITS.R")

## Load and manipulate data: 
a2 <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country=='AR' & !is.na(agec) & monthdate>='2005-01-01' & agec %in% c(3,4,5,6,7)) %>% 
  dplyr::select(-country)

outcome_name="J12_J18_prim"
country="AR"
intervention_date=as.Date("2012-01-01")
post_period=c(as.Date("2012-02-01"), as.Date('2012-12-01'))
eval_period=c(as.Date("2013-01-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2012-01-01")
denom='acm_noj_prim' 

## Define model input
burnin = 10000
samples = 100000
month1 <- 2012.0
month2 <- 2013.0

## Model 3: SC+ITS with ridge variable selection 
## Run the model over all age groups

n_cores<-3
ds.age.spl<-split(a2, a2$agec)
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(lubridate, quietly = TRUE)
  library(HDInterval, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(rjags, quietly = TRUE)
})
clusterExport(cl, c('burnin','samples','intervention_date','post_period','eval_period','month1','month2','outcome_name'), environment())
mod1<-pblapply(cl = cl,X=ds.age.spl,FUN=data_prep_model_SC_ITS)
stopCluster(cl)

## Save results
saveRDS(mod1,file="./Results/SC+ITS_AR.RDS")
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary


#Brazil
# Load the monthly dataset
rm(list = ls(all = TRUE))
setwd("~/Desktop/Kayoko/")

## Include useful functions
source("./R/call.jags.SC+ITS.R")

## Load and manipulate data: 
a2 <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country=='BR' & !is.na(agec) & monthdate>='2005-01-01' & agec %in% c(3,4,5,6,7)) %>% 
  dplyr::select(-country)

outcome_name="J12_J18_prim"
country="BR"
intervention_date=as.Date("2010-03-01")
post_period=c(as.Date("2010-04-01"), as.Date('2011-03-01'))
eval_period=c(as.Date("2011-04-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2010-04-01")
denom='acm_noj_prim' 

## Define model input
burnin = 10000
samples = 100000
month1 <- 2010.0
month2 <- 2011.0

## Model 3: SC+ITS with ridge variable selection 
## Run the model over all age groups

n_cores<-3
ds.age.spl<-split(a2, a2$agec)
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(lubridate, quietly = TRUE)
  library(HDInterval, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(rjags, quietly = TRUE)
})
clusterExport(cl, c('burnin','samples','intervention_date','post_period','eval_period','month1','month2','outcome_name'), environment())
mod1<-pblapply(cl = cl,X=ds.age.spl,FUN=data_prep_model_SC_ITS)
stopCluster(cl)

## Save results
saveRDS(mod1,file="./Results/SC+ITS_BR.RDS")
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary



## Chile

# Load the monthly dataset
rm(list = ls(all = TRUE))
setwd("~/Desktop/Kayoko/")

## Include useful functions
source("./R/call.jags.SC+ITS.R")

## Load and manipulate data: 
a2 <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country=='CH' & !is.na(agec) & monthdate>='2005-01-01' & agec %in% c(3,4,5,6,7)) %>% 
  dplyr::select(-country)

outcome_name="J12_J18_prim"
country="CH"
intervention_date=as.Date("2011-01-01")
post_period=c(as.Date("2011-02-01"), as.Date('2012-01-01'))
eval_period=c(as.Date("2012-02-01"), as.Date('2018-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2011-02-01")
denom='acm_noj_prim' 

## Define model input
burnin = 10000
samples = 200000
month1 <- 2011.0
month2 <- 2012.0

## Model 3: SC+ITS with ridge variable selection 
## Run the model over all age groups

n_cores<-3
ds.age.spl<-split(a2, a2$agec)
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(lubridate, quietly = TRUE)
  library(HDInterval, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(rjags, quietly = TRUE)
})
clusterExport(cl, c('burnin','samples','intervention_date','post_period','eval_period','month1','month2','outcome_name'), environment())
mod1<-pblapply(cl = cl,X=ds.age.spl,FUN=data_prep_model_SC_ITS)
stopCluster(cl)

## Save results
saveRDS(mod1,file="./Results/SC+ITS_CH.RDS")
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary


## Colombia
### 
# Load the monthly dataset
rm(list = ls(all = TRUE))
setwd("~/Desktop/Kayoko/")

## Include useful functions
source("./R/call.jags.SC+ITS.R")

## Load and manipulate data: 
a2 <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country=='COL' & !is.na(agec) & monthdate>='2005-01-01') %>% 
  dplyr::select(-country)

outcome_name="J12_J18_prim"
country="COL"
intervention_date=as.Date("2011-01-01")
post_period=c(as.Date("2011-02-01"), as.Date('2012-01-01'))
eval_period=c(as.Date("2012-02-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2011-02-01")
denom='acm_noj_prim' 

## Define model input
burnin = 10000
samples = 100000
month1 <- 2011.0
month2 <- 2012.0

## Model 3: SC+ITS with ridge variable selection 
## Run the model over all age groups

n_cores<-3
ds.age.spl<-split(a2, a2$agec)
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(lubridate, quietly = TRUE)
  library(HDInterval, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(rjags, quietly = TRUE)
})
clusterExport(cl, c('burnin','samples','intervention_date','post_period','eval_period','month1','month2','outcome_name'), environment())
mod1<-pblapply(cl = cl,X=ds.age.spl,FUN=data_prep_model_SC_ITS)
stopCluster(cl)

## Save results
saveRDS(mod1,file="./Results/SC+ITS_COL.RDS")
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary


## Mexico

# Load the monthly dataset
rm(list = ls(all = TRUE))
setwd("~/Desktop/Kayoko/")

## Include useful functions
source("./R/call.jags.SC+ITS.R")

## Load and manipulate data: 
a2 <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country=='MX' & !is.na(agec) & monthdate>='1999-01-01') %>% 
  dplyr::select(-country)

outcome_name="J12_J18_prim"
country="MX"
intervention_date=as.Date("2008-01-01")
post_period=c(as.Date("2008-02-01"), as.Date('2009-01-01'))
eval_period=c(as.Date("2009-02-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("1999-01-01")
post_period_start = as.Date("2008-02-01")
denom='acm_noj_prim' 

## Define model input
burnin = 10000
samples = 100000
month1 <- 2008.0
month2 <- 2009.0

## Model 3: SC+ITS with ridge variable selection 
## Run the model over all age groups

n_cores<-3
ds.age.spl<-split(a2, a2$agec)
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(lubridate, quietly = TRUE)
  library(HDInterval, quietly = TRUE)
  library(dplyr, quietly = TRUE)
  library(zoo, quietly = TRUE)
  library(rjags, quietly = TRUE)
})
clusterExport(cl, c('burnin','samples','intervention_date','post_period','eval_period','month1','month2','outcome_name'), environment())
mod1<-pblapply(cl = cl,X=ds.age.spl,FUN=data_prep_model_SC_ITS)
stopCluster(cl)

## Save results
saveRDS(mod1,file="./Results/SC+ITS_MX.RDS")
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary

























