rm(list = ls(all = TRUE))

### Load packages 
library(coda) 
library(rjags)
library(dplyr)
library(tidyverse)
library(HDInterval)
library(parallel)
library(pbapply)
library(lubridate)

## Include useful functions
source("./R/call.jags.SC+ITS.R")
source("./R/run_jags_model.R")
#Outcome is the same for all countries
outcome_name="J12_J18_prim"
## Define model input
burnin = 10000
samples = 100000
model="SC+ITS"

## Load and manipulate data: 
country="AR"
intervention_date=as.Date("2012-01-01")
post_period=c(as.Date("2012-02-01"), as.Date('2012-12-01'))
eval_period=c(as.Date("2013-01-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2012-01-01")
month1 <- 2012.0
month2 <- 2013.0
df <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country==country & !is.na(agec) & monthdate>='2005-01-01' & agec %in% c(3,4,5,6,7)) %>% 
  dplyr::select(-country)

## SC+ITS model
## Run the model over all age groups
mod1<-run_jags_model(df=df,model=model,country=country,data_prep_model=data_prep_model_SC_ITS,burnin=burnin,
                     samples=samples,intervention_date=intervention_date,post_period=post_period,
                     eval_period=eval_period,month1=month1,month2=month2,outcome_name=outcome_name)
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary


#Brazil
# Load the monthly dataset
country="BR"
intervention_date=as.Date("2010-03-01")
post_period=c(as.Date("2010-04-01"), as.Date('2011-03-01'))
eval_period=c(as.Date("2011-04-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2010-04-01")
month1 <- 2010.0
month2 <- 2011.0
df <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country==country & !is.na(agec) & monthdate>='2005-01-01' & agec %in% c(3,4,5,6,7)) %>% 
  dplyr::select(-country)

## SC+ITS model
## Run the model over all age groups
mod1<-run_jags_model(df=df,model=model,country=country,data_prep_model=data_prep_model_SC_ITS,burnin=burnin,
                     samples=samples,intervention_date=intervention_date,post_period=post_period,
                     eval_period=eval_period,month1=month1,month2=month2,outcome_name=outcome_name)
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary


## Chile
country="CH"
intervention_date=as.Date("2011-01-01")
post_period=c(as.Date("2011-02-01"), as.Date('2012-01-01'))
eval_period=c(as.Date("2012-02-01"), as.Date('2018-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2011-02-01")
month1 <- 2011.0
month2 <- 2012.0
## Load and manipulate data: 
df <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country==country & !is.na(agec) & monthdate>='2005-01-01' & agec %in% c(3,4,5,6,7)) %>% 
  dplyr::select(-country)

## SC+ITS model
## Run the model over all age groups
mod1<-run_jags_model(df=df,model=model,country=country,data_prep_model=data_prep_model_SC_ITS,burnin=burnin,
                     samples=samples,intervention_date=intervention_date,post_period=post_period,
                     eval_period=eval_period,month1=month1,month2=month2,outcome_name=outcome_name)
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary

## Colombia
# Load the monthly dataset
country="COL"
intervention_date=as.Date("2011-01-01")
post_period=c(as.Date("2011-02-01"), as.Date('2012-01-01'))
eval_period=c(as.Date("2012-02-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("2005-01-01")
post_period_start = as.Date("2011-02-01")
month1 <- 2011.0
month2 <- 2012.0
df <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country==country & !is.na(agec) & monthdate>='2005-01-01') %>% 
  dplyr::select(-country)

## SC+ITS model
## Run the model over all age groups
mod1<-run_jags_model(df=df,model=model,country=country,data_prep_model=data_prep_model_SC_ITS,burnin=burnin,
                     samples=samples,intervention_date=intervention_date,post_period=post_period,
                     eval_period=eval_period,month1=month1,month2=month2,outcome_name=outcome_name)
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary

## Mexico
# Load the monthly dataset
country="MX"
intervention_date=as.Date("2008-01-01")
post_period=c(as.Date("2008-02-01"), as.Date('2009-01-01'))
eval_period=c(as.Date("2009-02-01"), as.Date('2019-12-01'))
pre_period_start = as.Date("1999-01-01")
post_period_start = as.Date("2008-02-01")
month1 <- 2008.0
month2 <- 2009.0
df <- readRDS("./Data/PAHO_adults_ICD10reformatted_subchapters.rds") %>%
  filter(country==country & !is.na(agec) & monthdate>='1999-01-01') %>% 
  dplyr::select(-country)

## SC+ITS model
## Run the model over all age groups
mod1<-run_jags_model(df=df,model=model,country=country,data_prep_model=data_prep_model_SC_ITS,burnin=burnin,
                     samples=samples,intervention_date=intervention_date,post_period=post_period,
                     eval_period=eval_period,month1=month1,month2=month2,outcome_name=outcome_name)
rr.summary <- bind_rows(map(mod1 , ~.[["rr.q"]]))
rr.summary










