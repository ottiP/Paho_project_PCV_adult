################################################################################
# Indirect effects of PCV in older adults in 5 american countries              #                                                                       #
#        DATE: May 2023                                                      #
#    CODED BY: Ottavia Prunas (ottavia.prunas@yale.edu) & Kayoko Shioda        #
#     ADVISOR: Dan Weinberger (daniel.weinberger@yale.edu)                     #
################################################################################

#------------------------------------------------------------------------------#
# DESCRIPTION ----
#------------------------------------------------------------------------------#
rm(list = ls(all = TRUE))
# Set the working directory
#setwd('~/Documents/paho-adult-mortality-pcvs/')

# Call functions
# source('synthetic_control_functions.R', local = FALSE)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(purrr)

# Function that allows us to include multiple panels in one plot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# Create a list of countries
abbr_country <- c()
list_country <- data.frame(abbr = c("AR","BR","COL","CH","MX"),
                           country = c("Argentina", "Brazil", "Colombia", "Chile","Mexico"))
list_agegroups<-c("5-19y","20-39y","40-64y","65-79y","80+y")


#------------------------------------------------------------------------------#
# Yearly time series for obs vs. counter factual pneumo deaths 
# By country ----
#------------------------------------------------------------------------------#

##### Argentina #####
# Create empty objects to store results
abbr_country <- c()
title <- c()
plotres <- list()

# Load .rds file
a <- readRDS("~/Documents/paho-adult-mortality-pcvs-complete/Results/_AR_2023-05-14-173357.Rds")
mod1<- readRDS("./Results/SC_ridge_AR_nooffset.RDS")
rr.summary.sc <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/SC+ITS_AR.RDS")
rr.summary.sc.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/ITSnb_AR.rDS")
rr.summary.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/PrePost_AR.RDS")
rr.summary.prepost <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))

rr_df <- a$results$impact$rr_mean_combo
colnames(rr_df) <- c("Model","model","age_group","group.index","lcl","mean.rr","ucl","est.index")
rr_df <- rr_df %>% 
  filter(age_group %in% c(3,4,5,6,7)) 

rr_df$model[rr_df$model=="full"]<-"sc"
rr_df$model[rr_df$model=="time"]<-"its"
rr_df$model[rr_df$model=="time_no_offset"]<-"sc+its"
rr_df$model[rr_df$model=="pca"]<-"pre-post"
rr_df$mean.rr[rr_df$model=="sc"] <- as.numeric(rr.summary.sc[,2])
rr_df$lcl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,1])
rr_df$ucl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,3])
rr_df$mean.rr[rr_df$model=="pre-post"] <- as.numeric(rr.summary.prepost[,2])
rr_df$lcl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,1])
rr_df$ucl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,3])
rr_df$mean.rr[rr_df$model=="sc+its"] <- as.numeric(rr.summary.sc.its[1,])
rr_df$lcl[rr_df$model=="sc+its"]    <-  as.numeric(rr.summary.sc.its[2,])
rr_df$ucl[rr_df$model=="sc+its"]    <- as.numeric(rr.summary.sc.its[3,])
rr_df$mean.rr[rr_df$model=="its"] <- as.numeric(rr.summary.its[1,])
rr_df$lcl[rr_df$model=="its"]    <-  as.numeric(rr.summary.its[2,])
rr_df$ucl[rr_df$model=="its"]    <- as.numeric(rr.summary.its[3,])

rr_df <- rr_df[!rr_df$model=="pre-post",]
# Create time series plot by age group
pd <- position_dodge(0.5) 
plotres[[1]]<-ggplot(rr_df, aes(age_group, mean.rr, fill=model, label = model, colour=model))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1,position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) + 
  geom_hline(yintercept=1.0, linetype="dashed", color = "gray")+
  xlab("Age group")+
  ggtitle("a) Argentina") +
  ylab("Average RR")

##### Brazil #####
# Load .rds file
a <- readRDS("~/Documents/paho-adult-mortality-pcvs-complete/Results/_BR_2022-12-01-200141.Rds")
mod1<- readRDS("./Results/SC_ridge_BR_nooffset.RDS")
rr.summary.sc <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/SC+ITS_BR.RDS")
rr.summary.sc.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/ITSnb_BR.rDS")
rr.summary.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/PrePost_BR.RDS")
rr.summary.prepost <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))

rr_df <- a$results$impact$rr_mean_combo
colnames(rr_df) <- c("Model","model","age_group","group.index","lcl","mean.rr","ucl","est.index")
#rr_df <- rr_df[!rr_df$model=="pca",]
rr_df <- rr_df %>% 
  filter(age_group %in% c(3,4,5,6,7)) 

rr_df$model[rr_df$model=="full"]<-"sc"
rr_df$model[rr_df$model=="time"]<-"its"
rr_df$model[rr_df$model=="time_no_offset"]<-"sc+its"
rr_df$model[rr_df$model=="pca"]<-"pre-post"
rr_df$mean.rr[rr_df$model=="sc"] <- as.numeric(rr.summary.sc[,2])
rr_df$lcl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,1])
rr_df$ucl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,3])
rr_df$mean.rr[rr_df$model=="pre-post"] <- as.numeric(rr.summary.prepost[,2])
rr_df$lcl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,1])
rr_df$ucl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,3])
rr_df$mean.rr[rr_df$model=="sc+its"] <- as.numeric(rr.summary.sc.its[1,])
rr_df$lcl[rr_df$model=="sc+its"]    <-  as.numeric(rr.summary.sc.its[2,])
rr_df$ucl[rr_df$model=="sc+its"]    <- as.numeric(rr.summary.sc.its[3,])
rr_df$mean.rr[rr_df$model=="its"] <- as.numeric(rr.summary.its[1,])
rr_df$lcl[rr_df$model=="its"]    <-  as.numeric(rr.summary.its[2,])
rr_df$ucl[rr_df$model=="its"]    <- as.numeric(rr.summary.its[3,])


rr_df <- rr_df[!rr_df$model=="pre-post",]
# Create time series plot by age group
pd <- position_dodge(0.5) 
plotres[[2]]<-ggplot(rr_df, aes(age_group, mean.rr, fill=model, label = model, colour=model))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1,position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) + 
  geom_hline(yintercept=1.0, linetype="dashed", color = "gray")+
  xlab("Age group")+
  ggtitle("c) Brazil") +
  ylab("Average RR")


##### Chile #####
# Create empty objects to store results
# Load .rds file
a <- readRDS("~/Documents/paho-adult-mortality-pcvs-complete/Results/_CH_2022-12-05-132016.Rds")
mod1<- readRDS("./Results/SC_ridge_CH_nooffset.RDS")
rr.summary.sc <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/SC+ITS_CH.RDS")
rr.summary.sc.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/ITSnb_CH.rDS")
rr.summary.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/PrePost_CH.RDS")
rr.summary.prepost <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))

rr_df <- a$results$impact$rr_mean_combo
colnames(rr_df) <- c("Model","model","age_group","group.index","lcl","mean.rr","ucl","est.index")
#rr_df <- rr_df[!rr_df$model=="pca",]
rr_df <- rr_df %>% 
  filter(age_group %in% c(3,4,5,6,7)) 



rr_df$model[rr_df$model=="full"]<-"sc"
rr_df$model[rr_df$model=="time"]<-"its"
rr_df$model[rr_df$model=="time_no_offset"]<-"sc+its"
rr_df$model[rr_df$model=="pca"]<-"pre-post"
rr_df$mean.rr[rr_df$model=="sc"] <- as.numeric(rr.summary.sc[,2])
rr_df$lcl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,1])
rr_df$ucl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,3])
rr_df$mean.rr[rr_df$model=="pre-post"] <- as.numeric(rr.summary.prepost[,2])
rr_df$lcl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,1])
rr_df$ucl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,3])
rr_df$mean.rr[rr_df$model=="sc+its"] <- as.numeric(rr.summary.sc.its[1,])
rr_df$lcl[rr_df$model=="sc+its"]    <-  as.numeric(rr.summary.sc.its[2,])
rr_df$ucl[rr_df$model=="sc+its"]    <- as.numeric(rr.summary.sc.its[3,])
rr_df$mean.rr[rr_df$model=="its"] <- as.numeric(rr.summary.its[1,])
rr_df$lcl[rr_df$model=="its"]    <-  as.numeric(rr.summary.its[2,])
rr_df$ucl[rr_df$model=="its"]    <- as.numeric(rr.summary.its[3,])

rr_df <- rr_df[!rr_df$model=="pre-post",]
# Create time series plot by age group
pd <- position_dodge(0.5) 
plotres[[3]]<-ggplot(rr_df, aes(age_group, mean.rr, fill=model, label = model, colour=model))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1,position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) + 
  geom_hline(yintercept=1.0, linetype="dashed", color = "gray")+
  xlab("Age group")+
  ggtitle("e) Chile") +
  ylab("Average RR")


##### Colombia #####
# Create empty objects to store results
# Load .rds file
a <- readRDS("~/Documents/paho-adult-mortality-pcvs-complete/Results/_COL_2022-12-05-143204.Rds")
mod1<- readRDS("./Results/SC_ridge_COL_nooffset.RDS")
rr.summary.sc <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/SC+ITS_COL.RDS")
rr.summary.sc.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/ITSnb_COL.rDS")
rr.summary.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/PrePost_COL.RDS")
rr.summary.prepost <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))

rr_df <- a$results$impact$rr_mean_combo
colnames(rr_df) <- c("Model","model","age_group","group.index","lcl","mean.rr","ucl","est.index")
#rr_df <- rr_df[!rr_df$model=="pca",]

rr_df <- rr_df %>% 
  filter(age_group %in% c(3,4,5,6,7)) 

rr_df$model[rr_df$model=="full"]<-"sc"
rr_df$model[rr_df$model=="time"]<-"its"
rr_df$model[rr_df$model=="time_no_offset"]<-"sc+its"
rr_df$model[rr_df$model=="pca"]<-"pre-post"
rr_df$mean.rr[rr_df$model=="sc"] <- as.numeric(rr.summary.sc[,2])
rr_df$lcl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,1])
rr_df$ucl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,3])
rr_df$mean.rr[rr_df$model=="pre-post"] <- as.numeric(rr.summary.prepost[,2])
rr_df$lcl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,1])
rr_df$ucl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,3])
rr_df$mean.rr[rr_df$model=="sc+its"] <- as.numeric(rr.summary.sc.its[1,])
rr_df$lcl[rr_df$model=="sc+its"]    <-  as.numeric(rr.summary.sc.its[2,])
rr_df$ucl[rr_df$model=="sc+its"]    <- as.numeric(rr.summary.sc.its[3,])
rr_df$mean.rr[rr_df$model=="its"] <- as.numeric(rr.summary.its[1,])
rr_df$lcl[rr_df$model=="its"]    <-  as.numeric(rr.summary.its[2,])
rr_df$ucl[rr_df$model=="its"]    <- as.numeric(rr.summary.its[3,])

rr_df <- rr_df[!rr_df$model=="pre-post",]
# Create time series plot by age group
pd <- position_dodge(0.5) 
plotres[[4]]<-ggplot(rr_df, aes(age_group, mean.rr, fill=model, label = model, colour=model))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1,position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) + 
  geom_hline(yintercept=1.0, linetype="dashed", color = "gray")+
  xlab("Age group")+
  ggtitle("b) Colombia") +
  ylab("Average RR")


##### Mexico #####
# Create empty objects to store results
# Load .rds file
a <- readRDS("~/Documents/paho-adult-mortality-pcvs-complete/Results/_MX_2022-12-05-174046.Rds")
mod1<- readRDS("./Results/SC_ridge_MX_nooffset.RDS")
rr.summary.sc <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
load("./Results/SC+ITS_MX.RDS")
rr.summary.sc.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
mod1<- readRDS("./Results/ITSnb_MX.rDS")
rr.summary.its <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))
load("./Results/PrePost_MX.RDS")
rr.summary.prepost <- as.data.frame(bind_rows(map(mod1 , ~.[["rr.q"]])))

rr_df <- a$results$impact$rr_mean_combo
colnames(rr_df) <- c("Model","model","age_group","group.index","lcl","mean.rr","ucl","est.index")
#rr_df <- rr_df[!rr_df$model=="pca",]

rr_df <- rr_df %>% 
  filter(age_group %in% c(3,4,5,6,7)) 

rr_df$model[rr_df$model=="full"]<-"sc"
rr_df$model[rr_df$model=="time"]<-"its"
rr_df$model[rr_df$model=="time_no_offset"]<-"sc+its"
rr_df$model[rr_df$model=="pca"]<-"pre-post"
rr_df$mean.rr[rr_df$model=="sc"] <- as.numeric(rr.summary.sc[,2])
rr_df$lcl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,1])
rr_df$ucl[rr_df$model=="sc"]    <-  as.numeric(rr.summary.sc[,3])
rr_df$mean.rr[rr_df$model=="pre-post"] <- as.numeric(rr.summary.prepost[,2])
rr_df$lcl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,1])
rr_df$ucl[rr_df$model=="pre-post"]    <-  as.numeric(rr.summary.prepost[,3])
rr_df$mean.rr[rr_df$model=="sc+its"] <- as.numeric(rr.summary.sc.its[1,])
rr_df$lcl[rr_df$model=="sc+its"]    <-  as.numeric(rr.summary.sc.its[2,])
rr_df$ucl[rr_df$model=="sc+its"]    <- as.numeric(rr.summary.sc.its[3,])
rr_df$mean.rr[rr_df$model=="its"] <- as.numeric(rr.summary.its[1,])
rr_df$lcl[rr_df$model=="its"]    <-  as.numeric(rr.summary.its[2,])
rr_df$ucl[rr_df$model=="its"]    <- as.numeric(rr.summary.its[3,])

rr_df <- rr_df[!rr_df$model=="pre-post",]
# Create time series plot by age group
pd <- position_dodge(0.5) 
plotres[[5]]<-ggplot(rr_df, aes(age_group, mean.rr, fill=model, label = model, colour=model))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1,position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) + 
  geom_hline(yintercept=1.0, linetype="dashed", color = "gray")+
  xlab("Age group")+
  ggtitle("d) Mexico") +
  ylab("Average RR")


pdf("./Figures/Fig_RR_sensitivity_v1.pdf", width = 12, height = 9)
multiplot(plotres[[1]], plotres[[2]],plotres[[3]],plotres[[4]],plotres[[5]], 
          cols=2)
dev.off()


