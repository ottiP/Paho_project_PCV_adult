################################################################################
# Impact of PCV on childhood mortality in 10 PAHO countries                    #
#                                                                              #
#        DATE: February 2019                                                   #
#    CODED BY: Kayoko Shioda (kayoko.shioda@yale.edu)                          #
#     ADVISOR: Dan Weinberger (daniel.weinberger@yale.edu)                     #
################################################################################

#------------------------------------------------------------------------------#
# DESCRIPTION ----
#------------------------------------------------------------------------------#

# Figure 4 (Forest plot)
#
# Estimated impact of PCV by age group in ten Latin American and Caribbean
# countries.
#
# RR estimated by the "best" model, stratified by age group and country


#------------------------------------------------------------------------------#
# Load datasets ----
#------------------------------------------------------------------------------#
rm(list = ls(all = TRUE))
# Metafor analysis and plotting
library(metafor)
library(pbapply)
library(purrr)

#select what you want to plot : "best" estimate; full (sc) estimates, time-trend, or stl_pca
ds.select<-'rr_full'  #rr_best, rr_full, rr_time_trend, rr_pca
list_agegroups1<-c("5-19y","20-39y","40-64y","65-79y","80+y")
# Load dataset for each country and merge 10 files to create one big file.
#setwd("~/Documents/paho-adult-mortality-pcvs/")
pattern=('ITSnb')
file_list <- list.files(path="./Results/", pattern=(pattern))
test1 <- 
  all.res <-
  pblapply(file_list, function(x){
    print(x)
    path1 <- paste0("./Results/",x)
    test1 <- readRDS(path1)
  })

pattern=('SC_ridge_')
file_list <- list.files(path="./Results/", pattern=(pattern))
test2 <- 
  all.res <-
  pblapply(file_list, function(x){
    print(x)
    path1 <- paste0("./Results/",x)
    test2 <- readRDS(path1)
  })

results_sc<-map(test2, function(x) rbind(x[[1]]$rr.q, x[[2]]$rr.q,x[[3]]$rr.q, x[[4]]$rr.q,x[[5]]$rr.q))
results_sc <- do.call(rbind,results_sc)
results_sc <- as.data.frame(results_sc)
results_sc <-results_sc %>% mutate(age=c(paste0("ar ",list_agegroups1),paste0("br ",list_agegroups1),paste0("ch ",list_agegroups1),paste0("col ",list_agegroups1),
                                         paste0("mx ",list_agegroups1)))
colnames(results_sc) <- c("Lower","Median","Upper","age_group")


results_its<-map(test1, function(x) rbind(x[[1]]$rr.q, x[[2]]$rr.q,x[[3]]$rr.q, x[[4]]$rr.q,x[[5]]$rr.q))
results_its <- do.call(rbind,results_its)
results_its <- as.data.frame(results_its)
results_its <-results_its %>% mutate(age=c(paste0("ar ",list_agegroups1),paste0("br ",list_agegroups1),paste0("ch ",list_agegroups1),paste0("col ",list_agegroups1),
                                           paste0("mx ",list_agegroups1)))
colnames(results_its) <- c("Median","Lower","Upper","age_group")

dt <- rbind(results_its,results_sc)

dt <- results_sc %>%
  left_join(results_its, by=c('age_group'))

#------------------------------------------------------------------------------#
# Set up ----
#------------------------------------------------------------------------------#

# Error bars
dt$eb_l <- dt$Median.x - dt$Lower.x
dt$eb_u <- dt$Upper.x - dt$Median.x
dt$ebt_l <- dt$Median.y - dt$Lower.y
dt$ebt_u <- dt$Upper.y - dt$Median.y

# Create "age" and "agegrp"
dt$age <- NA
for (i in 1:nrow(dt)) {
  dt$age[i] <- as.character(unlist(strsplit(as.character(dt$age_group[i]), " ", fixed = TRUE))[2])
}
table(dt$age)
dt$agegrp <- ifelse(dt$age=="5-19y", 1, NA)
dt$agegrp <- ifelse(dt$age=="20-39y",2, dt$agegrp)
dt$agegrp <- ifelse(dt$age=="40-64y", 3, dt$agegrp)
dt$agegrp <- ifelse(dt$age=="65-79y",  4, dt$agegrp)
dt$agegrp <- ifelse(dt$age=="80+y",  5, dt$agegrp)

table(dt$age,dt$agegrp)

# Create "country"
dt$country <- NA
for (i in 1:nrow(results_sc)) {
  dt$country[i] <- as.character(unlist(strsplit(as.character(dt$age_group[i]), " ", fixed = TRUE))[1])
}
table(dt$country)

# Create country groups
dt$country.grp<-NA
dt$country.grp[dt$country %in% c('ar','br','col','mx')]<-2
dt$country.grp[dt$country %in% c('ch')]<-1

# Create country labels
dt$country.name<-NA
dt$country.name[dt$country=='ar']<-'Argentina'
dt$country.name[dt$country=='br']<-'Brazil'
dt$country.name[dt$country=='col']<-'Colombia'
dt$country.name[dt$country=='mx']<-'Mexico'
dt$country.name[dt$country=='ch']<-'Chile'

#------------------------------------------------------------------------------#
# Forest plot
# Age-group stratified ----
#------------------------------------------------------------------------------#
# http://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups

# Exclude empty rows
overall.plot<-dt[!is.na(dt$Median.x),]
# Exclude aggregated age groups
overall.plot<-dt[dt$age %in% c("5-19y","20-39y","40-64y","65-79y","80+y"),]

# Take a log of RRs and calculate their variance in a log scale
overall.plot$log.rr<-log(overall.plot$Median.x)
overall.plot$approx.var<- ((log(overall.plot$Upper.x) - log(overall.plot$Median.x))/1.96)^2
overall.plot$log.rr.t<-log(overall.plot$Median.y)
overall.plot$approx.var.t<- ((log(overall.plot$Upper.y) - log(overall.plot$Median.y))/1.96)^2

# Create RMA objects for each age group
#ma.5_19y<-rma.uni( yi=log.rr, vi=approx.var, data=overall.plot, slab=country , subset=(age=='5-19y'))
# ma.20_39y<-rma.uni( yi=log.rr, vi=approx.var, data=overall.plot, slab=country , subset=(age=='20-39y'))
# ma.40_64y<-rma.uni( yi=log.rr, vi=approx.var, data=overall.plot, slab=country , subset=(age=='40-64y'))
# ma.65_79y<-rma.uni( yi=log.rr, vi=approx.var, data=overall.plot, slab=country , subset=(age=='65-79y'))
# ma.80y<-rma.uni( yi=log.rr, vi=approx.var, data=overall.plot, slab=country , subset=(age=='80+y'))

# Stratified
# Split by age group to calculate number of data points for each
plot.spl <- split(overall.plot, overall.plot$agegrp)
plot.spl <- lapply(plot.spl, function(x) x<-x[order(x$country.grp, x$country),]) # Sort by country group and country name
n.country.grp <- sapply(plot.spl, function(x) nrow(x) )
n.country.grp # Number of data points for each age group

# Specify where each group starts and ends, providing space between groups
start.grp1<-3
start.grp2<-start.grp1+n.country.grp[2]+4
start.grp3<-start.grp2+n.country.grp[3]+4
start.grp4<-start.grp3+n.country.grp[4]+4
start.grp5<-start.grp4+n.country.grp[5]+4

plot.indices<-c(start.grp1:(start.grp1+n.country.grp[3]-1) ,
                start.grp2:(start.grp2+n.country.grp[2]-1),
                start.grp3:(start.grp3+n.country.grp[1]-1),
                start.grp4:(start.grp4+n.country.grp[4]-1),
                start.grp5:(start.grp5+n.country.grp[5]-1)
                #,
                #start.grp4:(start.grp4+n.country.grp[1]-1)
)


# Sort dataset
overall.plot.sorted <- overall.plot[order(-overall.plot$agegrp, 
                                          overall.plot$country),]

# Create a forest plot stratified by age group (<2m, 2-11m, 12-23m, 24-59m)
jpeg('./Figures/ForestplotByAgeSC.jpeg',
     width=5, height=8, units='in', res=200)

par(mar = c(4, 4, 1, 2)) # make the plots be closer together
forest(x = overall.plot.sorted$Median.x,rows=plot.indices, 
       annotate=FALSE,
       ci.lb = overall.plot.sorted$Lower.x, 
       #vi = overall.plot.sorted$approx.var,
       ci.ub = overall.plot.sorted$Upper.x,
       psize=0.8,
       refline=1, 
       slab = overall.plot.sorted$country.name,
       ylim = c(0,(max(plot.indices)+5)), 
       clim = c(0, Inf), # clim=c(0, 1.5),
       xlim = c(0, 2), # xlim=c(0, 2), 
       at = seq(0,2,0.5),
       cex = 0.8)
op <- par(cex=0.75, font=4)
text(0, c((start.grp5+n.country.grp[1]+0.5),
          (start.grp4+n.country.grp[2]+0.5),
          (start.grp3+n.country.grp[3]+0.5),
          (start.grp2+n.country.grp[4]+0.5),
          (start.grp1+n.country.grp[5]+0.5)),
     pos=4, c("5-19y","20-39y","40-64y","65-79y","80+y"), cex=1)
par(font=2)
text(0, (max(plot.indices)+4), "Country",  pos=4)
text(1.2, (max(plot.indices)+4), "RR [SC]", pos=2)
dev.off()


## SC+ITS together
# Create a forest plot stratified by age group (<2m, 2-11m, 12-23m, 24-59m)
jpeg('./Figures/ForestplotByAgeSCITS_new.jpeg',
     width=5, height=8, units='in', res=200)

par(mfrow = c(1, 2)) # 1-by-2 grid of plots
par(mar = c(5, 4, 1, 1)) # make the plots be closer together
forest(x = overall.plot.sorted$Median.x,rows=plot.indices, 
       annotate=FALSE,
       ci.lb = overall.plot.sorted$Lower.x, 
       vi = overall.plot.sorted$approx.var,
       ci.ub = overall.plot.sorted$Upper.x,
       psize=0.8,
       refline=1, 
       slab = overall.plot.sorted$country.name,
       ylim = c(0,(max(plot.indices)+5)), 
       clim = c(0, Inf), # clim=c(0, 1.5),
       xlim = c(-3, 3), # xlim=c(0, 2), 
       at = seq(0,2,0.5),
       cex = 0.8)
#op <- par(cex=0.75, font=4)
text(-1.2, c((start.grp5+n.country.grp[1]+0.5),
             (start.grp4+n.country.grp[2]+0.5),
             (start.grp3+n.country.grp[3]+0.5),
             (start.grp2+n.country.grp[4]+0.5),
             (start.grp1+n.country.grp[5]+0.5)),
     pos=4, c("5-19y","20-39y","40-64y","65-79y","80+y"), cex=1)
par(font=2)
text(-3.0, (max(plot.indices)+4), "Country",  pos=4)
text(3, (max(plot.indices)+4), "RR [SC]", pos=2)

par(mar=c(5,3,1,2))
forest(x = overall.plot.sorted$Median.y,rows=plot.indices, 
       ci.lb = overall.plot.sorted$Lower.y, 
       #vi = overall.plot.sorted$approx.var.t,
       ci.ub = overall.plot.sorted$Upper.y,
       psize=0.8, ##to make point markers the same size 
       refline=1, 
       slab=rep("",length(overall.plot.sorted$Median.y)),
       ylim = c(0,(max(plot.indices)+5)), 
       annotate=FALSE,
       clim = c(0, Inf), # clim=c(0, 1.5),
       xlim = c(-3, 3), # xlim=c(0, 2), 
       at = seq(0,2,0.5),
       cex = 0.8)

#op <- par(cex=0.75, font=4)
text(-1.2, c((start.grp5+n.country.grp[1]+0.5),
             (start.grp4+n.country.grp[2]+0.5),
             (start.grp3+n.country.grp[3]+0.5),
             (start.grp2+n.country.grp[4]+0.5),
             (start.grp1+n.country.grp[5]+0.5)), 
     pos=4, c("5-19y","20-39y","40-64y","65-79y","80+y"), cex=1)
# switch to bold fonts
par(font=2)
text(3, (max(plot.indices)+4), "RR [ITS]", pos=2)
dev.off()



