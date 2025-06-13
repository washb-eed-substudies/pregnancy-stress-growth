
rm(list=ls())

source(here::here("0_config.R"))

#library(boxr)
#box_auth()

#updated_pregnancy <- box_read(845078269128)

# read in data 
#d <- readRDS("./stress_growth.RDS")
d <- readRDS("data/bangladesh-cleaned-master-data.RDS")

table(d$vit_A_def)
summary(d$logFERR)
summary(d$logFERR_inf)



#dfull <- readRDS("./stress_growth.RDS")

# select variables/columns of interest 
exp_out <- c('dataid', 'childid', 'clusterid', 'ln_preg_cort', 'logRBP_inf','logSTFR_inf','vitD_nmol_per_L', 'ln_preg_estri', 'logCRP', 'logAGP', 'sumscore_t0_mom_Z', 'igf_t2', 'igf_t3', 'laz_t1', 'laz_t2', 'laz_t3')
cov <- c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "tr", "life_viol_any_t3_cat", "viol_any_preg_cat", "roof", "life_viol_any_t3", "viol_any_preg")
time_cov <- c("ageday_bt2", "month_blood_t0", "month_bt2", "ageday_bt3", "month_blood_t0", "month_bt3","time_of_day_cort_cont")
extract <- c(exp_out, cov, time_cov)
d <- d %>% select(all_of(extract)) %>% mutate(childid = as.integer(childid)) %>% mutate(dataid = as.integer(dataid))

names(d)


#drop Z-score, sd, and ratio measures
d <- d[,!(grepl("^(z_)",colnames(d)) | grepl("^(sd_)",colnames(d)))]

############# Fix implausible/offset time of sampling measures so they occur during sampling hours################

d$time_of_day_cort_implausible <- d$time_of_day_cort_cont

summary(d$time_of_day_cort_cont)
plot(hist(d$time_of_day_cort_cont))

d$time_of_day_cort_cont <- ifelse(d$time_of_day_cort_cont >18, d$time_of_day_cort_cont-12, d$time_of_day_cort_cont)
d$time_of_day_cort_cont <- ifelse(d$time_of_day_cort_cont<6 , d$time_of_day_cort_cont+12, d$time_of_day_cort_cont)
plot(hist(d$time_of_day_cort_cont))

table(d$time_of_day_cort_cont)

############# Check covariate missingness ###################
# a few exposures and outcomes
exp <- c('ln_preg_cort', 'logCRP', 'logAGP', 'sumscore_t0_mom_Z', 'igf_t2', 'igf_t3')
out <- c('laz_t1', 'laz_t2', 'laz_t3')

Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "roof",
         "tr", "life_viol_any_t3", "viol_any_preg", "ageday_bt2", "ageday_bt3", 
         "month_blood_t0", "month_bt2", "month_bt3") %>% unique()

W <- d %>% select(all_of(Wvars))  

generate_miss_tbl <- function(Wvars, d){
  W <- d %>% select(all_of(Wvars))  
  miss <- data.frame(name = names(W), missing = colSums(is.na(W))/nrow(W), row.names = c(1:ncol(W)))
  for (i in 1:nrow(miss)) {
    miss$class[i] <- class(W[,which(colnames(W) == miss[i, 1])])
  }
  miss 
}
generate_miss_tbl(Wvars, d)

# roof has low variability
mean(W$roof, na.rm=T)
sd(W$roof, na.rm=T)

# remove roof from covariates
#d <- subset(d, select=-roof)

# add missingness category to IPV covariates
d$life_viol_any_t3<-as.factor(d$life_viol_any_t3)
d$life_viol_any_t3<-addNA(d$life_viol_any_t3)
levels(d$life_viol_any_t3)[length(levels(d$life_viol_any_t3))]<-"Missing"

d$viol_any_preg<-as.factor(d$viol_any_preg)
d$viol_any_preg<-addNA(d$viol_any_preg)
levels(d$viol_any_preg)[length(levels(d$viol_any_preg))]<-"Missing"

for (i in exp){
  for (j in out){
    print(i)
    print(j)
    d_sub <- subset(d, !is.na(d[,i]) & !is.na(d[,j]))
    print(generate_miss_tbl(Wvars, d_sub))
  }
}



saveRDS(d, "./pregnancy_stress_growth_covariates_data.RDS")

