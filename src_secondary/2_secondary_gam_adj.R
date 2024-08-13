rm(list=ls())

#source(here::("./0-config.R"))

#d<-readRDS(paste0(dropboxDir, "Data/Cleaned/Audrie/pregnancy_child_immune_covariates_data.RDS"))

d <- readRDS("./sec_outcomes_covariates.RDS")
#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
         "tr", "life_viol_any_t3_cat", "viol_any_preg_cat")

Wvars[!(Wvars %in% colnames(d))]

#Add in time varying covariates:
Wvars2 <- c(Wvars, c("ageday_bt2", "month_blood_t0", "month_bt2")) 
Wvars3 <- c(Wvars, c("ageday_bt3", "month_blood_t0", "month_bt3"))

pick_covariates <- function(j){
  # j is outcome as string
  # choose correct adjustment set based on outcome
  if(grepl("t2", j)){Wset = Wvars2}
  else{Wset = Wvars3}
  return(Wset)
}




#Loop over exposure-outcome pairs

##Hypothesis 1
#Maternal plasma cortisol is inversely associated with child growth 

# X: maternal plasma cortisol - first & second trimester of pregnancy
# Y: child LAZ at 3, 14, 28 months, stunting 
Xvars <- c("ln_preg_cort")
Yvars <- c("whz_t1", "whz_t2", "whz_t3")

#Fit models
H1_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-c(pick_covariates(j), "time_of_day_cort_cont")
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset, forcedW = c("time_of_day_cort_cont"))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}


#Get primary contrasts
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  if(grepl("_def", H1_adj_models$X[i])){
    preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
  }else{
    preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  }
  H1_adj_res <-  bind_rows(H1_adj_res , preds$res)
}

#Make list of plots
H1_adj_plot_list <- NULL
H1_adj_plot_data <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H1_adj_models$fit[i][[1]], H1_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_adj_plot_list[[i]] <-  simul_plot$p
  H1_adj_plot_data <-  rbind(H1_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1_adj_models.RDS"))

#Save results
saveRDS(H1_adj_res, ("./results/adjusted/H1_whz_adj_res.RDS"))


#Save plots
#saveRDS(H1_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H1_adj_plot_data, ("./figure-data/H1_adj_whz_spline.data.RDS"))


## Hypothesis 2
# Maternal inflammation is inversely associated with in-utero and post-natal growth in children
# X: CRP, AGP, plasma 13-cytokine sum score in first & second trimester of pregnancy 
# Y: child LAZ at 3, 14, 28 month, stunting 
Xvars <- c("logCRP", "logAGP", "sumscore_t0_mom_Z")   
Yvars <- c("whz_t1", "whz_t2", "whz_t3")

#Fit models
H2_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-c(pick_covariates(j), "time_of_day_cort_cont")
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset, forcedW = c("time_of_day_cort_cont"))
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#Get primary contrasts
H2_adj_res <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_adj_res <-  bind_rows(H2_adj_res , preds$res)
}

#Make list of plots
H2_adj_plot_list <- NULL
H2_adj_plot_data <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H2_adj_models$fit[i][[1]], H2_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_adj_plot_list[[i]] <-  simul_plot$p
  H2_adj_plot_data <-  rbind(H2_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred%>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H2_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/adj_H2_adj_models.RDS"))

#Save results
saveRDS(H2_adj_res, ("./results/adjusted/H2_whz_adj_res.RDS"))


#Save plots
#saveRDS(H2_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H2_adj_adj_splines.RDS"))

#Save plot data
saveRDS(H2_adj_plot_data, ("./figure-data/H2_whz_adj_spline.data.RDS"))


##Hypothesis 3
#Maternal nutrition is negatively associated with child growth
Xvars <- c("vitD_nmol_per_L", "logSTFR_inf", "logRBP_inf")            
Yvars <- c("whz_t1", "whz_t2", "whz_t3")


#Fit models
H3_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}



#Get primary contrasts
H3_adj_res <- NULL
for(i in 1:nrow(H3_adj_models)){
  res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
  if(grepl("_def", H3_adj_models$X[i])){
    preds <- predict_gam_diff(fit=H3_adj_models$fit[i][[1]], d=H3_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
  }else{
    preds <- predict_gam_diff(fit=H3_adj_models$fit[i][[1]], d=H3_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  }
  H3_adj_res <-  bind_rows(H3_adj_res , preds$res)
}

#Make list of plots
H3_adj_plot_list <- NULL
H3_adj_plot_data <- NULL
for(i in 1:nrow(H3_adj_models)){
  res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H3_adj_models$fit[i][[1]], H3_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_adj_plot_list[[i]] <-  simul_plot$p
  H3_adj_plot_data <-  rbind(H3_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1_adj_models.RDS"))

#Save results
saveRDS(H3_adj_res, ("./results/adjusted/H3_whz_adj_res.RDS"))


#Save plots
#saveRDS(H1_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H3_adj_plot_data, ("./figure-data/H3_whz_adj_spline.data.RDS"))

## Hypothesis 4
# Maternal estriol is positively associated with child growth.
# X: maternal plasma estriol  - first & second trimester of pregnancy
# Y: child LAZ at 3, 14, 28 months, stunting 
Xvars <- c("ln_preg_estri")
Yvars <- c("whz_t1", "whz_t2", "whz_t3")

#Fit models
H4_adj_models <- NULL
for(i in Xvars){
  for(j in Yvars){
    print(i)
    print(j)
    Wset<-pick_covariates(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=Wset)
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}



#Get primary contrasts
H4_adj_res <- NULL
for(i in 1:nrow(H4_adj_models)){
  res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
  if(grepl("_def", H4_adj_models$X[i])){
    preds <- predict_gam_diff(fit=H4_adj_models$fit[i][[1]], d=H4_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
  }else{
    preds <- predict_gam_diff(fit=H4_adj_models$fit[i][[1]], d=H4_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  }
  H4_adj_res <-  bind_rows(H4_adj_res , preds$res)
}

#Make list of plots
H4_adj_plot_list <- NULL
H4_adj_plot_data <- NULL
for(i in 1:nrow(H4_adj_models)){
  res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
  simul_plot <- gam_simul_CI(H4_adj_models$fit[i][[1]], H4_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_adj_plot_list[[i]] <-  simul_plot$p
  H4_adj_plot_data <-  rbind(H4_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
}


#Save models
#saveRDS(H1_adj_models, paste0(dropboxDir,"results/stress-growth-models/models/H1_adj_models.RDS"))

#Save results
saveRDS(H4_adj_res, ("./results/adjusted/H4_whz_adj_res.RDS"))


#Save plots
#saveRDS(H1_adj_plot_list, paste0(dropboxDir,"results/stress-growth-models/figure-objects/H1_adj_splines.RDS"))

#Save plot data
saveRDS(H4_adj_plot_data, ("./figure-data/H4_whz_adj_spline.data.RDS"))