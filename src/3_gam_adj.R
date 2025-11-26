rm(list=ls())

# Define analysis configurations
analyses <- list(
  igf = list(
    data_file = "./sec_outcomes_covariates.RDS",
    outcomes = c("igf_t2", "igf_t3"),
    suffix = "_igf"
  ),
  laz = list(
    data_file = "./pregnancy_stress_growth_covariates_data.RDS",
    outcomes = c("laz_t1", "laz_t2", "laz_t3"),
    suffix = "_adj"
  ),
  whz = list(
    data_file = "./sec_outcomes_covariates.RDS",
    outcomes = c("whz_t1", "whz_t2", "whz_t3"),
    suffix = "_whz"
  )
)

# Set list of adjustment variables (common to all analyses)
Wvars <- c("sex", "birthord", "momage", "momheight", "momedu", 
           "hfiacat", "Nlt18", "Ncomp", "watmin", "walls", "floor", "HHwealth_scaled",
           "tr", "life_viol_any_t3_cat", "viol_any_preg_cat")

# Function to select appropriate covariates based on outcome timing
pick_covariates <- function(j, Wvars){
  # j is outcome as string
  # choose correct adjustment set based on outcome
  Wvars2 <- c(Wvars, c("ageday_bt2", "month_blood_t0", "month_bt2")) 
  Wvars3 <- c(Wvars, c("ageday_bt3", "month_blood_t0", "month_bt3"))
  
  if(grepl("t2", j)){Wset = Wvars2}
  else{Wset = Wvars3}
  return(Wset)
}

# Loop through each analysis type (IGF, LAZ, WHZ)
for(analysis_type in names(analyses)) {
  
  # Load data for current analysis
  d <- readRDS(analyses[[analysis_type]]$data_file)
  Yvars_common <- analyses[[analysis_type]]$outcomes
  suffix <- analyses[[analysis_type]]$suffix
  
  # Check which adjustment variables are available in the dataset
  Wvars_available <- Wvars[Wvars %in% colnames(d)]
  
  # Hypothesis 1:
  # Maternal plasma cortisol is inversely associated with child growth 
  # X: maternal plasma cortisol - first & second trimester of pregnancy
  # Y: child growth outcomes
  
  Xvars <- c("ln_preg_cort")
  Yvars <- Yvars_common
  
  # Fit models
  H1_adj_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      print(paste(i, j))
      Wset <- c(pick_covariates(j, Wvars_available), "time_of_day_cort_cont")
      res_adj <- fit_RE_gam(d=d, X=i, Y=j, W=Wset, forcedW = c("time_of_day_cort_cont"))
      res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      H1_adj_models <- bind_rows(H1_adj_models, res)
    }
  }
  
  # Get primary contrasts
  H1_adj_res <- NULL
  for(i in 1:nrow(H1_adj_models)){
    res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
    if(grepl("_def", H1_adj_models$X[i])){
      preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
    }else{
      preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    }
    H1_adj_res <- bind_rows(H1_adj_res, preds$res)
  }
  
  # Make list of plots
  H1_adj_plot_list <- NULL
  H1_adj_plot_data <- NULL
  for(i in 1:nrow(H1_adj_models)){
    res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
    simul_plot <- gam_simul_CI(H1_adj_models$fit[i][[1]], H1_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H1_adj_plot_list[[i]] <- simul_plot$p
    H1_adj_plot_data <- rbind(H1_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=1, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
  }
  
  # Save results and plot data
  saveRDS(H1_adj_res, paste0("./results/adjusted/H1", suffix, "_res.RDS"))
  saveRDS(H1_adj_plot_data, paste0("./figure-data/H1", suffix, "_spline.data.RDS"))
  
  
  # Hypothesis 2:
  # Maternal inflammation is inversely associated with in-utero and post-natal growth in children
  # X: CRP, AGP, plasma 13-cytokine sum score in first & second trimester of pregnancy 
  # Y: child growth outcomes

  Xvars <- c("logCRP", "logAGP", "sumscore_t0_mom_Z")   
  Yvars <- Yvars_common
  
  # Fit models
  H2_adj_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      print(paste(i, j))
      Wset <- c(pick_covariates(j, Wvars_available), "time_of_day_cort_cont")
      res_adj <- fit_RE_gam(d=d, X=i, Y=j, W=Wset, forcedW = c("time_of_day_cort_cont"))
      res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      H2_adj_models <- bind_rows(H2_adj_models, res)
    }
  }
  
  # Get primary contrasts
  H2_adj_res <- NULL
  for(i in 1:nrow(H2_adj_models)){
    res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
    preds <- predict_gam_diff(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    H2_adj_res <- bind_rows(H2_adj_res, preds$res)
  }
  
  # Make list of plots
  H2_adj_plot_list <- NULL
  H2_adj_plot_data <- NULL
  for(i in 1:nrow(H2_adj_models)){
    res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
    simul_plot <- gam_simul_CI(H2_adj_models$fit[i][[1]], H2_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H2_adj_plot_list[[i]] <- simul_plot$p
    H2_adj_plot_data <- rbind(H2_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=1, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
  }
  
  # Save results and plot data
  saveRDS(H2_adj_res, paste0("./results/adjusted/H2", suffix, "_res.RDS"))
  saveRDS(H2_adj_plot_data, paste0("./figure-data/H2", suffix, "_spline.data.RDS"))
  
  
  # Hypothesis 3:
  # Maternal nutrition is negatively associated with child growth
  # X: Vitamin D, ferritin, soluble transferrin receptor (sTfR) in first and second trimester of pregnancy 
  # Y: child growth outcomes
  
  Xvars <- c("vitD_nmol_per_L", "logSTFR_inf", "logRBP_inf")            
  Yvars <- Yvars_common
  
  # Fit models
  H3_adj_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      print(paste(i, j))
      Wset <- pick_covariates(j, Wvars_available)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j, W=Wset)
      res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      H3_adj_models <- bind_rows(H3_adj_models, res)
    }
  }
  
  # Get primary contrasts
  H3_adj_res <- NULL
  for(i in 1:nrow(H3_adj_models)){
    res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
    if(grepl("_def", H3_adj_models$X[i])){
      preds <- predict_gam_diff(fit=H3_adj_models$fit[i][[1]], d=H3_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
    }else{
      preds <- predict_gam_diff(fit=H3_adj_models$fit[i][[1]], d=H3_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    }
    H3_adj_res <- bind_rows(H3_adj_res, preds$res)
  }
  
  # Make list of plots
  H3_adj_plot_list <- NULL
  H3_adj_plot_data <- NULL
  for(i in 1:nrow(H3_adj_models)){
    res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
    simul_plot <- gam_simul_CI(H3_adj_models$fit[i][[1]], H3_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H3_adj_plot_list[[i]] <- simul_plot$p
    H3_adj_plot_data <- rbind(H3_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=1, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
  }
  
  # Save results and plot data
  saveRDS(H3_adj_res, paste0("./results/adjusted/H3", suffix, "_res.RDS"))
  saveRDS(H3_adj_plot_data, paste0("./figure-data/H3", suffix, "_spline.data.RDS"))
  
  
  # Hypothesis 4: 
  # Maternal estriol is positively associated with child growth
  # X: maternal plasma estriol - first & second trimester of pregnancy
  # Y: child growth outcomes
  
  Xvars <- c("ln_preg_estri")
  Yvars <- Yvars_common
  
  # Fit models
  H4_adj_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      print(paste(i, j))
      Wset <- pick_covariates(j, Wvars_available)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j, W=Wset)
      res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      H4_adj_models <- bind_rows(H4_adj_models, res)
    }
  }
  
  # Get primary contrasts
  H4_adj_res <- NULL
  for(i in 1:nrow(H4_adj_models)){
    res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
    if(grepl("_def", H4_adj_models$X[i])){
      preds <- predict_gam_diff(fit=H4_adj_models$fit[i][[1]], d=H4_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
    }else{
      preds <- predict_gam_diff(fit=H4_adj_models$fit[i][[1]], d=H4_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    }
    H4_adj_res <- bind_rows(H4_adj_res, preds$res)
  }
  
  # Make list of plots
  H4_adj_plot_list <- NULL
  H4_adj_plot_data <- NULL
  for(i in 1:nrow(H4_adj_models)){
    res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
    simul_plot <- gam_simul_CI(H4_adj_models$fit[i][[1]], H4_adj_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H4_adj_plot_list[[i]] <- simul_plot$p
    H4_adj_plot_data <- rbind(H4_adj_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=1, simul_plot$pred %>% subset(., select = c(Y,X,id,fit,se.fit,uprP, lwrP,uprS,lwrS))))
  }
  
  # Save results and plot data
  saveRDS(H4_adj_res, paste0("./results/adjusted/H4", suffix, "_res.RDS"))
  saveRDS(H4_adj_plot_data, paste0("./figure-data/H4", suffix, "_spline.data.RDS"))
}
