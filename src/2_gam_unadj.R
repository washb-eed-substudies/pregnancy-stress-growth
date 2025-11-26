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
    suffix = ""
  ),
  whz = list(
    data_file = "./secondary_outcomes_stress_growth.RDS",
    outcomes = c("whz_t1.x", "whz_t2.x", "whz_t3.x"),
    suffix = "_whz"
  )
)

# Loop through each outcome (LAZ, WHZ, IGF-1)
for(analysis_type in names(analyses)) {
  
  # Load data for current analysis
  d <- readRDS(analyses[[analysis_type]]$data_file)
  Yvars_common <- analyses[[analysis_type]]$outcomes
  suffix <- analyses[[analysis_type]]$suffix
  
  # Hypothesis 1:
  # Maternal plasma cortisol is inversely associated with child growth 
  # X: maternal plasma cortisol - first & second trimester of pregnancy
  # Y: child growth outcomes
  
  Xvars <- c("ln_preg_cort")
  Yvars <- Yvars_common
  
  # Fit models
  H1_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
      res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
      H1_models <- bind_rows(H1_models, res)
    }
  }
  
  # Get primary contrasts
  H1_res <- NULL
  for(i in 1:nrow(H1_models)){
    res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
    if(grepl("_def", H1_models$X[i])){
      preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
    }else{
      preds <- predict_gam_diff(fit=H1_models$fit[i][[1]], d=H1_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    }
    H1_res <-  bind_rows(H1_res , preds$res)
  }
  
  # Make list of plots
  H1_plot_list <- NULL
  H1_plot_data <- NULL
  for(i in 1:nrow(H1_models)){
    res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
    simul_plot <- gam_simul_CI(H1_models$fit[i][[1]], H1_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H1_plot_list[[i]] <-  simul_plot$p
    H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
  }
  
  # Save models, results, and plot data
  saveRDS(H1_models, paste0("./models/H1", suffix, "_models.RDS"))
  saveRDS(H1_res, paste0("./results/unadjusted/H1", suffix, "_res.RDS"))
  saveRDS(H1_plot_data, paste0("./figure-data/H1", suffix, "_unadj_spline_data.RDS"))
  
  
  # Hypothesis 2
  # Maternal inflammation is inversely associated with in-utero and post-natal growth in children
  # X: CRP, AGP, plasma 13-cytokine sum score in first & second trimester of pregnancy 
  # Y: child growth outcomes
  
  Xvars <- c("logCRP", "logAGP", "sumscore_t0_mom_Z")            
  Yvars <- Yvars_common
  
  # Fit models
  H2_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
      res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
      H2_models <- bind_rows(H2_models, res)
    }
  }
  
  # Get primary contrasts
  H2_res <- NULL
  for(i in 1:nrow(H2_models)){
    res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
    preds <- predict_gam_diff(fit=H2_models$fit[i][[1]], d=H2_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    H2_res <-  bind_rows(H2_res , preds$res)
  }
  
  # Make list of plots
  H2_plot_list <- NULL
  H2_plot_data <- NULL
  for(i in 1:nrow(H2_models)){
    res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
    simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H2_plot_list[[i]] <-  simul_plot$p
    H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
  }
  
  # Save models, results, and plot data
  saveRDS(H2_models, paste0("./models/H2", suffix, "_models.RDS"))
  saveRDS(H2_res, paste0("./results/unadjusted/H2", suffix, "_res.RDS"))
  saveRDS(H2_plot_data, paste0("./figure-data/H2", suffix, "_unadj_spline_data.RDS"))
  
  
  # Hypothesis 3:
  # Maternal nutrition is positively associated with child attained growth 
  # X: Vitamin D, ferritin, soluble transferrin receptor (sTfR), Vitamin A deficiency in first or second trimester 
  # Y: child growth outcomes
  
  Xvars <- c("logRBP_inf", "logSTFR_inf", "vitD_nmol_per_L")
  Yvars <- Yvars_common
  
  # Fit models
  H3_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
      res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
      H3_models <- bind_rows(H3_models, res)
    }
  }
  
  # Get primary contrasts
  H3_res <- NULL
  for(i in 1:nrow(H3_models)){
    res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
    if(grepl("_def", H3_models$X[i])){
      preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
    }else{
      preds <- predict_gam_diff(fit=H3_models$fit[i][[1]], d=H3_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    }
    H3_res <-  bind_rows(H3_res , preds$res)
  }
  
  # Make list of plots
  H3_plot_list <- NULL
  H3_plot_data <- NULL
  for(i in 1:nrow(H3_models)){
    res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
    simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H3_plot_list[[i]] <-  simul_plot$p
    H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
  }
  
  # Save models, results, and plot data
  saveRDS(H3_models, paste0("./models/H3", suffix, "_models.RDS"))
  saveRDS(H3_res, paste0("./results/unadjusted/H3", suffix, "_res.RDS"))
  saveRDS(H3_plot_data, paste0("./figure-data/H3", suffix, "_unadj_spline_data.RDS"))
  
  
  # Hypothesis 4:
  # Maternal estriol is positively associated with child attained growth 
  # X: Maternal estriol in first or second trimester 
  # Y: child growth outcomes
  
  Xvars <- c("ln_preg_estri")
  Yvars <- Yvars_common
  
  # Fit models
  H4_models <- NULL
  for(i in Xvars){
    for(j in Yvars){
      res_unadj <- fit_RE_gam(d=d, X=i, Y=j,  W=NULL)
      res <- data.frame(X=i, Y=j, fit=I(list(res_unadj$fit)), dat=I(list(res_unadj$dat)))
      H4_models <- bind_rows(H4_models, res)
    }
  }
  
  # Get primary contrasts
  H4_res <- NULL
  for(i in 1:nrow(H4_models)){
    res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
    if(grepl("_def", H4_models$X[i])){
      preds <- predict_gam_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y, binary=T)
    }else{
      preds <- predict_gam_diff(fit=H4_models$fit[i][[1]], d=H4_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
    }
    H4_res <-  bind_rows(H4_res , preds$res)
  }
  
  # Make list of plots
  H4_plot_list <- NULL
  H4_plot_data <- NULL
  for(i in 1:nrow(H4_models)){
    res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
    simul_plot <- gam_simul_CI(H4_models$fit[i][[1]], H4_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
    H4_plot_list[[i]] <-  simul_plot$p
    H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
  }
  
  # Save models, results, and plot data
  saveRDS(H4_models, paste0("./models/H4", suffix, "_models.RDS"))
  saveRDS(H4_res, paste0("./results/unadjusted/H4", suffix, "_res.RDS"))
  saveRDS(H4_plot_data, paste0("./figure-data/H4", suffix, "_unadj_spline_data.RDS"))
}
