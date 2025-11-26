# Adjust all p-values using Benjamini-Hochberg procedure
rm(list=ls())

# Define analysis configurations
analyses <- list(
  igf = list(
    suffix = "_igf"
  ),
  laz = list(
    suffix = ""
  ),
  whz = list(
    suffix = "_whz"
  )
)

# Loop through each outcome
for(analysis_type in names(analyses)) {
  
  suffix <- analyses[[analysis_type]]$suffix
  
  # Load all unadjusted results
  H1_res <- readRDS(paste0('./results/unadjusted/H1', suffix, '_res.RDS'))
  H2_res <- readRDS(paste0('./results/unadjusted/H2', suffix, '_res.RDS'))
  H3_res <- readRDS(paste0('./results/unadjusted/H3', suffix, '_res.RDS'))
  H4_res <- readRDS(paste0('./results/unadjusted/H4', suffix, '_res.RDS'))
  
  # Load all adjusted results
  H1_adj_res <- readRDS(paste0('./results/adjusted/H1', suffix, '_adj_res.RDS'))
  H2_adj_res <- readRDS(paste0('./results/adjusted/H2', suffix, '_adj_res.RDS'))
  H3_adj_res <- readRDS(paste0('./results/adjusted/H3', suffix, '_adj_res.RDS'))
  H4_adj_res <- readRDS(paste0('./results/adjusted/H4', suffix, '_adj_res.RDS'))
  
  # Add hypothesis identifier to each result set
  H1_res$H = 1
  H2_res$H = 2
  H3_res$H = 3
  H4_res$H = 4
  
  H1_adj_res$H = 1
  H2_adj_res$H = 2
  H3_adj_res$H = 3
  H4_adj_res$H = 4
  
  # Combine all results
  full_res <- rbind(H1_res, H2_res, H3_res, H4_res)
  full_adj_res <- rbind(H1_adj_res, H2_adj_res, H3_adj_res, H4_adj_res)
  
  # Apply Benjamini-Hochberg correction to each hypothesis (both unadj and adj)
  full_res <- full_res %>% 
    group_by(H) %>% 
    mutate(BH.Pval = p.adjust(Pval, method="BH")) %>%
    ungroup() %>%
    as.data.frame()
  
  full_adj_res <- full_adj_res %>% 
    group_by(H) %>% 
    mutate(BH.Pval = p.adjust(Pval, method="BH")) %>%
    ungroup() %>%
    as.data.frame()
  
  # Save corrected results back to individual hypothesis files
  cat("Saving corrected results...\n")
  saveRDS(full_res %>% filter(H==1) %>% select(-H), 
          paste0("./results/unadjusted/H1", suffix, "_res.RDS"))
  saveRDS(full_res %>% filter(H==2) %>% select(-H), 
          paste0("./results/unadjusted/H2", suffix, "_res.RDS"))
  saveRDS(full_res %>% filter(H==3) %>% select(-H), 
          paste0("./results/unadjusted/H3", suffix, "_res.RDS"))
  saveRDS(full_res %>% filter(H==4) %>% select(-H), 
          paste0("./results/unadjusted/H4", suffix, "_res.RDS"))
  
  saveRDS(full_adj_res %>% filter(H==1) %>% select(-H), 
          paste0("./results/adjusted/H1", suffix, "_adj_res.RDS"))
  saveRDS(full_adj_res %>% filter(H==2) %>% select(-H), 
          paste0("./results/adjusted/H2", suffix, "_adj_res.RDS"))
  saveRDS(full_adj_res %>% filter(H==3) %>% select(-H), 
          paste0("./results/adjusted/H3", suffix, "_adj_res.RDS"))
  saveRDS(full_adj_res %>% filter(H==4) %>% select(-H), 
          paste0("./results/adjusted/H4", suffix, "_adj_res.RDS"))
}
