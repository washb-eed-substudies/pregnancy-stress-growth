library(data.table)
library(tidyverse)

# source(here::here("0-config.R"))
source(here::here("./table scripts/table-functions.R"))

# New function for unadjusted-only tables
growth_tbl_flex_unadj <- function(name, expo_var, out_var, exposure, outcome, results, exp_col_size = 1, out_col_size = 1){
  ### This function produces unadjusted-only tables
  
  # build table
  tbl <- data.table(matrix(nrow=0, ncol=9))
  skipped<-F
  for (i in 1:length(exposure)) {
    for (j in 1:length(outcome)) {
      exp <- exposure[i]
      out <- outcome[j]
      filtered_res <- results[results$Y==out & results$X==exp,]
      unadj <- unadj <- paste(round(filtered_res$`point.diff`, 2), " (", round(filtered_res$`lb.diff`, 2), ", ", round(filtered_res$`ub.diff`, 2), ")", sep="")
      if (nrow(filtered_res)==0){
        skipped<-T
        next
      }
      
      sig <- ifelse(filtered_res$BH.Pval < 0.1, "*", "")
      pval <- paste(round(filtered_res$Pval, 2), sig, sep="")
      
      if(j==1|skipped==T){
        tbl <- rbind(tbl, list(expo_var[i], out_var[j], filtered_res$N, round(filtered_res$q1, 2), round(filtered_res$q3, 2), 
                               round(filtered_res$pred.q1, 2), round(filtered_res$pred.q3, 2), unadj, pval))
        skipped=F
      }else {
        tbl <- rbind(tbl, list("", out_var[j],  filtered_res$N, round(filtered_res$q1, 2), round(filtered_res$q3, 2), 
                               round(filtered_res$pred.q1, 2), round(filtered_res$pred.q3, 2), unadj, pval))
      }
    }
    if (i != length(exposure)) {
      tbl <- rbind(tbl, list("","","","","","","","",""))
    }
  }
  
  flextbl<-flextable(tbl, col_keys=names(tbl))
  flextbl <- set_header_labels(flextbl,
                               values = list("V1" = " ", "V2" = " ", "V3" = " ", "V4" = " ", "V5" = " ",
                                             "V6" = "Predicted Outcome at 25th Percentile", "V7" = "Predicted Outcome at 75th Percentile", 
                                             "V8" = "Coefficient (95% CI)", "V9" = "P-value"))
  flextbl <- add_header_row(flextbl, values = c("","","","","", "Unadjusted"), colwidths=c(1,1,1,1,1,4))
  flextbl <- add_header_row(flextbl, values = c(name, "Outcome","N","25th Percentile","75th Percentile", "Outcome, 75th Percentile v. 25th Percentile"), colwidths=c(1,1,1,1,1,4))
  
  flextbl <- hline(flextbl, part="header", border=fp_border(color="black"))
  flextbl <- hline_bottom(flextbl, part="body", border=fp_border(color="black"))
  flextbl <- hline_top(flextbl, part="header", border=fp_border(color="black"))
  flextbl <- align(flextbl, align = "center", part = "all")
  flextbl <- align(flextbl, j = c(1, 2), align = "left", part="all")
  
  flextbl <- add_footer_row(flextbl, top=F, 
                            values = "N, 25th Percentile, and 75th Percentile are from the unadjusted analyses", colwidths = 9)
  flextbl <- add_footer_row(flextbl, top=F, 
                            values = "* P < 0.1 after adjusting for multiple comparisons using the Benjamini-Hochberg procedure", colwidths = 9)
  flextbl <- fontsize(flextbl, part = "all", size = 7)
  flextbl <- width(flextbl, 1:9, width=c(exp_col_size, out_col_size, .3, .55, .55, .8, .8, 1, .5))
  
  flextbl
}

# load enrollment characteristics and results
H1 <- readRDS(file = './results/unadjusted/H1_res.RDS')
H1b <- readRDS(file = './results/unadjusted/H1_whz_res.RDS')
H1c <- readRDS(file = './results/unadjusted/H1_igf_res.RDS')

H2 <- readRDS(file = './results/unadjusted/H2_res.RDS')
H2b <- readRDS(file = './results/unadjusted/H2_whz_res.RDS')
H2c <- readRDS(file = './results/unadjusted/H2_igf_res.RDS')

H3 <- readRDS(file = ('./results/unadjusted/H3_res.RDS'))
H3b <- readRDS(file = './results/unadjusted/H3_whz_res.RDS')
H3c <- readRDS(file = './results/unadjusted/H3_igf_res.RDS')

H4 <- readRDS(file = ("./results/unadjusted/H4_res.RDS"))
H4b <- readRDS(file = './results/unadjusted/H4_whz_res.RDS')
H4c <- readRDS(file = './results/unadjusted/H4_igf_res.RDS')

H1adj <- readRDS(file = ('./results/adjusted/H1_adj_res.RDS'))
H1badj <- readRDS(file = ('./results/adjusted/H1_whz_adj_res.RDS'))
H1cadj <- readRDS(file = ('./results/adjusted/H1_igf_adj_res.RDS'))

H2adj <- readRDS(file=('./results/adjusted/H2_adj_res.RDS'))
H2badj <- readRDS(file = ('./results/adjusted/H2_whz_adj_res.RDS'))
H2cadj <- readRDS(file = ('./results/adjusted/H2_igf_adj_res.RDS'))

H3adj <- readRDS(file=('./results/adjusted/H3_adj_res.RDS'))
H3badj <- readRDS(file = ('./results/adjusted/H3_whz_adj_res.RDS'))
H3cadj <- readRDS(file = ('./results/adjusted/H3_igf_adj_res.RDS'))

H4adj <- readRDS(file=('./results/adjusted/H4_adj_res.RDS'))
H4badj <- readRDS(file = ('./results/adjusted/H4_whz_adj_res.RDS'))
H4cadj <- readRDS(file = ('./results/adjusted/H4_igf_adj_res.RDS'))

full_res <- bind_rows(H1, H1b, H1c, H2, H2b, H2c, H3, H3b, H3c, H4, H4b, H4c)
full_adj_res <- bind_rows(H1adj, H1badj, H1cadj, H2adj, H2badj, H2cadj, H3adj, H3badj, H3cadj, H4adj, H4badj, H4cadj)

#### MAIN TABLES ####

#### Table 2 Maternal Estriol H4 (Adjusted) ####
exposure <- c("ln_preg_estri") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Estriol (ng/mL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl2 <- growth_tbl("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl2flex <- growth_tbl_flex("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, .8, 1.4)

#### Table 2 Maternal Estriol H4 (Unadjusted) ####
tbl2_unadj <- growth_tbl("Maternal Estriol and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)
tbl2flex_unadj <- growth_tbl_flex_unadj("Maternal Estriol and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, .8, 1.4)

#### Table 3 Maternal Plasma Cortisol H1 (Adjusted) ####
exposure <- c("ln_preg_cort") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Cortisol (ug/dL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl3 <- growth_tbl("Maternal Plasma Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl3flex <- growth_tbl_flex("Maternal Plasma Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, 1.1, 1.4)

#### Table 3 Maternal Plasma Cortisol H1 (Unadjusted) ####
tbl3_unadj <- growth_tbl("Maternal Plasma Cortisol and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)
tbl3flex_unadj <- growth_tbl_flex_unadj("Maternal Plasma Cortisol and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, 1.1, 1.4)

#### Table 4 Maternal Inflammation and Child Growth H2 (Adjusted) ####
exposure <- c("logAGP", "logCRP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")   
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln AGP (g/L)", "Ln CRP (mg/L)", "Initial Ln IFN-y (pg/mL)", "Sum score of 13 cytokines") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl4 <- growth_tbl("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl4flex <- growth_tbl_flex("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, 1.1, 1.4)

#### Table 4 Maternal Inflammation and Child Growth H2 (Unadjusted) ####
tbl4_unadj <- growth_tbl("Maternal Inflammation and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)
tbl4flex_unadj <- growth_tbl_flex_unadj("Maternal Inflammation and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, 1.1, 1.4)

#### Table 5 Maternal Micronutrients H3 (Adjusted) ####
exposure <- c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Vitamin D (nmol/L)", "Vitamin D deficiency", "Ln RBP (umol/L)", "Vitamin A deficiency","Ln ferritin (ug/L)", "Ln sTfR (mg/L)", "Iron deficiency") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl5 <- growth_tbl("Maternal Micronutrients and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl5flex <- growth_tbl_flex("Maternal Micronutrients and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, 1.1, 1.4)

#### Table 5 Maternal Micronutrients H3 (Unadjusted) ####
tbl5_unadj <- growth_tbl("Maternal Micronutrients and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)
tbl5flex_unadj <- growth_tbl_flex_unadj("Maternal Micronutrients and Child Growth Status - Unadjusted", expo_var, out_var, exposure, outcome, full_res, 1.1, 1.4)

#### SAVE TABLES ####
# Save CSV files for adjusted tables
write.csv(tbl2, ('./tables/pregnancy-stress-H4.csv'))
write.csv(tbl3, ('./tables/pregnancy-stress-H1.csv'))
write.csv(tbl4, ('./tables/pregnancy-stress-H2.csv'))
write.csv(tbl5, ('./tables/pregnancy-stress-H3.csv'))

# Save CSV files for unadjusted tables
write.csv(tbl2_unadj, ('./tables/pregnancy-stress-H4-unadjusted.csv'))
write.csv(tbl3_unadj, ('./tables/pregnancy-stress-H1-unadjusted.csv'))
write.csv(tbl4_unadj, ('./tables/pregnancy-stress-H2-unadjusted.csv'))
write.csv(tbl5_unadj, ('./tables/pregnancy-stress-H3-unadjusted.csv'))

# Set page properties for landscape orientation
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape", width=11, height=8.5),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)

# Save adjusted tables to Word document
save_as_docx("Table 2: Maternal Estriol and Child Growth Status" = tbl2flex,
             "Table 3: Maternal Plasma Cortisol and Child Growth Status" = tbl3flex,
             "Table 4: Maternal Inflammation and Child Growth Status" = tbl4flex,
             "Table 5: Maternal Micronutrients and Child Growth Status" = tbl5flex,
             path='./tables/pregnancy-stress-adjusted.docx',
             pr_section = sect_properties) 

# Save unadjusted tables to Word document
save_as_docx("Table 2: Maternal Estriol and Child Growth Status - Unadjusted" = tbl2flex_unadj,
             "Table 3: Maternal Plasma Cortisol and Child Growth Status - Unadjusted" = tbl3flex_unadj,
             "Table 4: Maternal Inflammation and Child Growth Status - Unadjusted" = tbl4flex_unadj,
             "Table 5: Maternal Micronutrients and Child Growth Status - Unadjusted" = tbl5flex_unadj,
             path='./tables/pregnancy-stress-unadjusted-fixed.docx',
             pr_section = sect_properties)

#-----------------------------------------------
#Tables Including BH-Correction for Internal Use
#-----------------------------------------------

#-------------------------------
#Table 2 Maternal Estriol (BH-corrected)
#-------------------------------
exposure <- c("ln_preg_estri") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Estriol (ng/mL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl2 <- growth_tbl("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#-----------------------------------
#Table 3 Maternal plasma cortisol H1 (BH-corrected)
#-----------------------------------
exposure <- c("ln_preg_cort") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Cortisol (ug/dL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl3 <- growth_tbl("Maternal Plasma Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#--------------------------------------------------
# Table 4 Maternal Inflammation and Child Growth, H2 (BH-corrected)
#---------------------------------------------------
exposure <- c("logAGP", "logCRP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")   
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln AGP (g/L)", "Ln CRP (mg/L)", "Initial Ln IFN-y (pg/mL)", "Sum score of 13 cytokines") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl4 <- growth_tbl("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#-------------------------------
#Table 5 Maternal Micronutrients (BH-corrected)
#-------------------------------
exposure <- c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Vitamin D (nmol/L)", "Vitamin D deficiency", "Ln RBP (umol/L)", "Vitamin A deficiency","Ln ferritin (ug/L)", "Ln sTfR (mg/L)", "Iron deficiency") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl5 <- growth_tbl("Maternal Micronutrients and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#---------------------
#Save BH-corrected Tables
#---------------------
write.csv(BH_tbl2, ('./tables/pregnancy-stress-H4-BH.csv'))
write.csv(BH_tbl3, ('./tables/pregnancy-stress-H1-BH.csv'))
write.csv(BH_tbl4, ('./tables/pregnancy-stress-H2-BH.csv'))
write.csv(BH_tbl5, ('./tables/pregnancy-stress-H3-BH.csv'))

#making sure all column names are unique 
colnames(BH_tbl2) <- make.unique(colnames(BH_tbl2))
colnames(BH_tbl3) <- make.unique(colnames(BH_tbl3))
colnames(BH_tbl4) <- make.unique(colnames(BH_tbl4))
colnames(BH_tbl5) <- make.unique(colnames(BH_tbl5))

#create word doc for BH-corrected tables
doc <- read_docx()

# Add each BH-corrected table to the document
doc <- doc %>%
  body_add_flextable(flextable(BH_tbl2)) %>%
  body_add_par("Table 2: Maternal Estriol - BH Adjusted P-values", style = "heading 1") %>%
  body_add_flextable(flextable(BH_tbl3)) %>%
  body_add_par("Table 3: Maternal Plasma Cortisol - BH Adjusted P-values", style = "heading 1") %>%
  body_add_flextable(flextable(BH_tbl4)) %>%
  body_add_par("Table 4: Maternal Inflammation - BH Adjusted P-values", style = "heading 1") %>%
  body_add_flextable(flextable(BH_tbl5)) %>%
  body_add_par("Table 5: Maternal Micronutrients - BH Adjusted P-values", style = "heading 1")

# Save the Word document
print(doc, target = "./tables/pregnancy-stress-BH-tables.docx")