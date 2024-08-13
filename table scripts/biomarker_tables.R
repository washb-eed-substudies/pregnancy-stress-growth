rm(list=ls())
library(data.table)
library(tidyverse)

# source(here::here("0-config.R"))
source(here::here("C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/table scripts/table-functions.R"))

# load enrollment characteristics and results
H1 <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H1_res.RDS')
H1b <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H1_whz_res.RDS')
H1c <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H1_igf_res.RDS')

H2 <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H2_res.RDS')
H2b <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H2_whz_res.RDS')
H2c <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H2_igf_res.RDS')

H3 <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H3_res.RDS'))
H3b <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H3_whz_res.RDS')
H3c <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H3_igf_res.RDS')

H4 <- readRDS(file = ("C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H4_res.RDS"))
H4b <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H4_whz_res.RDS')
H4c <- readRDS(file = 'C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/unadjusted/H4_igf_res.RDS')

H1adj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H1_adj_res.RDS'))
H1badj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H1_whz_adj_res.RDS'))
H1cadj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H1_igf_adj_res.RDS'))


H2adj <- readRDS(file=('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H2_adj_res.RDS'))
H2badj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H2_whz_adj_res.RDS'))
H2cadj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H2_igf_adj_res.RDS'))

H3adj <- readRDS(file=('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H3_adj_res.RDS'))
H3badj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H3_whz_adj_res.RDS'))
H3cadj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H3_igf_adj_res.RDS'))

H4adj <- readRDS(file=('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H4_adj_res.RDS'))
H4badj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H4_whz_adj_res.RDS'))
H4cadj <- readRDS(file = ('C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/results/adjusted/H4_igf_adj_res.RDS'))


full_res <- bind_rows(H1, H1b, H1c, H2, H2b, H2c, H3, H3b, H3c, H4, H4b, H4c)
full_adj_res <- bind_rows(H1adj, H1badj, H1cadj, H2adj, H2badj, H2cadj, H3adj, H3badj, H3cadj, H4adj, H4badj, H4cadj)

#### MAIN TABLES ####
#### Table 2 Maternal Micronutrients H3 ####
exposure <- c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3"
             )
expo_var <- c("Vitamin D (nmol/L)", "Vitamin D deficiency", "Ln RBP (umol/L)", "Vitamin A deficiency","Ln ferritin (ug/L)", "Ln sTfR (mg/L)", "Iron deficiency") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl2 <- growth_tbl("Maternal Micronutrients and Child Growth", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl2flex <- growth_tbl_flex("Maternal Micronutrients and Child Growth", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, 1.1, 1.4)
tbl2supp <- growth_tbl("Maternal Micronutrients and Child Growth", expo_var, out_var, exposure, outcome, full_res, full_adj_res,)
tbl2flexsupp <- growth_tbl_flex("Maternal Micronutrients and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res,)

#### Table 3 Maternal plasma cortisol H1####
exposure <- c("ln_preg_cort") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Cortisol (ug/dL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl3 <- growth_tbl("Maternal Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl3flex <- growth_tbl_flex("Maternal Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, 1.1, 1.4)
tbl3supp <- growth_tbl("Maternal Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res)
tbl3flexsupp <- growth_tbl_flex("Maternal Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res)

#### Table 4 Maternal estriol H4####
exposure <- c("ln_preg_estri") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Estriol (ng/mL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
"Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
"Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl4 <- growth_tbl("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl4flex <- growth_tbl_flex("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, .8, 1.4)
tbl4supp <- growth_tbl("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res)
tbl4flexsupp <- growth_tbl_flex("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res)

#### Table 5 Maternal Inflammation and Child Growth, H2 ####
exposure <- c("logAGP", "logCRP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")   
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln AGP (g/L)", "Ln CRP (mg/L)", "Initial Ln IFN-y (pg/mL)", "Sum score of 13 cytokines") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

tbl5 <- growth_tbl("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T)
tbl5flex <- growth_tbl_flex("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, T, 1.1, 1.4)
tbl5supp <- growth_tbl("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res)
tbl5flexsupp <- growth_tbl_flex("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res)


#### SAVE TABLES ####
write.csv(tbl2, ('./tables/pregnancy-stress-H3.csv'))
write.csv(tbl3, ('./tables/pregnancy-stress-H1.csv'))
write.csv(tbl4, ('./tables/pregnancy-stress-H4.csv'))
write.csv(tbl5, ('./tables/pregnancy-stress-H2.csv'))

# write.csv(tbl2supp, ('./tables/immune-growth-supptable2.csv'))
# write.csv(tbl3supp, ('./tables/immune-growth-supptable3.csv'))
# write.csv(tbl4supp, ('./tables/supplementary/immune-growth-supptable4.csv'))
# write.csv(tbl5supp, ('./tables/supplementary/immune-growth-supptable5.csv'))

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape", width=11, height=8.5),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)


save_as_docx("Table 5: Maternal Micronutrients and Child Growth Status" = tbl2flex, 
             "Table 3: Maternal Plasma Cortisol and Child Growth Status" = tbl3flex,
             "Table 2: Maternal Estriol and Child Growth Status" = tbl4flex,
             "Table 4: Maternal Inflammation and Child Growth Status" = tbl5flex,
             path='C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/tables/pregnancy-stress.docx',
             pr_section = sect_properties) 

# save_as_docx("Table 4: Maternal Micronutrients and Child Immune Status" = tbl2flexsupp, 
#              "Table 2: Maternal Cortisol and Child Immune Status" = tbl3flexsupp, 
#              "Table 5: Maternal Estriol and Child Immune Status" = tbl4flexsupp, 
#              "Table 3: Maternal Immune Status and Child Immune Status" = tbl5flexsupp,
#              path='C:/Users/Classof2020ChenBelin/pregnancy-stress-growth/tables/pregnancy-stress-supp-tables.docx',
#              pr_section = sect_properties)

#-----------------------------------------------
#Tables Including BH-Correction for Internal Use
#-----------------------------------------------

#-------------------------------
#Table 2 Maternal Micronutrients
#-------------------------------
exposure <- c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3"
)
expo_var <- c("Vitamin D (nmol/L)", "Vitamin D deficiency", "Ln RBP (umol/L)", "Vitamin A deficiency","Ln ferritin (ug/L)", "Ln sTfR (mg/L)", "Iron deficiency") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl2 <- growth_tbl("Maternal Micronutrients and Child Growth", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#-----------------------------------
#Table 3 Maternal plasma cortisol H1
#-----------------------------------
exposure <- c("ln_preg_cort") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Cortisol (ug/dL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl3 <- growth_tbl("Maternal Cortisol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#---------------------------
#Table 4 Maternal estriol H4
#---------------------------
exposure <- c("ln_preg_estri") 
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln Estriol (ng/mL)") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl4 <- growth_tbl("Maternal Estriol and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#--------------------------------------------------
# Table 5 Maternal Inflammation and Child Growth, H2 
#---------------------------------------------------
exposure <- c("logAGP", "logCRP", "mom_t0_ln_ifn", "sumscore_t0_mom_Z")   
outcome <- c("laz_t1", "whz_t1", "laz_t2", "whz_t2", "igf_t2", "laz_t3", "whz_t3", "igf_t3")
expo_var <- c("Ln AGP (g/L)", "Ln CRP (mg/L)", "Initial Ln IFN-y (pg/mL)", "Sum score of 13 cytokines") 
out_var <- c("Length-for-Age Z-Score 3 months", "Weight-for-Length Z-Score 3 months", 
             "Length-for-Age Z-Score 14 months", "Weight-for-Length Z-Score 14 months", "Ln IGF-1 14 months (ug/L)", 
             "Length-for-Age Z-Score 28 months", "Weight-for-Length Z-Score 28 months", "Ln IGF-1 28 months (ug/L)")

BH_tbl5 <- growth_tbl("Maternal Inflammation and Child Growth Status", expo_var, out_var, exposure, outcome, full_res, full_adj_res, F)

#---------------------
#Save Tables
#---------------------
write.csv(BH_tbl2, ('./tables/pregnancy-stress-H3-BH.csv'))
write.csv(BH_tbl3, ('./tables/pregnancy-stress-H1-BH.csv'))
write.csv(BH_tbl4, ('./tables/pregnancy-stress-H4-BH.csv'))
write.csv(BH_tbl5, ('./tables/pregnancy-stress-H2-BH.csv'))