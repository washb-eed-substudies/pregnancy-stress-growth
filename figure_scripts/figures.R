rm(list=ls())
library(here)
library(tidyverse)
library(cowplot)
library(patchwork)

theme_ki <- function(){
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

# #load spline data
# H1_laz_spline <- readRDS(here("figure-data/H1_adj_spline.data.RDS"))
# H1_whz_spline <- readRDS(here("figure-data/H1_adj_whz_spline.data.RDS"))
# H1_igf_spline <- readRDS(here("figure-data/H1_igf_adj_spline.data.RDS"))
# H1_spline <- bind_rows(H1_laz_spline, H1_whz_spline, H1_igf_spline)
# 
# H2_laz_spline <- readRDS(here("figure-data/H2_adj_spline.data.RDS"))
# H2_whz_spline <- readRDS(here("figure-data/H2_whz_adj_spline.data.RDS"))
# H2_igf_spline <- readRDS(here("figure-data/H2_igf_adj_spline.data.RDS"))
# H2_spline <- bind_rows(H2_laz_spline, H2_whz_spline, H2_igf_spline)
# 
# H3_laz_spline <- readRDS(here("figure-data/H3_adj_spline.data.RDS"))
# H3_whz_spline <- readRDS(here("figure-data/H3_whz_adj_spline.data.RDS"))
# H3_igf_spline <- readRDS(here("figure-data/H3_igf_adj_spline.data.RDS"))
# H3_spline <- bind_rows(H3_laz_spline, H3_whz_spline, H3_igf_spline)
# 
# H4_laz_spline <- readRDS(here("figure-data/H4_adj_spline.data.RDS"))
# H4_whz_spline <- readRDS(here("figure-data/H4_whz_adj_spline.data.RDS"))
# H4_igf_spline <- readRDS(here("figure-data/H4_igf_adj_spline.data.RDS"))
# H4_spline <- bind_rows(H4_laz_spline, H4_whz_spline, H4_igf_spline)



#load results for quartiles
H1_laz_quart <- readRDS(here("results/adjusted/H1_adj_res.RDS"))
H1_whz_quart <- readRDS(here("results/adjusted/H1_whz_adj_res.RDS"))
H1_igf_quart <- readRDS(here("results/adjusted/H1_igf_adj_res.RDS"))
H1_quartiles <- bind_rows(H1_laz_quart, H1_whz_quart, H1_igf_quart)

H2_laz_quart <- readRDS(here("results/adjusted/H2_adj_res.RDS"))
H2_whz_quart <- readRDS(here("results/adjusted/H2_whz_adj_res.RDS"))
H2_igf_quart <- readRDS(here("results/adjusted/H2_igf_adj_res.RDS"))
H2_quartiles <- bind_rows(H2_laz_quart, H2_whz_quart, H2_igf_quart)

H3_laz_quart <- readRDS(here("results/adjusted/H3_adj_res.RDS"))
H3_whz_quart <- readRDS(here("results/adjusted/H3_whz_adj_res.RDS"))
H3_igf_quart <- readRDS(here("results/adjusted/H3_igf_adj_res.RDS"))
H3_quartiles <- bind_rows(H3_laz_quart, H3_whz_quart, H3_igf_quart)

H4_laz_quart <- readRDS(here("results/adjusted/H4_adj_res.RDS"))
H4_whz_quart <- readRDS(here("results/adjusted/H4_whz_adj_res.RDS"))
H4_igf_quart <- readRDS(here("results/adjusted/H4_igf_adj_res.RDS"))
H4_quartiles <- bind_rows(H4_laz_quart, H4_whz_quart, H4_igf_quart)

table(H1_quartiles$X)
table(H2_quartiles$X)
table(H3_quartiles$X)
table(H4_quartiles$X)

 
#-------------------------------------------------------------------------------
# Figure 2, 2 plots for estriol and cortisol exposures
#-------------------------------------------------------------------------------

plotdf2 <- bind_rows(H1_quartiles, H4_quartiles) %>% 
  mutate(x=factor(X, levels=c("ln_preg_estri","ln_preg_cort"), 
                  labels=c("Log-Maternal Plasma Cortisol", "Log-Maternal Plasma Estriol")),
         y=case_when(grepl("t1",Y) ~ "3 Months",
                     grepl("t2",Y) ~ "14 Months",
                     grepl("t3",Y) ~ "28 Months"),
         y=factor(y, levels=c("3 Months", "14 Months", "28 Months")),
         Outcome=case_when(grepl("laz",Y) ~ "LAZ",
                           grepl("whz",Y) ~ "WLZ",
                           grepl("igf",Y) ~ "IGF-1"),
         Outcome=factor(Outcome, levels=c("LAZ", "WLZ", "IGF-1")))


tableau10 <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", 
               "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")
p2 <- ggplot(plotdf2, aes(x=y, y=point.diff)) + 
  geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=x, shape=x),
                  position = position_dodge2(width = 0.5),
                  size = 1, show.legend = T) +
  facet_grid(vars(Outcome), scales = "free") + 
  labs(y = "Adjusted difference in mean child growth status outcome\nbetween 25th and 75th percentile of maternal exposure", 
       x="Child age") +
  geom_hline(yintercept = 0, linetype="dashed") +
  guides(color="none")+
  scale_color_manual(values = tableau10[c(1:2)]) + 
  theme_ki() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=10),
        strip.text = element_text(hjust=0.5, size=10),
        strip.placement = "outside",
        axis.text.y = element_text(hjust = 1, size=9),
        panel.spacing = unit(0.5, "lines")) +
  guides(shape=guide_legend(title="Maternal exposure"),
         color=guide_legend(title="Maternal exposure"))
p2


ggsave(p2, filename=here("figures/pregnancy-stress-growth-figure2.jpg"), width=6, height=5)


#-------------------------------------------------------------------------------
# Figure 3, 3 plots for CRP, AGP, and cytokine sum score
#-------------------------------------------------------------------------------

plotdf3 <- H2_quartiles%>% 
  mutate(x=factor(X, levels=c("logAGP","logCRP","sumscore_t0_mom_Z"), 
                  labels=c("Log-Maternal AGP", "Log-Maternal CRP", "Maternal cytokine sum score")),
         y=case_when(grepl("t1",Y) ~ "3 Months",
                     grepl("t2",Y) ~ "14 Months",
                     grepl("t3",Y) ~ "28 Months"),
         y=factor(y, levels=c("3 Months", "14 Months", "28 Months")),
         Outcome=case_when(grepl("laz",Y) ~ "LAZ",
                           grepl("whz",Y) ~ "WLZ",
                           grepl("igf",Y) ~ "IGF-1"),
         Outcome=factor(Outcome, levels=c("LAZ", "WLZ", "IGF-1")))


tableau10 <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", 
               "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")
p3 <- ggplot(plotdf3, aes(x=y, y=point.diff)) + 
  geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=x, shape=x),
                  position = position_dodge2(width = 0.5),
                  size = 1, show.legend = T) +
  facet_grid(vars(Outcome), scales = "free") + 
  labs(y = "Adjusted difference in mean child growth status outcome\nbetween 25th and 75th percentile of maternal exposure", 
       x="Child age") +
  geom_hline(yintercept = 0, linetype="dashed") +
  guides(color="none")+
  scale_color_manual(values = tableau10[c(3:5)]) + 
  theme_ki() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0),
        axis.title.x = element_text(size=11),
        axis.title.y = element_text(size=10),
        strip.text = element_text(hjust=0.5, size=10),
        strip.placement = "outside",
        axis.text.y = element_text(hjust = 1, size=9),
        panel.spacing = unit(0.5, "lines")) +
  guides(shape=guide_legend(title="Maternal exposure"),
         color=guide_legend(title="Maternal exposure"))
p3

ggsave(p3, filename=here("figures/pregnancy-stress-growth-figure3.jpg"), width=6, height=5)


#-------------------------------------------------------------------------------
# Figure 4, 4 plots for RBP, Vitamin A deficiency, ferritin, sTfR, and Vitamin D
#-------------------------------------------------------------------------------

 # logRBP_inf     logSTFR_inf vitD_nmol_per_L 
 plotdf3 <- H3_quartiles %>% 
   mutate(x=factor(X, levels=c("logRBP_inf","logSTFR_inf","vitD_nmol_per_L"), 
                   labels=c("Log-Maternal RBP", "Log-Maternal sTfR", "Vitamin D (nmol/L)")),
          y=case_when(grepl("t1",Y) ~ "3 Months",
                      grepl("t2",Y) ~ "14 Months",
                      grepl("t3",Y) ~ "28 Months"),
          y=factor(y, levels=c("3 Months", "14 Months", "28 Months")),
          Outcome=case_when(grepl("laz",Y) ~ "LAZ",
                            grepl("whz",Y) ~ "WLZ",
                            grepl("igf",Y) ~ "IGF-1"),
          Outcome=factor(Outcome, levels=c("LAZ", "WLZ", "IGF-1")))
 
 
 tableau10 <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", 
                "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC")
 p4 <- ggplot(plotdf3, aes(x=y, y=point.diff)) + 
   geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=x, shape=x),
                   position = position_dodge2(width = 0.5),
                   size = 1, show.legend = T) +
   facet_grid(vars(Outcome), scales = "free") + 
   labs(y = "Adjusted difference in mean child growth status outcome\nbetween 25th and 75th percentile of maternal exposure", 
        x="Child age") +
   geom_hline(yintercept = 0, linetype="dashed") +
   guides(color="none")+
   scale_color_manual(values = tableau10[c(6:9)]) + 
   theme_ki() +
   theme(legend.position = "bottom",
         plot.title = element_text(hjust = 0),
         axis.title.x = element_text(size=11),
         axis.title.y = element_text(size=10),
         strip.text = element_text(hjust=0.5, size=10),
         strip.placement = "outside",
         axis.text.y = element_text(hjust = 1, size=9),
         panel.spacing = unit(0.5, "lines")) +
   guides(shape=guide_legend(title="Maternal exposure"),
          color=guide_legend(title="Maternal exposure"))
 p4
 
 ggsave(p4, filename=here("figures/pregnancy-stress-growth-figure4.jpg"), width=6, height=5)
 
#-------------------------------------------------------------------------------
# Old code
#-------------------------------------------------------------------------------

# d1 <- d_for_plot(c("CRP", "AGP", "plasma 13-cytokine sum score"), 
#                  c("Child LAZ 3-Month", "Child LAZ 14-Month", "Child LAZ 28-month"), 
#                  c("logCRP", "logAGP", "sumscore_t0_mom_Z"),
#                  c("laz_t1", "laz_t2", "laz_t3"),
#                  H2_spline, H2_quartiles) 
# 
# #d1$x <- factor(d1$x,levels=c("Vitamin D", "Vitamin D deficiency", "Ln RBP", "Vitamin A deficiency","Ln ferritin", "Ln sTfR", "Iron deficiency"))
# #d1$x <- factor(d1$x, levels=rev(levels(d1$x)))
# d1$y <- factor(d1$y)
# d1$`Time of outcome measurement` <- factor(ifelse(grepl("t3", d1$Y), "Age 28 months", "Age 14 months"))
# 
# p <- ggplot(d1, aes(x=x, y=point.diff)) + 
#   geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=X, shape=`Time of outcome measurement`),
#                   position = position_dodge2(width = 0.5),
#                   size = 1, show.legend = T) +
#   facet_grid(rows=vars(y), scales = "free") + 
#   labs(y = "Adjusted difference in mean child immune status outcome\nbetween 25th and 75th percentile of maternal exposure", 
#        x="CRP, AGP, plasma 13-cytokine sum score in first & second trimester of pregnancy") +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   scale_shape_manual(breaks=c("Age 14 months","Age 28 months"), values=c(21,16)) +
#   guides(color="none")+
#   scale_color_manual(values = tableau10[c(7:1)]) + 
#   theme_ki() +
#   theme(plot.title = element_text(hjust = 0),
#         axis.title.x = element_text(size=11),
#         strip.text = element_text(hjust=0.5, size=10),
#         strip.placement = "outside",
#         axis.text.y = element_text(hjust = 1, size=9),
#         panel.spacing = unit(0.5, "lines"),
#         legend.position = "bottom")      
# p
# 
# 
# p %>% ggsave(filename="figures/hypo2.jpg", width=10, height=7)
# 
# 
# d1 <- d_for_plot(c("Vitamin D", "sTfR", "RBP"), 
#                  c("Child LAZ 3-Month", "Child LAZ 14-Month", "Child LAZ 28-month"), 
#                  c("vitD_nmol_per_L", "logSTFR_inf", "logRBP_inf"),
#                  c("laz_t1", "laz_t2", "laz_t3"),
#                  H3_spline, H3_quartiles) 
# 
# #d1$x <- factor(d1$x,levels=c("Vitamin D", "Vitamin D deficiency", "Ln RBP", "Vitamin A deficiency","Ln ferritin", "Ln sTfR", "Iron deficiency"))
# #d1$x <- factor(d1$x, levels=rev(levels(d1$x)))
# d1$y <- factor(d1$y)
# d1$`Time of outcome measurement` <- factor(ifelse(grepl("t3", d1$Y), "Age 28 months", "Age 14 months"))
# 
# p <- ggplot(d1, aes(x=x, y=point.diff)) + 
#   geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=X, shape=`Time of outcome measurement`),
#                   position = position_dodge2(width = 0.5),
#                   size = 1, show.legend = T) +
#   facet_grid(rows=vars(y), scales = "free") + 
#   labs(y = "Adjusted difference in mean child immune status outcome\nbetween 25th and 75th percentile of maternal exposure", 
#        x="RBP, sTfR, and Vitamin D in first & second trimester of pregnancy") +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   scale_shape_manual(breaks=c("Age 14 months","Age 28 months"), values=c(21,16)) +
#   guides(color="none")+
#   scale_color_manual(values = tableau10[c(7:1)]) + 
#   theme_ki() +
#   theme(plot.title = element_text(hjust = 0),
#         axis.title.x = element_text(size=11),
#         strip.text = element_text(hjust=0.5, size=10),
#         strip.placement = "outside",
#         axis.text.y = element_text(hjust = 1, size=9),
#         panel.spacing = unit(0.5, "lines"),
#         legend.position = "bottom")      
# p
# 
# 
# p %>% ggsave(filename="figures/hypo3.jpg", width=10, height=7)
# 
# 
# 
# d1 <- d_for_plot(c("ln Maternal Estriol"), 
#                  c("Child LAZ 3-Month", "Child LAZ 14-Month", "Child LAZ 28-month"), 
#                  c("ln_preg_estri"),
#                  c("laz_t1", "laz_t2", "laz_t3"),
#                  H4_spline, H4_quartiles) 
# 
# #d1$x <- factor(d1$x,levels=c("Vitamin D", "Vitamin D deficiency", "Ln RBP", "Vitamin A deficiency","Ln ferritin", "Ln sTfR", "Iron deficiency"))
# #d1$x <- factor(d1$x, levels=rev(levels(d1$x)))
# d1$y <- factor(d1$y)
# d1$`Time of outcome measurement` <- factor(ifelse(grepl("t3", d1$Y), "Age 28 months", "Age 14 months"))
# 
# p <- ggplot(d1, aes(x=x, y=point.diff)) + 
#   geom_pointrange(aes(ymin=lb.diff , ymax=ub.diff, color=X, shape=`Time of outcome measurement`),
#                   position = position_dodge2(width = 0.5),
#                   size = 1, show.legend = T) +
#   facet_grid(rows=vars(y), scales = "free") + 
#   labs(y = "Adjusted difference in mean child immune status outcome\nbetween 25th and 75th percentile of maternal exposure", 
#        x="Maternal Estriol in first & second trimester of pregnancy") +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   scale_shape_manual(breaks=c("Age 14 months","Age 28 months"), values=c(21,16)) +
#   guides(color="none")+
#   scale_color_manual(values = tableau10[c(7:1)]) + 
#   theme_ki() +
#   theme(plot.title = element_text(hjust = 0),
#         axis.title.x = element_text(size=11),
#         strip.text = element_text(hjust=0.5, size=10),
#         strip.placement = "outside",
#         axis.text.y = element_text(hjust = 1, size=9),
#         panel.spacing = unit(0.5, "lines"),
#         legend.position = "bottom")      
# p
# 
# 
# p %>% ggsave(filename="figures/hypo4.jpg", width=10, height=7)
# 
# 
# 
# d_for_plot <- function(x_name, y_name, x_var, y_var, spline, quart){
#   d <- NULL
#   for (i in 1: length(x_var)) {
#     for (j in 1:length(y_var)){
#       exists <- (quart%>%filter(X==x_var[i], Y==y_var[j]) %>% nrow()) != 0
#       print(exists)
#       if (exists){
#         a <- spline%>%filter(Xvar==x_var[i], Yvar==y_var[j])
#         b <- quart%>%filter(X==x_var[i], Y==y_var[j])%>%select(q1, q3)
#         print(a)
#         print(b)
#         new <- data.frame(x=x_name[i], y=y_name[j], spline%>%filter(Xvar==x_var[i], Yvar==y_var[j]), quart%>%filter(X==x_var[i], Y==y_var[j])%>%select(q1, q3))
#         d <- rbind(d, new)
#       }
#     }
#   }
#   print(d)
#   return(d)
# }
# 
# d1 <- d_for_plot(c("Vitamin D", "Vitamin D deficiency", "Ln RBP", "Vitamin A deficiency","Ln ferritin", "Ln sTfR", "Iron deficiency"),
#                  c("Ln AGP", "Ln CRP", "Ln IFN-y", "Sum score of 13\ncytokines",
#                    "Ln AGP", "Ln CRP", "Ln IFN-y", "Sum score of 13\ncytokines"), 
#                  c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def"),   
#                  c("t2_ln_agp", "t2_ln_crp", "t2_ln_ifn", "sumscore_t2_Z",
#                    "t3_ln_agp", "t3_ln_crp", "t3_ln_ifn","sumscore_t3_Z"),
#                  H12_spline, H12_quartiles)
# 
# d1$x <- factor(d1$x,levels=c("Vitamin D", "Vitamin D deficiency", "Ln RBP", "Vitamin A deficiency","Ln ferritin", "Ln sTfR", "Iron deficiency"))
# #d1$x <- factor(d1$x, levels=rev(levels(d1$x)))
# d1$y <- factor(d1$y)
# 
# Xvar = c("vitD_nmol_per_L", "vit_D_def", "logRBP_inf",  "vit_A_def", "logFERR_inf", "logSTFR_inf", "iron_def")
# d1 <- d1 %>% filter(!grepl("def", X))
# # 
# # colors_sub <- colors %>% filter(x %in% d1$x)
# 
# t2 <- d1 %>% filter(grepl("t2", Y)) %>% ggplot(aes(x=X))+
#   geom_smooth(aes(y = 'fit', color=x), se = F) +
#   geom_vline(aes(xintercept=q1), size=.5, color="grey30", linetype="dashed") +
#   geom_vline(aes(xintercept=q3), size=.5, color="grey30", linetype="dashed") +
#   geom_point(aes(y=Y), alpha=0.5) +
#   geom_ribbon(aes(ymin='lwrS', ymax='uprS', fill=x, color=x), alpha=0.5) + 
#   scale_colour_manual(values=tableau10[c(1,5,6,4)]) + 
#   scale_fill_manual(values=tableau10[c(1,5,6,4)]) + 
#   xlab(" ") + ylab(element_blank()) + 
#   facet_grid(cols = vars(x), rows = vars(y), scales = "free") +
#   theme_ki() +
#   theme(strip.text.x = element_text(size=10),
#         strip.text.y = element_text(size=10),
#         panel.spacing = unit(.3, "lines"))      
# t2
# 
# t3 <- d1 %>% filter(grepl("t3", Y)) %>% ggplot(aes(x=X))+
#   geom_smooth(aes(y = 'fit', color=X), se = F) +
#   geom_vline(aes(xintercept=q1), size=.5, color="grey30", linetype="dashed") +
#   geom_vline(aes(xintercept=q3), size=.5, color="grey30", linetype="dashed") +
#   geom_point(aes(y=Y), alpha=0.5) +
#   geom_ribbon(aes(ymin='lwrS', ymax='uprS', fill=X, color=X), alpha=0.5) + 
#   scale_colour_manual(values=tableau10[c(1,5,6,4)]) + 
#   scale_fill_manual(values=tableau10[c(1,5,6,4)]) + 
#   xlab(" ") + ylab(element_blank()) + 
#   facet_grid(cols = vars(x), rows = vars(y), scales="free") +
#   theme_ki() +
#   theme(strip.text.x = element_text(size=10),
#         strip.text.y = element_text(hjust=0.5, size=10),
#         panel.spacing = unit(.3, "lines"))      
# 
# t3
# H1_quartiles
# 
# t2 %>% ggsave(filename="figures/adj-splines-t2.jpg", width = 8, height = 9)
# t3 %>% ggsave(filename="figures/adj-splines-t3.jpg", width = 8, height = 9)
