library(tidyverse)

coh_all_measures <- read_csv('all_measures.csv')
sc <- function(x){scale(x, center=TRUE, scale=TRUE)}
coh_all_measures[3:19] <- apply(coh_all_measures[3:19], 2, sc) # z-scale all measures


### my 4-point lcoh vs standard 5-point lcoh
qqnorm(coh_all_measures$av_lcoh_1)
qqline(coh_all_measures$av_lcoh_1)
shapiro.test(coh_all_measures$av_lcoh_1) # no, p = 0.03195

qqnorm(coh_all_measures$av_lcoh4_1)
qqline(coh_all_measures$av_lcoh4_1)
shapiro.test(coh_all_measures$av_lcoh4_1) # yes, p = 0.9133

cor.test(coh_all_measures$av_lcoh_1, coh_all_measures$av_lcoh4_1, method = 'spearman')
# ********************************************************************
# Spearman's rank correlation rho
# data:  coh_all_measures$av_lcoh_1 and coh_all_measures$av_lcoh4_1
# S = 228, p-value = 5.089e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho  = 0.8 


### comp_gcoh vs comp_control_gcoh
cor.test(coh_all_measures$comp_glob_coh, coh_all_measures$comp_control_glob_coh, method = 'spearman')
# ***************************************************************************
# Spearman's rank correlation rho
# data:  coh_all_measures$comp_glob_coh and coh_all_measures$comp_control_glob_coh
# S = 362, p-value = 0.001728
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho  = 0.6824561 

### lcoh vs gcoh
qqnorm(coh_all_measures$av_gcoh_1) 
qqline(coh_all_measures$av_gcoh_1)
shapiro.test(coh_all_measures$av_gcoh_1) # yes, p = 0.4915

cor.test(coh_all_measures$av_lcoh_1, coh_all_measures$av_gcoh_1, method = 'spearman')
# *******************************************************************************
# Spearman's rank correlation rho
# data:  coh_all_measures$av_lcoh_1 and coh_all_measures$av_gcoh_1
# S = 244, p-value = 9.607e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho = 0.7859649 




### ### manual vs computed
### standard lcoh vs comp_lcoh
qqnorm(coh_all_measures$comp_av_loc_coh)
qqline(coh_all_measures$comp_av_loc_coh)
shapiro.test(coh_all_measures$comp_av_loc_coh) # no, p = 1.094e-05

cor.test(coh_all_measures$av_lcoh_1, coh_all_measures$comp_av_loc_coh, method = 'spearman')
# Spearman's rank correlation rho
# data:  coh_all_measures$av_lcoh_1 and coh_all_measures$comp_av_loc_coh
# S = 1010, p-value = 0.6413
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho = 0.1140351

### my lcoh vs comp_lcoh
cor.test(coh_all_measures$av_lcoh4_1, coh_all_measures$comp_av_loc_coh, method = 'spearman')
# Spearman's rank correlation rho
# data:  coh_all_measures$av_lcoh4_1 and coh_all_measures$comp_av_loc_coh
# S = 1096, p-value = 0.8768
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho = 0.03859649

### gcoh vs comp_gcoh
qqnorm(coh_all_measures$comp_glob_coh)
qqline(coh_all_measures$comp_glob_coh)
shapiro.test(coh_all_measures$comp_glob_coh) # no, p = 0.003201

cor.test(coh_all_measures$av_gcoh_1, coh_all_measures$comp_glob_coh, method = 'spearman')
# Spearman's rank correlation rho
# data:  coh_all_measures$av_gcoh_1 and coh_all_measures$comp_glob_coh
# S = 1220, p-value = 0.7757
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho = -0.07017544

### gcoh vs comp_control_gcoh
qqnorm(coh_all_measures$comp_control_glob_coh)
qqline(coh_all_measures$comp_control_glob_coh)
shapiro.test(coh_all_measures$comp_control_glob_coh) # no, p = 1.942e-06

cor.test(coh_all_measures$av_gcoh_1, coh_all_measures$comp_control_glob_coh, method = 'spearman')
# Spearman's rank correlation
# data:  coh_all_measures$av_gcoh_1 and coh_all_measures$comp_control_glob_coh
# S = 1050, p-value = 0.7482
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho = 0.07894737

### violations vs comp_violations
qqnorm(coh_all_measures$compl_viol_per_utt_reverse)
qqline(coh_all_measures$compl_viol_per_utt_reverse)
shapiro.test(coh_all_measures$compl_viol_per_utt_reverse) # no, p = 0.002367

qqnorm(coh_all_measures$comp_compl_viol_per_utt)
qqline(coh_all_measures$comp_compl_viol_per_utt)
shapiro.test(coh_all_measures$comp_compl_viol_per_utt) # no, p = 0.0001329

cor.test(coh_all_measures$compl_viol_per_utt_reverse, 
         coh_all_measures$comp_compl_viol_per_utt, method = 'spearman') #TIES AAA
# ****************************************************************************
# Pearson's product-moment correlation
# data:  coh_all_measures$compl_viol_per_utt_reverse and coh_all_measures$comp_compl_viol_per_utt
# t = 3.6277, df = 17, p-value = 0.00208
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.294813 0.857497
# sample estimates:
# cor = 0.6605678

### comment vs meta-comment
shapiro.test(coh_all_measures$av_comment) # normal, p-value = 0.1876 
qqnorm(coh_all_measures$av_comment)
shapiro.test(coh_all_measures$av_meta_comment) # not normal, p-value = 0.02685
qqnorm(coh_all_measures$av_meta_comment)
cor.test(coh_all_measures$av_comment, 
         coh_all_measures$av_meta_comment, method = 'spearman') #TIES AAA
# Spearman's rank correlation rho
# data:  coh_all_measures$av_comment and coh_all_measures$av_meta_comment
# S = 657.83, p-value = 0.07119
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.4229591 

### some t-tests

shapiro.test(coh_all_measures$av_switch) # normal, p-value = 0.6922
hist(coh_all_measures$av_switch)
qqnorm(coh_all_measures$av_switch)
boxplot(av_switch~diagnosis, coh_all_measures)
t.test(av_switch~diagnosis, coh_all_measures) #nope
# Welch Two Sample t-test
# data:  av_switch by diagnosis
# t = 1.6908, df = 17, p-value = 0.1091
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01503158  0.13635178
# sample estimates:
#   mean in group control   mean in group shizo 
# 0.1661413             0.1054812


shapiro.test(coh_all_measures$av_comment) # normal, p-value = 0.1876
hist(coh_all_measures$av_comment)
qqnorm(coh_all_measures$av_comment)
boxplot(av_comment~diagnosis, coh_all_measures)
t.test(av_comment~diagnosis, coh_all_measures)
# ***********************************************************************************
# Welch Two Sample t-test
# data:  av_comment by diagnosis
# t = 3.9277, df = 14.505, p-value = 0.001425
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.08752261 0.29659489
# sample estimates:
#   mean in group control   mean in group shizo 
#   0.26436147              0.07230272 

shapiro.test(coh_all_measures$av_meta_comment) # not normal, p-value = 0.02685
hist(coh_all_measures$av_meta_comment)
qqnorm(coh_all_measures$av_meta_comment)
boxplot(av_meta_comment~diagnosis, coh_all_measures) # identical

shapiro.test(coh_all_measures$compl_viol_per_utt_reverse) # not normal, p-value = 0.002367
hist(coh_all_measures$compl_viol_per_utt_reverse)
qqnorm(coh_all_measures$compl_viol_per_utt_reverse)
boxplot(compl_viol_per_utt_reverse~diagnosis, coh_all_measures)
wilcox.test(compl_viol_per_utt_reverse~diagnosis, coh_all_measures) #ties
# Wilcoxon rank sum test with continuity correction
# data:  compl_viol_per_utt_reverse by diagnosis
# W = 63, p-value = 0.1423
# alternative hypothesis: true location shift is not equal to 0