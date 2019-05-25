library(tidyverse)
library(ggrepel)
library(PerformanceAnalytics)
library(corrplot)
library(Hmisc)

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
# S = 1258, p-value = 0.6728
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.1035088 

### my lcoh vs comp_lcoh
cor.test(coh_all_measures$av_lcoh4_1, coh_all_measures$comp_av_loc_coh, method = 'spearman')
# Spearman's rank correlation rho
# data:  coh_all_measures$av_lcoh4_1 and coh_all_measures$comp_av_loc_coh
# S = 1096, p-value = 0.8768
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho = 0.03859649

cor.test(coh_all_measures$av_lcoh4_1, coh_all_measures$comp_av_cum_semi_loc_coh, method = 'spearman')
# Spearman's rank correlation rho
# 
# data:  coh_all_measures$av_lcoh4_1 and coh_all_measures$comp_av_cum_semi_loc_coh
# S = 1224, p-value = 0.7647
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#         rho 
# -0.07368421 

cor.test(coh_all_measures$av_lcoh_1, coh_all_measures$comp_av_cum_semi_loc_coh, method = 'spearman')
# Spearman's rank correlation rho
# data:  coh_all_measures$av_lcoh_1 and coh_all_measures$comp_av_cum_semi_loc_coh
# S = 1138, p-value = 0.9971
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.001754386 

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

coh_all_measures %>% 
  ggplot(aes(compl_viol_per_utt_reverse, comp_compl_viol_per_utt, label = ID))+
  geom_smooth(method='lm', linetype = 'dashed', color = 'darkgrey', se = FALSE)+
  geom_text_repel(aes(color = diagnosis))+
  geom_point(aes(color = diagnosis))+
  ggtitle('Correlation between manual and computational measure of violations of completeness',
          subtitle = 'rho = 0.57, p = 0.01')+
  labs(x = 'reversed manual metric of violations of completeness per utterance',
       y = 'computational metric of violations of completeness per utterance')

cor.test(coh_all_measures$compl_viol_per_utt_reverse, 
         coh_all_measures$comp_compl_viol_per_utt, method = 'spearman') #TIES AAA
# ****************************************************************************
# 	Spearman's rank correlation rho
# data:  coh_all_measures$compl_viol_per_utt_reverse and coh_all_measures$comp_compl_viol_per_utt
# S = 486.87, p-value = 0.01035
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.5729172  


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

### lcoh vs semi-lcoh
cor.test(coh_all_measures$av_lcoh_1, 
         coh_all_measures$comp_av_cum_semi_loc_coh, method = 'pearson')
# Pearson's product-moment correlation
# data:  coh_all_measures$av_lcoh_1 and coh_all_measures$comp_av_cum_semi_loc_coh
# t = 3.7054, df = 17, p-value = 0.001757
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.3076160 0.8611795
# sample estimates:
# cor 
# 0.6684308


### some t-tests

shapiro.test(coh_all_measures$comp_last_cum_semi_loc_coh) # normal, p-value = 0.07371
hist(coh_all_measures$comp_last_cum_semi_loc_coh)
qqnorm(coh_all_measures$comp_last_cum_semi_loc_coh)
boxplot(comp_last_cum_semi_loc_coh~diagnosis, coh_all_measures)
t.test(comp_last_cum_semi_loc_coh~diagnosis, coh_all_measures)
# Welch Two Sample t-test
# data:  comp_last_cum_semi_loc_coh by diagnosis
# t = 1.4869, df = 16.634, p-value = 0.1558
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.02466419  0.14174040
# sample estimates:
#   mean in group control   mean in group shizo 
# 0.8939322             0.8353941 


shapiro.test(coh_all_measures$comp_av_cum_semi_loc_coh) # not normal, p-value = 2.236e-06
hist(coh_all_measures$comp_av_cum_semi_loc_coh)
qqnorm(coh_all_measures$comp_av_cum_semi_loc_coh)
boxplot(comp_av_cum_semi_loc_coh~diagnosis, coh_all_measures)
wilcox.test(comp_av_cum_semi_loc_coh~diagnosis, coh_all_measures)
# Wilcoxon rank sum test
# data:  comp_av_cum_semi_loc_coh by diagnosis
# W = 68, p-value = 0.06525
# alternative hypothesis: true location shift is not equal to 0

shapiro.test(coh_all_measures$comp_av_loc_coh) # not normal, p-value = 0.01601
hist(coh_all_measures$comp_av_loc_coh)
qqnorm(coh_all_measures$comp_av_loc_coh)
boxplot(comp_av_loc_coh~diagnosis, coh_all_measures)
wilcox.test(comp_av_loc_coh~diagnosis, coh_all_measures)
# Wilcoxon rank sum test
# data:  comp_av_loc_coh by diagnosis
# W = 64, p-value = 0.1333
# alternative hypothesis: true location shift is not equal to 0

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

boxplot(av_lcoh4_1~diagnosis, coh_all_measures) #nope
wilcox.test(av_lcoh4_1~diagnosis, coh_all_measures)
# Wilcoxon rank sum test
# data:  av_lcoh4_1 by diagnosis
# W = 48, p-value = 0.8421

boxplot(comp_glob_coh~diagnosis, coh_all_measures) #nope
wilcox.test(comp_glob_coh~diagnosis, coh_all_measures)
# Wilcoxon rank sum test
# data:  comp_glob_coh by diagnosis
# W = 48, p-value = 0.8421
# alternative hypothesis: true location shift is not equal to 0

boxplot(comp_control_glob_coh~diagnosis, coh_all_measures) #nope
wilcox.test(comp_control_glob_coh~diagnosis, coh_all_measures)
# Wilcoxon rank sum test
# data:  comp_control_glob_coh by diagnosis
# W = 29, p-value = 0.211
# alternative hypothesis: true location shift is not equal to 0

shapiro.test(coh_all_measures$av_comment) # normal, p-value = 0.1876
hist(coh_all_measures$av_comment)
qqnorm(coh_all_measures$av_comment)
boxplot(av_comment~diagnosis, coh_all_measures)
title('comment per utterance score, t = 3.9, p = 0.001425')
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
wilcox.test(av_meta_comment~diagnosis, coh_all_measures)
# Wilcoxon rank sum test with continuity correction
# data:  av_meta_comment by diagnosis
# W = 46, p-value = 0.9674
# alternative hypothesis: true location shift is not equal to 0

shapiro.test(coh_all_measures$av_story) # normal, p-value = 0.9891
hist(coh_all_measures$av_story)
qqnorm(coh_all_measures$av_story)
boxplot(av_story~diagnosis, coh_all_measures) # identical
t.test(av_story~diagnosis, coh_all_measures)
# Welch Two Sample t-test
# data:  av_story by diagnosis
# t = -0.43185, df = 16.95, p-value = 0.6713
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1493721  0.0986225
# sample estimates:
#   mean in group control   mean in group shizo 
# 0.8158460             0.8412208 

shapiro.test(coh_all_measures$av_time) # normal, p-value = 0.835
hist(coh_all_measures$av_time)
qqnorm(coh_all_measures$av_time)
boxplot(av_time~diagnosis, coh_all_measures) # identical
t.test(av_time~diagnosis, coh_all_measures)
# Welch Two Sample t-test
# data:  av_time by diagnosis
# t = 0.030213, df = 16.156, p-value = 0.9763
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.1211131  0.1246180
# sample estimates:
#   mean in group control   mean in group shizo 
# 0.7737000             0.7719475 

p = c(0.8421, 0.6713, 0.9763, 0.001425, 0.9674, 0.1091)
p.adjust(p, n = length(p))

shapiro.test(coh_all_measures$comp_compl_viol_per_utt) # not normal, p-value = 0.002367
hist(coh_all_measures$compl_viol_per_utt_reverse)
qqnorm(coh_all_measures$compl_viol_per_utt_reverse)
boxplot(compl_viol_per_utt_reverse~diagnosis, coh_all_measures)
title('reversed completeness violation per utterance score')
wilcox.test(compl_viol_per_utt_reverse~diagnosis, coh_all_measures) #ties
# Wilcoxon rank sum test with continuity correction
# data:  compl_viol_per_utt_reverse by diagnosis
# W = 63, p-value = 0.1423
# alternative hypothesis: true location shift is not equal to 0

coh_all_measures %>% 
  mutate(num_utt = as.numeric(num_utt)) -> coh_all_measures


chart.Correlation(coh_all_measures[3:19], pch=10, method = 'spearman', histogram = FALSE)

# Insignificant correlation are left blank
res2 <- rcorr(as.matrix(coh_all_measures[3:19]))
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank",
         tl.col = "black", tl.cex = 0.75, method="number", number.cex = 0.65)
