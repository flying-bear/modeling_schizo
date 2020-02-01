library(tidyverse)
library(corrplot)
library(Hmisc)

coh_all_measures <- read_csv('all_measures_renamed.csv')
sc <- function(x){scale(x, center=TRUE, scale=TRUE)}
coh_all_measures[3:18] <- apply(coh_all_measures[3:18], 2, sc) # z-scale all measures

res2 <- rcorr(as.matrix(coh_all_measures[3:18]))
corrplot(res2$r, type="upper", order="alphabet", 
                       p.mat = res2$P, sig.level = 0.05, insig = "blank",
                       tl.col = "black", tl.cex = 0.75, method="number", number.cex = 0.65)
