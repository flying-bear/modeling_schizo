library(tidyverse)
library(cluster)
library(languageR)
library(factoextra)
library(FactoMineR)
library(ggfortify)

coh_all_measures <- read_csv('all_measures.csv')

coh_all_measures %>% 
  mutate(diagnosis = ifelse(diagnosis == 1, 'shizo', 'control')) -> coh_all_measures

no_nutt_no_diagnosis <- coh_all_measures[4:19]
row.names(no_nutt_no_diagnosis) <- coh_all_measures[[1]]

manual <- as.factor(c('manual measure','manual measure','manual measure','manual measure','manual measure',
                     'manual measure','manual measure','manual measure','manual measure','manual measure',
                     'manual measure','computed measure','computed measure','computed measure',
                     'computed measure','computed measure','computed measure'))

color_diagnosis <- ifelse(coh_all_measures$diagnosis == 'shizo', 'blue','red')

no_diagnosis <- as.data.frame(coh_all_measures[3:19])
row.names(no_diagnosis) <- coh_all_measures[[1]]

diagnosis <- as.factor(c('schizo','schizo','schizo','schizo','schizo',
                         'schizo','schizo','schizo','schizo',
                         'control','control','control','control','control',
                         'control','control','control','control','control'))

### CA using ggfortify
autoplot(prcomp(no_diagnosis), data = coh_all_measures, 
         colour = 'diagnosis', label = TRUE, label.size = 5,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 5)+ggtitle('Principal Correspondence Analysis using ggfortify, biplot of manual and computer measures of coherence', )

### CA using factoextra & FactoMineR
coh_ca <- CA(no_diagnosis, graph = FALSE)
fviz_ca_biplot(coh_ca, repel=TRUE, addlabels = TRUE, 
               col.row=diagnosis, invizible = 'col', # col.col = manual, 
               title = 'Principal Correspondence Analysis using factoextra & FactoMineR, biplot of manual and computer measures of coherence')+theme(plot.title = element_text(size = 12))

fviz_screeplot(coh_ca, addlabels = TRUE)
fviz_ca_col(coh_ca, col.col = manual, repel=TRUE,
            title = 'Principal Correspondence Analysis using factoextra & FactoMineR, biplot of manual and computer measures of coherence')+theme(plot.title = element_text(size = 12))
fviz_ca_row(coh_ca, col.row = diagnosis, repel=TRUE, addlabels = TRUE,
            title = 'Principal Correspondence Analysis using factoextra & FactoMineR, biplot of manual and computer measures of coherence')+theme(plot.title = element_text(size = 12))+geom_abline(mapping = NULL, data = NULL, intercept = 0.01, slope = -0.4, color = 'blue', linetype="dashed")



### CA using languageR
corr <- corres.fnc(no_diagnosis)
plot(corr, rlabels=coh_all_measures[[1]], rcex=0.75, rcol = color_diagnosis, ccol = 'grey')
title('Principal Correspondence Analysis using LanguageR, biplot of manual and computer measures of coherence')

### tree clustering
coh_dist <- dist(no_nutt_no_diagnosis)
plot(hclust(coh_dist, method = "ward.D2") , hang = -1)

### K-means
autoplot(kmeans(no_nutt_no_diagnosis, 2), data = coh_all_measures, size = 2, label = TRUE, label.size = 5)+ggtitle('Principal Correspondence Analysis for K-means clustering')
