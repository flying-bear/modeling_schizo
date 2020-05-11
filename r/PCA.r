library(tidyverse)
library(factoextra)
library(FactoMineR)
library(languageR)
library(ggfortify)
library(plyr)
to_analyze <- read_csv('metrics_child.csv')
meta <- read_csv('All_psych.csv')
to_analyze <- left_join(meta, to_analyze)
to_analyze <- column_to_rownames(to_analyze, var = "ID")
to_analyze$diagnosis = revalue(as.factor(meta$diagnosis), c('1'="schizophrenia", '0'="control", '2'='schizoaffective disorder', '3'='control with schizo-spectrum tendencies'))
only_analyze = select(to_analyze, -diagnosis)



##### BERT
bert_names <- names(data.frame(only_analyze))[grep("bert", names(data.frame(only_analyze)))]
add_names <- c('PANSS_Total', 'PANSS_O', 'PANSS_P', 'PANSS_N', 'TD', 'age', 'education')
bert <- select(only_analyze, c(bert_names, add_names))
bert %>% 
  na.omit() %>% 
  scale(center = TRUE, scale = TRUE) -> bert

dd <- na.omit(select(to_analyze, c(bert_names, add_names, 'diagnosis') ))
### CA using factoextra & FactoMineR
autoplot(prcomp(bert), data = dd, 
         colour = 'diagnosis', label = TRUE, label.size = 5,
         loadings = TRUE, loadings.colour = 'blue', loadings.label.repel=T,
         loadings.label = TRUE, loadings.label.size = 5)+ggtitle('Principal Correspondence Analysis using ggfortify, biplot on child task BERT with PANSS')
ggsave("child_PANSS_BERT.jpeg", width = 50, height = 27, units = "cm")

bert <- select(only_analyze, bert_names)
bert %>% 
  na.omit() %>% 
  scale(center = TRUE, scale = TRUE) -> bert

dd <- na.omit(select(to_analyze, c(bert_names, 'diagnosis') ))
### CA using factoextra & FactoMineR
autoplot(prcomp(bert), data = dd, 
         colour = 'diagnosis', label = TRUE, label.size = 5,
         loadings = TRUE, loadings.colour = 'blue', loadings.label.repel=T,
         loadings.label = TRUE, loadings.label.size = 5)+ggtitle('Principal Correspondence Analysis using ggfortify, biplot on child task BERT')
ggsave("child_BERT.jpeg", width = 50, height = 27, units = "cm")


##### SIF
sif_names <- names(data.frame(only_analyze))[grep("sif", names(data.frame(only_analyze)))]
sif <- select(only_analyze, c(sif_names, add_names))
sif %>% 
  na.omit() %>% 
  scale(center = TRUE, scale = TRUE) -> sif

dd <- na.omit(select(to_analyze, c(sif_names, add_names, 'diagnosis') ))
### CA using factoextra & FactoMineR
autoplot(prcomp(sif), data = dd, 
         colour = 'diagnosis', label = TRUE, label.size = 5,
         loadings = TRUE, loadings.colour = 'blue', loadings.label.repel=T,
         loadings.label = TRUE, loadings.label.size = 5)+ggtitle('Principal Correspondence Analysis using ggfortify, biplot on child task SIF with PANSS')
ggsave("child_PANSS_SIF.jpeg", width = 50, height = 27, units = "cm")

sif <- select(only_analyze, sif_names)
sif %>% 
  na.omit() %>% 
  scale(center = TRUE, scale = TRUE) -> sif

dd <- na.omit(select(to_analyze, c(sif_names, 'diagnosis') ))
### CA using factoextra & FactoMineR
autoplot(prcomp(sif), data = dd, 
         colour = 'diagnosis', label = TRUE, label.size = 5,
         loadings = TRUE, loadings.colour = 'blue', loadings.label.repel=T,
         loadings.label = TRUE, loadings.label.size = 5)+ggtitle('Principal Correspondence Analysis using ggfortify, biplot on child task SIF')
ggsave("child_SIF.jpeg", width = 50, height = 27, units = "cm")


##### ELMO
elmo_names <- names(data.frame(only_analyze))[grep("elmo", names(data.frame(only_analyze)))]
elmo <- select(only_analyze, c(elmo_names, add_names))
elmo %>% 
  na.omit() %>% 
  scale(center = TRUE, scale = TRUE) -> elmo

dd <- na.omit(select(to_analyze, c(elmo_names, add_names, 'diagnosis') ))
### CA using factoextra & FactoMineR
autoplot(prcomp(elmo), data = dd, 
         colour = 'diagnosis', label = TRUE, label.size = 5,
         loadings = TRUE, loadings.colour = 'blue', loadings.label.repel=T,
         loadings.label = TRUE, loadings.label.size = 5)+ggtitle('Principal Correspondence Analysis using ggfortify, biplot on child task ELMo with PANSS')
ggsave("child_PANSS_ELMo.jpeg", width = 50, height = 27, units = "cm")

elmo <- select(only_analyze, elmo_names)
elmo %>% 
  na.omit() %>% 
  scale(center = TRUE, scale = TRUE) -> elmo

dd <- na.omit(select(to_analyze, c(elmo_names, 'diagnosis') ))
### CA using factoextra & FactoMineR
autoplot(prcomp(elmo), data = dd, 
         colour = 'diagnosis', label = TRUE, label.size = 5,
         loadings = TRUE, loadings.colour = 'blue', loadings.label.repel=T,
         loadings.label = TRUE, loadings.label.size = 5)+ggtitle('Principal Correspondence Analysis using ggfortify, biplot on child task ELMo')
ggsave("child_ELMo.jpeg", width = 50, height = 27, units = "cm")
