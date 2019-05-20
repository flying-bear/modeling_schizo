library(irr)
library(tidyverse)

interrator_violations <- read_csv('interrator_agreement_violations.csv')
max(interrator_violations[2:6]) ### 5
min(interrator_violations[2:6]) ### 0
# Subjects = 19 
# Raters = 5 
agree(interrator_violations[2:6], tolerance = 0)  # %-agree = 47.4 
agree(interrator_violations[2:6], tolerance = 1)  # %-agree = 73.7 
agree(interrator_violations[2:6], tolerance = 2)  # %-agree = 94.7


sc <- function(x){ scale(x, center=TRUE, scale=TRUE)}
interrator_violations[2:6] <- apply(interrator_violations[2:6], 2, sc) ### Z-scaled
max(interrator_violations[2:6]) ### 2.592975
min(interrator_violations[2:6]) ### -0.8777664
# Subjects = 19 
# Raters = 5 
agree(interrator_violations[2:6], tolerance = 0.25)   # 36.8% at 0.25-point tolerance 
agree(interrator_violations[2:6], tolerance = 0.5)    # 47.4% at 0.5-point tolerance
agree(interrator_violations[2:6], tolerance = 1)      # 78.9% at 1-point tolerance
agree(interrator_violations[2:6], tolerance = 1.5)    # 94.7% at 1.5-point tolerance
agree(interrator_violations[2:6], tolerance = 2)      # 100% at 2-point tolerance
