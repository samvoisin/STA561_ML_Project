###############################################################################
######## Primary script for preparing data and fitting the Neural Nets ########
###############################################################################
###############################################################################
############## WARNING: SCRIPTS PARALLELIZED OVER MANY CORES ##################
############## ENSURE RESOURCES ARE AVAILABLE BEFORE RUNNING ##################
###############################################################################

library(here)


source("./Scripts/clean_emg_data.R")
source("./Scripts/sep_gestures.R")
source("./Scripts/PCA.R")
source("./Scripts/pad_matrices.R")
