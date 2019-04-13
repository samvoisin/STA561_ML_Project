###############################################################################
######### Add padding to ensure training data is of the same dimension ########
############# first check all files to find larges number of rows #############
###############################################################################
##############  WARNING: SCRIPT PARALLELIZED OVER MANY CORES ##################
############## ENSURE YOUR HAVE THE RESOURCES BEFORE RUNNING ##################
###############################################################################

library(doMC)

registerDoMC(cores = 2)

############################### Helper Functions ##############################

get_max_subj_dims <- function(subj) {
  # return size in rows of the largest data observation file in subj dir
  sizes <- c()
  gestfiles <- list.files(subj)
  for (f in gestfiles) {
    fname <- paste0(subj, f)
    tempdf <- read.table(fname, header = TRUE)
    sizes <- c(sizes, nrow(tempdf))
  }
  return(max(sizes))
}

get_max_global_dims <- function(mastdir) {
  # return max dim in rows among all data files in all subj dirs
  sizes <- c()
  subjdirs <- paste0(mastdir, list.files(mastdir), "/")
  nfiles <- length(subjdirs)
  for (s in subjdirs) {
    sizes <- c(sizes, get_max_subj_dims(s))
  }
  return(max(sizes))
}

pad_subj_matrix <- function(f, s) {
  # append k rows of zeros so that nrow(mat) = s
  mat <- data.matrix(read.table(f))
  c <- ncol(mat)
  k <- s - nrow(mat)
  zeros <- matrix(0, k, c)
  write.table(rbind(mat, zeros), f)
}

################################# Program Body ################################

# root file directory for data files
rootname <- "./clean_EMG_data_for_gestures-master/"
subjects <- paste0(rootname, list.files(rootname), "/")

maxrows <- get_max_global_dims(rootname)


pad_subj_matrix(s, 2000)
nrow(read.table(s))








