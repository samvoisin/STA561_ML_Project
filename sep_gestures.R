################################################################################
##############     Seperate gestures into their own files    ###################
################################################################################
################################################################################
##############  WARNING: SCRIPT PARALLELIZED OVER MANY CORES ###################
############## ENSURE YOUR HAVE THE RESOURCES BEFORE RUNNING ###################
################################################################################

library(here)
library(stringr)
library(tidyverse)
library(doMC)

registerDoMC(cores = 10)

############################### Helper Functions ###############################

parse_gest_file <- function(f) {
  
  df <- read.table(f, header = TRUE) # read txt file
  trl <- str_extract(f, "\\d{1}_clean") # trial number
  subjnum <- str_extract(f, "\\d{2}") # id subject number for filepath
  for (i in 1:6) {
      gestdf <- df %>% filter(class == i) %>% select(-class)
      # define file path for each observation of gesture in file
      # filename key: ./[dir]/[subj num {01,36}]/gesture_[gest num (1-6)]_[attempt num {1,2}]_[trial num {1,2}]
      # there are 2 trials (large txt file) per subject; 2 attempts per gesture per trial
      fp1 <- paste0(rootname, subjnum, "/", subjnum, "_gesture_", i, "_1_", trl, ".txt")
      fp2 <- paste0(rootname, subjnum, "/", subjnum, "_gesture_", i, "_2_", trl, ".txt")
      # find split point between first and second observation of gesture i
      t <- length(gestdf$time)
      for (j in 2:t) {
        # if time j differs from j - 1 by 1K miliseconds we have split
        if (gestdf$time[j] - gestdf$time[j - 1] > 1000) {
          write.table(gestdf[1:(j-1), ], fp1)
          write.table(gestdf[j:t, ], fp2)
        }
      }
  }
}

################################################################################


# root file directory for data files
rootname <- "./clean_EMG_data_for_gestures-master/"
subjects <- paste0(rootname, list.files(rootname))
datafiles1 <- c()
datafiles2 <- c()


# indentify both files for each subject
for (s in subjects) {
  datafiles1 <- c(datafiles1, paste0(s, "/", list.files(s)[1]))
  datafiles2 <- c(datafiles2, paste0(s, "/", list.files(s)[2]))
}


# data frame of data files
files <- data_frame(
  subjects = list.files(rootname),
  df1 = datafiles1,
  df2 = datafiles2
) %>% gather(subj, datafiles, -subjects) %>% select(-subj)

n <- nrow(files)

# seperate gestures in each 
foreach(i = (1:n)) %dopar% parse_gest_file(files$datafiles[i])




