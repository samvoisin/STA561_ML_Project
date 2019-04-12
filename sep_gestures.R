################################################################################
##############     Seperate gestures into their own files    ###################
################################################################################
################################################################################
##############   WARNING: SCRIPT PARALLELIZES OVER 3 CORES   ###################
############## ENSURE YOUR HAVE THE RESOURCES BEFORE RUNNING ###################
################################################################################

library(here)
library(stringr)
library(tidyverse)
library(doMC)

registerDoMC(cores = 4)

############################### Helper Functions ###############################

write_mat_to_file <- function(m, p) {
  # write matrix to a file at path
  df <- data.frame(m)
  write_csv(df, p)
}

parse_gest_file <- function(f) {
  
  df <- read.table(f, header = TRUE)
  trl <- str_extract(f, "\\d{1}_clean") # trial number
  subjnum <- str_extract(f, "\\d{2}/") # id subject number for filepath
  breakpt <- 0 # reset breakpoint for each file
  for (i in 2:nrow(df)) {
    # when gesture class switches cut matrix and write to csv
    if (df$class[i] != df$class[i - 1]) {
      classMat <- data.matrix(df[breakpt:(i - 1), ])
      fp <- paste0(rootname, subjnum, "gesture_", df$class[i], "_", trl, ".csv")
      write_mat_to_file(classMat, fp)
      breakpt <- i
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




