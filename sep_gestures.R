################################################################################
#################### Seperate gestures into their own files ####################
################################################################################

library(here)
library(stringr)

############################### Helper Functions ###############################

write_mat_to_file <- function(m, p) {
  # write matrix to a file at path
  df <- data.frame(m)
  write_csv(df, p)
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
)


# seperate gestures in each 
for (f in files$df1) {
  df <- read.table(f, header = TRUE)
  subjnum <- str_extract(f, "\\d{2}/") # id subject number for filepath
  breakpt <- 0 # reset breakpoint for each file
  for (i in 2:nrow(df)) {
    # when gesture class switches cut matrix and write to csv
    if (df$class[i] != df$class[i - 1]) {
      classMat <- data.matrix(df[breakpt:(i - 1), ])
      fp <- paste0(rootname, subjnum, "gesture_", df$class[i], "_1", ".csv")
      # if file already exists use filename "...i_2.csv"
      if (fp %in% list.files(paste0(rootname, subjnum))) {
        fp <- paste0(rootname, subjnum, "gesture_", df$class[i], "_2", ".csv")
      }
      write_mat_to_file(classMat, fp)
      breakpt <- i
    }
  }
}











