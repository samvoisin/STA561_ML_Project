################################################################################
##############  WARNING: SCRIPT PARALLELIZED OVER MANY CORES ###################
############## ENSURE YOUR HAVE THE RESOURCES BEFORE RUNNING ###################
################################################################################

# Clean EMG data using RMS filter

library(biosignalEMG)
suppressMessages(library(tidyverse))
library(doMC)

registerDoMC(cores = 6)


# Helper function to clean sEMG data
## Input: txt file_path, out directory path, subfolder to create (e.g. 01, 02, etc)
clean_semg = function(fil, dat_out, sub_f){
  
  dat = read_tsv(fil, col_names = TRUE) # Read txt file
  mov  = dat %>% select(class) %>% unlist() %>% factor() # extract class, turn into factor
  
  dat = dat %>% select(-time, -class) %>% # Convert to EMG data type
    as.emg(., samplingrate = 1000) # Set sampling rate at 1kHz
  
  ch1 = envelope(dat, method = "RMS", 
                 wsize = 200, channel = 1)$values # Apply RMS to 1st channel
  tmp = matrix(NA, length(ch1), 10) # Initialize matrix
  tmp[, 1] = seq(2, length(ch1) + 1, by= 1) #Set time vals in ms (i.e. 2 = 200 ms)
  tmp[, 2] = ch1 # Add RMS channel 1 vals
  
  # Do same RMS process to rest of channels
  for(i in 2:8){
    tmp[, i+1] = envelope(dat, method = "RMS", wsize = 200, channel = i)$values
  }
  
  # Creat df and add class column
  df = tmp %>% as.data.frame() %>% 
    as.tibble() %>%  
    setNames(., c("time", dat$data.name, "class")) %>% 
    mutate(class = mov)
 
  # Create subfolder in out_directory if doesn't exist
  dat_out = file.path(dat_out, sub_f)
  if (!dir.exists(dat_out)){
    dir.create(dat_out)
  }
  
  new_filname = basename(fil) %>% 
    str_replace(., "raw", "clean") # File name change from raw to clean
  dat_out = paste0(dat_out,"/", new_filname) # creat file out path
  write_tsv(df, dat_out) # write file as txt
  
}
# End of helper function

# helper for parallel
operate_on_subfolders <- function(direct) {
  # direct is directory where operation occurs
  files = list.files(direct, full.names = TRUE) # Get 2 files
  sub_f = basename(direct) #Get subfolder name to use in helper fcn and keep folder struct
  sapply(files, function(x) clean_semg(x, out_path, sub_f)) # Apply helper fcn to both files
}
# End of helper function

################################################################################

# Main script
data_path = "EMG_data_for_gestures-master" # raw data folder
full_path = file.path(getwd(), data_path) # create full path to raw data

out_dir = paste0("clean_", data_path) # Create new clean data folder if non-existent
if (!dir.exists(file.path(getwd(), out_dir))){
  dir.create(file.path(getwd(), out_dir))
}

out_path = file.path(getwd(), out_dir)

data_dirs = list.dirs(full_path)[-1] # Get all subfolders in raw data main folder
dirlen = length(data_dirs)

# For each subfolder (i.e. 01, 02, 03, etc)
foreach(d = (1:dirlen)) %dopar% operate_on_subfolders(data_dirs[d])

