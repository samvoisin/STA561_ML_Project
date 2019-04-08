# Clean EMG data using RMS filter

library(biosignalEMG)
suppressMessages(library(tidyverse))

clean_semg = function(fil, dat_out, sub_f){
  
  dat = read_tsv(fil, col_names = TRUE) 
  mov  = dat %>% select(class) %>% unlist() %>% factor()
  
  dat = dat %>% select(-time, -class) %>% 
    as.emg(., samplingrate = 100) # Set sampling rate at 100 ms
  
  ch1 = envelope(dat, method = "RMS", wsize = 200, channel = 1)$values
  tmp = matrix(NA, length(ch1), 10)
  tmp[, 1] = seq(2, length(ch1) + 1, by= 1)
  tmp[, 2] = ch1
  
  for(i in 2:8){
    tmp[, i+1] = envelope(dat, method = "RMS", wsize = 200, channel = i)$values
  }
  
  df = tmp %>% as.data.frame() %>% 
    as.tibble() %>%  
    setNames(., c("time", dat$data.name, "class")) %>% 
    mutate(class = mov)
 
  
  dat_out = file.path(dat_out, sub_f)
  if (!dir.exists(dat_out)){
    dir.create(dat_out)
  }
  new_filname = basename(fil) %>% str_replace(., "raw", "clean")
  dat_out = paste0(dat_out,"/", new_filname)
  write_tsv(df, dat_out)
  
}

data_path = "EMG_data_for_gestures-master" 
full_path = file.path(getwd(), data_path)

out_dir = paste0("clean_", data_path)
if (!dir.exists(file.path(getwd(), out_dir))){
  dir.create(file.path(getwd(), out_dir))
}

out_path = file.path(getwd(), out_dir)

data_dirs = list.dirs(full_path)[-1]

for (d in data_dirs){
  files = list.files(d, full.names = TRUE)
  sub_f = basename(d)
  sapply(files, function(x) clean_semg(x, out_path, sub_f))
}

