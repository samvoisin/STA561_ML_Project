#############################################################
##### ------------------------------------------------- #####
### ---------------------- PCA Plots -------------------- ###
##### ------------------------------------------------- #####
#############################################################



#library(here)
library(dplyr)

# same reading part as in the PCA data file
main_directory <- getwd()
setwd(path <- paste0(getwd(),"/","clean_EMG_data_for_gestures-master"))
ind <- list.files()
M <- matrix(NA, nrow=1, ncol=9)
m <- c()
colnames(M) <- c("time", paste0("channel",1:8))

print("\nReading Files...")
prog <- progress_estimated(n = length(ind))
for (i in ind){
  paste0(path, "/", i) %>% setwd()
  files_i <- list.files() %>% grepl("gesture", .) %>% list.files()[.]
  for (j in files_i){
    Mnew <- read.table(j, header=TRUE)
    M <- rbind(M,Mnew)
    mnew <- rep(j,nrow(Mnew))
    m <- c(m,mnew)
  }
  prog$tick()$print()
}
# exclude dummy row
M <- M[-1,]


# excluding time
X <- M[,names(M)!="time"]
# scaling and using PCA
pca1 <- princomp(scale(X))
# safe as data frame for splitting
pca_df <- as.data.frame(pca1$scores)


# Create new clean data folder if non-existent
out_dir <- "PCA_Plots"
if (!dir.exists(file.path(main_directory, out_dir))){
  dir.create(file.path(main_directory, out_dir))
}


















