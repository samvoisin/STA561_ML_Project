#############################################################
##### ------------------------------------------------- #####
### ---------------------- PCA Plots -------------------- ###
##### ------------------------------------------------- #####
#############################################################



#library(here)
library(dplyr)
library(ggplot2)
library(gridExtra)

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






### Plotting


Plot_df <- data.frame(prop_var=pca1$sdev^2/sum(pca1$sdev^2), comp=names(pca1$sdev),
                      cumulative=cumsum(pca1$sdev^2)/sum(pca1$sdev^2))

p1 <- ggplot(data=Plot_df, aes(x=comp, y=prop_var)) +
  geom_bar(stat="identity")+
  ylab("proportional variance")+
  ggtitle("Screeplot")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))




p2 <- ggplot(data=Plot_df, aes(x=comp, y=cumulative, group=1)) +
  geom_line()+
  geom_point()+
  ylab("cumulative proportional variance")+
  ggtitle("Cumulative variance plot")+
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))



# Create new clean data folder if non-existent
out_dir <- "PCA_Plots"
if (!dir.exists(file.path(main_directory, out_dir))){
  dir.create(file.path(main_directory, out_dir))
}


png(filename=file.path(main_directory, out_dir, "PCA_screeplot.png"),
    width = 900, height = 500)
grid.arrange(p1, p2, ncol=2)
dev.off()

pdf(file=file.path(main_directory, out_dir, "PCA_screeplot.pdf"),
    width = 9, height = 5)
grid.arrange(p1, p2, ncol=2)
dev.off()










