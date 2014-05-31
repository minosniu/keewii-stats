# setwd("c:\\Users\\Labuser\\Documents\\Github\\keewii-stats"); source('success_rate_patients.R',print.eval=TRUE)
rm(list = ls());
cur_path<-"C:\\Users\\Labuser\\Documents\\Github\\keewii-visual\\data\\Patients"
list_rater<-t(read.table(paste(c(cur_path,"\\rater-info.txt"), collapse=""), header=FALSE))
dirs_brac<-grep("",list.dirs(cur_path,recursive=FALSE),value=TRUE)
dirs_brac_cart<-grep("[t]+$",list.dirs(dirs_brac,recursive=FALSE),value=TRUE)
dirs_brac_polar<-grep("[r]+$",list.dirs(dirs_brac,recursive=FALSE),value=TRUE)
num_subject<-length(dirs_brac_cart) # number of subject
SR_BC=0; SR_BP=0; n=0;

for (m in 1:length(list_rater)){ # rater loop
  log_filename<-paste(c('test-log-',list_rater[m],'.txt'),collapse='')
  filenames_brac_cart<-list.files(dirs_brac_cart,log_filename, recursive=TRUE, full.names=TRUE)
  filenames_brac_polar<-list.files(dirs_brac_polar,log_filename, recursive=TRUE, full.names=TRUE)
  for (a in 1:length(filenames_brac_cart)){ # merge log file of each raters
    temp<-read.table(filenames_brac_cart[a],header=FALSE,nrows=26,encoding='UTF-8',fill=TRUE)[26,] #26 is the last line
    temp<-toString(temp[1,1]);SR_BC<-SR_BC+as.numeric(gsub("\\D", "",temp)); 
    temp<-read.table(filenames_brac_polar[a],header=FALSE,nrows=26,encoding='UTF-8',fill=TRUE)[26,]
    temp<-toString(temp[1,1]);SR_BP<-SR_BP+as.numeric(gsub("\\D", "",temp )); n<-n+1;
  }
}
SR_BC<-SR_BC/(100*n); SR_BP<-SR_BP/(100*n); 