# usage: setwd("c:\\Users\\Labuser\\Documents\\Github\\keewii-stats"); source('Fleiss_new.R',print.eval=TRUE)

# This function reads txt files in each folders of the subjects,
# and read subject's answer to check if raters agree with each other.
# Then, it runs Fleiss's Kappa agreement for inter-rater agreement.
# Four results will be shown: Fleiss_BC, Fleiss_BP, Fleiss_FC, Fleiss_FP.
# First Capital: B-brachioradialis, F-Flexor Pollicis Brevis
# Second Capital: C- Cartesian, P- Polar Coordinate
# Rater info: rater-info.txt in current path

rm(list = ls());
# Basic settings: Can be changed
cur_path<-"C:\\Users\\Labuser\\Documents\\Github\\keewii-visual\\data\\Healthy"
list_rater<-t(read.table(paste(c(cur_path,"\\rater-info.txt"), collapse=""), header=FALSE))
# Shouldn't be changed
library(irr) # fleiss' kappa
num_trials<-25; Vowel<-c('É', 'É', 'É', 'i', 'u');Encoding(Vowel)<-'UTF-8'
len_vowel<-length(Vowel)

# list filenames of brac & fpb, odd index is cartesian, even index is polar coordinate
dirs_brac<-grep("[C]+$",list.dirs(cur_path,recursive=FALSE),value=TRUE)
dirs_brac_cart<-grep("[t]+$",list.dirs(dirs_brac,recursive=FALSE),value=TRUE)
dirs_brac_polar<-grep("[r]+$",list.dirs(dirs_brac,recursive=FALSE),value=TRUE)
dirs_fpb<-grep("[B]+$",list.dirs(cur_path,recursive=FALSE),value=TRUE)
dirs_fpb_cart<-grep("[t]+$",list.dirs(dirs_fpb,recursive=FALSE),value=TRUE)
dirs_fpb_polar<-grep("[r]+$",list.dirs(dirs_fpb,recursive=FALSE),value=TRUE)

num_subject<-length(dirs_brac_cart) # number of subject
N<-num_subject*num_trials #total instances
Fleiss_BC<-matrix(, nrow = N, ncol = 0);
Fleiss_BP<-Fleiss_BC; Fleiss_FC<-Fleiss_BC; Fleiss_FP<-Fleiss_BC 


for (m in 1:length(list_rater)){ # rater loop
  temp_brac_cart<-matrix(numeric(0), 0,2) 
  temp_brac_polar<-matrix(numeric(0), 0,2) 
  temp_fpb_cart<-matrix(numeric(0), 0,2) 
  temp_fpb_polar<-matrix(numeric(0), 0,2)  
  log_filename<-paste(c('test-log-',list_rater[m],'.txt'),collapse='')
  filenames_brac_cart<-list.files(dirs_brac_cart,log_filename, recursive=TRUE, full.names=TRUE)
  filenames_brac_polar<-list.files(dirs_brac_polar,log_filename, recursive=TRUE, full.names=TRUE)
  filenames_fpb_cart<-list.files(dirs_fpb_cart,log_filename, recursive=TRUE, full.names=TRUE)
  filenames_fpb_polar<-list.files(dirs_fpb_polar,log_filename, recursive=TRUE, full.names=TRUE)
  for (a in 1:length(filenames_brac_cart)){ # merge log file of each raters
    temp<-read.table(filenames_brac_cart[a],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
    temp_brac_cart<-rbind(temp_brac_cart,temp)
    
    temp<-read.table(filenames_brac_polar[a],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
    temp_brac_polar<-rbind(temp_brac_polar,temp)
    
    temp<-read.table(filenames_fpb_cart[a],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
    temp_fpb_cart<-rbind(temp_fpb_cart,temp)
    
    temp<-read.table(filenames_fpb_polar[a],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
    temp_fpb_polar<-rbind(temp_fpb_polar,temp)
  }
  Fleiss_BC<-cbind(Fleiss_BC,temp_brac_cart[,2])
  Fleiss_BP<-cbind(Fleiss_BP,temp_brac_polar[,2])
  Fleiss_FC<-cbind(Fleiss_FC,temp_fpb_cart[,2])
  Fleiss_FP<-cbind(Fleiss_FP,temp_fpb_polar[,2])
}
kappam.fleiss(Fleiss_BC, exact = FALSE, detail = FALSE)
kappam.fleiss(Fleiss_BP, exact = FALSE, detail = FALSE)
kappam.fleiss(Fleiss_FC, exact = FALSE, detail = FALSE)
kappam.fleiss(Fleiss_FP, exact = FALSE, detail = FALSE)
