# usage: setwd("c:\\Users\\Labuser\\Documents\\Github\\keewii-stats"); source('success_rate_vowel_patients.R',print.eval=TRUE)

# This function reads txt files in each folders of the patients,
# and it calculates success rates for each vowel.
# It is similar with Fless_kappa.R, but the objective&result is different.
# First Capital: B-brachioradialis, F-Flexor Pollicis Brevis
# Second Capital: C- Cartesian, P- Polar Coordinate
# Rater info: rater-info.txt in current path

rm(list = ls());
# Basic settings: Can be changed
cur_path<-"C:\\Users\\Labuser\\Documents\\Github\\keewii-visual\\data\\Patients"
list_rater<-t(read.table(paste(c(cur_path,"\\rater-info.txt"), collapse=""), header=FALSE))
# Shouldn't be changed
library(irr) # fleiss' kappa
num_trials<-25; Vowel<-c("\u0251","\u025B","i","\u0254","u"); #Encoding(Vowel)<-'UTF-8'
len_vowel<-length(Vowel) # somehow changed to aoieu

# list filenames of brac & fpb, odd index is cartesian, even index is polar coordinate
dirs_brac<-grep("",list.dirs(cur_path,recursive=FALSE),value=TRUE)
dirs_brac_cart<-grep("[t]+$",list.dirs(dirs_brac,recursive=FALSE),value=TRUE)
dirs_brac_polar<-grep("[r]+$",list.dirs(dirs_brac,recursive=FALSE),value=TRUE)

num_subject<-length(dirs_brac_cart) # number of subject
N<-num_subject*num_trials #total instances
answer_BC<-matrix(, nrow = N, ncol = 0);answer_BP<-answer_BC; 

for (m in 1:length(list_rater)){ # rater loop
  temp_brac_cart<-matrix(numeric(0), 0,2) 
  temp_brac_polar<-matrix(numeric(0), 0,2) 
  log_filename<-paste(c('test-log-',list_rater[m],'.txt'),collapse='')
  filenames_brac_cart<-list.files(dirs_brac_cart,log_filename, recursive=TRUE, full.names=TRUE)
  filenames_brac_polar<-list.files(dirs_brac_polar,log_filename, recursive=TRUE, full.names=TRUE)
  for (a in 1:length(filenames_brac_cart)){ # merge log file of each raters
    temp<-read.table(filenames_brac_cart[a],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
    temp_brac_cart<-rbind(temp_brac_cart,temp)
    
    temp<-read.table(filenames_brac_polar[a],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
    temp_brac_polar<-rbind(temp_brac_polar,temp)
  }
  answer_BC<-cbind(answer_BC,temp_brac_cart[,2])
  answer_BP<-cbind(answer_BP,temp_brac_polar[,2])
}
answer_BC<-cbind(answer_BC,temp_brac_cart[,1])
answer_BP<-cbind(answer_BP,temp_brac_polar[,1]) # the last column is the true answers
Dim_table<-dim(answer_BC)

# add them if those subjects got the right answer.
SR_BC<-c(0,0,0,0,0);SR_BP<-SR_BC;
for (a in 1:Dim_table[1]){ #row
  for (b in 1:(Dim_table[2]-1)){ #col
    if (answer_BC[a,b]==answer_BC[a,5]){
      SR_BC[answer_BC[a,5]]<-SR_BC[answer_BC[a,5]]+1
    }
    if (answer_BP[a,b]==answer_BP[a,5]){
      SR_BP[answer_BC[a,5]]<-SR_BP[answer_BC[a,5]]+1
    }
  }
}
Tot<-length(list_rater)*5* length(dirs_brac)
SR_BC<-SR_BC/Tot;SR_BP<-SR_BP/Tot;
SR_BC<-c(SR_BC[1],SR_BC[3],SR_BC[4],SR_BC[2],SR_BC[5]) #aoeiu->aeiou
SR_BP<-c(SR_BP[1],SR_BP[3],SR_BP[4],SR_BP[2],SR_BP[5]) #aoeiu->aeiou






