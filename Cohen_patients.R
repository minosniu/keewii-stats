# usage: setwd("c:\\Users\\Labuser\\Documents\\Github\\keewii-stats"); source('Cohen_patients.R',print.eval=TRUE)

# This function reads txt files in each folders of the subjects,
# and read true answers and subject's answer.
# Then, it runs Cohen's Kappa agreement for intra-rater agreement.
# Four results will be shown: Cohen_BC, Cohen_BP, Cohen_FC, Cohen_FP.
# First Capital: B-brachioradialis, F-Flexor Pollicis Brevis
# Second Capital: C- Cartesian, P- Polar Coordinate
rm(list = ls());
# Basic settings: Can be changed
cur_path<-"C:\\Users\\Labuser\\Documents\\Github\\keewii-visual\\data\\Patients"
listener<- "amber" ;  
# Shouldn't be changed
library(fmsb) # Cohen's kappa
num_trials<-25;Vowel<-c('ɑ', 'ɛ', 'i', 'ɔ', 'u');Encoding(Vowel)<-'UTF-8'  
len_vowel<-length(Vowel)
Cohen_BC<-matrix(0, nrow = len_vowel, ncol = len_vowel);
Cohen_BP<-Cohen_BC; Cohen_FC<-Cohen_BC; Cohen_FP<-Cohen_BC 
# colnames(Cohen_matrix)<-Vowel; rownames(Cohen_matrix)<-Vowel;
log_filename<-paste(c('test-log-',listener,'.txt'),collapse='')
#answer_col<-3 # col1:path, col2: right answer, col3: answer
# list filenames of brac & fpb, odd index is cartesian, even index is polar coordinate
dirs_all<-grep("",list.dirs(cur_path,recursive=FALSE),value=TRUE)
filenames_brac<-list.files(dirs_all,log_filename, recursive=TRUE, full.names=TRUE)
numfile<-length(filenames_brac)/2 # number of subject
temp_brac_cart<-matrix(numeric(0), 0,2) 
temp_brac_polar<-matrix(numeric(0), 0,2) 

for (a in 1:numfile){
  temp<-read.table(filenames_brac[2*a-1],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
  temp_brac_cart<-rbind(temp_brac_cart,temp)
  
  temp<-read.table(filenames_brac[2*a],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
  temp_brac_polar<-rbind(temp_brac_polar,temp)
}
#temp_brac_cart<-read.table(filenames_brac[3],header=FALSE,nrows=num_trials,encoding='UTF-8')[,2:3]
N<-numfile*num_trials #250
for (k in 1:N)
  for (a in 1:len_vowel){
    for (b in 1:len_vowel){
      if (temp_brac_cart[k,1]==Vowel[b] & temp_brac_cart[k,2]==Vowel[a]){
        Cohen_BC[a,b]<-Cohen_BC[a,b]+1
      } 
      if (temp_brac_polar[k,1]==Vowel[b] & temp_brac_polar[k,2]==Vowel[a]){
        Cohen_BP[a,b]<-Cohen_BP[a,b]+1
      }  
    }
  }
Kappa.test(Cohen_BC, y=NULL, conf.level=0.95)
Kappa.test(Cohen_BP, y=NULL, conf.level=0.95)

