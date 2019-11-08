#This code is for data preprocessing and analysis

#load packages

library(stringr)
library(tm)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(dplyr)


bible_full = readLines("~/Desktop/Bible/bible.txt")
bible_text = str_replace(bible_full,"^\\s+","")


res_chapname = grep("Book [0-9][0-9]",bible_text,value = T)
res_chapline = grep("Book [0-9][0-9]",bible_text,value = F)
res_chapline[67] <- length(bible_full)+1
res_chapname <- sub('.*\t', '', res_chapname)


#Split the text in 66 chapters
for(i in 1:66) {
  assign(paste0("NIV_", res_chapname[i]), bible_text[res_chapline[i]:(res_chapline[(i+1)]-1)])
}





# Using loop to create a list of all names of chapter objects

chapter_names <- res_chapname

for(i in 1:length(chapter_names)){
  chapter_names[i] <- paste0("NIV_",chapter_names[i])
}

temp <- c()
temp2 <- c()
for(i in 1:length(chapter_names)){
  temp <- c()
  temp2 <- c()
  temp <- get(chapter_names[i])
  res = grep("[0-9][0-9][0-9]:[0-9][0-9][0-9]",temp,value = F)
  for(j in seq(1:(length(res)-1))){
    x = paste(temp[res[j]:(res[j+1]-1)],collapse = " ")
    temp2 = c(temp2,x)
  }
  assign(paste0(chapter_names[i],"_content"),temp2)
  
}


#Remove all leading numbers

temp <- c()
temp2 <- c()


for(i in 1:length(chapter_names)){
  temp <- c()
  temp2 <- c()
  temp <- get(paste0(chapter_names[i],"_content"))
  temp2 =  str_split_fixed(temp, "[0-9][0-9][0-9]:[0-9][0-9][0-9] ",2)
  temp2 = temp2[,2]
  
  
  assign(paste0(chapter_names[i],"_text"),temp2)
  
}


#####
#####
# descriptive statistics summary

NIV_Genesis_length<-sapply(strsplit(NIV_Genesis_text, " "), length) #The length of each verse in genesis
NIV_Genesis_total_length<-sum(NIV_Genesis_length) # Total of 38243 words in genesis
NIV_Genesis_mean_length<-mean(NIV_Genesis_length) #Average length of each verse in genesis is approximately 25


#Proverbs statistics
NIV_Proverbs_length<-sapply(strsplit(NIV_Proverbs_text, " "), length)
NIV_Proverbs_total_length<-sum(NIV_Proverbs_length)
NIV_Proverbs_mean_length<-mean(NIV_Proverbs_length)


#Psalms statistics
NIV_Psalms_length<-sapply(strsplit(NIV_Psalms_text, " "), length)
NIV_Psalms_total_length<-sum(NIV_Psalms_length)
NIV_Psalms_mean_length<-mean(NIV_Psalms_length)


#Matthew statistics
NIV_Matthew_length<-sapply(strsplit(NIV_Matthew_text, " "), length)
NIV_Matthew_total_length<-sum(NIV_Matthew_length)
NIV_Matthew_mean_length<-mean(NIV_Matthew_length)

NIV_John_length<-sapply(strsplit(NIV_John_text, " "), length)
NIV_John_total_length<-sum(NIV_John_length)
NIV_John_mean_length<-mean(NIV_John_length)

NIV_Romans_length<-sapply(strsplit(NIV_Romans_text, " "), length)
NIV_Romans_total_length<-sum(NIV_Romans_length)
NIV_Romans_mean_length<-mean(NIV_Romans_length)




NIV_six_books_length <- c(NIV_Genesis_total_length,NIV_Proverbs_total_length,NIV_Psalms_total_length,NIV_Matthew_total_length,NIV_John_total_length,NIV_Romans_total_length)









