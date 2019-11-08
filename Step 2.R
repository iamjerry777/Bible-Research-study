library(stringr)
library(tm)
library(quanteda)
bible_full = readLines("~/Desktop/Bible/bible.txt")
bible_text = str_replace(bible_full,"^\\s+","")

#mycorpus<-corpus(bible_text)
#summary(mycorpus)


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

# Please make sure run the data cleaning code BEFORE running this file.





