#Genesis

#Make it to a corpus

NIV_Genesis_text = Corpus(VectorSource(NIV_Genesis_text))
NIV_Genesis_text

#Change God To CGod
NIV_Genesis_text <- tm_map(NIV_Genesis_text, content_transformer(gsub), pattern = "God", replacement = "CGod")


# to lower case all texts
NIV_Genesis_text <- tm_map(NIV_Genesis_text, content_transformer(tolower))

#Remove Punctuation
NIV_Genesis_text = tm_map(NIV_Genesis_text,removePunctuation)

#Remove Stopwords in English

NIV_Genesis_text = tm_map(NIV_Genesis_text,removeWords,c("unto","thy","thee","said","thou"))
#NIV_Genesis_text = tm_map(NIV_Genesis_text,removeWords,c("god","God")) #QJ: I did this to make sure words won't appear 
NIV_Genesis_text = tm_map(NIV_Genesis_text,removeWords,stopwords("english"))


#Convert the corpus to a document term matrix
NIV_Genesis_dtm = DocumentTermMatrix(NIV_Genesis_text)

#Look for the most frequent 100 words in NIV Genesis
term.freq <- colSums(as.matrix(NIV_Genesis_dtm))


# cgod is God!!!!!
term.freq['cgod']   #R cannot distinguish between uppercase and lowercase 
#term.freq <- subset(term.freq), term.freq>=80)

# god is god!!!!!

term.freq['god']


df_genesis <- data.frame(term=names(term.freq),freq=term.freq) #QJ: renamed df1/df2 to df_[biblebook]
df_genesis <- arrange(df_genesis, desc(freq)) #QJ this is to extract top X later
df_genesis$term <-reorder(df_genesis$term,df_genesis$freq)

#Creates plot of the 100 most frequently appeared terms in genesis
ggplot(df_genesis[1:50,], aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("terms") + ylab("count") +coord_flip()
ggplot(df_genesis[1:10,], aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("terms") + ylab("count") +coord_flip()

#Wordcloud of genesis
Genesis_matrix <- as.matrix(NIV_Genesis_dtm)
word.freq <- sort(colSums(Genesis_matrix), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=100,random.order=F, scale=c(5, 1), colors=brewer.pal(9, "Spectral"))

# workcloud does not seem useful.

















#Proverbs Analysis

#Make it to a corpus

NIV_Proverbs_text = Corpus(VectorSource(NIV_Proverbs_text))
NIV_Proverbs_text


#Change God To CGod
NIV_Proverbs_text <- tm_map(NIV_Proverbs_text, content_transformer(gsub), pattern = "God", replacement = "CGod")


# to lower case all texts
NIV_Proverbs_text <- tm_map(NIV_Proverbs_text, content_transformer(tolower))

#Remove Punctuation
NIV_Proverbs_text = tm_map(NIV_Proverbs_text,removePunctuation)

#Remove Stopwords in English

NIV_Proverbs_text = tm_map(NIV_Proverbs_text,removeWords,c("unto","thy","thee","said","thou"))
#NIV_Proverbs_text = tm_map(NIV_Proverbs_text,removeWords,c("god","God")) #QJ: I did this to make sure words won't appear 
NIV_Proverbs_text = tm_map(NIV_Proverbs_text,removeWords,stopwords("english"))



#Convert the corpus to a document term matrix
NIV_Proverbs_dtm = DocumentTermMatrix(NIV_Proverbs_text)

#Look for the most frequent 100 words in NIV Proverbs
term.freq <- colSums(as.matrix(NIV_Proverbs_dtm))
term.freq['god']
term.freq['cgod']

#term.freq <- subset(term.freq, term.freq>=30)
df_proverbs <- data.frame(term=names(term.freq),freq=term.freq)
df_proverbs <- arrange(df_proverbs, desc(freq))
df_proverbs$term<-reorder(df_proverbs$term,df_proverbs$freq)

#Creates plot of the 100 most frequently appeared terms in Proverbs
ggplot(df_proverbs[1:10,], aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("terms") + ylab("count") +coord_flip()


#Wordcloud of proverbs
Proverbs_matrix <- as.matrix(NIV_Proverbs_dtm)
word.freq <- sort(colSums(Proverbs_matrix), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=50,random.order=F, scale=c(5, 1), colors=brewer.pal(9, "Spectral"))
















#Psalms Analysis

#Make it to a corpus

NIV_Psalms_text = Corpus(VectorSource(NIV_Psalms_text))
NIV_Psalms_text

#Remove Punctuation
NIV_Psalms_text = tm_map(NIV_Psalms_text,removePunctuation)

#Change God To CGod
NIV_Psalms_text <- tm_map(NIV_Psalms_text, content_transformer(gsub), pattern = "God", replacement = "CGod")


# to lower case all texts
NIV_Psalms_text <- tm_map(NIV_Psalms_text, content_transformer(tolower))

#Remove Stopwords in English

NIV_Psalms_text = tm_map(NIV_Psalms_text,removeWords,c("unto","thy","thee","said","thou"))
#NIV_Psalms_text = tm_map(NIV_Psalms_text,removeWords,c("god","God")) #QJ: I did this to make sure words won't appear 
NIV_Psalms_text = tm_map(NIV_Psalms_text,removeWords,stopwords("english"))



#Convert the corpus to a document term matrix
NIV_Psalms_dtm = DocumentTermMatrix(NIV_Psalms_text)

#Look for the most frequent 100 words in NIV Psalms
term.freq <- colSums(as.matrix(NIV_Psalms_dtm))
term.freq['cgod']
term.freq['god']
#term.freq <- subset(term.freq, term.freq>=80)
df_psalms <- data.frame(term=names(term.freq),freq=term.freq)
df_psalms <- arrange(df_psalms, desc(freq))
df_psalms$term<-reorder(df_psalms$term,df_psalms$freq)

#Creates plot of the 100 most frequently appeared terms in NIV Psalms
ggplot(df_psalms[1:10,], aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("terms") + ylab("count") +scale_x_discrete(breaks = df_psalms$term)+coord_flip()


#Wordcloud of NIV Psalms
Psalms_matrix <- as.matrix(NIV_Psalms_dtm)
word.freq <- sort(colSums(Psalms_matrix), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=100,random.order=F, scale=c(5, 1), colors=brewer.pal(9, "Spectral"))
















#Matthew Analysis

#Make it to a corpus

NIV_Matthew_text = Corpus(VectorSource(NIV_Matthew_text))
NIV_Matthew_text



#Change God To CGod
NIV_Matthew_text <- tm_map(NIV_Matthew_text, content_transformer(gsub), pattern = "God", replacement = "CGod")


# to lower case all texts
NIV_Matthew_text <- tm_map(NIV_Matthew_text, content_transformer(tolower))

#Remove Punctuation
NIV_Matthew_text = tm_map(NIV_Matthew_text,removePunctuation)

#Remove Stopwords in English

NIV_Matthew_text = tm_map(NIV_Matthew_text,removeWords,c("unto","thy","thee","said","thou"))
#NIV_Matthew_text = tm_map(NIV_Matthew_text,removeWords,c("god","God")) #QJ: I did this to make sure words won't appear 
NIV_Matthew_text = tm_map(NIV_Matthew_text,removeWords,stopwords("english"))



#Convert the corpus to a document term matrix
NIV_Matthew_dtm = DocumentTermMatrix(NIV_Matthew_text)

#Look for the most frequent 100 words in NIV Matthew
term.freq <- colSums(as.matrix(NIV_Matthew_dtm))
term.freq['cgod']
#term.freq <- subset(term.freq, term.freq>=40)
df_matthew <- data.frame(term=names(term.freq),freq=term.freq)
df_matthew <- arrange(df_matthew, desc(freq))
df_matthew$term<-reorder(df_matthew$term,df_matthew$freq)

#Creates plot of the 100 most frequently appeared terms in NIV Matthew
ggplot(df_matthew[1:10,], aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("terms") + ylab("count") +scale_x_discrete(breaks = df_matthew$term)+coord_flip()


#Wordcloud of NIV Matthew
Matthew_matrix <- as.matrix(NIV_Matthew_dtm)
word.freq <- sort(colSums(Matthew_matrix), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=50,random.order=F, scale=c(5, 1), colors=brewer.pal(9, "Spectral"))
















#John Analysis

#Make it to a corpus

NIV_John_text = Corpus(VectorSource(NIV_John_text))
NIV_John_text



#Change God To CGod
NIV_John_text <- tm_map(NIV_John_text, content_transformer(gsub), pattern = "God", replacement = "CGod")


# to lower case all texts
NIV_John_text <- tm_map(NIV_John_text, content_transformer(tolower))

#Remove Punctuation
NIV_John_text = tm_map(NIV_John_text,removePunctuation)

#Remove Stopwords in English

NIV_John_text = tm_map(NIV_John_text,removeWords,c("unto","thy","thee","said","thou"))
#NIV_John_text = tm_map(NIV_John_text,removeWords,c("god","God")) #QJ: I did this to make sure words won't appear 
NIV_John_text = tm_map(NIV_John_text,removeWords,stopwords("english"))



#Convert the corpus to a document term matrix
NIV_John_dtm = DocumentTermMatrix(NIV_John_text)

#Look for the most frequent 100 words in NIV John
term.freq <- colSums(as.matrix(NIV_John_dtm))
term.freq['god']
#term.freq <- subset(term.freq, term.freq>40)
df_john <- data.frame(term=names(term.freq),freq=term.freq)
df_john <- arrange(df_john, desc(freq))
df_john$term<-reorder(df_john$term,df_john$freq)

#Creates plot of the 100 most frequently appeared terms in NIV John
ggplot(df_john[1:10,], aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("terms") + ylab("count") +scale_x_discrete(breaks = df_john$term)+coord_flip()


#Wordcloud of NIV John
John_matrix <- as.matrix(NIV_John_dtm)
word.freq <- sort(colSums(John_matrix), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=50,random.order=F, scale=c(5, 1), colors=brewer.pal(9, "Spectral"))


















#Romans Analysis



#Make it to a corpus

NIV_Romans_text = Corpus(VectorSource(NIV_Romans_text))
NIV_Romans_text


#Change God To CGod
NIV_Romans_text <- tm_map(NIV_Romans_text, content_transformer(gsub), pattern = "God", replacement = "CGod")


# to lower case all texts
NIV_Romans_text <- tm_map(NIV_Romans_text, content_transformer(tolower))

#Remove Punctuation
NIV_Romans_text = tm_map(NIV_Romans_text,removePunctuation)

#Remove Stopwords in English

NIV_Romans_text = tm_map(NIV_Romans_text,removeWords,c("unto","thy","thee","said","thou"))
#NIV_Romans_text = tm_map(NIV_Romans_text,removeWords,c("god","God")) #QJ: I did this to make sure words won't appear 
NIV_Romans_text = tm_map(NIV_Romans_text,removeWords,stopwords("english"))



#Convert the corpus to a document term matrix
NIV_Romans_dtm = DocumentTermMatrix(NIV_Romans_text)

#Look for the most frequent 100 words in NIV Romans
term.freq <- colSums(as.matrix(NIV_Romans_dtm))
term.freq['cgod']
term.freq["jesus"]
#term.freq <- subset(term.freq, term.freq>=20)
df_romans <- data.frame(term=names(term.freq),freq=term.freq)
df_romans <- arrange(df_romans, desc(freq))
df_romans$term<-reorder(df_romans$term,df_romans$freq)

#Creates plot of the 100 most frequently appeared terms in NIV Romans
ggplot(df_romans[1:10,], aes(x=term,y=freq)) + geom_bar(stat="identity") + xlab("terms") + ylab("count") +scale_x_discrete(breaks = df_romans$term)+coord_flip()


#Wordcloud of NIV Romans
Romans_matrix <- as.matrix(NIV_Romans_dtm)
word.freq <- sort(colSums(Romans_matrix), decreasing=T)
wordcloud(words=names(word.freq), freq=word.freq, min.freq=20,random.order=F, scale=c(5, 1), colors=brewer.pal(9, "Spectral"))






#Some observations: 
#'Shall' is by far the most common word in these books - work to do: check words around 'shall' to uncover the context that shall is used : is it a command?
#  -'Lord' was very heavily used in the Old Testament but not New. 2nd/4th/1st in Gen/Prov/Psa but 10th/25th/7th in Matt/John/Rom
#- Among the chapters we analyzed above, only Psalms have both 'god' and 'God' appeared in its text, the rest chapters only have 'God' in them













