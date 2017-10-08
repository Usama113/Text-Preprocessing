cname=file.path("D:/Study/RStudioWorkspace/Text Preprocessing"
                ,"texts")
cname
dir(cname) #List of files in the folder

library(tm)


docs=VCorpus(DirSource(cname))
summary(docs)

inspect(docs[1])
writeLines(as.character(docs[1])) #To read the file on console

#Removing Punctuation
docs=tm_map(docs,removePunctuation)

#incase working with emails or standardized documents
for(j in seq(docs)){
  docs[[j]]=gsub("/"," ",docs[[j]])
  docs[[j]]=gsub("@"," ",docs[[j]])
  docs[[j]]=gsub("\\|"," ",docs[[j]])
  docs[[j]]=gsub("\u2028"," ",docs[[j]])
}

#Removing Numbers
docs=tm_map(docs,removeNumbers)

#Converting to lowercase
docs=tm_map(docs,tolower)
docs=tm_map(docs,PlainTextDocument)
DocsCopy=docs

#Removing Stopwords
docs=tm_map(docs,removeWords,stopwords("english"))
docs=tm_map(docs,PlainTextDocument)


#Removing Particular Words
docs=tm_map(docs,removeWords,c("syllogism","tautology"))


#Combining words that should stay together
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)


#Stemming/Removing the common word endings
docs_st <- tm_map(docs, stemDocument)   
docs_st <- tm_map(docs_st, PlainTextDocument)
#writeLines(as.character(docs_st[1]))

#Following lines not working properly
#docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
#docs_stc <- tm_map(docs_stc, PlainTextDocument)
#writeLines(as.character(docs_stc[1]))


#docs=docs_stc

#Stripping unnecessary whitespace
docs=tm_map(docs,stripWhitespace)
docs=tm_map(docs,PlainTextDocument)


#Stage the Data
dtm=DocumentTermMatrix(docs)
#inspect(dtm[1:5,1:20])#view first 5 docs and first 20 terms

#taking transpose of the matrix
tdm=TermDocumentMatrix(docs)

#Organize terms by frequency
freq=colSums(as.matrix(dtm))
freq
ord=order(freq)

#exporting matrix to Excel
m=as.matrix(dtm)
dim(m)

write.csv(m,file = "DocumentTermMatrix.csv")

#removing sparse terms
dtms=removeSparseTerms(dtm,0.2)
dtms

#frequency of remaining terms
freq <- colSums(as.matrix(dtm))


freq <- colSums(as.matrix(dtms))   
freq



#All terms that appear frequently 50 or more times
wf=findFreqTerms(dtm,lowfreq = 50)
wf

#Another way to do it
wf <- data.frame(word=names(freq), freq=freq)   
head(wf) 


#Plotting word frequencies
library(ggplot2)

p = ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Finding correalted terms
findAssocs(dtm,c("country","american"), corlimit = 0.85)


wordcloud::wordcloud(names(freq),freq,min.freq = 25)


