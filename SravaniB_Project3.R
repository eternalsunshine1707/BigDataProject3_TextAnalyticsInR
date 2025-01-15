#Installing the required Packages for the Project
install.packages("tm")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("SnowballC")
install.packages("stringr")

#Loading the packages as Libraries
library(tm)
library(plyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(stringr)

#Setting the Directory
setwd("C:\\Users\\Sravani\\OneDrive\\Documents\\2024_SPRING\\CS6444 - BIG DATA\\Projects\\Project_3\\eBook")
getwd()
eBook<-readLines("TarzanOfTheApes.txt")
eBook

#Separating the chapters into individual files
#Identifying the indices for chapters
indx_ch1 <- which(eBook == "Chapter I", arr.ind=TRUE)
indx_ch2 <- which(eBook == "Chapter II", arr.ind=TRUE)
indx_ch3 <- which(eBook == "Chapter III", arr.ind=TRUE)
indx_ch4 <- which(eBook == "Chapter IV", arr.ind=TRUE)
indx_ch5 <- which(eBook == "Chapter V", arr.ind=TRUE)
indx_ch6 <- which(eBook == "Chapter VI", arr.ind=TRUE)
indx_ch7 <- which(eBook == "Chapter VII", arr.ind=TRUE)
indx_ch8 <- which(eBook == "Chapter VIII", arr.ind=TRUE)
indx_ch9 <- which(eBook == "Chapter IX", arr.ind=TRUE)
indx_ch10 <- which(eBook == "Chapter X", arr.ind=TRUE)
indx_ch11 <- which(eBook == "Chapter XI", arr.ind=TRUE)
indx_ch12 <- which(eBook == "Chapter XII", arr.ind=TRUE)
indx_ch13 <- which(eBook == "Chapter XIII", arr.ind=TRUE)
indx_ch14 <- which(eBook == "Chapter XIV", arr.ind=TRUE)
indx_ch15 <- which(eBook == "Chapter XV", arr.ind=TRUE)
indx_ch16 <- which(eBook == "Chapter XVI", arr.ind=TRUE)

#Extracting the text for chapters 
chap1 <- eBook[(indx_ch1 + 1):(indx_ch2 - 1)]
chap2 <- eBook[(indx_ch2 + 1):(indx_ch3 - 1)]
chap3 <- eBook[(indx_ch3 + 1):(indx_ch4 - 1)]
chap4 <- eBook[(indx_ch4 + 1):(indx_ch5 - 1)]
chap5 <- eBook[(indx_ch5 + 1):(indx_ch6 - 1)]
chap6 <- eBook[(indx_ch6 + 1):(indx_ch7 - 1)]
chap7 <- eBook[(indx_ch7 + 1):(indx_ch8 - 1)]
chap8 <- eBook[(indx_ch8 + 1):(indx_ch9 - 1)]
chap9 <- eBook[(indx_ch9 + 1):(indx_ch10 - 1)]
chap10 <- eBook[(indx_ch10 + 1):(indx_ch11 - 1)]
chap11 <- eBook[(indx_ch11 + 1):(indx_ch12 - 1)]
chap12 <- eBook[(indx_ch12 + 1):(indx_ch13 - 1)]
chap13 <- eBook[(indx_ch13 + 1):(indx_ch14 - 1)]
chap14 <- eBook[(indx_ch14 + 1):(indx_ch15 - 1)]
chap15 <- eBook[(indx_ch15 + 1):(indx_ch16 - 1)]

#Retrieving the text from the chapters
chap4
chap15

#Creating Directory for chapters
dir.create("CHAPTERS")

#Writing chapters to the file
write.table(chap1, file = "chapters/chapter1.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap2, file = "chapters/chapter2.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap3, file = "chapters/chapter3.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap4, file = "chapters/chapter4.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap5, file = "chapters/chapter5.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap6, file = "chapters/chapter6.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap7, file = "chapters/chapter7.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap8, file = "chapters/chapter8.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap9, file = "chapters/chapter9.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap10, file = "chapters/chapter10.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap11, file = "chapters/chapter11.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap12, file = "chapters/chapter12.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap13, file = "chapters/chapter13.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap14, file = "chapters/chapter14.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(chap15, file = "chapters/chapter15.txt", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

#Creating VCorpus
TOTA<-VCorpus(DirSource("./CHAPTERS",ignore.case=FALSE, mode="text"))
str(TOTA)
inspect(TOTA) #Calculating no. of characters
TOTA

#Extracting the text from the corpus
#Retrieves first document from the folder
etext<-TOTA[[1]]
etext
etext[1] #Content
#Retrieves ninth document from the folder
etext<-TOTA[[9]]
etext
etext[1] #Content

#Creating the Document Term Matrix
TOTA_DTM<-DocumentTermMatrix(TOTA)
TOTA_DTM
inspect(TOTA_DTM)
str(TOTA_DTM)

#Creating the Term Document Matrix
TOTA_TDM<-TermDocumentMatrix(TOTA)
TOTA_TDM
inspect(TOTA_TDM)
str(TOTA_TDM)

#Converting our corpus “TOTA” into a tidy Corpus
TOTA_tidy<-tidy(TOTA)
TOTA_tidy

#Finding 10 longest words in all the chapters
TOTA_Words<-TOTA_tidy %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words
#Finding 10 longest sentences in all the chapters
TOTA_Sentences<-TOTA_tidy %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences

#Finding 10 longest words & sentences in 1st chapter
TOTA_tidy_Chap1<-tidy(TOTA[1])
TOTA_tidy_Chap1
TOTA_Words_Chap1<-TOTA_tidy_Chap1 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap1
TOTA_Sentences_Chap1<-TOTA_tidy_Chap1 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap1

#Finding 10 longest words & sentences in 2nd chapter
TOTA_tidy_Chap2<-tidy(TOTA[8])
TOTA_tidy_Chap2
TOTA_Words_Chap2<-TOTA_tidy_Chap2 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap2
TOTA_Sentences_Chap2<-TOTA_tidy_Chap2 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap2

#Finding 10 longest words & sentences in 3rd chapter
TOTA_tidy_Chap3<-tidy(TOTA[9])
TOTA_tidy_Chap3
TOTA_Words_Chap3<-TOTA_tidy_Chap3 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap3
TOTA_Sentences_Chap3<-TOTA_tidy_Chap3 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap3

#Finding 10 longest words & sentences in 4th chapter
TOTA_tidy_Chap4<-tidy(TOTA[10])
TOTA_tidy_Chap4
TOTA_Words_Chap4<-TOTA_tidy_Chap4 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap4
TOTA_Sentences_Chap4<-TOTA_tidy_Chap4 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap4

#Finding 10 longest words & sentences in 5th chapter
TOTA_tidy_Chap5<-tidy(TOTA[11])
TOTA_tidy_Chap5
TOTA_Words_Chap5<-TOTA_tidy_Chap5 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap5
TOTA_Sentences_Chap5<-TOTA_tidy_Chap5 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap5

#Finding 10 longest words & sentences in 6th chapter
TOTA_tidy_Chap6<-tidy(TOTA[12])
TOTA_tidy_Chap6
TOTA_Words_Chap6<-TOTA_tidy_Chap6 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap6
TOTA_Sentences_Chap6<-TOTA_tidy_Chap6 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap6

#Finding 10 longest words & sentences in 7th chapter
TOTA_tidy_Chap7<-tidy(TOTA[13])
TOTA_tidy_Chap7
TOTA_Words_Chap7<-TOTA_tidy_Chap7 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap7
TOTA_Sentences_Chap7<-TOTA_tidy_Chap7 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap7

#Finding 10 longest words & sentences in 8th chapter
TOTA_tidy_Chap8<-tidy(TOTA[14])
TOTA_tidy_Chap8
TOTA_Words_Chap8<-TOTA_tidy_Chap8 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap8
TOTA_Sentences_Chap8<-TOTA_tidy_Chap8 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap8

#Finding 10 longest words & sentences in 9th chapter
TOTA_tidy_Chap9<-tidy(TOTA[15])
TOTA_tidy_Chap9
TOTA_Words_Chap9<-TOTA_tidy_Chap9 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap9
TOTA_Sentences_Chap9<-TOTA_tidy_Chap9 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap9

#Finding 10 longest words & sentences in 10th chapter
TOTA_tidy_Chap10<-tidy(TOTA[2])
TOTA_tidy_Chap10
TOTA_Words_Chap10<-TOTA_tidy_Chap10 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap10
TOTA_Sentences_Chap10<-TOTA_tidy_Chap10 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap10

#Finding 10 longest words & sentences in 11th chapter
TOTA_tidy_Chap11<-tidy(TOTA[3])
TOTA_tidy_Chap11
TOTA_Words_Chap11<-TOTA_tidy_Chap11 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap11
TOTA_Sentences_Chap11<-TOTA_tidy_Chap11 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap11

#Finding 10 longest words & sentences in 12th chapter
TOTA_tidy_Chap12<-tidy(TOTA[4])
TOTA_tidy_Chap12
TOTA_Words_Chap12<-TOTA_tidy_Chap12 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap12
TOTA_Sentences_Chap12<-TOTA_tidy_Chap12 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap12

#Finding 10 longest words & sentences in 13th chapter
TOTA_tidy_Chap13<-tidy(TOTA[5])
TOTA_tidy_Chap13
TOTA_Words_Chap13<-TOTA_tidy_Chap13 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap13
TOTA_Sentences_Chap13<-TOTA_tidy_Chap13 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap13

#Finding 10 longest words & sentences in 14th chapter
TOTA_tidy_Chap14<-tidy(TOTA[6])
TOTA_tidy_Chap14
TOTA_Words_Chap14<-TOTA_tidy_Chap14 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap14
TOTA_Sentences_Chap14<-TOTA_tidy_Chap14 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap14

#Finding 10 longest words & sentences in 15th chapter
TOTA_tidy_Chap15<-tidy(TOTA[7])
TOTA_tidy_Chap15
TOTA_Words_Chap15<-TOTA_tidy_Chap15 %>% unnest_tokens(word, text) %>% select(id, word) %>% mutate(word_length=nchar(word)) %>% arrange(desc(word_length))
TOTA_Words_Chap15
TOTA_Sentences_Chap15<-TOTA_tidy_Chap15 %>% unnest_tokens(sentence, text, token = "regex", pattern = "(?<!\\b\\p{L}r)\\.") %>% select(id, sentence) %>% mutate(sentence_length=nchar(sentence)) %>% arrange(desc(sentence_length))
TOTA_Sentences_Chap15


#Data cleaning - Removing punctuations
TOTA<-tm_map(TOTA, content_transformer(gsub), pattern="'", replacement="")
removeNumPunct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
TOTA_cl<-tm::tm_map(TOTA, content_transformer(removeNumPunct))
TOTA_cl
str(TOTA_cl)
inspect(TOTA_cl)

#Ensuring everything is in lowercase
TOTA_Low<-tm_map(TOTA_cl, tm::content_transformer(tolower))
TOTA_Low
str(TOTA_Low)
inspect(TOTA_Low)

#Computing Document Term Matrix for the clean corpus
TOTA_DTM<-DocumentTermMatrix(TOTA_Low)
TOTA_DTM
str(TOTA_DTM)
inspect(TOTA_DTM)

#Converting Document Term Matrix to a Regular Matrix to visualize the sparsity
TOTA_Matrix<-as.matrix(TOTA_DTM)
TOTA_Matrix

#Removing stop words
myStopWords<-c(tm::stopwords("english"))
myStopWords
TOTA_Stop<-tm::tm_map(TOTA_Low,tm::removeWords,myStopWords)
tm::inspect(TOTA_Stop[[1]])

#Computing Term Document Matrix after removing stop words
TOTA_Stop_TDM<-tm::TermDocumentMatrix(TOTA_Stop)
TOTA_Stop_TDM

#Finding frequent terms
#Frequency: 5
freqTerms<-tm::findFreqTerms(TOTA_Stop_TDM, lowfreq = 5)
freqTerms

#Frequency: 8
freqTerms<-tm::findFreqTerms(TOTA_Stop_TDM, lowfreq = 8)
freqTerms

#Frequency: 11
freqTerms<-tm::findFreqTerms(TOTA_Stop_TDM, lowfreq = 11)
freqTerms

#Finding length of a string in frequent terms
nchar(freqTerms[361])
freqTerms[361]

#Computing term frequencies for each document
tfList<-list()
for(i in seq_along(TOTA_Stop)){
  tfList[[i]]<-termFreq(TOTA_Stop[[i]])
}
print(tfList[[1]])
print(tfList[[2]])
print(tfList[[3]])
print(tfList[[4]])
print(tfList[[5]])
print(tfList[[6]])
print(tfList[[7]])
print(tfList[[8]])
print(tfList[[9]])
print(tfList[[10]])
print(tfList[[11]])
print(tfList[[12]])
print(tfList[[13]])
print(tfList[[14]])
print(tfList[[15]])

#Inspecting TOTA_Stop_TDM again
tm::inspect(TOTA_Stop_TDM)

#Generating Dendrogram
TOTA_df<-as.data.frame(TOTA_Stop_TDM[[1]])
TOTA_Dist<-dist(TOTA_df)
TOTA_DG<-hclust(TOTA_Dist, method="ward.D2")
str(TOTA_DG)
plot(TOTA_DG)

#Eliminating some more sparse words
TOTA_Stop_TDM <- removeSparseTerms(TOTA_Stop_TDM, sparse = 0.85)
TOTA_Stop_TDM
tm::inspect(TOTA_Stop_TDM)
TOTA_Stop_TDM <- removeSparseTerms(TOTA_Stop_TDM, sparse = 0.5)
TOTA_Stop_TDM
tm::inspect(TOTA_Stop_TDM)

#Generating Dendrogram
TOTA_df<-as.data.frame(TOTA_Stop_TDM[[1]])
TOTA_Dist<-dist(TOTA_df)
TOTA_DG<-hclust(TOTA_Dist, method="ward.D2")
str(TOTA_DG)
plot(TOTA_DG)

#WORD CLOUD
#Getting sset of words from TOTA
tf_combined<-unlist(tfList)
tf_summed<-tapply(tf_combined, names(tf_combined), sum)
words<-names(tf_summed)
words

#Generating word cloud
pal<-brewer.pal(9,"BuGn")
str(pal)
TOTA_WC<-wordcloud(words, tf_summed, colors=pal[-(1:4)])
TOTA_WC<-wordcloud(words, tf_summed, min.freq = 10, colors=pal[-(1:8)])

#Word cloud with different pallet
pal2<-brewer.pal(9,"Spectral")
TOTA_WC<-wordcloud(words, tf_summed, colors = pal2)

install.packages("quanteda")
library(quanteda)

#Printing out the first few lines of the first document
TOTA_Text<-TOTA_cl[[1]]
TOTA_Text$content[1:10]

#kwic
TOTATokens <-lapply(TOTA_cl,function(x) quanteda::tokens(x$content))
TOTATokens
str(TOTATokens)

kwic(TOTATokens[[4]], pattern="against")
kwic(TOTATokens[[9]], pattern="him")
kwic(TOTATokens[[2]], pattern="likely")
kwic(TOTATokens[[13]], pattern="intelligence")


TOTA_DFM_Chap1 <- quanteda::dfm(TOTATokens[[1]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap2 <- quanteda::dfm(TOTATokens[[2]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap3 <- quanteda::dfm(TOTATokens[[3]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap4 <- quanteda::dfm(TOTATokens[[4]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap5 <- quanteda::dfm(TOTATokens[[5]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap6 <- quanteda::dfm(TOTATokens[[6]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap7 <- quanteda::dfm(TOTATokens[[7]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap8 <- quanteda::dfm(TOTATokens[[8]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap9 <- quanteda::dfm(TOTATokens[[9]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap10 <- quanteda::dfm(TOTATokens[[10]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap11 <- quanteda::dfm(TOTATokens[[11]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap12 <- quanteda::dfm(TOTATokens[[12]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap13 <- quanteda::dfm(TOTATokens[[13]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap14 <- quanteda::dfm(TOTATokens[[14]]) %>% dfm_remove(myStopWords)
TOTA_DFM_Chap15 <- quanteda::dfm(TOTATokens[[15]]) %>% dfm_remove(myStopWords)
str(TOTA_DFM_Chap1)
str(TOTA_DFM_Chap2)
str(TOTA_DFM_Chap3)

#Getting the frequency of terms in DFM
TOTADocFreq_Chap1 <- quanteda::docfreq(TOTA_DFM_Chap1)
TOTADocFreq_Chap2 <- quanteda::docfreq(TOTA_DFM_Chap2)
TOTADocFreq_Chap3 <- quanteda::docfreq(TOTA_DFM_Chap3)
TOTADocFreq_Chap4 <- quanteda::docfreq(TOTA_DFM_Chap4)
TOTADocFreq_Chap5 <- quanteda::docfreq(TOTA_DFM_Chap5)
TOTADocFreq_Chap6 <- quanteda::docfreq(TOTA_DFM_Chap6)
TOTADocFreq_Chap7 <- quanteda::docfreq(TOTA_DFM_Chap7)
TOTADocFreq_Chap8 <- quanteda::docfreq(TOTA_DFM_Chap8)
TOTADocFreq_Chap9 <- quanteda::docfreq(TOTA_DFM_Chap9)
TOTADocFreq_Chap10 <- quanteda::docfreq(TOTA_DFM_Chap10)
TOTADocFreq_Chap11 <- quanteda::docfreq(TOTA_DFM_Chap11)
TOTADocFreq_Chap12 <- quanteda::docfreq(TOTA_DFM_Chap12)
TOTADocFreq_Chap13 <- quanteda::docfreq(TOTA_DFM_Chap13)
TOTADocFreq_Chap14 <- quanteda::docfreq(TOTA_DFM_Chap14)
TOTADocFreq_Chap15 <- quanteda::docfreq(TOTA_DFM_Chap15)
str(TOTADocFreq_Chap5)
TOTADocFreq_Chap5

#Assigning weights to these words
TOTAWeights_Chap1 <- quanteda::dfm_weight(TOTA_DFM_Chap1)
TOTAWeights_Chap2 <- quanteda::dfm_weight(TOTA_DFM_Chap2)
TOTAWeights_Chap3 <- quanteda::dfm_weight(TOTA_DFM_Chap3)
TOTAWeights_Chap4 <- quanteda::dfm_weight(TOTA_DFM_Chap4)
TOTAWeights_Chap5 <- quanteda::dfm_weight(TOTA_DFM_Chap5)
TOTAWeights_Chap6 <- quanteda::dfm_weight(TOTA_DFM_Chap6)
TOTAWeights_Chap7 <- quanteda::dfm_weight(TOTA_DFM_Chap7)
TOTAWeights_Chap8 <- quanteda::dfm_weight(TOTA_DFM_Chap8)
TOTAWeights_Chap9 <- quanteda::dfm_weight(TOTA_DFM_Chap9)
TOTAWeights_Chap10 <- quanteda::dfm_weight(TOTA_DFM_Chap10)
TOTAWeights_Chap11 <- quanteda::dfm_weight(TOTA_DFM_Chap11)
TOTAWeights_Chap12 <- quanteda::dfm_weight(TOTA_DFM_Chap12)
TOTAWeights_Chap13 <- quanteda::dfm_weight(TOTA_DFM_Chap13)
TOTAWeights_Chap14 <- quanteda::dfm_weight(TOTA_DFM_Chap14)
TOTAWeights_Chap15 <- quanteda::dfm_weight(TOTA_DFM_Chap15)
str(TOTAWeights_Chap15)
TOTAWeights_Chap15

#Computing the TF-IDF score.
TOTA_TFIDF_Chap1 <- quanteda::dfm_tfidf(TOTA_DFM_Chap1,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap2 <- quanteda::dfm_tfidf(TOTA_DFM_Chap2,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap3 <- quanteda::dfm_tfidf(TOTA_DFM_Chap3,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap4 <- quanteda::dfm_tfidf(TOTA_DFM_Chap4,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap5 <- quanteda::dfm_tfidf(TOTA_DFM_Chap5,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap6 <- quanteda::dfm_tfidf(TOTA_DFM_Chap6,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap7 <- quanteda::dfm_tfidf(TOTA_DFM_Chap7,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap8 <- quanteda::dfm_tfidf(TOTA_DFM_Chap8,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap9 <- quanteda::dfm_tfidf(TOTA_DFM_Chap9,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap10 <- quanteda::dfm_tfidf(TOTA_DFM_Chap10,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap11 <- quanteda::dfm_tfidf(TOTA_DFM_Chap11,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap12 <- quanteda::dfm_tfidf(TOTA_DFM_Chap12,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap13 <- quanteda::dfm_tfidf(TOTA_DFM_Chap13,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap14 <- quanteda::dfm_tfidf(TOTA_DFM_Chap14,scheme_tf = "count", scheme_df="inverse")
TOTA_TFIDF_Chap15 <- quanteda::dfm_tfidf(TOTA_DFM_Chap15,scheme_tf = "count", scheme_df="inverse")
str(TOTA_TFIDF_Chap7)
TOTA_TFIDF_Chap7

#str(TOAToken)
install.packages("syuzhet")
library(syuzhet)

#Extracting the text as data frame
TOTA_df <- as.data.frame(TOTA_cl[[1]]$content)
TOTA_df

#Reading the file as one large string
TOTA_asString <- get_text_as_string("C:\\Users\\Sravani\\OneDrive\\Documents\\2024_SPRING\\CS6444 - BIG DATA\\Projects\\Project_3\\eBook\\TarzanOfTheApes.txt")
TOTA_asString

#Getting sentences-parses the text string into individual sentences.
TOTA_sentence <- get_sentences(TOTA_asString)
TOTA_sentence[1:10]
str(TOTA_sentence)

#Different sentiment analysis methods in R to analyze the sentiment of the text stored in TOTA_sentence
TOTA_Sentiment <- get_sentiment(TOTA_sentence,"syuzhet")
TOTA_Sentiment
TOTA_Bing <- get_sentiment(TOTA_sentence,"bing")
TOTA_Bing
TOTA_Afinn <- get_sentiment(TOTA_sentence,"afinn")
TOTA_Afinn
TOTA_nrc <- get_sentiment(TOTA_sentence,"nrc")
TOTA_nrc

#Sentiment dictionary for “syuzhet”
TOTA_Dictionary <- get_sentiment_dictionary()
TOTA_Dictionary
#First part of the Bing sentiment Dictionary
TOTA_Dictionary_Bing <- get_sentiment_dictionary("bing")
TOTA_Dictionary_Bing
#First part of the Afinn sentiment Dictionary
TOTA_Dictionary_Afinn <- get_sentiment_dictionary("afinn")
TOTA_Dictionary_Afinn
#First part of the nrc sentiment Dictionary
TOTA_Dictionary_nrc <- get_sentiment_dictionary("nrc")
TOTA_Dictionary_nrc

#Finding the sum of sentiment scores for sentiment analysis results obtained from the 'syuzhet' package to get a measure of the overall emotional valence in the text
TOTA_Sum <- sum(TOTA_Sentiment)
TOTA_Sum
#Sum of sentiment scores obtained from the Bing method.
TOTA_BingSum <- sum(TOTA_Bing)
TOTA_BingSum
#Sum of sentiment scores obtained using Afinn method
TOTA_AfinnSum <- sum(TOTA_Afinn)
TOTA_AfinnSum
#Sum of sentiment scores obtained using nrc method
TOTA_nrcSum <- sum(TOTA_nrc)
TOTA_nrcSum

#Finding the mean of sentiment scores for sentiment analysis results obtained from the 'syuzhet' package 
TOTA_Mean <- mean(TOTA_Sentiment)
TOTA_Mean
#Mean of sentiment scores obtained using the Bing method
TOTA_BingMean <- mean(TOTA_Bing)
TOTA_BingMean
#Mean of sentiment scores obtained using the Afinn method
TOTA_AfinnMean <- mean(TOTA_Afinn)
TOTA_AfinnMean
#Mean of sentiment scores obtained using the nrc method
TOTA_nrcMean <- mean(TOTA_nrc)
TOTA_nrcMean

#Summary results of the sentiments generated from these dictionaries to understand the distribution of sentiment
summary(TOTA_Sentiment)
summary(TOTA_Bing)
summary(TOTA_Afinn)
summary(TOTA_nrc)

#Plotting to understand how the sentiment of text changes from the beginning of the text till the end
plot(TOTA_Sentiment, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")
plot(TOTA_Bing, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")
plot(TOTA_Afinn, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")
plot(TOTA_nrc, main="Tarzan of the Apes", xlab="Narrative", ylab="Emotional Valence")


#Creating plots to visualize the distribution of the emotional valence of "Tarzan of the Apes" based on sentiment scores, grouped into bins
TOTA_SentimentPctValue <- get_percentage_values(TOTA_Sentiment , bins=10)
structure(TOTA_SentimentPctValue)
plot(TOTA_SentimentPctValue, main="Tarzan of the Apes" , xlab="Narrative" , ylab="Emotional Valence" , col="green")

TOTA_SentimentPctValue <- get_percentage_values(TOTA_Sentiment , bins=20)
structure(TOTA_SentimentPctValue)
plot(TOTA_SentimentPctValue, main="Tarzan of the Apes" , xlab="Narrative" , ylab="Emotional Valence" , col="maroon")

TOTA_SentimentPctValue <- get_percentage_values(TOTA_Sentiment , bins=30)
structure(TOTA_SentimentPctValue)
plot(TOTA_SentimentPctValue, main="Tarzan of the Apes" , xlab="Narrative" , ylab="Emotional Valence" , col="black")