library(readr)
library(tm)
library(SnowballC)
library(wordcloud)
library(wordcloud2)
library(readxl)
Brexit <- read_excel("Brexit2017.xlsx")

Brexit2017=Corpus(VectorSource(Brexit$...17))

Brexit2017=tm_map(Brexit2017,content_transformer(tolower)) 
Brexit2017=tm_map(Brexit2017, removeWords, stopwords('english')) 
Brexit2017=tm_map(Brexit2017, removePunctuation) 
Brexit2017=tm_map(Brexit2017, removeNumbers) 
Brexit2017=tm_map(Brexit2017, stripWhitespace)
Brexit2017=tm_map(Brexit2017, StripString, 'http[[:alnum:]]*')

View(as.matrix(TermDocumentMatrix(Brexit2017)))
m <- DocumentTermMatrix(Brexit2017)
findFreqTerms(m, 40)


wordcloud(Brexit$...17)
(Brexit$...17=Corpus(DirSource('Tweets')))


wordcloud(Brexit$...17, min.freq=50, max.words=100, vfont=c('gothic italian','plain'), random.order=FALSE, scale=c(5,0.5), rot.per=.3, color=brewer.pal(8,'Dark2')[c(2,4,8)]) 

wordcloud(Brexit$...20, min.freq=50, max.words=100, vfont=c('gothic italian','plain'), random.order=FALSE, scale=c(5,0.5), rot.per=.3, color=brewer.pal(8,'Dark2')[c(2,4,8)]) 


#q2

library(syuzhet)
BrexitPolarity=data.frame(Brexit$...17, Syuzhet=get_sentiment(Brexit$...17,method='syuzhet')
                          BrexitPolarity <-BrexitPolarity %>%
                            mutate(Emotion = ifelse(Syuzhet>0, 'Positive', ifelse(Suzhet<0, 'Negative', 'Neutral')))
BrexitPolarity
 

#BrexitFreq                         
BrexitFreq= colSums(as.matrix(DTM)) ,
BrexitFreq=sort(BrexitFreq, decreasing =T),
cbind(BrexitFreq),
grayLevels=gray((BrexitFreq+10)/(max(BrexitFreq)+10)),

wordcloud(words=names(BrexitFreq), freq=BrexitFreq, rot.per = 0.25, max.words=40, random.order=FALSE, vfont=c(script, bold), color=gray)









library(syuzhet)
BrexitPolarity=data.frame(Brexit$...17, Syuzhet=get_sentiment(Brexit$...17,method='syuzhet')
                          BrexitPolarity <-SpeachPolarity %>%
                            mutate(Emotion = ifelse(Syuzhet>0, 'Positive', ifelse(Suzhet<0, 'Negative', 'Neutral')))
view(BrexitPolarity)
                          







library(syuzhet)
BrexitPolarity=data.frame(Syuzhet=get_sentiment(Brexit$...17,method='syuzhet'),
                          BrexitPolarity <-SpeachPolarity %>%
                            mutate(Emotion = ifelse(Syuzhet>0, 'Positive', ifelse(Suzhet<0, 'Negative', 'Neutral')))
view(BrexitPolarity)
                          





library(syuzhet)
PosNeg=data.frame(text = sapply(Brexit2017, as.character),
                    stringsAsFactors = FALSE)

Sentiment=get_sentiment(PosNeg$text)

#q3

BrexitPolarity<- BrexitPolarity[order(BrexitPolarity),]
view(BrexitPolarity)

Top10Neg=head(BrexitPolarity, 10)
Top10Neg

Top10Pos=tail(BrexitPolarity, 10)
Top10Pos







SpeechEmot=get_nrc_sentiment(PosNeg$text)
SpeechEmot=data.frame(Speech=substr(as.character(row.names(PosNeg$text)),1,
                                    nchar(as.character(row.names(PosNeg$text)))-4),SpeechEmot)


SpeechEmot=Corpus(VectorSource(SpeechEmot)) 
BrexitTDM=TermDocumentMatrix("Brexit2017.xlsx") 
BrexitTDM=removeSparseTerms(BrexitTDM,0.99) 

colnames(BrexitTDM)=c('Anger','Disgust','Fear','Sadness','Surprise','Joy', 
                                                                  'Anticipation','Trust') 
BrexitTDM=as.matrix(BrexitTDM) 
  comparison.cloud(BrexitTDM, max.words=2000, vfont=c('serif','plain'), random.order=F, rot.per=.25, scale=c(2.5,.5), title.size=1.5) 






letterCloud(Brexit$...17, 'Tweets')

for(i in 1:nrow(SpeechEmot)) 
{ SpeechEmot$Emotion[i]= ifelse((table(as.numeric(SpeechEmot[i,3:10]))[names(table(as.numeric (SpeechEmot
        [i,3:10]))) == max(SpeechEmot[i,3:10])])==1, 'FindEmotion','') 
        for(column in 3:10)
        { 
      SpeechEmot$Emotion[i]=ifelse(SpeechEmot$Emotion[i] == 'FindEmotion', 
      ifelse(SpeechEmot[i,column] == max(SpeechEmot[i,3:10]), 
      colnames(SpeechEmot[column]), SpeechEmot$Emotion[i]),SpeechEmot$Emotion[i]) } } 

BrexitCompareEmot=c(anger=paste(subset(SpeechEmot, Emotion=='Anger')$...17, sep='\n', collapse=' '), paste(subset(SpeechEmot, Emotion=='Disgust')$Tweet, sep='\n', collapse=' ')) 
BrexitCompareEmot=c(BrexitCompareEmot, paste(subset(SpeechEmot, Emotion=='Fear')$...17, sep='\n', collapse=' ')) 
BrexitCompareEmot=c(BrexitCompareEmot, paste(subset(SpeechEmot,Emotion=='Sadness')$...17, sep='\n',collapse=' '))

BrexitCompareEmot=c(BrexitCompareEmot, paste(subset(SpeechEmot,Emotion=='Surprise')$...17, sep='\n',collapse=' ')) 
BrexitCompareEmot=c(BrexitCompareEmot, paste(subset(SpeechEmot,Emotion=='Joy')$...17, sep='\n', collapse=' ')) 
BrexitCompareEmot=c(BrexitCompareEmot, paste(subset(SpeechEmot,Emotion=='Anticipation')$...17, sep='\n',collapse=' ')) 
BrexitCompareEmot=c(BrexitCompareEmot,paste(subset(SpeechEmot,Emotion=='Trust')$...17, sep='\n',collapse=' '))


BrexitCompCorp=Corpus(VectorSource(BrexitCompareEmot)) 
BrexitCompTDM=TermDocumentMatrix(BrexitCompCorp) 
BrexitCompTDM=removeSparseTerms(BrexitCompTDM,0.99) 
colnames(BrexitCompTDM)=c('Anger','Disgust','Fear','Sadness','Surprise','Joy',
                          'Anticipation','Trust') 
BrexitCompTDM=as.matrix(BrexitCompTDM)
comparison.cloud(BrexitCompTDM, max.words=2000, vfont=c('serif','plain'), random.order=F, rot.per=.25, scale=c(2.5,.5), title.size=1.5)



comparison.cloud(as.matrix(TermDocumentMatrix(Brexit$...17, control=list(removePunctuation=T, weighting=weightTfIdf))), 
                 colors=brewer.pal(9,'Set1'), title.size= 1.5, vfont=c('serif','italic'))

commonality.cloud(as.matrix(TermDocumentMatrix(Brexit$...17, control=list(removePunctuation=T))), 
                  colors='slateblue3')




CasinoDTM=removeSparseTerms(CasinoDTM,0.1)
findFreqTerms(CasinoDTM, 500)


