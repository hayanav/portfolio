library(tm)
library(cluster)
library(RCA)
library(igraph)
library(topicmodels)

SearchReplace=content_transformer(function(x,pattern1,pattern2) gsub(pattern1,pattern2,x))
Citizens=read.csv("Government.csv")


ACorp=Corpus(VectorSource(Citizens$Text))
ACorp=tm_map(ACorp,content_transformer(tolower))
ACorp=tm_map(ACorp,removeWords,stopwords("english"))
ACorp=tm_map(ACorp,removePunctuation)
ACorp=tm_map(ACorp,removeNumbers)
ACorp=tm_map(ACorp,SearchReplace,'[\r\n]','') 
ACorp=tm_map(ACorp,SearchReplace,'amp', ' ') 
ACorp=tm_map(ACorp,SearchReplace,'aadhar','aadhaar') 
ACorp=tm_map(ACorp,SearchReplace,' aadhhar ', ' aadhaar ') 
ACorp=tm_map(ACorp,SearchReplace,' addhar ', ' aadhaar ') 
ACorp=tm_map(ACorp,SearchReplace,' adhaar ', ' aadhaar ') 
ACorp=tm_map(ACorp,SearchReplace,' adhar ', ' aadhaar ') 
ACorp=tm_map(ACorp,SearchReplace,' asdhar ', ' aadhaar ') 
ACorp=tm_map(ACorp,SearchReplace,'indian express','indianexpress') 
ACorp=tm_map(ACorp,SearchReplace,'deccan chronicle','deccanchronicle')


ACorp=tm_map(ACorp,SearchReplace,' accusation ', ' accuse ') 
ACorp=tm_map(ACorp,SearchReplace,' accusations ', ' accuse ') 
ACorp=tm_map(ACorp,SearchReplace,' accused ', ' accuse ') 
ACorp=tm_map(ACorp,SearchReplace,' accuses ', ' accuse ') 
ACorp=tm_map(ACorp,SearchReplace,' accusing ', ' accuse ')

term='aadhaar' 
anet1=as.data.frame(findAssocs(ATDM, term, 0.20)) 
anet1=cbind(term,rownames(anet1),anet1) 
colnames(anet1)=c('word1','word2','freq') 
rownames(anet1)=NULL

term='takes' 
anet1=as.data.frame(findAssocs(ATDM, term, 0.20)) 
anet1=cbind(term,rownames(anet1),anet1) 
colnames(anet1)=c('word1','word2','freq') 
rownames(anet1)=NULL

anet2=as.data.frame(rbind(anet1,anet2))

term='updation' 
anet1=as.data.frame(findAssocs(ATDM, term, 0.20)) 
anet1=cbind(term,rownames(anet1),anet1) 
colnames(anet1)=c('word1','word2','freq') 
rownames(anet1)=NULL
anet2=as.data.frame(rbind(anet1,anet2))



AadhaarGraph=graph_from_data_frame(anet2[1:2], directed=F) 
AadhaarGraph=simplify(AadhaarGraph)
plot(AadhaarGraph)
ReyDist=dist(ReyTDM,method='euclidean')
ReyClust=hclust(d=ReyDist, method='ward.D')

plot(ReyClust, yaxt='n', xlab='', ylab='', hang=1, main='', sub='', cex=1.75)

rect.hclust(ReyClust, k=3, border='red2')
ReyTopics=cutree(ReyClust,k=6)

ReyKClust=kmeans(ReyDist, 6) 

ReyKClust$cluster













