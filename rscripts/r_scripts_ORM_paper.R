## import libraries
# note that if you do not have a specific library yet then you should install the library first
# by running install.packages("name_of_library") (e.g. install.packages("tm"))
library(tm)
library(topicmodels)
library(lsa)
library(RWeka)

## text data

vacancy_sent<-c("Ability or Experience in reviewing and authoring Aircraft Flight Manuals, Apps Spec and Pilot's Guides.",
                "Work with KEMP Management to gain approval for new product concepts/ideas.",
                "Handle client queries and/or requests.",
                "3-5 years of supervisory or product management experience required.",
                "Understanding of XML, parsing, send/receive and experience with web services.",
                "Responsible for developing and maintaining quality management procedures and systems",
                "Experience with J2EE technology components (e.g. JSP, Servlets, XML, and Web Services) is a 
                requirement.",
                "Minimum 5 years of experience in Marketing or Product Management roles.",
                "Handling consultant and client queries.",
                "Define customer applications for the product and design product positioning to support these applications.",
                "3-5 years of experience in the Engineering and/or Maintenance field strongly preferred.")

##constructing the corpus
vacancy_corpus<-VCorpus(VectorSource(vacancy_sent))

######
###text data preprocessing example
######
#textpreprocess<-list(function(x) removePunctuation(x, preserve_intra_word_dashes=T),function(x) stemDocument(x, "english"),function(x) removeWords(x, stopwords(
#  "english")),stripWhitespace )
#stripwords<-content_transformer(function(x) removeWords(x, c("eg")))

#vacancy_corpus_new<-tm_map(vacancy_corpus, FUN=tm_reduce, tmFuns=textpreprocess)
vacancy_corpus <- tm_map(vacancy_corpus, content_transformer(function(x) gsub("(?!\\-)[[:punct:]]", " ", x, perl=TRUE)))
vacancy_corpus <- tm_map(vacancy_corpus, content_transformer(tolower))
vacancy_corpus <- tm_map(vacancy_corpus, removeWords, stopwords("smart"))
vacancy_corpus <- tm_map(vacancy_corpus, stemDocument)
vacancy_corpus <- tm_map(vacancy_corpus, stripWhitespace)
vacancy_corpus[[1]]$content

#vacancy_corpus_new<-tm_map(vacancy_corpus_new,stripwords)
inspect(vacancy_corpus)

######
### document by term matrix with raw frequency weighting example
######

vacancy_dtm<-DocumentTermMatrix(vacancy_corpus, control=list(wordLengths=c(2,Inf)))
inspect(vacancy_dtm)

######
### bigram proximity matrix example
######
vacancy_sentbi<-NULL

for(j in 1:11){vacancy_sentbi[j]<-paste(removePunctuation(vacancy_sent[j], 
                                                          preserve_intra_word_dashes=T),"periodsign", sep=" ")}
vacancy_corpusbi<-Corpus(VectorSource(vacancy_sentbi))
inspect(vacancy_corpusbi)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
txtTdmBi <- TermDocumentMatrix(vacancy_corpusbi, control = list(tokenize = 
                                                                  BigramTokenizer,tolower=FALSE,wordLengths=c(1,Inf)))
inspect(txtTdmBi)

######
### latent semantic analysis example
######

## data transformation
#vacancy_dtm<-DocumentTermMatrix(vacancy_corpus_new, control=list(wordLengths=c(1,Inf)))
#inspect(vacancy_dtm)

## apply lsa (using the lsa package)
vacancy_dtm_lsa<-lsa(t(vacancy_dtm), dim=2)

##display the lower rank matrix
vacancy_dtm_lsa$tk %*% diag(vacancy_dtm_lsa$sk) %*% t(vacancy_dtm_lsa$dk)

###
dist.mat.lsa <- proxy::dist(t(as.textmatrix(vacancy_dtm_lsa)), method="cosine")

cosineDist <- function(x){
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}

fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y 
                                                                  )) + geom_text(data = points, aes(x = x, y = y, label = paste("D", 1:11, sep="")))

### Clustering
plot(hclust(dist.mat.lsa, method="ward.D"), labels=paste("D", 1:11, sep=""))

## display the correlation matrix between documents
vacancy_dtm_lowerrank<-vacancy_dtm_lsa$tk %*% diag(vacancy_dtm_lsa$sk) %*% t(vacancy_dtm_lsa$dk)
cor(vacancy_dtm_lowerrank)

######
### topic models example
######
mydtm<-vacancy_dtm
k=3
SEED<-2017

myTM <-
  list(VEM = LDA(mydtm, k = k, control = list(seed = SEED)),
       VEM_fixed = LDA(mydtm, k = k,
                       control = list(estimate.alpha = FALSE, seed = SEED)),
       Gibbs = LDA(mydtm, k = k, method = "Gibbs",
                   control = list(seed = SEED, burnin = 1000,
                                  thin = 100, iter = 1000)),
       CTM = CTM(mydtm, k = k,
                 control = list(seed = SEED,
                                var = list(tol = 10^-4), em = list(tol = 10^-3))))

##display terms for the three topics from applying LDA
terms(myTM$VEM, k=5)
##display document topic assignment from applying CTM
topics(myTM$VEM, k=5)

## display terms for the three topics from applying CTM
terms(myTM$CTM,k=5)
##display document topic assignment from applying CTM
topics(myTM$CTM, k=5)