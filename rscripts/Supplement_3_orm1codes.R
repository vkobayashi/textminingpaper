#########################################
### LOAD THE TRAINING DATA              #
#########################################

### Load the training data
### Data "jobactivityall.txt" and "jobattributell.txt"
### should be requested from the Authors.

jobact_data<-read.table("jobactivityall.txt", header=F, sep=",", strip.white=TRUE,colClasses="character")
jobattr_data<-read.table("jobattributeall.txt", header=F, sep=",", strip.white=TRUE,colClasses="character")

### Assign column names
names(jobact_data)<-c("jobid", "description")
names(jobattr_data)<-c("jobid", "description")

#########################################
### PREPROCESSING                       #
#########################################
library(tm)
library(stringr)
### Stopwords
mystopwords<-tm::stopwords("en")[-which(tm::stopwords("en") %in% c("to", "have", "has", "had", "must","can", "could", "may","might", "shall","should","will","would", "able"))]

### Function to transform each sentence
sent_transform<-function(sent){
  sent<-tolower(sent) # lower case
  sent<-removeWords(sent, mystopwords) # remove words in mystopwords
  sent <- gsub("[^[:alnum:][:space:]\\+\\#\\.\\-]", " ", sent)
  #sent<-removePunctuation(sent, preserve_intra_word_dashes=TRUE) # remove punctuation except intra word dashes e.g. pro-active
  sent <-str_replace_all(sent, "\\.(?<!\\b)", "")
  sent<-stripWhitespace(sent) # remove extra white spaces between words
  sent<-str_trim(sent) # remove trailing white spaces
  return(sent) # return the preprocessed sentence
}

### Prepare sentence annotator for part-of-speech (pos) tagging
library(openNLP)
library(openNLPmodels.en)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

### Apply sentence transformation
jobact_transform<-sapply(jobact_data$description, sent_transform, USE.NAMES=FALSE)
jobact_transform<-jobact_transform[jobact_transform != ""] # remove empty rows

jobattr_transform<-sapply(jobattr_data$description, sent_transform, USE.NAMES=FALSE)
jobattr_transform<-jobattr_transform[jobattr_transform != ""] # remove empty rows

### Combine worker attributes and work activities sentences
combine_job <- c(jobact_transform, jobattr_transform)

### Function to get the pos tags of the words
fftags<-function(sent){
  if(str_count(as.String(sent), "\\S+") == 1 ) { return("noun")} # tag of single word sentence will be noun
  else {a2 <- annotate(sent, list(sent_token_annotator, word_token_annotator))
  a3 <- annotate(sent, pos_tag_annotator, a2)
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  tags<-replace(tags, which(tags %in% c("VBD", "VB", "VBG", "VBN","VBP","VBZ")), "verb")
  tags<-replace(tags, which(tags %in% c("NN", "NNS", "NNP", "NNPS")), "noun")
  tags<-replace(tags, which(tags %in% c("JJ", "JJR", "JJS")), "adjective")
  tags<-replace(tags, which(tags %in% c("RB", "RBR", "RBS")), "adverb")
  tags<-replace(tags, which(tags %in% c("PRP", "PRP$")), "pronoun")
  tags<-tags[! tags %in% c(".", ",")]
  return(tags)}
}

tags_act <- lapply(jobact_transform, fftags)
tags_attr <- lapply(jobattr_transform, fftags)

combine_job_tags <- c(tags_act,tags_attr)
#########################################
### FEATURE ANALYSIS                    #
#########################################

### First word and pos tag of the first word; work activities
firstword_jobact_pos<-character()
lastword_jobact_pos<-character()
firstword_jobact_actual<-character()
lastword_jobact_actual<-character()
for(i in 1:length(jobact_transform)){
  firstword_jobact_pos<-append(firstword_jobact_pos, tags_act[[i]][1])
  lastword_jobact_pos<-append(lastword_jobact_pos, tags_act[[i]][length(tags_act[[i]])])
  firstword_jobact_actual<-append(firstword_jobact_actual, word(jobact_transform[i],1))
  lastword_jobact_actual<-append(lastword_jobact_actual, word(jobact_transform[i],-1))   
}

### First word and its tag; worker attributes
firstword_jobattr_pos<-character()
lastword_jobattr_pos<-character()
firstword_jobattr_actual<-character()
lastword_jobattr_actual<-character()
for(i in 1:length(jobattr_transform)){
  firstword_jobattr_pos<-append(firstword_jobattr_pos, tags_attr[[i]][1])
  lastword_jobattr_pos<-append(lastword_jobattr_pos, tags_attr[[i]][length(tags_attr[[i]])])
  firstword_jobattr_actual<-append(firstword_jobattr_actual, word(jobattr_transform[i],1))
  lastword_jobattr_actual<-append(lastword_jobattr_actual, word(jobattr_transform[i],-1))   
}

### pos tags of first words for work activities and worker attributes sentences

sort(table(sapply(tags_act,"[[",1)))
sort(table(sapply(tags_attr,"[[",1)))

# no doubt most work activity sentences starts with a verb and
# worker attribute sentences either start with a noun or an adjective.

### Construct a corpus for each job type information. We will use the corpus to find frequent terms and other statistics.

jobact_corpus<-VCorpus(VectorSource(jobact_transform))
jobattr_corpus<-VCorpus(VectorSource(jobattr_transform))

jobact_dtm<- DocumentTermMatrix(jobact_corpus, control=list(wordLengths = c(2, Inf)))
jobattr_dtm<- DocumentTermMatrix(jobattr_corpus, control=list(wordLengths = c(2, Inf)))

### Investigate the frequent terms

frequent_act<-findFreqTerms(jobact_dtm, lowfreq=4)
frequent_attr<-findFreqTerms(jobattr_dtm, lowfreq=3)

### Unique frequent terms
unique_frequent_jobact<-setdiff(frequent_act,frequent_attr)

unique_frequent_jobattr<-setdiff(frequent_attr,frequent_act)

### Unique first and last words
unique_firstword_jobact<-unique(firstword_jobact_actual[! firstword_jobact_actual %in% firstword_jobattr_actual])

unique_firstword_jobattr<-unique(firstword_jobattr_actual[! firstword_jobattr_actual %in% firstword_jobact_actual])

unique_lastword_jobact<-unique(lastword_jobact_actual[! lastword_jobact_actual %in% lastword_jobattr_actual])

unique_lastword_jobattr<-unique(lastword_jobattr_actual[! lastword_jobattr_actual %in% lastword_jobact_actual])

### Keywords

frequent_terms_act <- findFreqTerms(jobact_dtm, lowfreq=30)
frequent_terms_attr <- findFreqTerms(jobattr_dtm, lowfreq=30)

mydict <- unique(c(frequent_terms_act, frequent_terms_attr))

#########################################
### FEATURE CONSTRUCTION
#########################################

### Get the pos tag of the first word
ff1 <- function(sent_tags){
  return(sent_tags[1])
}

### First word unique in work activity?
ff2<-function(sent){
  if(word(sent,1) %in% unique_firstword_jobact) return(1)
  else return (0)
}

### First word unique in worker attribute?
ff3<-function(sent){
  if(word(sent,1) %in% unique_firstword_jobattr) return(1)
  else return(0)
}

### Last word unique in work activity?
ff4<-function(sent){
  if(word(sent,-1) %in% unique_lastword_jobact) return(1)
  else return(0)
}

### Last word unique in worker attribute?
ff5<-function(sent){
  if(word(sent,-1) %in% unique_lastword_jobattr) return(1)
  else return(0)
}

### Proportion of Adjectives
ff6<-function(sent_tags){
  return(sum(sent_tags=="adjective")/max(table(sent_tags),na.rm=TRUE))
}

### Proportion of Verbs
ff7<-function(sent_tags){
  return(sum(sent_tags=="verb")/max(table(sent_tags),na.rm=TRUE))
}

### Proportion of TOs
ff8<-function(sent_tags){
  return(sum(sent_tags=="TO")/max(table(sent_tags),na.rm=TRUE))
}

### Proportion of MDs
ff9<-function(sent_tags){
  return(sum(sent_tags=="MD")/max(table(sent_tags),na.rm=TRUE))
}

### Proportion of CDs
ff10<-function(sent_tags){
  return(sum(sent_tags=="CD")/max(table(sent_tags),na.rm=TRUE))
}


### Proportion of Adverbs
ff11<-function(sent_tags){
  return(sum(sent_tags=="adverb")/max(table(sent_tags),na.rm=TRUE))
}

### Proportion of Nouns
ff12<-function(sent_tags){
  return(sum(sent_tags=="noun")/max(table(sent_tags),na.rm=TRUE))
}

### Proportion of Verbs followed by another pos tag
ff13<-function(sent_tags){
  vector_tags<-sent_tags
  index_verbs<-which(vector_tags=="verb")
  index_verbs<-index_verbs[! index_verbs==length(vector_tags)]
  if(length(index_verbs)==0) return(c(0,0,0,0,0))
  else {
    follow_verb<-c(vector_tags[index_verbs+1])
    return(c(sum(follow_verb=="noun"),sum(follow_verb=="verb"),sum(follow_verb=="adjective"), sum(follow_verb=="adverb"), sum(!(follow_verb %in% c("noun", "verb", "adjective","adverb"))))/length(index_verbs))
  }
}

### unique words
ff14<-function(sent){
  vec_sent <- str_split(sent, " ")
  return(c(sum(vec_sent[[1]] %in% unique_frequent_jobact),sum(vec_sent[[1]] %in% unique_frequent_jobattr))/length(vec_sent[[1]]))
}

### Keywords

mycorpus <- VCorpus(VectorSource(combine_job))

job_freq_dtm <- DocumentTermMatrix(mycorpus, control=list(dictionary=mydict,wordLengths = c(2, Inf) ))
inspect(job_freq_dtm)

job_freq_dtm_mat <- as.matrix(job_freq_dtm)


#########################################
### CONSTRUCT THE FEATURE MATRIX
#########################################

fflist <- list()

for(i in 1:length(combine_job)){
  if(length(combine_job_tags[[i]])==0){print(i)}
  featurevector<-c(ff1(combine_job_tags[[i]]),ff2(combine_job[i]), ff3(combine_job[i]),ff4(combine_job[i]),ff5(combine_job[i]),
                   ff6(combine_job_tags[[i]]),ff7(combine_job_tags[[i]]),
                   ff8(combine_job_tags[[i]]),ff9(combine_job_tags[[i]]),ff10(combine_job_tags[[i]]),
                   ff11(combine_job_tags[[i]]),ff12(combine_job_tags[[i]]),
                   ff13(combine_job_tags[[i]]),ff14(combine_job[i]))
  fflist[[i]]<-featurevector
}

ffmat <- do.call("rbind", fflist)
ffmat <- cbind(ffmat,job_freq_dtm_mat,c(rep(1,1790), rep(0,2130)))

traindataframe<-as.data.frame(ffmat, stringsAsFactors = FALSE)

traindataframe[,2:168] <- lapply(traindataframe[,2:168], as.numeric)
traindataframe[,c(1,169)]<-lapply(traindataframe[,c(1,169)], as.factor)
names(traindataframe)[110] <- "problem_solving"

summary(traindataframe)

#########################################
### BUILD CLASSIFIERS                  #
#########################################
library(TunePareto)
library(randomForest) # random forest
library(kernlab) # support vector machines
library(e1071)

### Evaluation

indexcv<-generateCVRuns(labels=c(rep(1,1790), rep(0,2130)), ntimes=1, nfold=10, stratified=TRUE)

svm_accu <- NULL
lr_accu <- NULL
nb_accu <- NULL
rf_accu<- NULL
for(j in 1:10){
  print(j)
  
  train<-traindataframe[-indexcv[[1]][[j]],]
  test<-traindataframe[indexcv[[1]][[j]],]

  lr_model <- glm(V169 ~.,family=binomial(link='logit'),data=train)
  rf_model<-randomForest(V169~., data=train, importance=TRUE, ntree=1000)
  nb_model <- naiveBayes(V169~., data=train, laplace=.1)
  svm_model <- ksvm(V169~., data=train, type= "C-svc", kernel="polydot", kpar= list(degree=1, scale=1, offset=0), C=1,
                    prob.model=TRUE)
  
  lr_tabular <- table(ifelse(predict(lr_model, test)>0.5,1,0), test$V169)
  rf_tabular<-table(predict(rf_model, test), test$V169)
  nb_tabular<-table(predict(nb_model, test), test$V169)
  svm_tabular<-table(predict(svm_model, test), test$V169)
  
  rf_accu<-append(rf_accu,(rf_tabular[1,1]+rf_tabular[2,2])/nrow(test))
  nb_accu<-append(nb_accu,(nb_tabular[1,1]+nb_tabular[2,2])/nrow(test))
  svm_accu<-append(svm_accu,(svm_tabular[1,1]+svm_tabular[2,2])/nrow(test))
  lr_accu <- append(lr_accu,(lr_tabular[1,1]+lr_tabular[2,2])/nrow(test))
}
mean(nb_accu)
mean(svm_accu)
mean(rf_accu)
mean(lr_accu)

### Random Forest
rf_model<-randomForest(V169~., data=traindataframe, importance=TRUE, ntree=1000) #save this

### Naive Bayes
nb_model <- naiveBayes(V169~., data=traindataframe, laplace=.1) #save this

### Support Vector Machines
svm_model <- ksvm(V169~., data=traindataframe, type= "C-svc", kernel="polydot", kpar= list(degree=1, scale=1, offset=0), C=1,prob.model=TRUE) #save this

### Logistic Regression
lr_model <- glm(V169~., data=traindataframe, family=binomial(link='logit'))

###############################################
### USE THE MODELS TO CLASSIFY NEW SENTENCES  #
###############################################

all_jobs <- read.table("english_sentences.txt", header=F, sep="\t", stringsAsFactors = FALSE)

all_jobs_sub <- all_jobs # consider all sentences
rm(all_jobs) # remove the original data frame

names(all_jobs_sub) <- c("jobid", "description") # add column names

### Transform each sentence
joball_transform <- sapply(all_jobs_sub$description, sent_transform, USE.NAMES=FALSE)
### Remove empty rows
not_missing_rows <- joball_transform != ""
joball_transform<-joball_transform[not_missing_rows]
all_jobs_sub <- all_jobs_sub[not_missing_rows,]

### Get the POS tags
tags_all <-list()

for(jobindx in 1:length(joball_transform)){
  print(jobindx)
  tags_all[[jobindx]] <- fftags(joball_transform[jobindx])
  
}

### Construct the feature matrix

fflist_new <- list()

for(i in 1:length(joball_transform)){
  if(length(tags_all[[i]])==0){print(i)}
  featurevector<-c(ff1(tags_all[[i]]),ff2(joball_transform[i]), ff3(joball_transform[i]),ff4(joball_transform[i]),ff5(joball_transform[i]),
                   ff6(tags_all[[i]]),ff7(tags_all[[i]]),
                   ff8(tags_all[[i]]),ff9(tags_all[[i]]),ff10(tags_all[[i]]),
                   ff11(tags_all[[i]]),ff12(tags_all[[i]]),
                   ff13(tags_all[[i]]),ff14(joball_transform[i]))
  fflist_new[[i]]<-featurevector
}

mycorpus_new <- VCorpus(VectorSource(joball_transform))

job_freq_dtm_new <- DocumentTermMatrix(mycorpus_new, control=list(dictionary=mydict,wordLengths = c(2, Inf) ))
inspect(job_freq_dtm_new)

job_freq_dtm_mat_new <- as.matrix(job_freq_dtm_new)

ffmat_new <- do.call("rbind", fflist_new)
ffmat_new <- cbind(ffmat_new,job_freq_dtm_mat_new)

testdf<-as.data.frame(ffmat_new, stringsAsFactors = FALSE)

testdf[,2:168] <- lapply(testdf[,2:168], as.numeric)
first_tag <- testdf$V1 %in% as.character(levels(traindataframe$V1))
testdf <- testdf[first_tag,]
all_jobs_sub <- all_jobs_sub[first_tag,]
testdf[,1]<-factor(testdf[,1], levels=levels(traindataframe$V1) )
names(testdf)[110] = "problem_solving"

### Run the classifiers
rf_pred <- predict(rf_model, testdf, type="prob")
nb_pred <- predict(nb_model, testdf, type="raw")
svm_pred <- predict(svm_model, testdf, type="probabilities")
lr_pred <- predict(lr_model, testdf, type="response")

### Obtain new activity and attribute sentences.
for(k in 1:nrow(rf_pred)){
if(max(rf_pred[k,])>.90){
  if(rf_pred[k,1] > rf_pred[k,2]){
    write(paste(all_jobs_sub[k,], collapse="\t"),"newattributes.txt", append=TRUE)
  } else{
    write(paste(all_jobs_sub[k,], collapse="\t"),"newactivities.txt", append=TRUE)
  }
}  
}


############################################################
### ANALYZE WORKER ATTRIBUTES                              #
############################################################
library(tm)
library(slam)
library(ldatuning)
library(topicmodels)
library(dplyr)
library(stringi)
library(LDAvis)


agg_attr_industry <- read.table("Supplement_2_worker_attributes_data.txt", header=TRUE, sep="\t", stringsAsFactors = FALSE, quote="")

attr_corpus <- VCorpus(VectorSource(agg_attr_industry$description))
attr_corpus <- tm_map(attr_corpus, content_transformer(tolower))
attr_corpus <- tm_map(attr_corpus, content_transformer(function(x) gsub("/"," ", x)))
attr_corpus <- tm_map(attr_corpus, removeWords, c(tm::stopwords("en"), "experience","years", "ability","skills","strong", "preferred","required","must","related","plus",
                                                  "minimum","including","equivalent","previous","proven","demonstrated","degree","least","good",
                                                  "able","excellent"))

attr_dtm <- DocumentTermMatrix(attr_corpus, control=list(removePunctuation=TRUE, removeNumbers=TRUE, wordLengths=c(2, Inf)))

term_idf <- log2(nDocs(attr_dtm)/col_sums(attr_dtm > 0))

summary(term_idf)

highq <- quantile(term_idf, probs=0.50, names=FALSE)

attr_dtm_new <- attr_dtm[, term_idf <= highq]
attr_dtm_new <- attr_dtm_new[row_sums(attr_dtm_new)>0,]
Terms(attr_dtm_new)

## Top 50 most frequent words
sort(col_sums(attr_dtm_new), decreasing=TRUE)[1:50]

########################################################
## Topic Modeling                                      #
########################################################

############## The code below may take some time to run
## Number of topics
SEED = 2010

result <- FindTopicsNumber(attr_dtm_new, 
                           topics=seq(80,180,20), 
                           metrics=c("CaoJuan2009","Arun2010","Deveaud2014","Griffiths2004"), 
                           method="Gibbs",
                           control = list(seed = SEED, burnin=1000, thin=100, iter=1000),
                           mc.cores=5L,
                           verbose=TRUE)

FindTopicsNumber_plot(result) # based from the CaoJuan2009, we choose 140 topics


## Run LDA with Gibbs sampling. Set the number of topics equal to 140
k<-140

attrTM_gibbs <- LDA(attr_dtm_new, k = k, control = list(seed = SEED, burnin=1000, thin=100, iter=1000), method="Gibbs")
#########################################################

#save(attrTM_gibbs, result, file="gibbs140topics.rda")
load("gibbs140topics.rda")

# Inspect the Topics
Terms<-terms(attrTM_gibbs,10)
Terms

#Inspect the assignment of documents to topics
Topics <- topics(attrTM_gibbs,1)
Topics

########################################################
## Intra-topic word correlations                       #
## to better interpret the topics                      #
########################################################

# Topic 86
plot(t(attr_dtm_new), terms=c("new","learning","technologies","learn","quickly","willingness",
                              "desire","applications","concepts","adapt","changing"), corThreshold=0.1, weighting=TRUE)
# Topic 105
plot(t(attr_dtm_new), terms=c("results","goals","others","deliver","drive","driven",
                              "achieve","influence","teams","leadership","objectives",
                              "lead","motivate"), corThreshold=0.1, weighting=TRUE)

#Topic 15
plot(t(attr_dtm_new), terms=c("attitude","positive","team","interpersonal","flexible","can",
                              "energetic","player","work","enthusiastic","personality",
                              "demonstrate","exhibit"), corThreshold=0.1, weighting=TRUE)

########################################################
## Inter-topic relationship                            #
########################################################

topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  
  temp_frequency <- doc_term
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = col_sums(temp_frequency)
                            )
  rm(temp_frequency)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq )
  
  return(json_lda)
}

attr_corpus_sub <- VCorpus(VectorSource(agg_attr_industry$description[as.numeric(dimnames(attr_dtm_new)$Docs)]))
names(attr_corpus_sub)<- dimnames(attr_dtm_new)$Docs

attr_json <- topicmodels_json_ldavis(fitted = attrTM_gibbs, corpus=attr_corpus_sub, doc_term=attr_dtm_new)
serVis(attr_json)

########################################################
## Cluster job titles                                  #
########################################################

### Jensen-Shannon divergence
jensenShannon <- function(x, y) {
  m <- 0.5 * (x + y)
  sqrt(0.5 * sum(x * log(x/m)) + 0.5 * sum(y * log(y/m)))
}

jobattr_theta <- posterior(attrTM_gibbs)$topics
jobattr_phi <- posterior(attrTM_gibbs)$terms

### PCA based on Jensen-Shannon divergence
jsSammon<-function (phi) 
{
  dist.mat <- proxy::dist(x = phi, method = jensenShannon,convert_similarities = FALSE)
  pca.fit <- stats::cmdscale(dist.mat, k = 2)
  data.frame(x = pca.fit[, 1], y = pca.fit[, 2])
}

### Choose 5000 job titles
indx <- sample(1:nrow(jobattr_theta),5000)
jobattr_theta_sub <- jobattr_theta[indx,] 

### Compute distances
dist_jobattr <- proxy::dist(x=jobattr_theta_sub, method=jensenShannon, convert_similarities = FALSE)

### Cluster job titles
cluster_jobattr <-hclust(dist_jobattr, method="ward.D")
### Set the number of cluster equal to 100
jobattr_cluCut <- cutree(cluster_jobattr, k=100)

## Select cluster 20
clus100_mat <- jobattr_theta_sub[jobattr_cluCut==20,,drop=FALSE]
agg_attr_industry[rownames(clus100_mat),4]
hclust100 <- hclust(proxy::dist(x= clus100_mat, method=jensenShannon), method="ward.D")

## Dendogram of cluster 20
plot(hclust100, labels=substr(agg_attr_industry[rownames(clus100_mat),4], start=1,  stop =30), cex=.8)


