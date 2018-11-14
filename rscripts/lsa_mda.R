
# script stolen from http://goo.gl/YbQyAQ

# install.packages("tm")
# install.packages("ggplot2")
# install.packages("lsa")
# install.packages("scatterplot3d")
#install.packages("SnowballC")
#if !(require('SnowballC')) then install.packages("SnowballC")
library(tm)
library(ggplot2)
library(lsa)
library(scatterplot3d)
library(SnowballC)



#------------------------------------------------------------------------------

# 1. Prepare data from http://goo.gl/1RB32f

text <- c("The discipline of phenomenology is defined by its domain of study, 
  its methods, and its main results.",

"Phenomenology studies structures of conscious experience as experienced from 
the first-person point of view, along with relevant conditions of experience. 
The central structure of an experience is its intentionality, the way it is 
directed through its content or meaning toward a certain object in the world.",

"We all experience various types of experience including perception, 
imagination, thought, emotion, desire, volition, and action. Thus, the domain 
of phenomenology is the range of experiences including these types (among 
  others). Experience includes not only relatively passive experience as in 
vision or hearing, but also active experience as in walking or hammering a 
nail or kicking a ball. (The range will be specific to each species of being 
  that enjoys consciousness; our focus is on our own, human, experience. Not 
  all conscious beings will, or will be able to, practice phenomenology, as 
  we do.)",

"Conscious experiences have a unique feature: we experience them, we live 
through them or perform them. Other things in the world we may observe and 
engage. But we do not experience them, in the sense of living through or 
performing them. This experiential or first-person feature — that of being 
experienced — is an essential part of the nature or structure of conscious 
experience: as we say, “I see / think / desire / do …” This feature is both 
a phenomenological and an ontological feature of each experience: it is part 
of what it is for the experience to be experienced (phenomenological) and part 
of what it is for the experience to be (ontological).",

"How shall we study conscious experience? We reflect on various types of 
experiences just as we experience them. That is to say, we proceed from the
 first-person point of view. However, we do not normally characterize an 
 experience at the time we are performing it. In many cases we do not have
  that capability: a state of intense anger or fear, for example, consumes 
  all of one's psychic focus at the time. Rather, we acquire a background 
  of having lived through a given type of experience, and we look to our 
  familiarity with that type of experience: hearing a song, seeing a sunset, 
  thinking about love, intending to jump a hurdle. The practice of 
  phenomenology assumes such familiarity with the type of experiences to 
  be characterized. Importantly, also, it is types of experience that 
  phenomenology pursues, rather than a particular fleeting experience 
  — unless its type is what interests us.",

"Classical phenomenologists practiced some three distinguishable methods. 
(1) We describe a type of experience just as we find it in our own (past) 
experience. Thus, Husserl and Merleau-Ponty spoke of pure description of 
lived experience. (2) We interpret a type of experience by relating it to 
relevant features of context. In this vein, Heidegger and his followers 
spoke of hermeneutics, the art of interpretation in context, especially 
social and linguistic context. (3) We analyze the form of a type of experience. 
In the end, all the classical phenomenologists practiced analysis of 
experience, factoring out notable features for further elaboration.",

"These traditional methods have been ramified in recent decades, expanding 
the methods available to phenomenology. Thus: (4) In a logico-semantic model 
of phenomenology, we specify the truth conditions for a type of thinking 
(say, where I think that dogs chase cats) or the satisfaction conditions for 
a type of intention (say, where I intend or will to jump that hurdle). (5) 
In the experimental paradigm of cognitive neuroscience, we design empirical 
experiments that tend to confirm or refute aspects of experience (say, 
  where a brain scan shows electrochemical activity in a specific region 
  of the brain thought to subserve a type of vision or emotion or motor 
  control). This style of “neurophenomenology” assumes that conscious 
experience is grounded in neural activity in embodied action in appropriate 
surroundings — mixing pure phenomenology with biological and physical 
science in a way that was not wholly congenial to traditional 
phenomenologists.",

"What makes an experience conscious is a certain awareness one has of the experience while living through or performing it. This form of inner awareness has been a topic of considerable debate, centuries after the issue arose with Locke's notion of self-consciousness on the heels of Descartes' sense of consciousness (conscience, co-knowledge). Does this awareness-of-experience consist in a kind of inner observation of the experience, as if one were doing two things at once? (Brentano argued no.) Is it a higher-order perception of one's mind's operation, or is it a higher-order thought about one's mental activity? (Recent theorists have proposed both.) Or is it a different form of inherent structure? (Sartre took this line, drawing on Brentano and Husserl.) These issues are beyond the scope of this article, but notice that these results of phenomenological analysis shape the characterization of the domain of study and the methodology appropriate to the domain. For awareness-of-experience is a defining trait of conscious experience, the trait that gives experience a first-person, lived character. It is that lived character of experience that allows a first-person perspective on the object of study, namely, experience, and that perspective is characteristic of the methodology of phenomenology.",

"Conscious experience is the starting point of phenomenology, but experience shades off into less overtly conscious phenomena. As Husserl and others stressed, we are only vaguely aware of things in the margin or periphery of attention, and we are only implicitly aware of the wider horizon of things in the world around us. Moreover, as Heidegger stressed, in practical activities like walking along, or hammering a nail, or speaking our native tongue, we are not explicitly conscious of our habitual patterns of action. Furthermore, as psychoanalysts have stressed, much of our intentional mental activity is not conscious at all, but may become conscious in the process of therapy or interrogation, as we come to realize how we feel or think about something. We should allow, then, that the domain of phenomenology — our own experience — spreads out from conscious experience into semi-conscious and even unconscious mental activity, along with relevant background conditions implicitly invoked in our experience. (These issues are subject to debate; the point here is to open the door to the question of where to draw the boundary of the domain of phenomenology.)")

view <- factor(rep(c("view 1", "view 2", "view 3"), each = 3))
view
df <- data.frame(text, view, stringsAsFactors = FALSE)
df



#------------------------------------------------------------------------------

# prepare corpus
corpus <- Corpus(VectorSource(df$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))

# error below, can't stem
corpus  <- tm_map(corpus, stemDocument, language = "english") 
corpus  



#------------------------------------------------------------------------------

# MDS with raw term-document matrix compute distance matrix
td.mat <- as.matrix(TermDocumentMatrix(corpus))
td.mat
dist.mat <- dist(t(as.matrix(td.mat)))
dist.mat  # check distance matrix



#------------------------------------------------------------------------------

# MDS
fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
    color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = 
    row.names(df)))




#------------------------------------------------------------------------------

# MDS with LSA
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)  # weighting
lsaSpace <- lsa(td.mat.lsa)  # create LSA space
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))  # compute distance matrix
dist.mat.lsa  # check distance mantrix


#------------------------------------------------------------------------------

# MDS
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 2)
points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
    color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = row.names(df)))


#------------------------------------------------------------------------------

# plot
fit <- cmdscale(dist.mat.lsa, eig = TRUE, k = 3)
colors <- rep(c("blue", "green", "red"), each = 3)
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color = colors, 
    pch = 16, main = "Semantic Space Scaled to 3D", xlab = "x", ylab = "y", 
    zlab = "z", type = "h")
