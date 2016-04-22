#Code in R for Problem 3(a & b)
library(tm)
setwd("C:/Users/akhilkateja/Desktop/R")

#Read Data
d <-
  read.table(
    'SMSSpamCollection', sep = '\t', col.names = c('type','text'),
    quote = "", row.names = NULL, stringsAsFactors = FALSE
  )
count <- table(d$type);
count_ham <- count[1]
count_Spam <- count[2]

#Create Training(80%) and Testing Data(20%)
sms_train <- d[1:4459,]
sms_test <- d[4460:5574,]

sms_train_spam <- sms_train[which(sms_train$type == 'spam'),]
train_spam_count = nrow(sms_train_spam)
sms_train_ham <- sms_train[which(sms_train$type == 'ham'),]
train_ham_count = nrow(sms_train_ham)


#Clean Data
f <- function(data) {
  corpus_clean <- tm_map(data, tolower)
  corpus_clean <- tm_map(corpus_clean, removeNumbers)
  corpus_clean <- tm_map(corpus_clean, removePunctuation)
  corpus_clean <- tm_map(corpus_clean, stripWhitespace)
  corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
  return(corpus_clean)
}

sms_corpus_spam <- Corpus(VectorSource(sms_train_spam$text))
corpus_clean_spam <- f(sms_corpus_spam)
sms_dtm_spam <- DocumentTermMatrix(corpus_clean_spam)
freq_spam <- colSums(as.matrix(sms_dtm_spam))
num_spam_words <- sum(freq_spam)

sms_corpus_ham <- Corpus(VectorSource(sms_train_ham$text))
corpus_clean_ham <- f(sms_corpus_ham)
sms_dtm_ham <- DocumentTermMatrix(corpus_clean_ham)
freq_ham <- colSums(as.matrix(sms_dtm_ham))
num_ham_words <- sum(freq_ham)



N = 20000
s = ''
result = c()
pspam = train_spam_count / 4459;
pham = train_ham_count / 4459;



#Calculate probabilities and make predictions

p <- function(alpha,dataset) {
  for (i in 1:nrow(dataset)) {
    spamProb = 0
    hamProb = 0
    sms_corpus <- Corpus(VectorSource(dataset[i,]$text))
    corpus_clean <- f(sms_corpus)
    sms_dtm <- DocumentTermMatrix(corpus_clean)
    freq <- (as.matrix(sms_dtm))
    wordnames <- colnames(freq)
    for (j in 1:length(wordnames)) {
      if (!is.null(wordnames) && !is.na(freq_spam[wordnames[j]])) {
        spamProb = spamProb + log((freq_spam[wordnames[j]] + alpha) / (num_spam_words +
                                                                         (N * alpha)))
      }else{
        spamProb = spamProb + log((alpha) / (num_spam_words + (N * alpha)))
      }
      if (!is.null(wordnames) && !is.na(freq_ham[wordnames[j]])) {
        hamProb = hamProb + log((freq_ham[wordnames[j]] + alpha) / (num_ham_words +
                                                                      (N * alpha)))
      }else{
        hamProb = hamProb + log((alpha) / (num_ham_words + (N * alpha)))
      }
    }
    
    spamProb = spamProb + log(pspam)
    hamProb = hamProb++log(pham)
    if (spamProb > hamProb) {
      s = 'spam'
    }else{
      s = 'ham'
    }
    result[i] = s
  }
  
  count = 0
  truepositive = 0
  falsepositive = 0
  truenegative = 0
  falsenegative = 0
  for (i in 1:length(result))
  {
    if (result[i] == dataset$type[i]) {
      if (result[i] == 'spam') {
        truepositive = truepositive + 1;
      }else if (result[i] == 'ham') {
        truenegative = truenegative + 1
      }
      
    }else{
      count = count + 1;
      if (dataset$type[i] == 'ham' && result[i] == 'spam') {
        falsepositive = falsepositive + 1
      }else if (dataset$type[i] == 'spam' && result[i] == 'ham') {
        falsenegative = falsenegative + 1
      }
    }
  }
  
  
  
  accuracy = 1 - (count / length(result))
  confusionMatrix <-
    matrix(
      c(truepositive,falsepositive,falsenegative,truenegative),ncol = 2,byrow =
        TRUE
    )
  colnames(confusionMatrix) <- c("Positive","Negactive")
  rownames(confusionMatrix) <- c("Positive","Negative")
  confusionMatrix <- as.table(confusionMatrix)
  
  precision = truepositive / (truepositive + falsepositive)
  recall = truepositive / (truepositive + falsenegative)
  fscore = (2 * precision * recall) / (precision + recall)
  f_score_accuracy <- c(accuracy,fscore)
  return (f_score_accuracy)
}

#Calculate accuracy, precision, fscore, recall, confusion matrix for alpha=0.1
#for first testing data and then for training data
test_values_alpha = c(p(0.1,sms_test))
train_values_alpha = c(p(0.1,sms_train))


#Calculate values for alpha=2^i, where i=-5 to 0.
test_values = c(
  p(2 ^ -5,sms_test),p(2 ^ -4,sms_test),p(2 ^ -3,sms_test),p(2 ^ -2,sms_test),p(2 ^ -1,sms_test),p(2 ^ 0,sms_test)
)
train_values = c(
  p(2 ^ -5,sms_train),p(2 ^ -4,sms_train),p(2 ^ -3,sms_train),p(2 ^ -2,sms_train),p(2 ^ -1,sms_train),p(2 ^ 0,sms_train)
)

test_accuracy = c(test_values[1],test_values[3],test_values[5],test_values[7],test_values[9],test_values[11])
train_accuracy = c(
  train_values[1],train_values[3],train_values[5],train_values[7],train_values[9],train_values[11]
)
i <- c(-5,-4,-3,-2,-1,0)
#plot graph for i vs accuracy
plot(
  i,test_accuracy,ylim = c(0.94,1),type = "o",col = "red",ylab = "accuracy"
)
points(i,train_accuracy,type = "o",col = "green")
legend(
  -5,1,c("Testing Data","Training Data"), cex = 0.8,
  col = c("red","green"), pch = 21:22, lty = 1:2
)
title(main = "i vs accuracy", col.main = "red", font.main = 4)

test_fscore = c(test_values[2],test_values[4],test_values[6],test_values[8],test_values[10],test_values[12])
train_fscore = c(
  train_values[2],train_values[4],train_values[6],train_values[8],train_values[10],train_values[12]
)
#plot graph for i vs fscore
plot(
  i,test_fscore,ylim = c(0.8,1),type = "o",col = "red",ylab = "fscore"
)
points(i,train_fscore,type = "o",col = "green")
legend(
  -5,1,c("Testing Data","Training Data"), cex = 0.8,
  col = c("red","green"), pch = 21:22, lty = 1:2
)
title(main = "i vs fscore", col.main = "red", font.main = 4)