all = read.table("data.tsv",stringsAsFactors = F,header = T)
splits = read.table("splits.csv", header = T)
s = 1
######load enviroment
mypackages = c("kableExtra", "text2vec", "tokenizers","xgboost","pROC")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)

###### split dataset
train = all[-which(all$new_id%in%splits[,s]),]
test = all[which(all$new_id%in%splits[,s]),]

###### load vocabulary 
f= file("myVocab.txt")
vocab = readLines(f)
close(f)

###### prediction
xgboost_predict <- function(train_data, label, test_data) {
  xgb.model = xgboost(data = train_data, label=label,
                      objective = "binary:logistic", eval_metric = "auc",
                      eta = 0.09,
                      nrounds = 1000,
                      verbose = TRUE)
  return (predict(xgb.model, test_data, type="response"))
}
 make_prediction <- function(vocab, train_data, test_data, tok.fun = word_tokenizer){
# define preprocessing function and tokenization function
    it_train = itoken(train_data$review, 
                      preprocessor = tolower, 
                      tokenizer = tok.fun, 
                      ids = train_data$new_id, 
                      progressbar = FALSE)
    it_test = itoken(test_data$review, 
                     preprocessor = tolower, 
                     tokenizer = tok.fun, 
                     ids = test_data$new_id, 
                     progressbar = FALSE)
    vectorizer = vocab_vectorizer(create_vocabulary(vocab, ngram = c(1L, 4L)))
    dtm_train  = create_dtm(it_train, vectorizer)
    dtm_test = create_dtm(it_test, vectorizer)
    # define tfidf model
    tfidf = TfIdf$new()
    # fit model to train data and transform train data with fitted model
    dtm_train_tfidf = fit_transform(dtm_train, tfidf)
    dtm_test_tfidf = transform(dtm_test, tfidf)# apply pre-trained tf-idf transformation to test data
    preds = xgboost_predict(dtm_train, train_data$sentiment, dtm_test)
    return (preds)
 }
 set.seed(8139)
  pred = make_prediction(vocab, train, test)
  output.data = cbind(new_id = test$new_id, prob = round(pred,2))
  roc_obj <- roc(test$sentiment, output.data[,2])
  roc_obj
write.csv(output.data, "mysubmission.txt", row.names = FALSE, quote = FALSE) 