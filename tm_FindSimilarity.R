### Using only top keywords to build DTM

# vocab$vocab <- vocab$vocab[terms %in% user_keywords, ]
topkeywords <- readRDS("top1600keywords.Rda")
topkeyword.list <- unique(topkeywords[,1]) #刪除重複字詞
system.time(a.vocab$vocab <- a.vocab$vocab[terms %in% topkeyword.list, ] )

vectorizer <- vocab_vectorizer(a.vocab)
condensed.dtm <- create_dtm(a.token, vectorizer)

dim(condensed.dtm)
rm(a.vocab)

### TF-IDF 
model_tfidf <- TfIdf$new()
model_tfidf$fit(condensed.dtm)
condensed.dtm_tfidf <- model_tfidf$transform(condensed.dtm)
keywords <- TermDocFreq(condensed.dtm_tfidf)
dim(condensed.dtm_tfidf)
condensed.matrix.tfidf <- as.matrix(condensed.dtm_tfidf)
# rm(condensed.dtm)
# rm(condensed.dtm_tfidf)
row_sum <- rowSums(condensed.matrix.tfidf, na.rm = TRUE)
condensed.matrix.tfidf$sum <- row_sum

vocab$vocab[terms %in% user_keywords, ] 

CosineSimilarity <- sim2(condensed.dtm_tfidf, y = NULL, method = c("cosine"), norm = c("none"),verbose = TRUE)
saveRDS(CosineSimilarity, file="CosineSimilarity.Rda")
transform_filter_commons(CosineSimilarity)

saveRDS(condensed.matrix.tfidf, file="condensed.matrix.tfidf.Rda")