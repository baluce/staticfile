setwd("D:/東吳巨資/研二上/Text Mining/tm2016_sample_data")
tmdata <-read.csv("tmdata.csv")

library(rJava)
library(Rwordseg)
library(tm)
library(tmcn)
library(text2vec)
library(tokenizers)
library(stringi)
library(textmineR)
# library(lsa)


#### 載入主題資料 ####

tmdata[,8] <- as.character(tmdata[,8])

# subset title = 影劇娛樂
doc <-tmdata[tmdata$section =="娛樂"|tmdata$section =="娛樂新聞"|tmdata$section =="娛樂總覽" |tmdata$section =="影劇新聞" ,]

# subset title = 運動
doc <-tmdata[tmdata$section =="運動"|tmdata$section =="體育"|tmdata$section =="運動天地" |tmdata$section =="體育總覽"|tmdata$section =="運動休閒" ,]

# subset title = 兩岸
doc <-tmdata[tmdata$section =="兩岸"|tmdata$section =="兩岸國際"|tmdata$section =="兩岸新聞" ,]

# subset title = 財經
doc <-tmdata[tmdata$section =="股市"|tmdata$section =="產經"|tmdata$section =="財經總覽"|tmdata$section =="財經新聞" ,]

# subset title = 保健
doc <-tmdata[tmdata$section =="家庭與健康總覽" ,]

# subset title = 政治
doc <-tmdata[tmdata$section =="政治新聞",]

# subset title =社會
doc <-tmdata[tmdata$section =="社會"|tmdata$section =="社會新聞" ,]

# all
doc <-tmdata


#### 預處理載入資料 #### 
# 除去標點/數字/標點符號

doc[, 8] <- removePunctuation(doc[, 8])
doc[, 8] <- removeNumbers(doc[, 8])
doc[, 8] <- gsub("[A-Za-z]", "", doc[, 8])
doc[, 8] <- gsub(" {2,}", " ", doc[, 8])

#### 斷字 ####
a.token <- itoken(doc[, 8], tokenizer = tokenize_characters, id=doc[, 1])
a.vocab <- create_vocabulary(a.token, ngram=c(2, 6), sep_ngram="")

a.vocab # 檢查斷字結果

# 產生Document-Term Matrix
# vectorizer <- vocab_vectorizer(a.vocab)
# dtm <- create_dtm(a.token, vectorizer)
# dim(dtm)

# 刪除太常出現與太不常出現的字詞
pruned_vocab <- prune_vocabulary(a.vocab, term_count_min = 10, doc_proportion_max = 0.5, doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(pruned_vocab)
a.token <- itoken(doc[, 8], tokenizer = tokenize_characters, id=doc[, 1])

# 產生Document-Term Matrix
dtm <- create_dtm(a.token, vectorizer)
dim(dtm)

## Make a copy of general martix of DTM
# matrix.dtm <- as.matrix(dtm)

#### 計算TF-IDF #### 
model_tfidf <- TfIdf$new()
model_tfidf$fit(dtm)
dtm_tfidf <- model_tfidf$transform(dtm)
# matrix.tfidf <- as.matrix(dtm_tfidf)
keywords <- TermDocFreq(dtm_tfidf)
keywords$tfidf <- keywords$term_freq*keywords$idf

#### 挑出前200個關鍵字 ####
top200keywords <- head(keywords[order(keywords$tfidf, decreasing=TRUE), ], 500)
top200keywords #

# 刪除 Sub-keywords
myData <- top200keywords
len1 <- nrow(myData)-1
not.keepwords <- data.frame()

for (i in (1:len1)) {
  a <- myData[grepl(myData$term[i],myData$term),]
  if (length(a$term)==0){return} else
  {
    Keepword <- a$term[nchar(a$term)==max(nchar(a$term))]
    new.notkeepwords <- subset(a, !(a$term %in% Keepword))
    not.keepwords <- rbind(new.notkeepwords, not.keepwords)
  }
}
myData <- subset(myData, !(myData$term %in% not.keepwords$term))

## 1~ 8
top200keywords8 <-head(myData[order(myData$tfidf, decreasing=TRUE), ], 200)

## combine 8 categorical top200keywords
top200keywords_sum <- rbind(top200keywords1, top200keywords2, top200keywords3, top200keywords4, top200keywords5, top200keywords6, top200keywords7, top200keywords8)

#影劇娛樂、運動、兩岸、財經、保健、政治、社會

# 儲存關鍵字
saveRDS(top200keywords_sum,file = "top200keywords_sum.Rda")
saveRDS(a.vocab, file ="a.vocab.Rda")
# write.table(top200keywords, file = "top200keywords.csv", sep = ",", row.names = TRUE)