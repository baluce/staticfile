### tagging keywords
topkeywords <- readRDS("top1600keywords.Rda")
terms <- as.character(topkeywords[,1])
terms <- unique(terms) #刪除重複字詞

doc$keywordtag <- apply(sapply(terms, grepl, doc$content), 1, function(x) paste(terms[x], collapse=','))

saveRDS(doc,file = "tmdata_with_tags.Rda")
