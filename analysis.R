library(tosca)
library(ldaPrototype)
library(lubridate)
library(ggplot2)
library(GGally)
library(ggthemes)

folder = readLines("folder.txt")
obj_all = filterDate(readRDS("clean_updated.rds"), e.date = folder)
obj_wirtschaft = filterDate(readRDS("clean_updated_wirtschaft.rds"), e.date = folder)
obj = filterDate(readRDS("obj_updated_wirtschaft_unsicher.rds"), e.date = folder)
setwd(folder)
dir.create("analysis")
pdf(file.path("analysis", "corpus.pdf"), width = 10, height = 7)
rel_wirtschaft = plotScot(obj_all, obj_wirtschaft$meta$id, rel = TRUE,
                          main = "Share of \"wirtschaft\"-corpus on entire corpus (words)",
                          ylim = c(0, 0.5), type = "words")
rel_wirtschaftq = plotScot(obj_all, obj_wirtschaft$meta$id, rel = TRUE, unit = "quarter",
                           main = "Share of \"wirtschaft\"-corpus on entire corpus (words)",
                           ylim = c(0, 0.5), type = "words")
rel_unsicher = plotScot(obj_wirtschaft, obj$meta$id, rel = TRUE,
                        main = "Share of \"wirtschaft\"&\"unsicher\"-corpus on \"wirtschaft\"-corpus (words)",
                        ylim = c(0, 0.15), type = "words")
rel_unsicherq = plotScot(obj_wirtschaft, obj$meta$id, rel = TRUE, unit = "quarter",
                         main = "Share of \"wirtschaft\"&\"unsicher\"-corpus on \"wirtschaft\"-corpus (words)",
                         ylim = c(0, 0.15), type = "words")
rel_all = plotScot(obj_all, obj$meta$id, rel = TRUE,
                   main = "Share of \"wirtschaft\"&\"unsicher\"-corpus on entire corpus (words)",
                   ylim = c(0, 0.04), type = "words")
plotScot(obj_all, obj$meta$id, rel = TRUE, unit = "quarter",
         main = "Share of \"wirtschaft\"&\"unsicher\"-corpus on entire corpus (words)",
         ylim = c(0, 0.04))
abs = plot(obj, main = "Number of articles of \"wirtschaft\"&\"unsicher\"-corpus")
plot(obj, main = "Number of articles of \"wirtschaft\"&\"unsicher\"-corpus", unit = "quarter")
rm(obj_all)
rm(obj_wirtschaft)
dev.off()

write.csv(abs, file.path("analysis", "corpus.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
write.csv(rel_unsicher, file.path("analysis", "corpus_rel_to_wirtschaft.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
write.csv(rel_all, file.path("analysis", "corpus_rel_to_entire.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)

file = "ldachunked.rds"
lda = readRDS(file.path("lda", file))
docs = readRDS(file.path("lda", "docsall.rds"))
ids = names(docs)
assignments = getAssignments(lda)
vocab = colnames(getTopics(lda))

K = getK(lda)

dir.create(file.path("analysis", "topTexts"))

mat = topTexts(lda, ids, 100)
showTexts(obj, mat, file = "analysis/topTexts/")

topwords = topWords(getTopics(lda), 200)
prop = round(rowSums(getTopics(lda)) / sum(getTopics(lda)) * 100, 4)
out = rbind(prop, topwords)
colnames(out) = paste("Topic", seq_len(K))
row.names(out) = c("Proportion (%)", 1:200)
write.csv(out,
          file = file.path("analysis", "topwords.csv"),
          fileEncoding = "UTF-8")

tab = plotTopic(object = obj, ldaresult = lda, ldaID = ids)
tabrel = plotTopic(object = obj, ldaresult = lda, ldaID = ids, rel = TRUE)

write.csv(tab, file = file.path("analysis", "topics.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
write.csv(tabrel, file = file.path("analysis", "topics_rel.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)

tabrel_wirtschaft = cbind(date = tabrel$date,
                          tabrel[, -1] *
                            rel_unsicher$proportion[match(tabrel$date, rel_unsicher$date)])
write.csv(tabrel_wirtschaft, file = file.path("analysis", "topics_rel_to_wirtschaft.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)
tabrel_all = cbind(date = tabrel$date,
                   tabrel_wirtschaft[, -1] *
                     rel_wirtschaft$proportion[match(tabrel$date, rel_wirtschaft$date)])
write.csv(tabrel_all, file = file.path("analysis", "topics_rel_to_entire.csv"),
          fileEncoding = "UTF-8", row.names = FALSE)

tabrelq = plotTopic(object = obj, ldaresult = lda, ldaID = ids, rel = TRUE, unit = "quarter")
tabrel_wirtschaftq = cbind(date = tabrelq$date,
                           tabrelq[, -1] *
                             rel_unsicherq$proportion[match(tabrelq$date, rel_unsicherq$date)])
tabrel_allq = cbind(date = tabrelq$date,
                    tabrel_wirtschaftq[, -1] *
                      rel_wirtschaftq$proportion[match(tabrelq$date, rel_wirtschaftq$date)])
quarters = lubridate::floor_date(obj$meta$date[match(names(docs), obj$meta$id)], "quarter")
topicsq = lapply(tabrelq$date, function(x){
  tmp = table(factor(unlist(assignments[quarters == x])+1, levels = 1:K), 
              factor(unlist(lapply(docs[quarters == x], function(y) y[1,]))+1, levels = seq_len(length(vocab))))
  tmp = matrix(as.integer(tmp), nrow = K)
  colnames(tmp) = vocab
  tmp
})
topwordsq = lapply(topicsq, topWords, numWords = 50)
topwordsq = lapply(1:K, function(k) sapply(seq_along(topwordsq), function(t) topwordsq[[t]][,k]))
dir.create(file.path("analysis", "topWordsPerQuarter"))
for(i in 1:K){
  out = rbind(round(tabrelq[, i+1] * 100, 4),
              round(tabrel_wirtschaftq[, i+1] * 100, 4),
              round(tabrel_allq[, i+1] * 1000, 4),
              topwordsq[[i]])
  colnames(out) = as.character(tabrelq$date)
  row.names(out) = c("Share (in %) on \"wirtschaft\"&\"unsicher\"-corpus",
                     "Share (in %) on \"wirtschaft\"-corpus",
                     "Share (in Promille) on entire corpus",
                     1:50)
  write.csv(out,
            file = file.path("analysis", "topWordsPerQuarter", paste0(colnames(tabrelq)[i+1], ".csv")),
            fileEncoding = "UTF-8")
}


### covid
covid = union(union(vocab[grepl("corona", vocab)], vocab[grepl("covid", vocab)]),
              vocab[grepl("pandemie", vocab)])
covid_ids = match(covid, vocab)

a = (unname(unlist(lapply(docs, function(x) x[1,])))+1) %in% covid_ids
b = (unlist(getAssignments(lda))+1)[a]
d = floor_date(rep(obj$meta$date[match(names(docs), obj$meta$id)],
                   lengths(getAssignments(lda)))[a], "month")

o = table(b, d)
o = o[, colnames(o) >= c("2020-01-01")]
tab = plotTopic(obj, lda, names(docs), unit = "month")
out = t(o)
write.csv(out, file.path("analysis", "corona.csv"))
out = out / tab[match(rownames(out), as.character(tab$date)), 2:15]
colnames(out) = rownames(o)
rownames(out) = colnames(o)
write.csv(out, file.path("analysis", "corona_rel_to_topic.csv"))
writeLines(covid, file.path("analysis", "corona.txt"))

### fear
fear = setdiff(vocab[grepl("angst", vocab)],
               c("abstiegsangst", "gangster", "terrorangst", "kriegsangst"))
fear_ids = match(fear, vocab)

a = (unname(unlist(lapply(docs, function(x) x[1,])))+1) %in% fear_ids
b = (unlist(getAssignments(lda))+1)[a]
d = floor_date(rep(obj$meta$date[match(names(docs), obj$meta$id)],
                   lengths(getAssignments(lda)))[a], "quarter")

o = table(b, d)
tab = plotTopic(obj, lda, names(docs), unit = "month")
out = t(o)
write.csv(out, file.path("analysis", "fear.csv"))
out = out / tab[match(rownames(out), as.character(tab$date)), 2:15]
colnames(out) = rownames(o)
rownames(out) = colnames(o)
write.csv(out, file.path("analysis", "fear_rel_to_topic.csv"))
writeLines(fear, file.path("analysis", "fear.txt"))


### 
topicnames = paste0("T", 1:K, ".", topWords(getTopics(lda)))

### quarterly topic sims (calculation: see above)
xquarter = sort(unique(quarters))
nquarter = length(xquarter)

topics = lapply(1:K, function(k){
  tmp = sapply(topicsq, function(x) x[k,])
  colnames(tmp) = paste0("Q", 1:nquarter)
  tmp
})

sims = lapply(1:K, function(k){
  cosineTopics(topics[[k]], pm.backend = "socket", ncpus = 4)
})

valq = sapply(sims, function(x) c(NA, x$sims[cbind(2:nquarter,2:nquarter-1)]))
val_first = sapply(sims, function(x) x$sims[,1])
val_last = sapply(sims, function(x) x$sims[80,])

### monthly topic sims
months = lubridate::floor_date(obj$meta$date[match(names(docs), obj$meta$id)], "month")
topicsm = lapply(tab$date, function(x){
  tmp = table(factor(unlist(assignments[months == x])+1, levels = 1:K), 
              factor(unlist(lapply(docs[months == x], function(y) y[1,]))+1, levels = seq_len(length(vocab))))
  tmp = matrix(as.integer(tmp), nrow = K)
  colnames(tmp) = vocab
  tmp
})
xmonths = sort(unique(months))
nmonths = length(xmonths)
topwordsm = lapply(topicsm, topWords, numWords = 50)
topwordsm = lapply(1:K, function(k) sapply(seq_along(topwordsm), function(t) topwordsm[[t]][,k]))

topics = lapply(1:K, function(k){
  tmp = sapply(topicsm, function(x) x[k,])
  colnames(tmp) = paste0("Q", 1:nmonths)
  tmp
})

sims = lapply(1:K, function(k){
  cosineTopics(topics[[k]], pm.backend = "socket", ncpus = 4)
})

valm = sapply(sims, function(x) c(NA, x$sims[cbind(2:nmonths,2:nmonths-1)]))

labels = c("Topic 1: Corporate Culture",
           "Topic 2: EU Conflicts",
           "Topic 3: Energy & Climate Change Mitigation",
           "Topic 4: Companies & Markets",
           "Topic 5: Geopolitics",
           "Topic 6: Society",
           "Topic 7: Financial Markets I",
           "Topic 8: German Politics I",
           "Topic 9: Miscellaneous",
           "Topic 10: Financial Markets II",
           "Topic 11: Leisure & Hospitality",
           "Topic 12: Central banks",
           "Topic 13: German Economy",
           "Topic 14: German Politics II")

xmin = min(xmonths)

plot2 = ggmatrix(lapply(1:14, function(i){
  ggplot() + geom_line(aes(x = xmonths, y = valm[,i]), col = "darkgrey") + ylim(c(0,1)) +
    geom_line(aes(x = xquarter, y = val_first[,i], col = "green")) +
    geom_line(aes(x = xquarter, y = val_last[,i], col = "red")) +
    geom_line(aes(x = xquarter, y = valq[,i])) +
    annotate("text", x = xmin, y = 0.05, label = labels[i], hjust = 0, vjust = 0) +
    theme_excel_new() + scale_colour_excel_new() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
    )
  
}), nrow = 7, ncol = 2, ylab = "Cosine Similarity")

plot3 = ggmatrix(lapply(1:14, function(i){
  ggplot() + geom_line(aes(x = xmonths, y = valm[,i]), col = "darkgrey") + ylim(c(0,1)) +
    geom_line(aes(x = xquarter, y = val_first[,i], col = "green")) +
    geom_line(aes(x = xquarter, y = val_last[,i], col = "red")) +
    geom_line(aes(x = xquarter, y = valq[,i])) +
    annotate("text", x = xmin, y = 0.05, label = labels[i], hjust = 0, vjust = 0) +
    theme_excel_new() + scale_colour_excel_new() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_line(),
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    ) + scale_x_date(date_breaks = "year", date_labels = "%m.%d.%y")
  
}), nrow = 7, ncol = 2, ylab = "Cosine Similarity")

pdf(file.path("analysis", "topics_cosine_excel2.pdf"), width = 8, height = 10)
print(plot2)
dev.off()

tiff(file.path("analysis", "topics_cosine_excel2.tiff"),
     width = 1600, height = 2000, pointsize = 200, compression = "lzw", res = 200,
     type = "cairo")
print(plot2)
dev.off()

pdf(file.path("analysis", "topics_cosine_excel3.pdf"), width = 8, height = 10)
print(plot3)
dev.off()

tiff(file.path("analysis", "topics_cosine_excel3.tiff"),
     width = 1600, height = 2000, pointsize = 200, compression = "lzw", res = 200,
     type = "cairo")
print(plot3)
dev.off()
