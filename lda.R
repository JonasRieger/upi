starttime = Sys.time()

library(tosca)
library(ldaPrototype)
library(ldaGibbs)
library(lubridate)

folder = readLines("folder.txt")

obj = readRDS("clean_updated_wirtschaft_unsicher.rds")
setwd(folder)
dir.create("lda")
setwd("lda")

obj = filterDate(obj, e.date = as.Date(folder))
obj$meta = obj$meta[order(obj$meta$date),]
obj$text = obj$text[match(obj$meta$id, names(obj$text))]

K = 14
alpha = eta = 1/K
num.iterations = 200
vocab.limit = 5

cuts = seq.Date(as.Date("2006-01-01"), as.Date("2021-01-01"), "quarter")
cache = seq.Date(as.Date("2005-04-01"), as.Date("2020-01-01"), "quarter")

init = filterDate(obj, e.date = cuts[1]-1)
wl = makeWordlist(init$text)
vocab = wl$words[wl$wordtable > vocab.limit]

docs = docs_all = LDAprep(init$text, vocab)
init = LDAPrototype(docs, vocab, K = K, alpha = alpha, eta = eta,
                    num.iterations = num.iterations, pm.backend = "socket", ncpus = 4,
                    seeds = 501:600)
lda = getLDA(init)
assignments = getAssignments(lda)
saveRDS(vocab, file = "vocab0.rds")
saveRDS(docs, file = "docs0.rds")
saveRDS(init, file = "proto_init.rds")

for(i in seq_len(length(cuts)-1)){
  chunk = filterDate(obj, s.date = cuts[i], e.date = cuts[i+1]-1)
  wl = makeWordlist(chunk$text)
  tmp = wl$words[wl$wordtable > vocab.limit]
  ind = !(tmp %in% vocab)
  if(any(ind)) vocab = c(vocab, tmp[ind])
  docs_neu = LDAprep(chunk$text, vocab)
  
  n.docs = length(docs_all)
  docs_all[(n.docs+1):(n.docs+length(docs_neu))] = docs_neu
  names(docs_all)[(n.docs+1):length(docs_all)] = names(docs_neu)
  
  assignments[(n.docs+1):length(docs_all)] = lapply(
    sapply(docs_all[(n.docs+1):length(docs_all)], ncol),
    function(n) as.integer(sample(K, n, replace = TRUE)-1))
  
  ind_chunk = names(docs_all) %in% obj$meta$id[obj$meta$date >= cache[i] & obj$meta$date < cuts[i+1]]
  n.init = sum(obj$meta$date >= cache[i] & obj$meta$date < cuts[i])
  docs = docs_all[ind_chunk]
  
  topics = matrix(as.integer(
    table(unlist(assignments[ind_chunk]) + 1, #Topics
          factor(unlist(lapply(docs, function(x) x[1,])) + 1, seq_len(length(vocab)), vocab)) #Vocab
  ), nrow = K)
  colnames(topics) = vocab
  
  initial = list(
    assignments = assignments[ind_chunk],
    topics = topics,
    topic_sums = matrix(as.integer(rowSums(topics))))
  
  res = ldaGibbs(docs, K, vocab, num.iterations, alpha, eta, initial = initial, n.init = n.init)
  
  assignments[(n.docs+1):length(docs_all)] = res$assignments[(n.init+1):length(docs)]
  topics = matrix(as.integer(
    table(unlist(assignments) + 1, #Topics
          factor(unlist(lapply(docs_all, function(x) x[1,])) + 1, seq_len(length(vocab)), vocab)) #Vocab
  ), nrow = K)
  colnames(topics) = vocab
  document_sums = cbind(getDocument_sums(lda), res$document_sums[,(n.init+1):length(docs)])
  
  lda = LDA(assignments = assignments, topics = topics, document_sums = document_sums,
            param = list(K = K, alpha = alpha, eta = eta, num.iterations = num.iterations))
  saveRDS(vocab, file = paste0("vocab", i, ".rds"))
  saveRDS(docs, file = paste0("docs", i, ".rds"))
  saveRDS(docs_all, file = paste0("docsall.rds"))
  saveRDS(lda, file = paste0("ldachunked.rds"))
}

gc()

difftime(Sys.time(), starttime, units = "hours")
