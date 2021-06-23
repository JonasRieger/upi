starttime = Sys.time()

library(tosca)
library(ldaPrototype)
library(ldaGibbs)
library(lubridate)
library(stringr)

folder = readLines("folder.txt")

obj = readRDS("clean_updated_wirtschaft_unsicher.rds")

obj = filterDate(obj, e.date = as.Date(folder))
obj$meta = obj$meta[order(obj$meta$date),]
obj$text = obj$text[match(obj$meta$id, names(obj$text))]

K = 14
alpha = eta = 1/K
num.iterations = 200
vocab.limit = 5

cuts = c(as.Date(folder) + 1 - months(3), as.Date(folder) + 1)
cache = as.Date(folder) + 1 - years(1)

prev_q = as.Date(folder) + 1 - months(3) - 1
max_voc = max(as.numeric(str_extract(list.files(file.path(prev_q, "lda"), "vocab"), "[0-9]+")))

docs_all = readRDS(file.path(prev_q, "lda", "docsall.rds"))
vocab = readRDS(file.path(prev_q, "lda", paste0("vocab", max_voc, ".rds")))
lda = readRDS(file.path(prev_q, "lda", "ldachunked.rds"))
assignments = getAssignments(lda)

setwd(folder)
dir.create("lda")
setwd("lda")
saveRDS(vocab, "init_vocab.rds")
saveRDS(docs_all, "init_docsall.rds")
saveRDS(lda, "init_ldachunked.rds")

message("start fitting")
i = 1
chunk = filterDate(obj, s.date = cuts[i], e.date = cuts[i+1]-1)
wl = makeWordlist(chunk$text)
tmp = wl$words[wl$wordtable > vocab.limit]
#tmp = wl$words[wl$wordtable > vocab.limit & wl$wordtable > min(vocab.rel*sum(wl$wordtable), vocab.max)]
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
n.init = sum(names(docs_all) %in% obj$meta$id[obj$meta$date >= cache[i] & obj$meta$date < cuts[i]])
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
document_sums = cbind(getDocument_sums(lda), res$document_sums[,(n.init+1):length(docs)])

topics = matrix(as.integer(
  table(unlist(assignments) + 1, #Topics
        factor(unlist(lapply(docs_all, function(x) x[1,])) + 1, seq_len(length(vocab)), vocab)) #Vocab
), nrow = K)
colnames(topics) = vocab
lda = LDA(assignments = assignments, topics = topics, document_sums = document_sums,
          param = list(K = K, alpha = alpha, eta = eta, num.iterations = num.iterations))

saveRDS(vocab, file = paste0("vocab.rds"))
saveRDS(docs, file = paste0("docs.rds"))
saveRDS(docs_all, file = paste0("docsall.rds"))
saveRDS(lda, file = paste0("ldachunked.rds"))

difftime(Sys.time(), starttime, units = "mins")
