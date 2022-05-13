starttime = Sys.time()

library(tosca)
library(data.table)

setwd(file.path("//media/TextMining/DoCMA/data", "Working_Paper_Uncertainty"))
folder = readLines("folder.txt")

sw = union(removeUmlauts(tm::stopwords("de")), readLines("stopwords_deutsch_lang.txt"))

setwd(folder)

tablist = list()

## save clean, wl, docs and vocab
for(type in c("_wirtschaft_unsicher", "_wirtschaft", "")){
  obj = readRDS(file.path("..", paste0("obj_updated", type, ".rds")))
  obj = filterDate(obj, e.date = as.Date(folder))
  obj = cleanTexts(obj, sw = sw)
  saveRDS(obj, file.path("..", paste0("clean_updated", type, ".rds")))
  
  obj = filterDate(obj, s.date = as.Date("2001-01-01"))
  
  tab = cbind(plotScot(obj, type = "docs"), type = "docs")
  colnames(tab) = c("date", paste0("n", type), "type")
  tab2 = cbind(plotScot(obj, type = "words"), type = "words")
  colnames(tab2) = c("date", paste0("n", type), "type")
  
  tablist[[type]] = rbindlist(list(tab, tab2))
  #wl = makeWordlist(obj$text)
  #saveRDS(wl, paste0("wl", type, ".rds"))
  #vocab = wl$words[wl$wordtable > 5]
  #saveRDS(vocab, paste0("vocab", type, ".rds"))
  #docs = LDAprep(obj$text, vocab)
  #saveRDS(docs, paste0("docs", type, ".rds"))
}

tab = merge(merge(tablist[[3]], tablist[[2]]), tablist[[1]])
saveRDS(tab, file = file.path("..", "counts.rds"))
fwrite(tab, file = file.path("..", "counts.csv"))

gc()

difftime(Sys.time(), starttime, units = "hours")
