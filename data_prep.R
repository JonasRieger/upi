starttime = Sys.time()

library(tosca)

setwd(file.path("//media/TextMining/DoCMA/data", "Working_Paper_Uncertainty"))
folder = readLines("folder.txt")

sw = union(removeUmlauts(tm::stopwords("de")), readLines("stopwords_deutsch_lang.txt"))

setwd(folder)

## save clean, wl, docs and vocab
for(type in c("_wirtschaft_unsicher", "_wirtschaft", "")){
  obj = readRDS(file.path("..", paste0("obj_updated", type, ".rds")))
  obj = filterDate(obj, e.date = as.Date(folder))
  obj = cleanTexts(obj, sw = sw)
  saveRDS(obj, file.path("..", paste0("clean_updated", type, ".rds")))
  wl = makeWordlist(obj$text)
  saveRDS(wl, paste0("wl", type, ".rds"))
  vocab = wl$words[wl$wordtable > 5]
  saveRDS(vocab, paste0("vocab", type, ".rds"))
  docs = LDAprep(obj$text, vocab)
  saveRDS(docs, paste0("docs", type, ".rds"))
}

gc()

difftime(Sys.time(), starttime, units = "hours")
