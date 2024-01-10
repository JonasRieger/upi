# 2h, 64GB

library(tmT)
library(tosca)
library(data.table)
library(lubridate)
library(rollinglda)
library(ldaPrototype)
library(ggplot2)
library(GGally)
library(ggthemes)

rootpath = "//media/TextMining/DoCMA/data"
setwd(rootpath)

starttime = Sys.time()

objlist = list()
files_welt = setdiff(list.files(recursive = TRUE, path = file.path("Welt", "data")),
                     readLines(file.path("Working_Paper_Uncertainty", "welt.txt")))
files_sz = setdiff(list.files(path = file.path("Sueddeutsche", "ftp-SZ"), recursive = TRUE),
                   readLines(file.path("Working_Paper_Uncertainty", "sz.txt")))
files_hb = setdiff(list.files(path = file.path("HB_WiWo", "HB", "data")),
                   readLines(file.path("Working_Paper_Uncertainty", "hb.txt")))
if(length(files_welt) > 0 && length(files_sz) > 0 && length(files_hb) > 0){
  ## Welt
  setwd(file.path(rootpath, "Welt" ,"data"))
  obj = readNexis(file = files_welt, encoding = "UTF-8")
  obj = filterID(obj, obj$meta$id[obj$meta$resource %in% c("Die Welt", "DIE WELT")])
  obj$text = obj$text[!duplicated(names(obj$text))]
  obj$meta = obj$meta[match(names(obj$text), obj$meta$id),]
  objlist[[1]] = obj

  ## SZ
  setwd(file.path(rootpath, "Sueddeutsche", "ftp-SZ"))
  obj = readSZ(file = files_sz)
  obj$text = lapply(obj$text, function(x) paste(unlist(x), collapse = " "))
  obj$text = obj$text[!duplicated(names(obj$text))]
  obj$meta = obj$meta[match(names(obj$text), obj$meta$id),]
  objlist[[2]] = obj

  ## HB
  setwd(file.path(rootpath, "HB_WiWo", "HB", "data"))
  obj = readHBWiWo(file = files_hb)
  obj$text = obj$text[!duplicated(names(obj$text))]
  obj$meta = obj$meta[match(names(obj$text), obj$meta$id),]
  objlist[[3]] = obj

  ## merge Welt, SZ, HB
  e.date = min(do.call("c", lapply(objlist, function(x) max(x$meta$date, na.rm = TRUE))))
  if (e.date != ceiling_date(e.date, "month")-1){
    message("rounding e.date (actually ", e.date, ") to last day of month: ",
            ceiling_date(e.date, "month")-1)
    e.date = ceiling_date(e.date, "month")-1
  }

  obj = mergeTextmeta(objlist)
  rm(objlist)
  gc()

  obj$text = removeUmlauts(obj$text)
  obj$text = removeHTML(obj$text, hex = FALSE, symbols = TRUE)
  obj$text = removeUmlauts(obj$text)
  obj$text = removeXML(obj$text)
  obj$text = lapply(obj$text, function(x) gsub("&[^;]*;", " ", x))
  obj$text = lapply(obj$text, function(x) gsub("\u00AD", "", x)) #hier space einfuegen
  obj$text = lapply(obj$text, function(x) gsub("\u00A0", "", x)) #hier space einfuegen

  message(sum(duplicated(names(obj$text))), " duplicates deleted")
  obj$text = obj$text[!duplicated(names(obj$text))]

  ## save new objects
  setwd(file.path(rootpath, "Working_Paper_Uncertainty"))
  writeLines(as.character(as.Date(readLines("folder.txt"))+1), "sdate.txt")
  writeLines(as.character(e.date), "folder.txt")
  dir.create(as.character(e.date))
  setwd(as.character(e.date))

  saveRDS(obj, "obj_new.rds")
  id = names(which(filterWord(obj$text, "wirtschaft", ignore.case = TRUE, out = "bin")))
  obj2 = filterID(obj, id)
  saveRDS(obj2, "obj_new_wirtschaft.rds")
  id = names(which(filterWord(obj2$text, "unsicher", ignore.case = TRUE, out = "bin")))
  obj3 = filterID(obj2, id)
  saveRDS(obj3, "obj_new_wirtschaft_unsicher.rds")

  ## merge with older objects and save
  setwd(file.path(rootpath, "Working_Paper_Uncertainty"))
  writeLines(c(readLines("welt.txt"), files_welt), "welt.txt")
  writeLines(c(readLines("sz.txt"), files_sz), "sz.txt")
  writeLines(c(readLines("hb.txt"), files_hb), "hb.txt")
  obj = mergeTextmeta(list(readRDS("obj_updated.rds"), obj))
  saveRDS(obj, "obj_updated.rds")
  rm(obj)
  gc()
  obj2 = mergeTextmeta(list(readRDS("obj_updated_wirtschaft.rds"), obj2))
  saveRDS(obj2, "obj_updated_wirtschaft.rds")
  rm(obj2)
  gc()
  obj3 = mergeTextmeta(list(readRDS("obj_updated_wirtschaft_unsicher.rds"), obj3))
  saveRDS(obj3, "obj_updated_wirtschaft_unsicher.rds")
  rm(obj3)
  gc()

  difftime(Sys.time(), starttime, units = "hours")

  rm(list = ls())
  gc()
  ####### data_prep:
  message("#########\ndata_prep\n#########")
  starttime = Sys.time()

  setwd(file.path("//media/TextMining/DoCMA/data", "Working_Paper_Uncertainty"))
  folder = readLines("folder.txt")
  s.date = as.Date(readLines("sdate.txt"))
  e.date = as.Date(folder)

  sw = union(removeUmlauts(tm::stopwords("de")), readLines("stopwords_deutsch_lang.txt"))

  setwd(folder)

  tablist = list()

  ## save clean, wl, docs and vocab
  for(type in c("_wirtschaft_unsicher", "_wirtschaft", "")){
    obj = readRDS(file.path("..", paste0("obj_updated", type, ".rds")))
    obj$text = obj$text[!duplicated(names(obj$text))]
    obj$meta = obj$meta[!duplicated(obj$meta$id),]
    obj = filterDate(obj, s.date = s.date, e.date = e.date)
    obj = cleanTexts(obj, sw = sw)
    saveRDS(obj, paste0("clean_new", type, ".rds"))
    obj = mergeTextmeta(list(
      readRDS(file.path("..", paste0("clean_updated", type, ".rds"))),
      obj))
    saveRDS(obj, file.path("..", paste0("clean_updated", type, ".rds")))

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

  rm(list = ls())
  gc()
setwd("..")
  ####### roll_update:
  message("#########\nroll_update\n#########")

  folder = readLines("folder.txt")
  s.date = as.Date(readLines("sdate.txt"))
  e.date = as.Date(folder)

  # overwrites the manually set date above; assumes monthly updates
  chunks = seq.Date(from = s.date, to = e.date, by = "month")

  # load
  obj = readRDS("clean_updated_wirtschaft_unsicher.rds")
  init = readRDS(file.path(as.character(s.date-1), "roll.rds"))

  # modeling
  setwd(folder)

  obj = filterDate(obj, s.date = s.date, e.date = e.date)
  obj = textmeta(meta = obj$meta, text = obj$text)
  texts = obj$text
  dates = obj$meta$date[match(names(texts), obj$meta$id)]

  roll = updateRollingLDA(init, texts, dates, chunks, "3 quarter")
  saveRDS(roll, "roll.rds")

  rm(list = ls())
  gc()
setwd("..")
  ####### analysis:
  message("#########\nanalysis\n#########")

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

  folder = readLines("folder.txt")

  counts = readRDS("counts.rds")
  obj = filterDate(readRDS("obj_updated_wirtschaft_unsicher.rds"), e.date = folder)
  setwd(folder)
  dir.create("analysis")

  counts = counts[type == "words"]

  pdf(file.path("analysis", "words.pdf"), width = 10, height = 7)
  print(ggplot(counts) + aes(x = date, y = n/1e6) + geom_line() +
          ylim(c(0, max(counts$n)/1e6)) +
          xlab("monthly") + ylab("n (in Million)"))
  print(ggplot(counts) + aes(x = date, y = n_wirtschaft/n) + geom_line() +
          ylim(c(0, max(counts$n_wirtschaft/counts$n))) +
          xlab("monthly"))
  print(ggplot(counts) + aes(x = date, y = n_wirtschaft_unsicher/n_wirtschaft) + geom_line() +
          ylim(c(0, max(counts$n_wirtschaft_unsicher/counts$n_wirtschaft))) +
          xlab("monthly"))
  print(ggplot(counts) + aes(x = date, y = n_wirtschaft_unsicher/n) + geom_line() +
          ylim(c(0, max(counts$n_wirtschaft_unsicher/counts$n))) +
          xlab("monthly"))
  dev.off()

  file = "roll.rds"
  roll = readRDS(file)
  K = getK(getLDA(roll))

  ## topTexts
  dir.create(file.path("analysis", "topTexts"))
  mat = topTexts(getLDA(roll), getNames(roll), 100, tnames = gsub(":", "", gsub(" ", "_", labels)))
  showTexts(obj, mat, file = "analysis/topTexts/")

  ## topTexts per Month
  dir.create(file.path("analysis", "topTextsPerMonth"))
  for(m in as.character(seq.Date(as.Date("2001-01-01"), as.Date(folder), "month"))){
    m = as.Date(m)
    doc_sums = list(document_sums = getDocument_sums(getLDA(roll))[,getDates(roll) >= m & getDates(roll) < m + months(1)])
    mat = topTexts(doc_sums, getNames(roll)[getDates(roll) >= m & getDates(roll) < m + months(1)], 10, tnames = gsub(":", "", gsub(" ", "_", labels)))
    dir.create(file.path("analysis", "topTextsPerMonth", as.character(substr(m, 0, 7))))
    showTexts(obj, mat, file = paste0("analysis/topTextsPerMonth/", as.character(substr(m, 0, 7)), "/"))
  }

  ## topwords
  topwords = topWords(getTopics(getLDA(roll)), 200)
  prop = round(rowSums(getTopics(getLDA(roll))) / sum(getTopics(getLDA(roll))) * 100, 4)
  out = rbind(prop, topwords)
  colnames(out) = labels
  row.names(out) = c("Proportion (%)", 1:200)
  write.csv(out,
            file = file.path("analysis", "topwords.csv"),
            fileEncoding = "UTF-8")

  ## topics
  tab = plotTopic(object = obj, ldaresult = getLDA(roll), ldaID = getNames(roll))
  tabrel = plotTopic(object = obj, ldaresult = getLDA(roll), ldaID = getNames(roll), rel = TRUE)
  colnames(tab) = colnames(tabrel) = c("date", labels)

  write.csv(tab, file = file.path("analysis", "topics.csv"),
            fileEncoding = "UTF-8", row.names = FALSE)
  write.csv(tabrel, file = file.path("analysis", "topics_rel.csv"),
            fileEncoding = "UTF-8", row.names = FALSE)

  tabrel_wirtschaft = cbind(date = tabrel$date,
                            tabrel[, -1] *
                              counts[match(tabrel$date, date), n_wirtschaft_unsicher/n_wirtschaft])
  write.csv(tabrel_wirtschaft, file = file.path("analysis", "topics_rel_to_wirtschaft.csv"),
            fileEncoding = "UTF-8", row.names = FALSE)
  tabrel_all = cbind(date = tabrel$date,
                     tabrel[, -1] *
                       counts[match(tabrel$date, date), n_wirtschaft_unsicher/n])
  write.csv(tabrel_all, file = file.path("analysis", "topics_rel_to_entire.csv"),
            fileEncoding = "UTF-8", row.names = FALSE)

  # delete topic 9 (Misc.) for upi calculation
  upi = data.frame(date = tabrel_all$date,
                   upi = rowSums(tabrel_all[,-c(1,10)]),
                   sub_real_economy = rowSums(tabrel_all[, c(1,3,4,11,13)+1]),
                   sub_politics = rowSums(tabrel_all[, c(2,5,6,8,12,14)+1]),
                   sub_financial_markets = rowSums(tabrel_all[, c(7,10)+1]))
  write.csv(upi, file = file.path("analysis", "upi.csv"),
            fileEncoding = "UTF-8", row.names = FALSE)

  upi_tmp = upi
  upi = unname(upi)
  sub = rbind(cbind(a = upi[, c(1,3)], b = "Real Economy"),
              cbind(a = upi[, c(1,4)], b = "Politics"),
              cbind(a = upi[, c(1,5)], b = "Financial Markets"))
  colnames(sub) = c("date", "upi", "Subindex")
  upi = upi_tmp

  pdf(file.path("analysis", "upi.pdf"), width = 10, height = 7)
  print(ggplot(upi) + aes(x = date, y = upi) + geom_line() + xlab("") + ylab("UPI (Share)"))
  print(ggplot(sub) +
          aes(x = date, y = upi, group = Subindex, color = Subindex) +
          geom_line() +
          xlab("") + ylab("Share") + theme(legend.position = "top"))
  dev.off()

  ## topics cosine
  # quarter
  quarters = lubridate::floor_date(getDates(roll), "quarter")
  xquarter = sort(unique(quarters))
  nquarter = length(xquarter)

  topicsq = lapply(xquarter, function(x){
    tmp = table(factor(unlist(getAssignments(getLDA(roll))[quarters == x])+1, levels = 1:getK(getLDA(roll))),
                factor(unlist(lapply(getDocs(roll)[quarters == x], function(y) y[1,]))+1,
                       levels = seq_len(length(getVocab(roll)))))
    tmp = matrix(as.integer(tmp), nrow = getK(getLDA(roll)))
    colnames(tmp) = getVocab(roll)
    tmp
  })

  topics = lapply(1:getK(getLDA(roll)), function(k){
    tmp = sapply(topicsq, function(x) x[k,])
    colnames(tmp) = paste0("Q", 1:nquarter)
    tmp
  })

  sims = lapply(1:getK(getLDA(roll)), function(k){
    cosineTopics(topics[[k]], pm.backend = "socket", ncpus = 4)
  })

  valq = sapply(sims, function(x) c(NA, x$sims[cbind(2:nquarter,2:nquarter-1)]))
  valq_first = sapply(sims, function(x) x$sims[,1])
  valq_last = sapply(sims, function(x) x$sims[nquarter,])
  saveRDS(sims, file.path("analysis", "sim_quarterly.rds"))

  # month
  months = lubridate::floor_date(getDates(roll), "month")
  xmonth = sort(unique(months))
  nmonth = length(xmonth)

  topicsm = lapply(xmonth, function(x){
    tmp = table(factor(unlist(getAssignments(getLDA(roll))[months == x])+1, levels = 1:getK(getLDA(roll))),
                factor(unlist(lapply(getDocs(roll)[months == x], function(y) y[1,]))+1,
                       levels = seq_len(length(getVocab(roll)))))
    tmp = matrix(as.integer(tmp), nrow = getK(getLDA(roll)))
    colnames(tmp) = getVocab(roll)
    tmp
  })

  topics = lapply(1:getK(getLDA(roll)), function(k){
    tmp = sapply(topicsm, function(x) x[k,])
    colnames(tmp) = paste0("Q", 1:nmonth)
    tmp
  })

  sims = lapply(1:getK(getLDA(roll)), function(k){
    cosineTopics(topics[[k]], pm.backend = "socket", ncpus = 4)
  })

  valm = sapply(sims, function(x) c(NA, x$sims[cbind(2:nmonth,2:nmonth-1)]))
  valm_first = sapply(sims, function(x) x$sims[,1])
  valm_last = sapply(sims, function(x) x$sims[nmonth,])
  saveRDS(sims, file.path("analysis", "sim_monthly.rds"))

  xmin = min(xmonth)

  cosine_quarterly = ggmatrix(lapply(1:14, function(i){
    ggplot() + geom_line(aes(x = xmonth, y = valm[,i]), col = "darkgrey") + ylim(c(0,1)) +
      geom_line(aes(x = xquarter, y = valq_first[,i], col = "green")) +
      geom_line(aes(x = xquarter, y = valq_last[,i], col = "red")) +
      geom_line(aes(x = xquarter, y = valq[,i])) +
      annotate("text", x = xmin, y = 0.05, label = labels[i], hjust = 0, vjust = 0)
  }), nrow = 7, ncol = 2, ylab = "Cosine Similarity")

  cosine_monthly = ggmatrix(lapply(1:14, function(i){
    ggplot() + geom_line(aes(x = xquarter, y = valq[,i]), col = "darkgrey") + ylim(c(0,1)) +
      geom_line(aes(x = xmonth, y = valm_first[,i], col = "green")) +
      geom_line(aes(x = xmonth, y = valm_last[,i], col = "red")) +
      geom_line(aes(x = xmonth, y = valm[,i])) +
      annotate("text", x = xmin, y = 0.05, label = labels[i], hjust = 0, vjust = 0)
  }), nrow = 7, ncol = 2, ylab = "Cosine Similarity")

  pdf(file.path("analysis", "topics_cosine_quarter.pdf"), width = 8, height = 10)
  print(cosine_quarterly)
  dev.off()

  tiff(file.path("analysis", "topics_cosine_quarter.tiff"),
       width = 1600, height = 2000, pointsize = 200, compression = "lzw", res = 200,
       type = "cairo")
  print(cosine_quarterly)
  dev.off()

  pdf(file.path("analysis", "topics_cosine_month.pdf"), width = 8, height = 10)
  print(cosine_monthly)
  dev.off()

  tiff(file.path("analysis", "topics_cosine_month.tiff"),
       width = 1600, height = 2000, pointsize = 200, compression = "lzw", res = 200,
       type = "cairo")
  print(cosine_monthly)
  dev.off()

  ## topWordsPerMonth
  topwordsm = lapply(topicsm, topWords, numWords = 50)
  topwordsm = lapply(1:K, function(k) sapply(seq_along(topwordsm), function(t) topwordsm[[t]][,k]))
  dir.create(file.path("analysis", "topWordsPerMonth"))
  for(i in 1:K){
    out = rbind(round(tabrel[, i+1] * 100, 4),
                round(tabrel_wirtschaft[, i+1] * 100, 4),
                round(tabrel_all[, i+1] * 1000, 4),
                topwordsm[[i]])
    colnames(out) = as.character(tabrel$date)
    row.names(out) = c("Share (in %) on \"wirtschaft\"&\"unsicher\"-corpus",
                       "Share (in %) on \"wirtschaft\"-corpus",
                       "Share (in Promille) on entire corpus",
                       1:50)
    write.csv(out,
              file = file.path("analysis", "topWordsPerMonth",
                               paste0(gsub(":", "", gsub(" ", "_", colnames(tabrel)[i+1])), ".csv")),
              fileEncoding = "UTF-8")
  }

  ## covid and fear
  vocab = getVocab(roll)

  ## covid
  covid = union(union(vocab[grepl("corona", vocab)], vocab[grepl("covid", vocab)]),
                vocab[grepl("pandemie", vocab)])
  covid_ids = match(covid, vocab)

  a = (unname(unlist(lapply(getDocs(roll), function(x) x[1,])))+1) %in% covid_ids
  b = (unlist(getAssignments(getLDA(roll)))+1)[a]
  d = floor_date(rep(getDates(roll),
                     lengths(getAssignments(getLDA(roll))))[a], "month")

  o = table(b, d)
  o = o[, colnames(o) >= c("2020-01-01")]
  tab = plotTopic(obj, getLDA(roll), getNames(roll), unit = "month")
  out = t(o)
  tmp = out
  colnames(tmp) = labels
  write.csv(tmp, file.path("analysis", "corona.csv"))
  out = out / tab[match(rownames(out), as.character(tab$date)), 2:15]
  colnames(out) = rownames(o)
  rownames(out) = colnames(o)
  colnames(out) = labels
  write.csv(out, file.path("analysis", "corona_rel_to_topic.csv"))
  writeLines(covid, file.path("analysis", "corona.txt"))

  ## fear
  fear = setdiff(vocab[grepl("angst", vocab)],
                 c("abstiegsangst", "gangster", "terrorangst", "kriegsangst"))
  fear_ids = match(fear, vocab)

  a = (unname(unlist(lapply(getDocs(roll), function(x) x[1,])))+1) %in% fear_ids
  b = (unlist(getAssignments(getLDA(roll)))+1)[a]
  d = floor_date(rep(getDates(roll),
                     lengths(getAssignments(getLDA(roll))))[a], "quarter")

  o = table(b, d)
  tab = plotTopic(obj, getLDA(roll), getNames(roll), unit = "month")
  out = t(o)
  tmp = out
  colnames(tmp) = labels
  write.csv(tmp, file.path("analysis", "fear.csv"))
  out = out / tab[match(rownames(out), as.character(tab$date)), 2:15]
  colnames(out) = rownames(o)
  rownames(out) = colnames(o)
  colnames(out) = labels
  write.csv(out, file.path("analysis", "fear_rel_to_topic.csv"))
  writeLines(fear, file.path("analysis", "fear.txt"))
}else{
  message("Keine neuen Daten!")
}
