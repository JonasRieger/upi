starttime = Sys.time()

library(tmT)
library(tosca)

rootpath = "//media/TextMining/DoCMA/data"

objlist = list()
files_welt = setdiff(list.files(recursive = TRUE, path = file.path("Welt", "data")),
                     readLines(file.path("Working_Paper_Uncertainty", "welt.txt")))
files_sz = setdiff(list.files(path = file.path("Sueddeutsche", "ftp-SZ")),
                   readLines(file.path("Working_Paper_Uncertainty", "sz.txt")))
files_hb = setdiff(list.files(path = file.path("HB_WiWo", "HB", "data")),
                   readLines(file.path("Working_Paper_Uncertainty", "hb.txt")))
if(length(files_welt) > 1 && length(files_sz) > 1 && length(files_hb) > 1){
  ## Welt
  setwd(file.path(rootpath, "Welt" ,"data"))
  obj = readNexis(file = files_welt, encoding = "UTF-8")
  obj = filterID(obj, obj$meta$id[obj$meta$resource %in% c("Die Welt", "DIE WELT")])
  objlist[[1]] = obj
  
  ## SZ
  files_sz = setdiff(list.files(path = file.path("Sueddeutsche", "ftp-SZ")),
                     readLines(file.path("Working_Paper_Uncertainty", "sz.txt")))
  setwd(file.path(rootpath, "Sueddeutsche", "ftp-SZ"))
  obj = readSZ(file = files_sz)
  obj$text = lapply(obj$text, function(x) paste(unlist(x), collapse = " "))
  objlist[[2]] = obj
  
  ## HB
  setwd(file.path(rootpath, "HB_WiWo", "HB", "data"))
  obj = readHBWiWo(file = files_hb)
  objlist[[3]] = obj
  
  ## merge Welt, SZ, HB
  e.date = min(do.call("c", lapply(objlist, function(x) max(x$meta$date, na.rm = TRUE))))
  
  obj = mergeTextmeta(objlist)
  rm(objlist)
  gc()
  
  obj$text = removeUmlauts(obj$text)
  obj$text = removeHTML(obj$text, hex = FALSE, symbols = TRUE)
  obj$text = removeUmlauts(obj$text)
  obj$text = removeXML(obj$text)
  obj$text = lapply(obj$text, function(x) gsub("&[^;]*;", " ", x))
  
  obj$meta = obj$meta[order(obj$meta$date),]
  obj$text = obj$text[match(obj$meta$id, names(obj$text))]
  obj$text = obj$text[!duplicated(obj$text)]
  obj$meta = obj$meta[obj$meta$id %in% names(obj$text),]
  
  ## save new objects
  setwd(file.path(rootpath, "Working_Paper_Uncertainty"))
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
}else{
  message("There is no new data!")
}
