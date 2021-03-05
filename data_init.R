starttime = Sys.time()

library(tosca)

rootpath = "//media/TextMining/DoCMA/data"
setwd(file.path(rootpath, "Working_Paper_Uncertainty"))

objlist = list()

## Welt
file = file.path(rootpath, "Welt", "welt-data.rds")
obj = filterDate(readRDS(file), s.date = as.Date("2001-01-01"))
obj = filterID(obj, obj$meta$id[obj$meta$resource %in% c("Die Welt", "DIE WELT")])
objlist[[1]] = obj

## SZ
file = file.path(rootpath, "Sueddeutsche", "SZ-data.rds")
obj = filterDate(readRDS(file), s.date = as.Date("2001-01-01"))
obj$text = lapply(obj$text, function(x) paste(unlist(x), collapse = " "))
objlist[[2]] = obj

## HB
file = file.path(rootpath, "HB_WiWo", "HB", "HB-data.rds")
objlist[[3]] = filterDate(readRDS(file), s.date = as.Date("2001-01-01"))

## merge Welt, SZ, HB
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

## save objects
new = !"obj_updated.rds" %in% list.files()
saveRDS(obj, "obj_init.rds")
if(new) saveRDS(obj, "obj_updated.rds")
id = names(which(filterWord(obj$text, "wirtschaft", ignore.case = TRUE, out = "bin")))
obj = filterID(obj, id)
saveRDS(obj, "obj_init_wirtschaft.rds")
if(new) saveRDS(obj, "obj_updated_wirtschaft.rds")
id = names(which(filterWord(obj$text, "unsicher", ignore.case = TRUE, out = "bin")))
obj = filterID(obj, id)
saveRDS(obj, "obj_init_wirtschaft_unsicher.rds")
if(new) saveRDS(obj, "obj_updated_wirtschaft_unsicher.rds")
gc()

difftime(Sys.time(), starttime, units = "hours")
