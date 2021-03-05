library(tosca)
library(data.table)

for(type in c("", "_wirtschaft", "_wirtschaft_unsicher")){
  obj = readRDS(paste0("obj_updated", type, ".rds"))
  
  folder = readLines("folder.txt")
  
  obj = filterDate(obj, e.date = as.Date(folder))
  obj$meta = obj$meta[order(obj$meta$date),]
  obj$text = obj$text[match(obj$meta$id, names(obj$text))]
  text = sapply(obj$text, paste0, collapse = " ")
  text = unname(text)
  
  out = as.data.table(obj$meta)
  out[, text := text]
  
  fwrite(out, file = paste0("obj_updated", type, ".csv"))
}
