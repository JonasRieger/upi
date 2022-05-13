library(rollingldaR3.6)
library(tosca)
library(lubridate)

folder = readLines("folder.txt")

#####
# Only the following line have to be modified manually for updates.
start_update = "2022-04-01"
end_update = "2022-04-30"
#####
end_update = folder
# overwrites the manually set date above; assumes monthly updates:
start_update = as.Date(end_update)+1 - months(1)
chunks = seq.Date(from = as.Date(start_update), to = as.Date(end_update), by = "month")

# load
obj = readRDS("clean_updated_wirtschaft_unsicher.rds")
init = readRDS(file.path(as.character(as.Date(start_update)-1), "roll.rds"))

# modeling
setwd(folder)

obj = filterDate(obj, s.date = start_update, e.date = end_update)
obj = textmeta(meta = obj$meta, text = obj$text)
texts = obj$text
dates = obj$meta$date[match(names(texts), obj$meta$id)]

roll = updateRollingLDA(init, texts, dates, chunks, "3 quarter")
saveRDS(roll, "roll.rds")
