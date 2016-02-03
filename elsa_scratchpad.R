index <- subset(index_file, select = c(idauniq, mortstat, yrdeath))
mor <- subset(mort, select = c(idauniq, dodyr))
ids <- subset(elsa, select = idauniq)
ids <- merge(ids, index, by = "idauniq", all.x = T)
ids <- merge(ids, mor, by = "idauniq", all.x = T)
ids$mor.death <- ifelse(is.na(ids$dodyr), 0, 1)

tmp <- cbind(with(elsa, aggregate(survival, by = list(status_packyrs), FUN = sum)), with(elsa, aggregate(event, by = list(status_packyrs), FUN = sum)))
rateratio(cbind(tmp[,4], tmp[,2]))

epitab(table(elsa$status_packyrs, elsa$event))
