ids <- unique(c(wave0$idauniq, wave1$idauniq, wave2$idauniq, wave3$idauniq, wave4$idauniq, wave5$idauniq, wave6$idauniq, mort$idauniq))
ids0 <- ids %in% wave0$idauniq
ids1 <- ids %in% wave1$idauniq
ids2 <- ids %in% wave2$idauniq
ids3 <- ids %in% wave3$idauniq
ids4 <- ids %in% wave4$idauniq
ids5 <- ids %in% wave5$idauniq
ids6 <- ids %in% wave6$idauniq
ids_mort <- ids %in% mort$idauniq
tracker <- data.frame(idauniq = ids, wave0 = ids0, wave1 = ids1, wave2 = ids2, wave3 = ids3, wave4 = ids4, wave5 = ids5, wave6 = ids6, mort = ids_mort)
tracker$numwaves <- tracker$wave0 + tracker$wave1 + tracker$wave2 + tracker$wave3 + tracker$wave4 + tracker$wave5 + tracker$wave6

w1_if <- data.frame(idauniq = wave1$idauniq, finstat = wave1$finstat)
w1_if$finstat <- as.character(w1_if$finstat)
w1_if$finstat <- ifelse(w1_if$finstat == "C1CM", w1_if$finstat, "noncore")
w1_if$wave <- rep(1, nrow(w1_if))
w3_if <- read.table("w3_if.txt", sep = "\t", header = T)
w3_if$finstat <- as.character(w3_if$finstat)
w3_if$finstat <- ifelse(w3_if$finstat == "C1CM" | w3_if$finstat == "C3CM" | w3_if$finstat == "C4CM" | w3_if$finstat == "C6CM", w3_if$finstat, "noncore")
w3_if$wave <- rep(3, nrow(w3_if))
w4_if <- data.frame(idauniq = wave4$idauniq, finstat = wave4$finstat)
w4_if$finstat <- as.character(w4_if$finstat)
w4_if$finstat <- ifelse(w4_if$finstat == "C1CM" | w4_if$finstat == "C3CM" | w4_if$finstat == "C4CM" | w4_if$finstat == "C6CM", w4_if$finstat, "noncore")
w4_if$wave <- rep(4, nrow(w4_if))
w6_if <- data.frame(idauniq = wave6$idauniq, finstat = wave6$finstatw6)
w6_if$finstat <- revalue(as.character(w6_if$finstat), c("1" = "C1CM", "7" = "C3CM", "14" = "C4CM", "25" = "C6CM"))
w6_if$finstat <- ifelse(w6_if$finstat == "C1CM" | w6_if$finstat == "C3CM" | w6_if$finstat == "C4CM" | w6_if$finstat == "C6CM", w6_if$finstat, "noncore")
w6_if$wave <- rep(6, nrow(w6_if))

id_finstat <- rbind(w1_if, w3_if, w4_if, w6_if)
id_freqs <- data.frame(idauniq = table(id_finstat$idauniq))
names(id_freqs) <- c("idauniq", "freq")
id_finstat <- merge(id_finstat, id_freqs, by = "idauniq")
id_finstat <- id_finstat[order(id_finstat$idauniq, id_finstat$wave),]
id_finstat <- id_finstat[!duplicated(id_finstat$idauniq),]

tracker <- merge(tracker, id_finstat[,1:2], by = "idauniq", all.x = T)


