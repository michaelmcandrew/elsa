#Amelia scratchpad

tmp <- subset(elsa, select = c(numsmok, indager, startsmk, cigst1, wealth5, income5, bmivg6, age_group, sex, ethnicr, topqual))
# tmp <- tmp[tmp$cigst1 != "never",]
tmp <- tmp[tmp$cigst1 == "ex",]
# tmp$cigst1 <- NULL
a.out <- amelia(tmp, m = 5, noms = c("wealth5", "income5", "bmivg6", "sex", "ethnicr", "topqual"), idvars = c("cigst1", "age_group")) #left age group out because indager is in

#appears that imputing numsmok with cigst1 in the model doesn't work, because numsmok is only a number when cigst1 is ex (i.e. no variance). When imputing numsmok, restrict dataset to ex-smokers? Don't want to impute for never or current smokers

tmp2 <- data.frame(orig = tmp$startsmk, imp1 = a.out$imputations[[1]]$startsmk, imp2 = a.out$imputations[[2]]$startsmk, imp3 = a.out$imputations[[3]]$startsmk, imp4 = a.out$imputations[[4]]$startsmk, imp5 = a.out$imputations[[5]]$startsmk)

se.mean <- function(x) sqrt(var(x, na.rm = T) / length(na.omit(x)))
ex.mean <- function(x) aggregate(x[,3], by = list(x[,8]), FUN = mean)
ex.se <- function(x) aggregate(x[,3], by = list(x[,8]), FUN = se.mean)
b.out <- lapply(a.out$imputations, FUN = ex.mean)
b.out <- as.data.frame(b.out)[,c(2,4,6,8,10)]
se.out <- lapply(a.out$imputations, FUN = ex.se)
se.out <- as.data.frame(se.out)[,c(2,4,6,8,10)]
mi.meld(q = b.out, se = se.out, byrow = F)