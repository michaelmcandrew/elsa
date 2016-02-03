# This script creates a survival model for different SES and smoking-status groups. Wave 1 is the baseline, but some baseline data is taken from 'wave 0' HSE data (1998-2001) and physical activity baseline data is taken from wave 2.

library(plyr) # for 'revalue'
library(dplyr) # for 'mutate'
library(survival) # for hazard models
library(RColorBrewer) # for colouring in charts 

# import. Waves 3-6 are not used except in the 'tracker' showing which each individual responded to.

wave0_1998 <- read.table("wave_0_1998_data.tab", sep="\t", header = T)
wave0_1999 <- read.table("wave_0_1999_data.tab", sep="\t", header = T)
wave0_2001 <- read.table("wave_0_2001_data.tab", sep="\t", header = T)
wave1 <- read.table("wave_1_core_data_v3.tab", sep="\t", header = T)
wave1.fin <- read.table("wave_1_financial_derived_variables.tab", sep="\t", header = T)
wave1.edu <- read.csv("elsa_w1_education.csv", sep =",", header = T)
wave2 <- read.table("wave_2_core_data_v4.tab", sep="\t", header = T)
wave2.der <- read.table("wave_2_derived_variables.tab", sep="\t", header = T)
wave3 <- read.table("wave3_subset.txt", sep="\t", header = T)
wave4 <- read.table("wave_4_elsa_data_v3.tab", sep="\t", header = T)
wave5 <- read.table("wave_5_elsa_data_v4.tab", sep="\t", header = T)
wave6 <- read.table("wave_6_elsa_data_v2.tab", sep="\t", header = T)
index_file <- read.table("index_file_wave_0-wave_5_v2.tab", sep="\t", header = T)

# create wave0 dataset

wave0_1998_sub <- subset(wave0_1998, select = c("idauniq", "ager", "sex", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "sclass", "bmivg6", "passm"))
wave0_1998_sub$year <- rep(1998, nrow(wave0_1998_sub))
wave0_1999_sub <- subset(wave0_1999, select = c("idauniq", "ager", "sex", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "sclass", "bmivg6", "passm"))
wave0_1999_sub$year <- rep(1999, nrow(wave0_1999_sub))
wave0_2001_sub <- subset(wave0_2001, select = c("idauniq", "ager", "sex", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "hrpsoccl", "bmivg6", "passm"))
names(wave0_2001_sub)[grep("hrpsoccl", names(wave0_2001_sub))] <- "sclass"
wave0_2001_sub$year <- rep(2001, nrow(wave0_2001_sub))

wave0 <- rbind(wave0_1998_sub, wave0_1999_sub, wave0_2001_sub)
wave0 <- wave0[wave0$ager >= 50,]
wave0 <- wave0[wave0$cigst1 >= 0,]
wave0$sex <- as.factor(wave0$sex)
wave0$sex <- revalue(wave0$sex, c("1" = "male", "2" = "female"))
wave0$cigst1 <- as.character(wave0$cigst1)
wave0$cigst1 <- revalue(wave0$cigst1, c("1" = "never", "2" = "ex", "3" = "ex", "4" = "current"))
mean_start_age <- mean(wave0$startsmk[wave0$startsmk > 0]) #for missing data. crude
wave0$startsmk_imp <- ifelse((wave0$cigst1 == "ex" | wave0$cigst1 == "current") & wave0$startsmk < 0, mean_start_age, wave0$startsmk)
wave0$smk_yrs_current <- ifelse(wave0$cigst1 == "current", wave0$ager - wave0$startsmk_imp, 0)
wave0$packyrs_current <- wave0$smk_yrs_current * (wave0$cigdyal/20)
wave0$packyrs_current <- ifelse(wave0$packyrs_current < 0, 0, wave0$packyrs_current)
mean_numsmok <- mean(wave0$numsmok[wave0$numsmok > 0]) #for missing data. crude
wave0$numsmok_imp <- ifelse(wave0$cigst1 == "ex" & wave0$numsmok < 0, mean_numsmok, wave0$numsmok)
wave0$smk_yrs_ex <- ifelse(wave0$cigst1 == "ex", wave0$ager - wave0$startsmk_imp - wave0$endsmoke, 0)
wave0$smk_yrs_ex <- ifelse(wave0$smk_yrs_ex < 0, 0, wave0$smk_yrs_ex)
wave0$packyrs_ex <- wave0$smk_yrs_ex * (wave0$numsmok_imp/20)
wave0$packyrs_all <- wave0$packyrs_current + wave0$packyrs_ex
wave0$packyrs_grp <- findInterval(wave0$packyrs_all, c(0, 20, 40))
wave0$packyrs_grp <- as.character(wave0$packyrs_grp)
wave0$packyrs_grp <- revalue(wave0$packyrs_grp, c("1" = "<20", "2" = "20-40", "3" = "40+"))
wave0$status_packyrs <- ifelse(wave0$cigst1 == "never", "never", paste0(wave0$cigst1, wave0$packyrs_grp))
wave0$status_packyrs <- factor(wave0$status_packyrs, levels = c("never", "ex<20", "current<20", "ex20-40", "current20-40", "ex40+", "current40+"))
wave0$sclass <- as.factor(wave0$sclass)
wave0$sclass <- revalue(wave0$sclass, c("1" = "I-II", "2" = "I-II", "3" = "III-NM", "4" = "III-M", "5" = "IV-V", "6" = "IV-V", "7" = "0", "8" = "0", "-1" = "0")) # CHECK MISSING DATA!
wave0$bmivg6 <- revalue(as.factor(wave0$bmivg6), c("-1" = "0", "1" = "<20", "2" = "20-25", "3" = "25-30", "4" = "30-35", "5" = "35-40", "6" = "40+"))
wave0$bmivg6 <- relevel(wave0$bmivg6, ref = "20-25")
wave0$passm <- revalue(as.factor(wave0$passm), c("-9" = "0", "1" = "yes", "2" = "no"))
wave0$passm <- relevel(wave0$passm, ref = "no")

# track participation in surveys

ids <- unique(c(wave0$idauniq, wave1$idauniq, wave2$idauniq, wave3$idauniq, wave4$idauniq, wave5$idauniq, wave6$idauniq))
ids0 <- ids %in% wave0$idauniq
ids1 <- ids %in% wave1$idauniq
ids2 <- ids %in% wave2$idauniq
ids3 <- ids %in% wave3$idauniq
ids4 <- ids %in% wave4$idauniq
ids5 <- ids %in% wave5$idauniq
ids6 <- ids %in% wave6$idauniq
ids_index <- ids %in% index_file$idauniq
tracker <- data.frame(idauniq = ids, wave0 = ids0, wave1 = ids1, wave2 = ids2, wave3 = ids3, wave4 = ids4, wave5 = ids5, wave6 = ids6, index = ids_index)
tracker$numwaves <- tracker$wave0 + tracker$wave1 + tracker$wave2 + tracker$wave3 + tracker$wave4 + tracker$wave5 + tracker$wave6

index_file_subset <- data.frame(idauniq = index_file$idauniq, mortstat = index_file$mortstat, yrdeath = index_file$yrdeath, mortwave = index_file$mortwave, maincod = index_file$maincod)
tracker <- merge(tracker, index_file_subset, by = "idauniq")
tracker$mortwave <- as.character(tracker$mortwave)

tracker$mortwave <- revalue(tracker$mortwave, c("0" = -1, "11" = 0, "12" = 0, "13" = 0, "21" = 1, "22" = 1, "23" = 1, "31" = 2, "32" = 2, "33" = 2, "41" = 3, "42" = 3, "43" = 3, "51" = 4, "52" = 4, "53" = 4, "61" = 5, "63" = 5)) #mortwave now shows which wave they died after
tracker <- mutate(tracker, finalyear = max.col(tracker[2:8], 'last'), streak = rowSums(tracker[2:8]) == finalyear)
tracker$finalyear <- tracker$finalyear - 1
tracker$mortcomplete <- ifelse((tracker$mortwave == tracker$finalyear) & tracker$streak == TRUE, 1, 0)
tracker$complete <- ifelse(tracker$mortcomplete == 1 | tracker$numwaves == 7, 1, 0)

# create dataset for analysis: age, sex, ethnicity (NOT YET INCLUDED), SES measures, current smoking, pack-years, age started, age stopped, mortstat, maincod, mortwave; ARE THERE FURTHER SES-RELATED FACTORS?

tracker2 <- tracker[tracker$wave1 == T,] #only those in Wave 1
elsa <- data.frame(idauniq = tracker2$idauniq, wave0 = tracker2$wave0)
elsa <- elsa[elsa$wave0 == T,] #remove those not in wave 0 - REMOVES 2,000??
fin <- subset(wave1.fin, select = c("idauniq", "nfwq5_bu_s", "yq5_bu_s")) #wealth quintile, income quintile
names(fin)[grep("nfwq5_bu_s", names(fin))] <- "wealth5"
names(fin)[grep("yq5_bu_s", names(fin))] <- "income5"
elsa <- merge(elsa, fin, by = "idauniq")
wealth.missing <- is.na(elsa$wealth5)
elsa$wealth5 <- as.factor(ifelse(wealth.missing, 0, elsa$wealth5))
elsa$wealth5 <- factor(elsa$wealth5, levels = c("5", "4", "3", "2", "1", "0"))
income.missing <- is.na(elsa$income5)
elsa$income5 <- as.factor(ifelse(income.missing, 0, elsa$income5))
elsa$income5 <- factor(elsa$income5, levels = c("5", "4", "3", "2", "1", "0"))
wave0.sub <- subset(wave0, select = c("idauniq", "sex", "cigst1", "packyrs_all", "packyrs_grp", "status_packyrs", "startsmk", "sclass", "bmivg6", "passm"))
elsa <- merge(elsa, wave0.sub, by = "idauniq")
accom <- subset(wave1, select = c("hopro01", "hopro02", "hopro03", "hopro04", "hopro05", "hopro06", "hopro07", "hopro08", "hopro09", "hopro10"))
resp_probs <- c(5, 6, 8, 10, 12)
accom.probs <- t(apply(accom, 1, function(x) resp_probs %in% x))
wave1.accom <- data.frame(idauniq = wave1$idauniq, accomprobs = ifelse(rowSums(accom.probs) > 0, 1, 0))
elsa <- merge(elsa, wave1.accom, by = "idauniq")
elsa$accomprobs <- as.factor(elsa$accomprobs)
wave1.age <- data.frame(idauniq = wave1$idauniq, ager = wave1$indager)
wave1.age$age_group <- findInterval(wave1.age$ager, c(50, 60, 70, 80))
wave1.age$age_group <- as.factor(wave1.age$age_group)
wave1.age$age_group <- revalue(wave1.age$age_group, c("1" = "50-59", "2" = "60-69", "3" = "70-79", "4" = "80+"))
elsa <- merge(elsa, wave1.age, by = "idauniq")
edu <- data.frame(idauniq = wave1.edu$idauniq, edu = wave1.edu$qual3)
edu$edu <- revalue(edu$edu, c("higher than a-level" = "a-level+", "o-level or equivalent" = "o-level", "less than o-level or equiv" = "below o-level", "don't know" = "missing", "not asked" = "missing", "refused" = "missing"))
edu$edu <- factor(edu$edu, levels = c("a-level+", "o-level", "below o-level", "missing"))
elsa <- merge(elsa, edu, by = "idauniq")

sclass.missing <- is.na(elsa$sclass)
elsa$sclass <- as.factor(ifelse(sclass.missing, "0", as.character(elsa$sclass)))
elsa$sclass <- factor(elsa$sclass, levels = c("I-II", "III-NM", "III-M", "IV-V", "0"))

bmi.missing <- is.na(elsa$bmivg6)
elsa$bmivg6 <- as.factor(ifelse(bmi.missing, "0", as.character(elsa$bmivg6)))
elsa$bmivg6 <- factor(elsa$bmivg6, levels = c("20-25", "<20", "25-30", "30-35", "35-40", "40+", "0"))

deaths <- subset(index_file, select = c("idauniq", "mortstat", "maincod", "yrdeath"))
elsa <- merge(elsa, deaths, by = "idauniq")
elsa <- elsa[elsa$idauniq != 104145,] # this case is weird - mortstat = 2 (i.e. dead), yrdeath = -2 (i.e. info not avaialble)

elsa$passm <- (factor(elsa$passm, levels = c("no", "yes")))

o.pa1 <- wave1$heacta
o.pa2 <- wave1$heactb
o.pa3 <- wave1$heactc
pa1 <- ifelse(o.pa1 < 0, NA, ifelse(o.pa1 == 1, 0, 1))
pa2 <- ifelse(o.pa2 < 0, NA, ifelse(o.pa2 == 1, 0, 1))
pa3 <- ifelse(o.pa3 < 0, NA, ifelse(o.pa3 == 1, 0, 1))
pa <- pa1 + pa2 + pa3
pa <- ifelse(pa > 0, 1, 0)
pa <- ifelse(o.pa1 == 4 & o.pa2 == 4 & o.pa3 == 4, 0, pa)
pa <- ifelse(o.pa3 == 1 | o.pa3 == 2 | o.pa3 == 3 | o.pa2 == 3, 1, pa)
pa <- ifelse(o.pa2 == 1 | o.pa2 == 2 | o.pa1 == 3, 2, pa)
pa <- ifelse(o.pa1 == 1 | o.pa1 == 2, 3, pa)
pa <- as.character(pa)
pa[is.na(pa)] <- "missing"
pa <- factor(revalue(pa, c("0" = "sedentary", "1" = "low", "2" = "moderate", "3" = "high")), levels = c("high", "moderate", "low", "sedentary", "missing"))
wave1.pa <- data.frame(idauniq = wave1$idauniq, palevel = pa)
elsa <- merge(elsa, wave1.pa, by = "idauniq")

# diet

attach(wave4)
w4porsal <- ifelse(scvega >= 0, scvega, NA)
w4porveg <- ifelse(scvegb >= 0, scvegb/3, NA)
w4porpul <- ifelse(scvegc >= 0, scvegc/2, NA)
w4porpul <- ifelse(w4porpul > 1, 1, w4porpul)
w4porvegd <- ifelse(scvegd >= 0, scvegd/3, NA)
w4porfrdrnk <- ifelse(scfruii >= 0, scfruii, NA)
w4porfrdrnk <- ifelse(w4porfrdrnk >= 1, 1, w4porfrdrnk)
w4porfrlg <- ifelse(scfruid >= 0, scfruid * 2, NA)
w4porfrsm <- ifelse(scfruib >= 0, scfruib/2, NA)
w4poroth <- ifelse(scfruia >= 0 & scfruie >= 0 & scfruic >= 0, scfruia + scfruie + scfruic, NA)
w4porfroz <- ifelse(scfruif >= 0, scfruif/3, NA)
w4porfdish <- ifelse(scfruih >= 0, scfruih/3, NA)
w4porfdry <- ifelse(scfruig >= 0, scfruig, NA)
detach(wave4)

df <- data.frame(w4porsal = w4porsal, w4porveg = w4porveg, w4porpul = w4porpul, w4porvegd = w4porvegd, w4porfrdrnk = w4porfrdrnk, w4porfrlg = w4porfrlg, w4porfrsm = w4porfrsm, w4poroth = w4poroth, w4porfroz = w4porfroz, w4porfdish = w4porfdish, w4porfdry = w4porfdry)
df.n <- df
df.n[is.na(df.n)] <- 0
df$w4porfrt <- ifelse(is.na(w4porfrlg) & is.na(w4porfrsm) & is.na(w4poroth), NA, df.n$w4porfrlg + df.n$w4porfrsm + df.n$w4poroth)
df$w4vegpor <- ifelse(is.na(w4porsal) & is.na(w4porveg) & is.na(w4porpul) & is.na(w4porvegd), NA, df.n$w4porsal + df.n$w4porveg + df.n$w4porpul + df.n$w4porvegd)
df$w4frtpor <- ifelse(is.na(w4porfrdrnk) & is.na(w4porfrlg) & is.na(w4porfrsm) & is.na(w4poroth) & is.na(w4porfdry) & is.na(w4porfroz) & is.na(w4porfdish), NA, df.n$w4porfrdrnk + df.n$w4porfrlg + df.n$w4porfrsm + df.n$w4poroth + df.n$w4porfdry + df.n$w4porfroz + df.n$w4porfdish)
df.n <- df
df.n[is.na(df.n)] <- 0
df$w4porfv <- ifelse(is.na(df$w4vegpor) & is.na(df$w4frtpor), NA, df.n$w4vegpor + df.n$w4frtpor)
wave4.ftvg <- data.frame(idauniq = wave4$idauniq, ftvg_4 = df$w4porfv)

df <- data.frame(idauniq = wave5$idauniq, veg = wave5$scveg, fruit = wave5$scfru)
df$veg <- ifelse(df$veg < 0, NA, df$veg)
df$fruit <- ifelse(df$fruit < 0, NA, df$fruit)
df.n <- df
df.n[is.na(df.n)] <- 0
df$ftvgtot <- ifelse(is.na(df$veg) & is.na(df$fruit), NA, df.n$veg + df.n$fruit)
wave5.ftvg <- data.frame(idauniq = df$idauniq, ftvg_5 = df$ftvgtot)

df <- data.frame(idauniq = wave6$idauniq, veg = wave6$scveg, fruit = wave6$scfru)
df$veg <- ifelse(df$veg < 0, NA, df$veg)
df$fruit <- ifelse(df$fruit < 0, NA, df$fruit)
df.n <- df
df.n[is.na(df.n)] <- 0
df$ftvgtot <- ifelse(is.na(df$veg) & is.na(df$fruit), NA, df.n$veg + df.n$fruit)
wave6.ftvg <- data.frame(idauniq = df$idauniq, ftvg_6 = df$ftvgtot)

elsa <- merge(elsa, wave4.ftvg, by = "idauniq", all.x = T)
elsa <- merge(elsa, wave5.ftvg, by = "idauniq", all.x = T)
elsa <- merge(elsa, wave6.ftvg, by = "idauniq", all.x = T)
p <- ncol(elsa)
elsa$ftvg <- apply(elsa[,(p-2):p], 1, mean, na.rm = T)
elsa$ftvg <- findInterval(elsa$ftvg, 0:8)
elsa <- subset(elsa, select = -c(ftvg_4, ftvg_5, ftvg_6))

# describe smoking by SES

meanci <- function(x, a = 0.975) {
	m <- mean(x)
	n <- length(x)
	s <- sd(x)
	SE <- s/sqrt(n)
	t <- qt(a, df = n-1)
	int <- t * SE
	return(c(m, m - int, m + int))
}

wealth_means <- as.matrix(`aggregate`(elsa$packyrs_all, by = list(elsa$wealth5), FUN = meanci))
wealth_means <- wealth_means[,2:4]
wealth_means.all <- t(as.matrix(meanci(elsa$packyrs_all)))
wealth_means <- rbind(wealth_means, wealth_means.all)
wealth_means <- data.frame(mean = as.numeric(wealth_means[,1]), ci_l = as.numeric(wealth_means[,2]), ci_u = as.numeric(wealth_means[,3]), row.names = c(5:1, "missing", "total"))

par(mar=c(7,3,3,3))
y <- barplot(as.numeric(wealth_means$mean), names.arg = row.names(wealth_means), las = 2, ylim = c(0, 25))
segments(y, as.numeric(wealth_means$ci_u), y, as.numeric(wealth_means$ci_l), lwd = 2)
segments(y - 0.3, as.numeric(wealth_means$ci_l), y + 0.3, as.numeric(wealth_means$ci_l), lwd=1)
segments(y - 0.3, as.numeric(wealth_means$ci_u), y + 0.3, as.numeric(wealth_means$ci_u), lwd=1)


# prototype survival analysis. Cox's model allows for time-dependent covariates (e.g. smoking status at each follow-up). However, historical smoking is likely to be a bigger cause of disease than smoking within the follow-up survey year, so this was used as the smoking measure. Does this seem right?

elsa <- elsa[elsa$mortstat > 0,] # excludes no permission or 'dead from other sources'
startyr <- 2002
endyr <- 2012
elsa$survival <- ifelse(elsa$mortstat == 1, endyr - startyr, elsa$yrdeath - startyr)
elsa$all_cause_mort <- elsa$mortstat - 1
elsa$resp_mort <- ifelse(elsa$maincod == 3, 1, 0)
Surv.prot <- Surv(elsa$survival, elsa$all_cause_mort)

coxph.out <- function(model) {
	x <- summary(model)
	x1 <- x$conf.int
	x2 <- coef(x)
	expcoef <- round(x1[,1],1)
	lower <- round(x1[,3],1)
	upper <- round(x1[,4],1)
	p <- x2[,5]
	stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", ""))))
	results <- paste(expcoef, " (", lower, "-", upper, ")", stars, sep = "")
	return(cbind(row.names(x1), results))
}

covariates1 <- c("status_packyrs", "income5", "wealth5", "sclass", "educend", "bmivg6", "passm", "palevel", "accomprobs", "age_group", "sex")
ncovs <- length(covariates1)
unadj <- NULL
for (i in 1:ncovs) {
	surv.mod <- coxph(formula(paste0("Surv.prot", "~", covariates1[i])), data = elsa)
	surv.mod.out <- coxph.out(surv.mod)
	unadj <- rbind(unadj, surv.mod.out)	
}

covariates2 <- c("status_packyrs", "income5", "wealth5", "sclass", "educend", "bmivg6", "passm", "palevel", "accomprobs")
ncovs <- length(covariates2)
adj_basic <- NULL
for (i in 1:ncovs) {
	surv.mod <- coxph(formula(paste0("Surv.prot", "~", covariates2[i], "+", "age_group", "+", "sex")), data = elsa)
	surv.mod.out <- coxph.out(surv.mod)
	surv.mod.out <- surv.mod.out[1:(nrow(surv.mod.out)-4),]
	adj_basic <- rbind(adj_basic, surv.mod.out)	
}
space <- matrix(rep(0,8), 4, 2)
adj_basic <- rbind(adj_basic, space)

covariates3 <- c("status_packyrs", "bmivg6", "passm", "palevel", "accomprobs")
ncovs <- length(covariates3)
adj_SES <- NULL
for (i in 1:ncovs) {
	surv.mod <- coxph(formula(paste0("Surv.prot", "~", covariates3[i], "+", "income5", "+", "wealth5", "+", "sclass", "+", "educend", "+", "age_group", "+", "sex")), data = elsa)
	surv.mod.out <- coxph.out(surv.mod)
	surv.mod.out <- surv.mod.out[1:(nrow(surv.mod.out)-20),]
	adj_SES <- rbind(adj_SES, surv.mod.out)	
}
space1 <- matrix(rep(0,32), 16, 2)
space2 <- matrix(rep(0,8), 4, 2)
adj_SES <- rbind(adj_SES[1:6,], space1, adj_SES[7:18,], space2)

adj <- coxph(formula(paste("Surv.prot", "~", paste0(covariates1, collapse = "+"))), data = elsa)
adj <- coxph.out(adj)

full.out <- cbind(unadj, adj_basic[,2], adj_SES[,2], adj[,2])
full.out <- data.frame(unadjusted = full.out[,2], age_sex_adj = full.out[,3], SES_adj = full.out[,4], fully_adj = full.out[,5], row.names = full.out[,1])
write.csv(full.out, file = "elsa_results.csv")

ns <- NULL
ncovs <- length(covariates1)
for (i in 1:ncovs){
	x <- elsa[,grep(covariates1[i], names(elsa))]
	x <- as.matrix(table(x))
	ns <- rbind(ns, x)	
}

write.csv(ns, file = "elsa_ns.csv")

# ** ROUGH **


# describe physical activity by income

x <- with(elsa, table(palevel, income5))
y <- t(matrix(rep(apply(x, 2, sum), 5), 6, 5))
z <- x/y
z <- z[c(1,5,4,3,2),] * 100
colnames(z) <- c("5: highest\nincome", "4", "3", "2", "1: lowest\nincome", "missing")
row.names(z)[5] <- "missing"
cols <- brewer.pal(5, "BrBG")
widths <- colSums(y)
png("palevel_income5.png", width = 600, height = 600)
par(xpd = TRUE, mar = c(5, 3, 3, 7))
barplot(z, col = cols)
legend("topright", inset = c(-0.19, 0), legend = rev(row.names(z)), fill = rev(cols), pt.cex = 1.5, pt.lwd = 1, box.lwd = 0)
dev.off()

# describe smoking by SES

meansd <- function(x, d = 1) {
	m <- format(round(mean(x), d), nsmall = d)
	s <- format(round(sd(x), d), nsmall = d)
	return(paste0(m, " (", s, ")"))
}

sa <- aggregate(elsa$packyrs_all, by = list(elsa$income5), FUN = meansd)
sb <- aggregate(elsa$packyrs_all, by = list(elsa$wealth5), FUN = meansd)
sc <- aggregate(elsa$packyrs_all, by = list(elsa$educend), FUN = meansd)
sd <- aggregate(elsa$packyrs_all, by = list(elsa$sclass), FUN = meansd)
s.sum <- rbind(sa, sb, sc, sd)


pdf("smoking_boxplots.pdf")

packyrs_chart <- elsa$packyrs_all
ymax <- 75
col = "lightgrey"
whisk <- 2
staple <- 1
outl <- F
op <- par(oma = c(3, 3, 3, 0), mfrow = c(2, 2), cex = 0.75, mar = c(2, 0, 2, 0), xpd = F, cex.main = 1)

plot(packyrs_chart ~ elsa$income5, ylim = c(0, ymax), xlab = '', col = col, main = "Income quintile", whisklty = whisk, staplelty = staple, outline = outl)
plot(packyrs_chart ~ elsa$wealth5, ylim = c(0, ymax), xlab = '', col = col, yaxt = 'n', main = "Wealth quintle", whisklty = whisk, staplelty = staple, outline = outl)
plot(packyrs_chart ~ elsa$educend, ylim = c(0, ymax), xlab = '', col = col, main = "Age finished education", whisklty = whisk, staplelty = staple, outline = outl)
plot(packyrs_chart ~ elsa$sclass, ylim = c(0, ymax), xlab = '', col = col, yaxt = 'n', main = "Occupation", whisklty = whisk, staplelty = staple, outline = outl)

mtext("Boxplot of pack-years by measures of socio-economic status", side = 3, outer = T, line = 1, cex = 0.75)

dev.off()

elsa_smokers <- elsa[elsa$cigst1 != "never",]

packyrs_chart <- elsa_smokers$packyrs_all
ymax <- 75
op <- par(oma = c(3, 3, 3, 0), mfrow = c(2, 2), cex = 1, mar = c(2, 0, 0, 0), xpd = TRUE)

plot(packyrs_chart ~ elsa_smokers$income5, , ylim = c(0, ymax))
plot(packyrs_chart ~ elsa_smokers$wealth5, ylim = c(0, ymax))
plot(packyrs_chart ~ elsa_smokers$educend, ylim = c(0, ymax))
plot(packyrs_chart ~ elsa_smokers$sclass, ylim = c(0, ymax))


