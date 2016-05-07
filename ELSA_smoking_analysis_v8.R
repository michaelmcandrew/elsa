# This script creates a survival model for different SES and smoking-status groups. Wave 1 is the baseline, but some baseline data is taken from 'wave 0' HSE data (1998-2001) and physical activity baseline data is taken from wave 2.

library(survival) # for hazard models
library(RColorBrewer) # for colouring in charts 
library(epitools)

recode <- function(vect, originals, replacements) {
	u <- rep(NA, length(vect))
	for (i in 1:length(originals)) {
		if(is.na(originals[i])) {
			u[is.na(vect)] <- replacements[i]
		} else {
		u[vect == originals[i]] <- replacements[i]
		}
	}
	return(ifelse(vect %in% originals, u, vect))
}

quant <- function(variable, n) {
    x <- data.frame(id = 1:length(variable), variable = variable)
    x <- x[order(x$variable),]
    cases <- length(variable[!is.na(variable)])
    sizes <- floor(cases/n)
    breaks <- rep(sizes, n)
    left <- cases - (sizes * n)
    if(left > 0) {
        breaks[1:left] <- rep(sizes + 1)
    }
    vec <- NULL
    for (u in 1:n) {
        vec <- c(vec, rep(u, breaks[u]))
    }
    vec <- c(vec, rep(NA, length(variable) - cases))
    return(vec[order(x$id)])
}

RII <- function(x) {
	ranks <- rank(x)
	ranks[is.na(x)] <- NA
	return(ranks / length(ranks[!is.na(x)]))
}

# IMPORT
# wave 0/1: baseline data. wave 4/5/6 diet data

wave0_1998 <- read.table("wave_0_1998_data.tab", sep="\t", header = T)
wave0_1999 <- read.table("wave_0_1999_data.tab", sep="\t", header = T)
wave0_2001 <- read.table("wave_0_2001_data.tab", sep="\t", header = T)
wave1 <- read.table("wave_1_core_data_v3.tab", sep="\t", header = T)
wave1.fin <- read.table("wave_1_financial_derived_variables.tab", sep="\t", header = T)
# wave1.edu <- read.csv("elsa_w1_education.csv", sep =",", header = T)
wave2 <- read.table("wave_2_core_data_v4.tab", sep="\t", header = T)
# wave2.der <- read.table("wave_2_derived_variables.tab", sep="\t", header = T)
wave3 <- read.table("wave3_subset.txt", sep="\t", header = T) #this is a SUBSET OF WAVE 3
# wave3 <- read.table("wave_3_elsa_data_v4.txt", sep="\t", header = T)
wave4 <- read.table("wave_4_elsa_data_v3.tab", sep="\t", header = T)
wave5 <- read.table("wave_5_elsa_data_v4.tab", sep="\t", header = T)
wave6 <- read.table("wave_6_elsa_data_v2.tab", sep="\t", header = T)
# index_file <- read.table("index_file_wave_0-wave_5_v2.tab", sep="\t", header = T)
mort <- read.table("Mortality_update_2013.txt", sep="\t", header = T)

# CREATE WAVE 0 DATASET. NS-SEC not available in 1998 & 1999, so used social class I - V

wave0_1998_sub <- subset(wave0_1998, select = c("idauniq", "finstat", "yintb", "ager", "sex", "ethnicr", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "sclass", "eqvinc", "topqual3", "bmivg6", "passm"))
wave0_1998_sub$year <- rep(1998, nrow(wave0_1998_sub))
wave0_1999_sub <- subset(wave0_1999, select = c("idauniq", "finstat", "yintb", "ager", "sex", "ethnicr", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "sclass", "eqvinc", "topqual3", "bmivg6", "passm"))
wave0_1999_sub$year <- rep(1999, nrow(wave0_1999_sub))
wave0_2001_sub <- subset(wave0_2001, select = c("idauniq", "finstat", "yintb", "ager", "sex", "ethnicr", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "hrpsoccl", "eqvinc", "topqual3", "bmivg6", "passm"))
names(wave0_2001_sub)[grep("hrpsoccl", names(wave0_2001_sub))] <- "sclass"
wave0_2001_sub$year <- rep(2001, nrow(wave0_2001_sub))

wave0 <- rbind(wave0_1998_sub, wave0_1999_sub, wave0_2001_sub)

wave0$sex <- factor(wave0$sex, levels = c(2, 1), labels = c("female", "male"))
wave0$ethnicr <- factor(ifelse(wave0$ethnicr < 0, NA, wave0$ethnicr), levels = c(1, 2), labels = c("white", "nonwhite"))

wave0$cigst1 <- recode(wave0$cigst1, c(1, 2, 3, 4, -8, -9), c(1, 2, 2, 3, NA, NA))
wave0$cigst1 <- factor(wave0$cigst1, levels = c(1, 2, 3), labels = c("never", "ex", "current"))
wave0$startsmk <- ifelse(wave0$startsmk < 0, NA, wave0$startsmk)
wave0$startsmk <- ifelse(wave0$startsmk == 97, NA, wave0$startsmk) # these appear to be errors - the age of starting smoking is higher than the age
wave0$cigdyal <- ifelse(wave0$cigdyal < 0, NA, wave0$cigdyal)
wave0$numsmok <- ifelse(wave0$numsmok < 0, NA, wave0$numsmok) # number of cigarettes used to smoke
wave0$endsmoke <- ifelse(wave0$endsmoke < 0, NA, wave0$endsmoke) # years since stopped smoking
invalid_ex <- wave0$cigst1 == "ex" & wave0$ager < (wave0$startsmk + wave0$endsmoke) # years since stopping + age started > age; i.e. invalid
invalid_ex[is.na(invalid_ex)] <- F
wave0$startsmk[invalid_ex] <- NA
wave0$endsmoke[invalid_ex] <- NA

sclass_ordered <- recode(wave0$sclass, c(-1, 1, 2, 3, 4, 5, 6, 7, 8), c(NA, 6, 5, 4, 3, 2, 1, NA))
wave0$sclassRII <- RII(sclass_ordered)

wave0$sclass <- recode(wave0$sclass, c(-1, 1, 2, 3, 4, 5, 6, 7, 8), c(NA, 1, 1, 1, 2, 3, 3, 4, NA))
wave0$sclass <- factor(wave0$sclass, levels = c(1, 2, 3, 4), labels = c("nonmanual", "skilledmanual", "manual", "army"))
wave0$bmivg6 <- ordered(ifelse(wave0$bmivg6 < 0, NA, wave0$bmivg6), levels = 1:6, labels = c("<=20", ">20-25", ">25-30", ">30-35", ">35-40", ">40"))
wave0$passm <- factor(ifelse(wave0$passm < 0, NA, wave0$passm), levels = c(1, 2), labels = c("yes", "no")) #smokers in household

wave0$eqvinc <- ifelse(wave0$eqvinc < 0, NA, wave0$eqvinc)
wave0$eqv5 <- quant(wave0$eqvinc, 5) # NEEDS TO BE FOR EACH YEAR SEPERATELY
wave0$eqv3 <- quant(wave0$eqvinc, 3)
wave0$eqvRII <- RII(wave0$eqvinc)

ed_ordered <- recode(wave0$topqual3, c(-1, 1:7), c(NA, 1:5, NA, 6))
wave0$topqual3RII <- RII(ed_ordered)
wave0$topqual3 <- recode(wave0$topqual3, c(-1, 1:7), c(NA, 1, 1, 2:6))
wave0$topqual3 <- factor(wave0$topqual3, levels = 1:6, labels = c("he", "a-level", "gcse", "cse", "other", "none"))

wave0$cSES <- apply(data.frame(wave0$sclassRII, wave0$eqvRII, wave0$topqual3RII), 1, mean, na.rm = T)
wave0$cSES3 <- factor(quant(wave0$cSES, 3), levels = 3:1, labels = c("3-high", 2, "1-low"))

names(wave0)[names(wave0) == "ager"] <- "indager"

# deaths

deaths <- subset(mort, select = c("idauniq", "doDmnth", "dodyr", "agedeath2", "maincod", "icd9chl", "icd10chl"))
deaths$doDmnth <- ifelse(is.na(deaths$doDmnth), 6, deaths$doDmnth) # 20 missing - replace with 6 (halfway through the year)
smk.death.10 <- ifelse(deaths$icd10chl == "Lung cancer" | deaths$icd10chl == "Chronic obstructive pulmonary disease", 1, 0)
smk.death.9 <- ifelse(deaths$icd9chl == "Lung cancer" | deaths$icd9chl == "Chronic obstructive pulmonary disease", 1, 0)
deaths$smk.death <- ifelse(smk.death.9 == 1 | smk.death.10 == 1, 1, 0)
deaths$lc.death <- ifelse(deaths$icd10chl == "Lung cancer" | deaths$icd9chl == "Lung cancer", 1, 0)
deaths$copd.death <- ifelse(deaths$icd10chl == "Chronic obstructive pulmonary disease" | deaths$icd9chl == "Chronic obstructive pulmonary disease", 1, 0)
deaths$all.cause.death <- rep(1, nrow(deaths))
deaths <- subset(deaths, select = c("idauniq", "doDmnth", "dodyr", "smk.death", "lc.death", "copd.death", "smk.death2", "lc.death2", "copd.death2", "all.cause.death"))

# CREATE ANALYSIS DATASETS: Wave0

# add deaths to Wave 0. Assume interview in Jan of HSE year.

wave0 <- merge(wave0, deaths, by = "idauniq", all.x = T)
wave0$all.cause.death <- ifelse(is.na(wave0$all.cause.death), 0, 1)
wave0$smk.death2 <- ifelse(is.na(wave0$smk.death), 0, wave0$smk.death) # alive or died by other causes = 0
wave0$lc.death2 <- ifelse(is.na(wave0$lc.death), 0, wave0$lc.death)
wave0$copd.death2 <- ifelse(is.na(wave0$copd.death), 0, wave0$copd.death)
wave0$survival <- with(wave0, (dodyr - year) * 12 + doDmnth)
wave0$survival <- ifelse(is.na(wave0$survival), (2013 - wave0$year) * 12 + (4 - 0), wave0$survival) # end of study is April 2013. Assume interview in Jan of HSE year

# CREATE ANALYSIS DATASETS: ELSA

elsa <- data.frame(idauniq = intersect(wave0$idauniq, wave1$idauniq))

# wave 0 data

wave0.sub <- subset(wave0, select = c("idauniq", "finstat", "sex", "ethnicr", "cigst1", "startsmk", "endsmoke", "cigdyal", "numsmok", "sclass", "sclassRII", "bmivg6", "passm"))
elsa <- merge(elsa, wave0.sub, by = "idauniq")

# wave 1 financial variables. PROVIDED QUINTILES ARE NOT CLEAR QUINTLES OF CONTINUOUS DATA...

fin <- subset(wave1.fin, select = c("idauniq", "nfwq5_bu_s", "yq5_bu_s", "nettotw_bu_s", "totinc_bu_s")) #wealth quintile, income quintile, wealth, income
names(fin) <- c("idauniq", "wealth5", "income5", "wealth", "income")
fin$wealth3 <- quant(fin$wealth, 3)
fin$income3 <- quant(fin$income, 3)
fin$incomeRII <- RII(fin$income)
fin$wealthRII <- RII(fin$wealth)

elsa <- merge(elsa, fin, by = "idauniq")
elsa$wealth5 <- factor(elsa$wealth5, levels = 5:1, labels = c("5-high", "4", "3", "2", "1-low"))
elsa$income5 <- factor(elsa$income5, levels = 5:1, labels = c("5-high", "4", "3", "2", "1-low"))

# wave 1 data (and remove partners)

wave1.sub <- subset(wave1, select = c("idauniq", "indager", "edqual", "iintdtm", "iintdty"))
wave1.sub$age_group <- findInterval(wave1.sub$indager, c(50, 60, 70, 80))
wave1.sub$age_group <- factor(wave1.sub$age_group, levels = 1:4, labels = c("50-59", "60-69", "70-79", "80+"))
wave1.sub$topqual <- recode(wave1.sub$edqual, c(1:7, -9, -8, -1), c(1, 1, 2, 3, 4, 5, 6, NA, NA, NA))
wave1.sub$topqual <- factor(wave1.sub$topqual, levels = 1:6, labels = c("he", "alevel", "olevel", "below_olevel", "other", "none"))
edqual_ordered <- recode(wave1.sub$edqual, c(1:7, -9, -8, -1), c(6:2, NA, 1, NA, NA, NA))
wave1.sub$topqualRII <- RII(edqual_ordered)
wave1.sub <- subset(wave1.sub, select = c("idauniq", "indager", "age_group", "topqual", "topqualRII", "iintdtm", "iintdty"))
elsa <- merge(elsa, wave1.sub, by = "idauniq")
elsa <- elsa[elsa$finstat == "C1CM",]

accom <- subset(wave1, select = c("hopro01", "hopro02", "hopro03", "hopro04", "hopro05", "hopro06", "hopro07", "hopro08", "hopro09", "hopro10"))
probs <- c(5, 6, 8, 10, 12)
accom.probs <- t(apply(accom, 1, function(x) probs %in% x))
wave1.accom <- data.frame(idauniq = wave1$idauniq, accomprobs = ifelse(rowSums(accom.probs) > 0, 1, 0))
elsa <- merge(elsa, wave1.accom, by = "idauniq")
elsa$accomprobs <- factor(elsa$accomprobs, levels = c(0, 1), labels = c("no", "yes"))

# composite SES

elsa$cSES <- apply(data.frame(elsa$sclassRII, elsa$incomeRII, elsa$wealthRII, elsa$topqualRII), 1, mean, na.rm = T)
elsa$cSES3 <- factor(quant(elsa$cSES, 3), levels = 3:1, labels = c("3-high", 2, "1-low"))

# physical activity - derive from wave 1 questions

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
pa <- factor(pa, levels = 3:0, labels = c("high", "moderate", "low", "sedentary"))
wave1.pa <- data.frame(idauniq = wave1$idauniq, palevel = pa)
elsa <- merge(elsa, wave1.pa, by = "idauniq")

# diet - derive from wave 4, 5 or 6 (earliest wave for those participating in more than 1)

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
elsa$ftvg <- factor(elsa$ftvg, levels = 9:0)

# add deaths to ELSA

elsa <- merge(elsa, deaths, by = "idauniq", all.x = T)
elsa$all.cause.death <- ifelse(is.na(elsa$all.cause.death), 0, 1)
elsa$smk.death2 <- ifelse(is.na(elsa$smk.death), 0, elsa$smk.death) # alive or died by other causes = 0
elsa$survival <- with(elsa, (dodyr - iintdty) * 12 + (doDmnth - iintdtm))
elsa$survival <- ifelse(is.na(elsa$survival), (2013 - elsa$iintdty) * 12 + (4 - elsa$iintdtm), elsa$survival) # end of study is April 2013

# add variables for censoring at age 70

c.age <- 70
wave0$year.c <- wave0$yintb + c.age - wave0$indager
wave0$surv.c <- (wave0$year.c - wave0$yintb + 1) * 12
wave0$surv.c <- ifelse(wave0$surv.c < 0, 0, wave0$surv.c)
wave0$surv.a <- pmin(wave0$surv.c, wave0$survival)
wave0$smk.death3 <- ifelse(wave0$surv.c < wave0$survival, 0, wave0$smk.death2)

# basic epi measures by current status

#risk ratios
epitab(with(elsa, t(table(smk.death2, cigst1)), method = "riskratio"))
epitab(with(elsa, t(table(all.cause.death, cigst1)), method = "riskratio"))

#rate ratios
years_at_risk <- aggregate(elsa$survival, by = list(elsa$cigst1), FUN = sum)
all_deaths <- aggregate(elsa$all.cause.death, by = list(elsa$cigst1), FUN = sum)
smk_deaths <- aggregate(elsa$smk.death2, by = list(elsa$cigst1), FUN = sum)
tab_all <- cbind(all_deaths, years_at_risk)[,-c(1, 3)]
rownames(tab_all) <- c("never", "ex", "current")
epitab(as.matrix(tab_all), method = "rateratio")
tab_all <- cbind(smk_deaths, years_at_risk)[,-c(1, 3)]
rownames(tab_all) <- c("never", "ex", "current")
epitab(as.matrix(tab_all), method = "rateratio")

years_at_risk <- aggregate(elsa$survival, by = list(elsa$cSES3), FUN = sum)
all_deaths <- aggregate(elsa$all.cause.death, by = list(elsa$cSES3), FUN = sum)
smk_deaths <- aggregate(elsa$smk.death2, by = list(elsa$cSES3), FUN = sum)
tab_all <- cbind(all_deaths, years_at_risk)[,-c(1, 3)]
rownames(tab_all) <- c("Higher", "Middle", "Lower")
epitab(as.matrix(tab_all), method = "rateratio")
tab_all <- cbind(smk_deaths, years_at_risk)[,-c(1, 3)]
rownames(tab_all) <- c("Higher", "Middle", "Lower")
epitab(as.matrix(tab_all), method = "rateratio")

# generate smoking exposure variables

elsa$duration <- ifelse(elsa$cigst1 == "current", elsa$indager - elsa$startsmk, ifelse(elsa$cigst1 == "ex", elsa$indager - (elsa$startsmk + elsa$endsmoke), 0)) 
elsa$intensity <- ifelse(elsa$cigdyal == 0, elsa$numsmok, elsa$cigdyal)
elsa$packyrs <- (elsa$duration * elsa$intensity)/20

wave0$duration <- ifelse(wave0$cigst1 == "current", wave0$indager - wave0$startsmk, ifelse(wave0$cigst1 == "ex", wave0$indager - (wave0$startsmk + wave0$endsmoke), 0)) 
wave0$intensity <- ifelse(wave0$cigst1 == "never", 0, ifelse(wave0$cigdyal == 0, wave0$numsmok, wave0$cigdyal))
wave0$packyrs <- (wave0$duration * wave0$intensity)/20
wave0$logcigyrs <- (log(wave0$intensity + 1) * wave0$duration) / 100
wave0$sqrtpys <- sqrt(wave0$packyrs)

wave0$endsmoke2 <- ifelse(is.na(wave0$endsmoke), 0, wave0$endsmoke)
CSI <- function(duration, tsc, intensity, hl, d) (1-0.5^(pmax(duration + tsc - d, 0)/hl)) * (0.5^(pmax(tsc - d, 0)/hl)) * log(intensity + 1)
hls <- 2:50
ds <- seq(0, 5, 0.1)
CSI.dat <- data.frame(sex = wave0$sex, indager = wave0$indager, duration = wave0$duration, tsc = wave0$endsmoke2, intensity = wave0$intensity, event = wave0$smk.death2, survival = wave0$survival)
CSI.surv <- function(dat, hl, d) {
	dat$CSI <- CSI(dat$duration, dat$tsc, dat$intensity, hl, d)
	surv.prot <- Surv(dat$survival, event = dat$event, type = "right")
	model <- coxph(surv.prot ~ CSI + indager + sex, data = dat)
	return(extractAIC(model)[2])
}
CSI.AICs <- sapply(hls, function(y) sapply(ds, function(x) CSI.surv(CSI.dat, y, x)))
row.names(CSI.AICs) <- ds
colnames(CSI.AICs) <- hls
min.AIC <- which(CSI.AICs == min(CSI.AICs), arr.ind = TRUE)
hl <- hls[min.AIC[2]]
d <- ds[min.AIC[1]]
wave0$CSI <- CSI(wave0$duration, wave0$endsmoke2, wave0$intensity, hl, d)

measures <- c("cigst1", "packyrs", "logcigyrs", "sqrtpys", "CSI")

# compare measures

compsurv <- function(measure, dat) {
	surv.prot <- Surv(dat$survival, event = dat$smk.death2, type = "right")
	return(coxph(formula(paste0("surv.prot", "~", measure, "+ indager + sex")), data = dat))
}
x.l <- lapply(measures, compsurv, dat = wave0)
AICs <- sapply(x.l, extractAIC)
zphs <- lapply(x.l, cox.zph)
op <- par(oma = c(0, 0, 0, 0), mfrow = c(3, 2), cex = 1, mar = c(2, 2, 2, 2), xpd = F, cex = 0.7)
for (i in 1:length(measures)) {
	plot(zphs[[i]][1], main = measures[i])
	abline(h = 0, lty = 1)
}

# stratified survival prototype

stsurv <- function(dat) {
	surv.prot <- Surv(dat$survival, event = dat$smk.death2, type = "right")
	return(coxph(surv.prot ~ CSI + indager + sex, data = dat))
}

x.l <- split(wave0, f = wave0$cSES3)
tmp <- lapply(x.l, FUN = stsurv)



# DESCRIBE SMOKING BY SES

meanci <- function(x, a = 0.975) {
	x <- x[!is.na(x)]
	m <- mean(x)
	n <- length(x)
	s <- sd(x)
	SE <- s/sqrt(n)
	t <- qt(a, df = n-1)
	int <- t * SE
	return(c(m, m - int, m + int))
}

wealth_means <- as.matrix(aggregate(elsa$packyrs_all, by = list(elsa$wealth5), FUN = meanci))
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

elsa$event <- ifelse(is.na(elsa$smk.death), 0, elsa$smk.death)
Surv.prot <- Surv(elsa$survival, event = elsa$all.cause.death, type = "right")

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

covariates1 <- c("packyrs_all", "income5", "wealth5", "sclass", "bmivg6", "passm", "palevel", "accomprobs", "age_group", "sex")
ncovs <- length(covariates1)
unadj <- NULL
for (i in 1:ncovs) {
	surv.mod <- coxph(formula(paste0("Surv.prot", "~", covariates1[i])), data = elsa)
	surv.mod.out <- coxph.out(surv.mod)
	unadj <- rbind(unadj, surv.mod.out)	
}

covariates2 <- c("status_packyrs", "income5", "wealth5", "sclass", "bmivg6", "passm", "palevel", "accomprobs")
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
	surv.mod <- coxph(formula(paste0("Surv.prot", "~", covariates3[i], "+", "income5", "+", "wealth5", "+", "sclass", "+", "age_group", "+", "sex")), data = elsa)
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
sc <- aggregate(elsa$packyrs_all, by = list(elsa$topqual), FUN = meansd)
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
plot(packyrs_chart ~ elsa_smokers$topqual, ylim = c(0, ymax))
plot(packyrs_chart ~ elsa_smokers$sclass, ylim = c(0, ymax))