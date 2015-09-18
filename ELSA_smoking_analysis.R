# This script creates a survival model for different SES and smoking-status groups. Wave 1 is the baseline, but some baseline data is taken from wave 0 HSE data. Extra packages required: plyr (for 'revalue'), dplyr (for 'mutate'), survival (for regression models)

library(plyr)
library(dplyr)

# import. Waves 2+ are not used

wave0_1998 <- read.table("wave_0_1998_data.tab", sep="\t", header = T)
wave0_1999 <- read.table("wave_0_1999_data.tab", sep="\t", header = T)
wave0_2001 <- read.table("wave_0_2001_data.tab", sep="\t", header = T)
wave1 <- read.table("wave_1_core_data_v3.tab", sep="\t", header = T)
wave1.fin <- read.table("wave_1_financial_derived_variables.tab", sep="\t", header = T)
wave2 <- read.table("wave_2_core_data_v4.tab", sep="\t", header = T)
wave2.der <- read.table("wave_2_derived_variables.tab", sep="\t", header = T)
wave3 <- read.table("wave3_subset.txt", sep="\t", header = T)
wave4 <- read.table("wave_4_elsa_data_v3.tab", sep="\t", header = T)
wave5 <- read.table("wave_5_elsa_data_v4.tab", sep="\t", header = T)
wave6 <- read.table("wave_6_elsa_data_v2.tab", sep="\t", header = T)
index_file <- read.table("index_file_wave_0-wave_5_v2.tab", sep="\t", header = T)

# create wave0 dataset

wave0_1998_sub <- subset(wave0_1998, select = c("idauniq", "ager", "sex", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "educend", "sclass", "bmivg6", "passm"))
wave0_1998_sub$year <- rep(1998, nrow(wave0_1998_sub))
wave0_1999_sub <- subset(wave0_1999, select = c("idauniq", "ager", "sex", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "educend", "sclass", "bmivg6", "passm"))
wave0_1999_sub$year <- rep(1999, nrow(wave0_1999_sub))
wave0_2001_sub <- subset(wave0_2001, select = c("idauniq", "ager", "sex", "cigdyal", "numsmok", "smokyrs", "endsmoke", "cigst1", "startsmk", "educend", "hrpsoccl", "bmivg6", "passm"))
names(wave0_2001_sub)[grep("hrpsoccl", names(wave0_2001_sub))] <- "sclass"
wave0_2001_sub$year <- rep(2001, nrow(wave0_2001_sub))

wave0 <- rbind(wave0_1998_sub, wave0_1999_sub, wave0_2001_sub)
wave0 <- wave0[wave0$ager >= 50,]
wave0 <- wave0[wave0$cigst1 >= 0,]
wave0$sex <- as.factor(wave0$sex)
wave0$sex <- revalue(wave0$sex, c("1" = "male", "2" = "female"))
wave0$age_group <- findInterval(wave0$ager, c(50, 60, 70, 80))
wave0$age_group <- as.factor(wave0$age_group)
wave0$age_group <- revalue(wave0$age_group, c("1" = "50-59", "2" = "60-69", "3" = "70-79", "4" = "80+"))
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
wave0$status_packyrs <- relevel(as.factor(wave0$status_packyrs), ref = "never")
wave0$educend <- as.factor(wave0$educend)
wave0$educend <- revalue(wave0$educend, c("1" = "other", "2" = "U15", "3" = "U15", "4" = "15-16", "5" = "15-16", "6" = "17+", "7" = "17+", "8" = "17+", "-9" = "other", "-8" = "other"))
wave0$educend <- relevel(as.factor(wave0$educend), ref = "17+")
wave0$sclass <- as.factor(wave0$sclass)
wave0$sclass <- revalue(wave0$sclass, c("1" = "I-II-IIINM", "2" = "I-II-IIINM", "3" = "I-II-IIINM", "4" = "IIIM", "5" = "IV/V", "6" = "IV/V", "7" = "0", "8" = "0", "-1" = "0")) # CHECK MISSING DATA!
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

# create dataset for analysis: age, sex, ethnicity (NOT YET INCLUDED), SES measures, current smoking, pack-years, age started, age stopped, mortstat, maincod, mortwave; NOT YET SES-RELATED FACTORS, PASSIVE SMOKING, OBESITY, PHYSICAL ACTIVITY, ETC.

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
wave0.sub <- subset(wave0, select = c("idauniq", "sex", "age_group", "cigst1", "packyrs_all", "packyrs_grp", "status_packyrs", "startsmk", "educend", "sclass", "bmivg6", "passm"))
elsa <- merge(elsa, wave0.sub, by = "idauniq")

sclass.missing <- is.na(elsa$sclass)
elsa$sclass <- as.factor(ifelse(sclass.missing, "0", as.character(elsa$sclass)))
elsa$sclass <- relevel(elsa$sclass, ref = "I-II-IIINM")

bmi.missing <- is.na(elsa$bmivg6)
elsa$bmivg6 <- as.factor(ifelse(bmi.missing, "0", as.character(elsa$bmivg6)))
elsa$bmivg6 <- factor(elsa$bmivg6, levels = c("20-25", "<20", "25-30", "30-35", "35-40", "40+", "0"))

deaths <- subset(index_file, select = c("idauniq", "mortstat", "maincod", "yrdeath"))
elsa <- merge(elsa, deaths, by = "idauniq")
elsa <- elsa[elsa$idauniq != 104145,] # this case is weird - mortstat = 2 (i.e. dead), yrdeath = -2 (i.e. info not avaialble)
wave2.der.sub <- subset(wave2.der, select = c("idauniq", "palevel"))
elsa <- merge(elsa, wave2.der.sub, by = "idauniq")
elsa$palevel <- revalue(as.factor(elsa$palevel), c("-8" = "0", "-6" = "0", "-1" = "0", "0" = "sedentary", "1" = "low", "2" = "moderate", "3" = "high"))
elsa$palevel <- relevel(elsa$palevel, ref = "high")

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
	expcoef <- round(x1[,1],2)
	lower <- round(x1[,3],2)
	upper <- round(x1[,4],2)
	p <- x2[,5]
	stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, ".", ""))))
	results <- paste(expcoef, " (", lower, "-", upper, ")", stars, sep = "")
	return(cbind(row.names(x1), results))
}

covariates1 <- c("status_packyrs", "income5", "wealth5", "sclass", "educend", "bmivg6", "passm", "palevel", "age_group", "sex")
ncovs <- length(covariates1)
unadj <- NULL
for (i in 1:ncovs) {
	surv.mod <- coxph(formula(paste0("Surv.prot", "~", covariates1[i])), data = elsa)
	surv.mod.out <- coxph.out(surv.mod)
	unadj <- rbind(unadj, surv.mod.out)	
}

covariates2 <- c("status_packyrs", "income5", "wealth5", "sclass", "educend", "bmivg6", "passm", "palevel")
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

covariates3 <- c("status_packyrs", "bmivg6", "passm", "palevel", "age_group", "sex")
ncovs <- length(covariates3)
adj_SES <- NULL
for (i in 1:ncovs) {
	surv.mod <- coxph(formula(paste0("Surv.prot", "~", covariates3[i], "+", "income5", "+", "wealth5", "+", "sclass", "+", "educend")), data = elsa)
	surv.mod.out <- coxph.out(surv.mod)
	surv.mod.out <- surv.mod.out[1:(nrow(surv.mod.out)-16),]
	adj_SES <- rbind(adj_SES, surv.mod.out)	
}
space <- matrix(rep(0,32), 16, 2)
adj_SES <- rbind(adj_SES[1:6,], space, adj_SES[7:22,])

adj <- coxph(formula(paste("Surv.prot", "~", paste0(covariates1, collapse = "+"))), data = elsa)
adj <- coxph.out(adj)

full.out <- cbind(unadj, adj_basic[,2], adj_SES[,2], adj[,2])
full.out <- data.frame(unadjusted = full.out[,2], age_sex_adj = full.out[,3], SES_adj = full.out[,4], fully_adj = full.out[,5], row.names = full.out[,1])

# survival curve

# mulitnomial regression...
