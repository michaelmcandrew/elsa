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
df$w4porftvg <- findInterval(df$w4porfv, 0:8)
wave4.ftvg <- data.frame(idauniq = wave4$idauniq, ftvg_4 = df$w4porftvg)

df <- data.frame(idauniq = wave5$idauniq, veg = wave5$scveg, fruit = wave5$scfru)
df$veg <- ifelse(df$veg < 0, NA, df$veg)
df$fruit <- ifelse(df$fruit < 0, NA, df$fruit)
df.n <- df
df.n[is.na(df.n)] <- 0
df$ftvgtot <- ifelse(is.na(df$veg) & is.na(df$fruit), NA, df.n$veg + df.n$fruit)
df$ftvg <- findInterval(df$ftvgtot, 0:8)
wave5.ftvg <- data.frame(idauniq = df$idauniq, ftvg_5 = df$ftvg)

df <- data.frame(idauniq = wave6$idauniq, veg = wave6$scveg, fruit = wave6$scfru)
df$veg <- ifelse(df$veg < 0, NA, df$veg)
df$fruit <- ifelse(df$fruit < 0, NA, df$fruit)
df.n <- df
df.n[is.na(df.n)] <- 0
df$ftvgtot <- ifelse(is.na(df$veg) & is.na(df$fruit), NA, df.n$veg + df.n$fruit)
df$ftvg <- findInterval(df$ftvgtot, 0:8)
wave6.ftvg <- data.frame(idauniq = df$idauniq, ftvg_6 = df$ftvg)

elsa <- merge(elsa, wave4.ftvg, by = "idauniq", all.x = T)
elsa <- merge(elsa, wave5.ftvg, by = "idauniq", all.x = T)
elsa <- merge(elsa, wave6.ftvg, by = "idauniq", all.x = T)
p <- ncol(elsa)
elsa$ftvg_a <- apply(elsa[,p-2:p], 1, mean, na.rm = T)