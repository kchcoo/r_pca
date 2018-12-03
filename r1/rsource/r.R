rm(list=ls())

setwd("C:/JISOO/Lab/Others/kch/r1/")

Nfiles <- dir(pattern="_N", path="DATA/INP/")
Ndatas <- lapply(paste0("DATA/INP/",Nfiles), read.csv)
N <- do.call(rbind, Ndatas)
N <- N[!duplicated(N$Assign_ID),]
colnames(N)[1] <- "Time.N"
colnames(N)[3] <- "Wgt.N"
write.csv(N,"DATA/OUTP/N.csv", row.names=F)

Sfiles <- dir(pattern="_S", path="DATA/INP/")
Sdatas <- lapply(paste0("DATA/INP/",Sfiles), read.csv)
S <- do.call(rbind, Sdatas)
S <- S[!duplicated(S$Assign_ID),]
colnames(S)[1] <- "Time.S"
colnames(S)[3] <- "Wgt.S"
write.csv(S,"DATA/OUTP/S.csv", row.names=F)

all <- merge(N, S, by="Assign_ID", all=T)
all$PM <- all$Wgt.S - all$Wgt.N
