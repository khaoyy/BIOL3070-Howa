# Day 1 Example Plot (2025.08.28)

viremia <- read.csv("viremia_data_full.csv")
colnames(viremia) <- c("Bird","n","Species","Family","Order","1","3","4","6")

# Choose colors

cols <- c("black","grey", col=1:26)

# Plot by species

plot(c(1,3,4,6),as.numeric(viremia[1,6:9]), type = "l", lwd = 2, ylim = range(viremia[,6:9],na.rm=TRUE),
     xlab="Day Post Infection", ylab = "Log PFR/ml Serum")
for (i in 2:nrow(viremia)){
  lines(c(1,3,4,6),as.numeric(viremia[i,6:9]),lwd=2, col=1:26)
}

