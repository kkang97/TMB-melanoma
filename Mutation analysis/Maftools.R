if (!require("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("maftools")


library(maftools)
setwd("C:\\Users")     
maf = read.maf(maf = 'input.maf') 

pdf(file="summary.pdf",width=8,height=5)
plotmafSummary(maf = maf, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE, fs = 1)
dev.off()

pdf(file="waterfall.pdf",width=7,height=6)
oncoplot(maf = maf, top = 30, fontSize = 0.8 ,showTumorSampleBarcodes = F )
dev.off()

pdf(file="interaction.pdf",width=7,height=6)
somaticInteractions(maf = maf, top = 20, pvalue = c(0.05, 0.001),showCounts = FALSE, fontSize = 0.6, sigSymbolsSize = 1)
dev.off()
