library(DESeq2)
setwd("") 
logFCfiler=1.5                
fdrFilter=0.05              
conNum=137                  
treatNum=312 

countData <- read.table('449SKCMGroupbyTMB.txt', sep = '\t', header=T,check.names=F, row.names = 1)
condition0<-c(rep('control',conNum),rep('treat',treatNum))
condition<-factor(condition0, levels = c('control', 'treat'))
colData<-data.frame(row.names=colnames(countData), condition)

dds <- DESeqDataSetFromMatrix(countData, DataFrame(condition), design= ~ condition )
#head(dds)
dds2 <- DESeq(dds)
#resultsNames(dds2)

res <- results(dds2)
head(res, n=5)
mcols(res, use.names = TRUE)
summary(res)

table(res$padj<0.05)

write.table(res, 'control_treat.DESeq2.txt', col.names = NA, sep = '\t', quote = FALSE)

res <- res[order(res$padj),]
diff_gene_deseq2 <- subset(res,padj < 0.05 & (log2FoldChange >1.5 | log2FoldChange < -1.5))
#head (diff_gene_deseq2, n=5)