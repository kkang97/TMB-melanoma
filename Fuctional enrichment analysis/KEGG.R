#install.packages("colorspace")
#install.packages("stringi")
#install.packages("ggplot2")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("DOSE")
#BiocManager::install("clusterProfiler")
#BiocManager::install("enrichplot")


library("clusterProfiler")
library("org.Hs.eg.db")
library("enrichplot")
library("ggplot2")

pvalueFilter=0.05                #pֵ��������
qvalueFilter=0.05                #�������pֵ��������

setwd("C:\\Users\\86181\\Desktop\\KK\\449vTMBimmune\\16.KEGG")              #���ù���Ŀ¼
rt=read.table("id.txt",sep="\t",header=T,check.names=F)            #��ȡid.txt�ļ�
rt=rt[is.na(rt[,"entrezID"])==F,]                                  #ȥ������idΪNA�Ļ���
gene=rt$entrezID
geneFC=rt$logFC
names(geneFC)=gene

colorSel="qvalue"
if(qvalueFilter>0.05){
	colorSel="pvalue"
}

#kegg��������
kk <- enrichKEGG(gene = gene, organism = "hsa", pvalueCutoff =1, qvalueCutoff =1)   #��������
KEGG=as.data.frame(kk)
KEGG$geneID=as.character(sapply(KEGG$geneID,function(x)paste(rt$gene[match(strsplit(x,"/")[[1]],as.character(rt$entrezID))],collapse="/")))
KEGG=KEGG[(KEGG$pvalue<pvalueFilter & KEGG$qvalue<qvalueFilter),]
write.table(KEGG,file="KEGG.txt",sep="\t",quote=F,row.names = F)                          #���渻�����

showNum=30
if(nrow(KEGG)<30){
	showNum=nrow(KEGG)
}

#��״ͼ
pdf(file="barplot.pdf",width = 10,height = 7)
barplot(kk, drop = TRUE, showCategory = showNum, color = colorSel)
dev.off()

#����ͼ
pdf(file="bubble.pdf",width = 10,height = 7)
dotplot(kk, showCategory = showNum, orderBy = "GeneRatio",color = colorSel)
dev.off()