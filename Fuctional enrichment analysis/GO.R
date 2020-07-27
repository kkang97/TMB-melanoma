#install.packages("colorspace")
#install.packages("stringi")
#install.packages("ggplot2")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("DOSE")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("clusterProfiler")

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("enrichplot")

library("clusterProfiler")
library("org.Hs.eg.db")
library("enrichplot")
library("ggplot2")

pvalueFilter=0.05                #pֵ��������
qvalueFilter=0.05                #�������pֵ��������

setwd("C:\\Users")              #���ù���Ŀ¼
InputName0<-"id"
InputName<-paste(InputName0,".txt",sep="")
rt=read.table(InputName,sep="\t",header=T,check.names=F)            #��ȡid.txt�ļ�
rt=rt[is.na(rt[,"entrezID"])==F,]                                 #ȥ������idΪNA�Ļ���
gene=rt$entrezID

#GO��������
kk <- enrichGO(gene = gene,
               OrgDb = org.Hs.eg.db, 
               pvalueCutoff =pvalueFilter, 
               qvalueCutoff = qvalueFilter,
               ont="all",#ѡ��������GO������all�������ֶ���
               readable =T #readable����������ʱ���û��������ǻ���ID����ʾ�������Ķ�
)
GOOurputName<-paste("GO.",InputName0, ".txt", sep="" )

write.table(kk,file=GOOurputName,sep="\t",quote=F,row.names = F)                 #���渻�����

colorSel="qvalue"
if(qvalueFilter>0.05){
  colorSel="pvalue"
}

#��״ͼ
barplotNamePdf<-paste("GO.barplot.", InputName0, ".pdf", sep="" )
pdf(file=barplotNamePdf,width = 12,height = 10)
barplot(
  kk, 
  drop = TRUE, 
  showCategory =10,#ÿ�����͵�GO�����ʾ10����
  split="ONTOLOGY", #���ղ�ͬ�������и�
  color = colorSel#����colorSel�������渳ֵ��pvalue����ɫ
) + 
  facet_grid(ONTOLOGY~., scale='free')
dev.off()

barplotNameTiff<-paste("GO.barplot.", InputName0, ".tiff", sep="" )
tiff(file=barplotNameTiff,width = 25,height = 20,units ="cm",compression="lzw",bg="white",res=600)
barplot(
  kk, 
  drop = TRUE, 
  showCategory =10,#ÿ�����͵�GO�����ʾ10����
  split="ONTOLOGY", #���ղ�ͬ�������и�
  color = colorSel#����colorSel�������渳ֵ��pvalue����ɫ
) + 
  facet_grid(ONTOLOGY~., scale='free')
dev.off()

#����ͼ
bubbleNamePdf<-paste("GO.bubble.", InputName0, ".pdf", sep="" )
pdf(file=bubbleNamePdf,width = 12,height =10)
dotplot(
  kk,showCategory = 10,
  split="ONTOLOGY",
  orderBy = "GeneRatio", 
  color = colorSel) + 
  facet_grid(ONTOLOGY~., scale='free')
dev.off()
bubbleNameTiff<-paste("GO.bubble.", InputName0, ".tiff", sep="" )
tiff(file=bubbleNameTiff,width = 25,height = 20,units ="cm",compression="lzw",bg="white",res=600)
dotplot(
  kk,showCategory = 10,
  split="ONTOLOGY",
  orderBy = "GeneRatio", 
  color = colorSel) + 
  facet_grid(ONTOLOGY~., scale='free')
dev.off()