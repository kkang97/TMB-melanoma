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

pvalueFilter=0.05                #p值过滤条件
qvalueFilter=0.05                #矫正后的p值过滤条件

setwd("C:\\Users")              #设置工作目录
InputName0<-"id"
InputName<-paste(InputName0,".txt",sep="")
rt=read.table(InputName,sep="\t",header=T,check.names=F)            #读取id.txt文件
rt=rt[is.na(rt[,"entrezID"])==F,]                                 #去除基因id为NA的基因
gene=rt$entrezID

#GO富集分析
kk <- enrichGO(gene = gene,
               OrgDb = org.Hs.eg.db, 
               pvalueCutoff =pvalueFilter, 
               qvalueCutoff = qvalueFilter,
               ont="all",#选择做哪种GO分析，all就是三种都做
               readable =T #readable会在输出结果时，用基因名而非基因ID来显示，方便阅读
)
GOOurputName<-paste("GO.",InputName0, ".txt", sep="" )

write.table(kk,file=GOOurputName,sep="\t",quote=F,row.names = F)                 #保存富集结果

colorSel="qvalue"
if(qvalueFilter>0.05){
  colorSel="pvalue"
}

#柱状图
barplotNamePdf<-paste("GO.barplot.", InputName0, ".pdf", sep="" )
pdf(file=barplotNamePdf,width = 12,height = 10)
barplot(
  kk, 
  drop = TRUE, 
  showCategory =10,#每种类型的GO最多显示10个。
  split="ONTOLOGY", #按照不同类型来切割
  color = colorSel#按照colorSel，即上面赋值的pvalue来上色
) + 
  facet_grid(ONTOLOGY~., scale='free')
dev.off()

barplotNameTiff<-paste("GO.barplot.", InputName0, ".tiff", sep="" )
tiff(file=barplotNameTiff,width = 25,height = 20,units ="cm",compression="lzw",bg="white",res=600)
barplot(
  kk, 
  drop = TRUE, 
  showCategory =10,#每种类型的GO最多显示10个。
  split="ONTOLOGY", #按照不同类型来切割
  color = colorSel#按照colorSel，即上面赋值的pvalue来上色
) + 
  facet_grid(ONTOLOGY~., scale='free')
dev.off()

#气泡图
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
