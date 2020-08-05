#install.packages("VennDiagram")

library(VennDiagram)
setwd("C:\\Users")

geneList=list()

rt=read.table("immune.txt",header=F,sep="\t",check.names=F)
geneList[["Immune"]]=as.vector(rt[,1])

rt=read.table("diff.txt",header=T,sep="\t",check.names=F,row.names=1)
#logFC2=rt[abs(rt$logFC)>2,]
#geneList[["|logFC|>2"]]=row.names(logFC2)
logFC1.5=rt[abs(rt$logFC)>1.5,]
geneList[["|logFC|>1.5"]]=row.names(logFC1.5)

venn.plot=venn.diagram(geneList,filename=NULL,main.cex = 2,
                       fill=c("darkblue", "darkgreen"),cat.cex=1)
pdf(file="venn.pdf",width=8,height=8)
grid.draw(venn.plot)
dev.off()
upGenes=Reduce(intersect,geneList)

write.table(file="intersectGenes.txt",upGenes,sep="\t",quote=F,row.names=F,col.names=F)
