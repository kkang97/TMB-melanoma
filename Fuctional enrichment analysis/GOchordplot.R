#install.packages("digest")
#install.packages("GOplot")

library(GOplot)
setwd("C:\\Users")
go=read.table("go.txt",header=T,sep="\t",check.names=F)
genelist=read.table("gene.txt",header=T,sep="\t",check.names=F)

circ <- circle_dat(go, genelist)
tiff(file="circ.tiff",width = 20,height = 20,units ="cm",compression="lzw",bg="white",res=300)
GOCircle(circ,table.legend = F,label.size=5,nsub=nrow(go))
dev.off()

termNum=nrow(go)
geneNum=50
chord <- chord_dat(circ, genelist[1:geneNum,1:2], as.character(go[1:termNum,3]))
tiff(file="chord.tiff",width = 32,height = 32,units ="cm",compression="lzw",bg="white",res=300)
GOChord(chord, ribbon.col=brewer.pal (termNum,'Set3'),gene.size=6)
dev.off()

tiff(file="cluster.tiff",width = 30,height = 30,units ="cm",compression="lzw",bg="white",res=300)
GOCluster(circ, as.character(go[1:termNum,3]))
dev.off()

