library(psych)
library(Hmisc)
library(igraph)
#################first step
otu <- read.xlsx("otu_table.xlsx",sheet = 1)
rownames(otu) <- otu[,1]
otu <- otu[,2:11]
#otu <- read.csv("otu_table.csv",row.names = 1)
otu1 <- otu
otu1[otu1>0] <- 1
otu <- otu[which(rowSums(otu1) >=5),]
otu <- t(otu)
corr_otu <- corr.test(otu,method="spearman",
                      adjust="fdr")
adjp <- corr_otu$p.adj
n <- (1+sqrt(1+8*length(adjp)))/2
p <- matrix(nrow = n,ncol = n)
p[upper.tri(p)] <- adjp
diag(p) <- 0
p[lower.tri(p)] <- t(p)[lower.tri(p)]

p[p>=0.05] <- -1
p[p < 0.05& p>0] <- 1
p[p==-1] <- 0
corr_otu$r[abs(corr_otu$r)< 0.6] <- 0
corr_otu$r <- corr_otu$r * p
s <- corr_otu$r
write.table(data.frame(s,check.names = FALSE),"otu.txt",col.names = NA,sep="\t",quote = FALSE)


############second step
adjm <- matrix(sample(1:1,100,replace=TRUE,prob=c(0.9,0.1)),ncol = 10)
g1 <- graph_from_adjacency_matrix(adjm,weighted = TRUE,mode = "undirected")
g1
E(g1)$weight
g2 <- graph_from_adjacency_matrix(adjm,weighted = NULL,mode = "undirected")
g2
E(g2)$weight
g3 <- graph_from_adjacency_matrix(adjm,weighted = NULL,mode = "undirected",diag = FALSE)
g3
E(g3)$weight
g4 <- graph_from_adjacency_matrix(adjm,weighted = TRUE,mode = "directed")
g4
E(g4)$weight

m <- graph_from_adjacency_matrix(s,weighted = TRUE,mode = "undirected",diag = FALSE)#
m <- delete.vertices(m,names(degree(m)[degree(m)==0]))#



E(m)$correlation <- E(m)$weight
E(m)$weight <- abs(E(m)$weight)



taxonomy <- read.xlsx("species.xlsx",sheet = 1)
rownames(taxonomy ) <- taxonomy [,1]
taxonomy  <- taxonomy [,-1]
#taxonomy <- read.csv("species.csv",row.name=1)
taxonomy <-taxonomy[as.character(V(m)$name),]
#View(taxonomy)
V(m)$phylum <- taxonomy$phylum
V(m)$class <- taxonomy$class
V(m)$order <- taxonomy$order
V(m)$family <- taxonomy$family
V(m)$genus <- taxonomy$genus
#V(m)$species <- taxonomy$species

####edge
edge <- data.frame(as_edgelist(m,names = TRUE))

df<- as.data.frame(E(m)$correlation)
df[df>0] <- 1
df[df<0] <- -1
colnames(df) <- c("cor")
edge <- data.frame(
  source=edge[[1]],
  target=edge[[2]],
  weight=E(m)$weight,
  correlation=E(m)$correlation,
  cor=df)
edge
write.csv(edge,"network.edge_low.csv",row.names = F)


###node
node <- data.frame(
  id=names(V(m)),
  #phylum = V(m)$phylum ,
  #class = V(m)$class ,
  #order = V(m)$order ,
  family = V(m)$family ,
  genus = V(m)$genus ,
  # species = V(m)$species 
)
write.csv(node,"network.node_low.csv",row.names = F)

write.graph(m,"network.graphml",format = "graphml")

