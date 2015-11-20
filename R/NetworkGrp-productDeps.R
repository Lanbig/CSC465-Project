library(igraph)
setwd("~/Dropbox/CSC465/CSC465-Project/")

raw_transaction = read.table("./Dataset/transaction_data.csv", sep=",", header=T ,na.strings = " ")
raw_product = read.table("./Dataset/product.csv", sep=",", header=T, na.strings = " ")

raw_total <- merge(raw_transaction,raw_product,by="PRODUCT_ID", all.x=TRUE)

#remove NA
raw_total_nNA <- raw_total[complete.cases(raw_total),]

head(raw_transaction)
head(raw_product)


raw_Day1 = raw_total_nNA[raw_total_nNA$DAY == 400, ]
raw_Day1

edges = data.frame(p0=rep(0, 10000), p1=rep(0, 10000))

nEdges = 0
for (household in unique(raw_Day1$household_key))
{
  hPurchases = raw_Day1[raw_Day1$household_key == household, ]
  for (i in 1:nrow(hPurchases))
  {
    row = hPurchases[i, ]
    row
    hPurchases$DEPARTMENT[i]
    prodID = hPurchases$DEPARTMENT[i]
    for (j in i:nrow(hPurchases))
    {
      prodID2 = hPurchases$DEPARTMENT[j]
      if (prodID != prodID2)
      {
        nEdges = nEdges + 1
        edges$p0[nEdges] = as.character(prodID)
        edges$p1[nEdges] = as.character(prodID2)
      }
    }
  }
}
edges2 = edges[1:nEdges, ]

edges2 <- cbind(edges2,1)
colnames(edges2)[3] <- "num_edge"

library(plyr)
edges2_agg <- ddply(edges2,.(p0,p1),numcolwise(sum))

edges2_agg <- edges2_agg[complete.cases(edges2_agg),]

write.csv(edges2_agg, file="dep.csv")
#####################

g = graph.data.frame(edges, directed=F)
#g = simplify(g, remove.multiple = T)
E(g)$weight <- edge.betweenness(g)
#V(g)$color <- sample(rainbow(7, alpha=0.8))
#layout=layout.fruchterman.reingold
#edge.width=exp(edge.betweenness(g)),
#vertex.size=betweenness(g)*10,
plot(g,layout=layout.circle,	# the layout method. see the igraph documentation for details
     main='Network Graph',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1)

dev.copy(device = png, filename = 'NetworkGrp-ProductDep.png', width = 1024, height = 768) 
dev.off()

#####################

g = graph.data.frame(edges, directed=T)
g = simplify(g, remove.multiple = T)
#V(g)$color <- sample(rainbow(7, alpha=1))
V(g)$number <- sample(1:50, vcount(g), replace=TRUE)

plot(g,  layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Network Graph',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     edge.width=edge.betweenness(g),
     edge.arrow.size=0.3,
     vertex.color=V(g)$color,
     vertex.label.cex=1)

dev.copy(device = png, filename = 'NetworkGrp-ProductDep-D.png', width = 1024, height = 768) 
dev.off()


