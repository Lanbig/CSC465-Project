library(igraph)
setwd("~/Dropbox/CSC465/dunnhumby_The-Complete-Journey/dunnhumby - The Complete Journey CSV")

raw_transaction = read.table("transaction_data.csv", sep=",", header=T ,na.strings = " ")
raw_product = read.table("product.csv", sep=",", header=T, na.strings = " ")

raw_total <- merge(raw_transaction,raw_product,by="PRODUCT_ID", all.x=TRUE)

#remove NA
raw_total_nNA <- raw_total[complete.cases(raw_total),]

head(raw_transaction)
head(raw_product)


raw_Day1 = raw_total_nNA[raw_total_nNA$DAY == 2, ]
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
edges = edges[1:nEdges, ]
#####################

g = graph.data.frame(edges, directed=F)
g = simplify(g, remove.multiple = T)
E(g)$weight <- edge.betweenness(g)

plot(g,  layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Network Graph',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     edge.width=edge.betweenness(g) * 3,
     vertex.label.cex=1)

#####################

g = graph.data.frame(edges, directed=T)
g = simplify(g, remove.multiple = T)

plot(g,  layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Network Graph',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(g)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     edge.width=edge.betweenness(g),
     vertex.label.cex=1)



