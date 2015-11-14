
require(igraph)

raw = read.table("transaction_data.csv", sep=",", header=T)

head(raw)

raw_Day1 = raw[raw$DAY == 1, ]
raw_Day1

edges = data.frame(p0=rep(0, 10000), p1=rep(0, 10000))

nEdges = 0
for (household in unique(raw_Day1$household_key))
{
  hPurchases = raw_Day1[raw_Day1$household_key == household, ]
  for (i in 1:nrow(hPurchases))
  {
    row = hPurchases[i, ]
    prodID = hPurchases$PRODUCT_ID[i]
    for (j in i:nrow(hPurchases))
    {
      prodID2 = hPurchases$PRODUCT_ID[j]
      if (prodID != prodID2)
      {
        nEdges = nEdges + 1
        edges$p0[nEdges] = prodID
        edges$p1[nEdges] = prodID2
      }
    }
  }
}
edges = edges[1:nEdges, ]

g = graph.data.frame(edges, directed=F)
g = simplify(g, remove.multiple = T)

comps <- components(g)$membership
colbar <- rainbow(max(comps)+1)
V(g)$color <- colbar[comps+1]

plot(g, layout=layout.fruchterman.reingold, vertex.label = ifelse(degree(g) > 100, V(g)$name, NA), vertex.size=5 )
plot(g, layout=layout.fruchterman.reingold, vertex.label=NA, vertex.size=5 )
plot(g, layout=layout,
     vertex.size=map(betweenness(g),c(1,15)),
     edge.width=map(edge.betweenness(g), c(1,10)))


#library(tcltk)
#tkplot(g)
