#HayaNaviwala
#20823842

library(igraph)
CoSponsors=read.csv('cosponsors.csv', header=T)
Legislators=read.csv('legislators.csv', header=T)
CoSponsorGraph=graph_from_data_frame(CoSponsors,directed=F, vertices = Legislators)
CSG=simplify(CoSponsorGraph)
cliques(CSG, min=5, max=5)
clique.number(CSG)
largest_cliques(CSG)
assortativity(CSG, Legislators$Tenure, directed=F)

#assortativity_nominal(CSG,Legislators$Gender,directed=F)
#assortativity_nominal(CSG,Legislators$Party,directed=F)

assortativity_degree(CSG, directed=F)
cluster_walktrap(CSG)
wc=cluster_walktrap(CSG) 
wc[4]

wc=cluster_walktrap(CSG, steps=200) 
wc

plot(wc, CSG)
oc=cluster_optimal(CSG)
plot(oc, CSG, margin=-.275)

plot(oc, CSG, margin=-.275,edge.color='darkred')

plot(oc, CSG, margin=-.275,
     edge.color='darkred',vertex.label=V(CSG)$Legislator)

plot(oc, CSG, margin=-.275,
     edge.color='darkred',vertex.label=V(CSG)$Legislator, vertex.size=evcent(CSG)$vector*5)

plot(oc, CSG, margin=-.275,
     edge.color='darkred',vertex.label=V(CSG)$Legislator, vertex.size=evcent(CSG)$vector*5, vertex.shape='sphere')

plot(oc, CSG, margin=-.275,
     edge.color='darkred',vertex.label=V(CSG)$Legislator, vertex.size=evcent(CSG)$vector*5, vertex.shape='sphere',vertex.label.cex=.7)

plot(oc, CSG, margin=-.275,
     edge.color='darkred',vertex.label=V(CSG)$Legislator, vertex.size=evcent(CSG)$vector*5, vertex.shape='sphere',vertex.label.cex=.7,vertex.label.dist=.25)

