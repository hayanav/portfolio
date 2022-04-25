#hayanaviwala
#20823842

#tutorial 4.1
CoSponsors=read.csv('cosponsors.csv',header=T)
Legislators=read.csv('legislators.csv', header=T)


CoSponsorGraph=graph_from_data_frame(CoSponsors, directed=F)
CoSponsorGraph=graph_from_data_frame(CoSponsors, directed=F, vertices=Legislators)
V(CoSponsorGraph)
E(CoSponsorGraph)
CSG=simplify(CoSponsorGraph)

#tutorial 4.2
plot(CSG)
tkplot(CSG)

plot(CSG, layout=layout_as_tree(CSG))
plot(CSG, layout=layout_with_gem(CSG))

V(CSG)$color=ifelse(V(CSG)$Party=='D', 'blue','red')

plot(CSG, layout=layout_as_tree(CSG)) 
title(sub='A: Reingold-Tilford \n(Democrats=Blue;Republicans=Red')

V(CSG)$color=ifelse(V(CSG)$Gender=='M', 'yellow','green')
plot(CSG, layout=layout_with_gem(CSG)) 
title(sub='B: Gem \n(Males=Yellow;Females=Green)')

#tutorial 4.3
vcount(CSG)
ecount(CSG)

graph.density(CSG)
diameter(CSG)

farthest.nodes(CSG)
reciprocity(CSG)

transitivity(CSG)
degree(CSG)

evcent(CSG)
evcent(CSG)$vector

plot(CSG)
plot(CSG, vertex.size=evcent(CSG)$VECTOR*5^1.5)
plot(CSG, vertex.size=evcent(CSG)$vector*5^1.5, vertex.shape='sphere')
plot(CSG, vertex.size=evcent(CSG)$vector*5^1.5, vertex.shape='sphere', vertex.label.color=ifelse(V(CSG)$Party=='D','blue','red'))
plot(CSG, vertex.size=evcent(CSG)$vector*5^1.5, vertex.shape='sphere', vertex.label.color=ifelse(V(CSG)$Party=='D','blue','red'), 
     vertex.color='white',
     vertex.label.family='sans',
     vertex.label.cex=.95,
     vertex.label.font=2,
     margin=-.35)

constraint(CSG)
shortest.paths(CSG)
subcomponent(CSG, 1)
subcomponent(CSG, 8)









