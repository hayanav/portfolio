library(igraph)

Members= read.csv("Members.csv", header=T)
CommitteeEMail= read.csv("CommitteeEMail.csv", header=T)

#GraphDensity
Membersgraph=graph_from_data_frame(Members, directed=F)
Membersgraph=graph_from_data_frame(Members, directed=F, vertices=CommitteeEMail)
V(Membersgraph)
E(Membersgraph)
MG= simplify(Membersgraph)
plot(MG)

graph.density(MG)

#GraphReciprocity
reciprocity(MG)

#Egometrics- degree
degree(MG)

#Egometrics- closeness
closeness(MG)

#Egometrics- betweenness
betweenness(MG)

#Egometrics- eigenvectorcentrality
evcent(MG)


#KeyActorAnalysis- betweenness vs eigenvector
x <-betweenness(MG)
y <-evcent(MG)
plot(x, y, xlabel= 'betweenness', ylabel= 'evcent')


x <-betweenness(MG)
y <-evcent(MG)

plot(from, to, xlab='From', ylab='Closing Prices', 
     main='Closing Prices by Year' , col='dark red',
     xlim=c(0,100), ylim=c(0,100))
 
plot(evcent ~ betweenness | data=MG, 
    xlab='Year', ylab='Closing Prices', 
     main='Betweenness vs Eigenvector',
    labels=row.names(MG))



plot(evce ~ betweenness | vector, data=MG,
            xlab="Weight of Car", ylab="Miles Per Gallon",
            main="Enhanced Scatter Plot",
            labels=row.names(MG))

x <- betweenness
y <- evcent

plot(x, y, main="Betweenness vs Eigenvector",
     xlab="Betweenness ", ylab="Eigenvector",
     pch = 19, frame = FALSE)
#scatterplot matrix of ego metrics
pairs(betweenness(MG))

