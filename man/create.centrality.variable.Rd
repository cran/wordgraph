\name{create.centrality.variable}
\alias{create.centrality.variable}
\title{Return a vector with the centrality scores}
\usage{
create.centrality.variable(
  centralityfunctionstr,
  columnwithoriginalwords,
  awordreport,
  verbose = FALSE
)
}
\arguments{
\item{centralityfunctionstr}{The type of the centrality function to use}
\item{columnwithoriginalwords}{The column to look for the words}
\item{awordreport}{The awordreport to look for the centrality of each word}
\item{verbose}{Set to true to print every correspondence}
}
\value{
A vector containing the centralities of the words contained in columnwithoriginalwords
}
\description{
The function return a vector with the centrality scores of the items (i.e. words) of the variable columnwithoriginalwords. The centrality scores are retreived from the wordreport, an object that has been created previously by the function get.all.graphs.
}
\author{
Epaminondas Diamantopoulos
}
\examples{
# It is a time consuming function...

\donttest{
 allgraphs = get.all.graphs(c("diet1stword", "diet2ndword", "diet3rdword"),
 freeassociationdata)

 centralities_vector1 = create.centrality.variable("centrality_betweenness",
 freeassociationdata$exercise1stword, allgraphs$wordreport)

 centralities_vector2 = create.centrality.variable("centrality_alpha",
 freeassociationdata$exercise1stword, allgraphs$wordreport)

 centralities_vector3 = create.centrality.variable("centrality_authority",
 freeassociationdata$exercise1stword, allgraphs$wordreport)

 centralities_vector4 = create.centrality.variable("centrality_closeness",
 freeassociationdata$exercise1stword, allgraphs$wordreport)

 centralities_vector5 = create.centrality.variable("centrality_pagerank",
 freeassociationdata$exercise1stword, allgraphs$wordreport)

 centralities_vector6 = create.centrality.variable("centrality_eigen",
 freeassociationdata$exercise1stword, allgraphs$wordreport)
 }
}
