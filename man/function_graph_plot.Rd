\name{graph_plot}
\alias{graph_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots a graph.
}
\description{
Plots a graph created by get.all.graphs or get.grap h.1.n.group.centrality.with.function functions.
}
\usage{
graph_plot(agraph_object)}
\arguments{
  \item{agraph_object}{A graph object}
}
\author{
Epaminondas Diamantopoulos
}
\seealso{
get.all.graphs
}
\examples{
# The function graph_plot provides graph functionality for a graph
# object created by get.all.graphs function.

\donttest{
# First give...
 allgraphs = get.all.graphs(c("diet1stword", "diet2ndword",
     "diet3rdword"), freeassociationdata)

# Plot a particular graph by giving...
graph_plot(allgraphs$graphs$centrality_closeness)
}
}
