\name{get.graph.1.n.group.centrality.with.function}
\alias{get.graph.1.n.group.centrality.with.function}
\title{
Creates a ggraph object.
}
\description{
Creates the ggraph object, i.e. the graph depicting the words containing the variables vector wordvars
The combinations of all word pairs are computed for all concecutive pair of variables and
the centrality of each word is calculated according to centralityfunction.
}
\usage{
get.graph.1.n.group.centrality.with.function(
        wordvars,
        centralityfunction,
        data.df,
        iscircled = FALSE)
}
\arguments{
  \item{wordvars}{The vector containing the names of the variables}
  \item{centralityfunction}{The centrality function to apply. Acceptable entries one of: tidygraph::centrality_authority(), tidygraph::centrality_betweenness(), tidygraph::centrality_closeness(), tidygraph::centrality_pagerank(), tidygraph::centrality_eigen(), tidygraph::centrality_alpha()}
  \item{data.df}{The data frame where the variables belong.}
  \item{iscircled}{Should take also the combination between last and first variable (var_n - var1)? Default is FALSE.}
}
\value{
A list contains the graph data for the selected centrality index.
}
\author{
Epaminondas Diamantopoulos
}
\seealso{
get.all.graphs
}
\examples{
# It is a time consuming function...

\donttest{
agraph = get.graph.1.n.group.centrality.with.function(
    c("diet1stword", "diet2ndword", "diet3rdword"),
    tidygraph::centrality_authority(),
    freeassociationdata)

# To plot the graph simply call:
agraph

# The centrality scores are available at:agraph$data$centrality
hist(agraph$data$centrality)

# Other available centrality functions...

 agraph = get.graph.1.n.group.centrality.with.function(
             c("diet1stword", "diet2ndword", "diet3rdword"),
            tidygraph::centrality_betweenness(), freeassociationdata)

# Note: closeness centrality is not well-defined for disconnected graphs.
 agraph = get.graph.1.n.group.centrality.with.function(
             c("diet1stword", "diet2ndword", "diet3rdword"),
             tidygraph::centrality_closeness(), freeassociationdata)

 agraph = get.graph.1.n.group.centrality.with.function(
            c("diet1stword", "diet2ndword", "diet3rdword"),
            tidygraph::centrality_pagerank(), freeassociationdata)

 agraph = get.graph.1.n.group.centrality.with.function(
             c("diet1stword", "diet2ndword", "diet3rdword"),
             tidygraph::centrality_eigen(), freeassociationdata)

 agraph = get.graph.1.n.group.centrality.with.function(
            c("diet1stword", "diet2ndword", "diet3rdword"),
            tidygraph::centrality_alpha(), freeassociationdata)
}
}

\keyword{aplot}
\keyword{hplot}
\keyword{graphs}
