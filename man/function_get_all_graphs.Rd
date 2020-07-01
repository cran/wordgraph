\name{get.all.graphs}
\alias{get.all.graphs}
\title{
Creates one ggraph object for each available centrality functions.
}
\description{
Creates a ggraph object for each available centrality function. A data frame with the words and all centralities indexes is also included. The combinations of all word pairs are computed for all concecutive pair of variables and the centrality of each word is calculated according to all available centrality functions. The wordreport list gives the opportunity to the researcher to compare centralities indexes for the words.}
\usage{
get.all.graphs(
          wordvars,
          data.df,
          iscircled = FALSE,
          verbose = FALSE)
}
\arguments{
  \item{wordvars}{The vector containing the names of the variables}
  \item{data.df}{The data frame where the variables belong.}
  \item{iscircled}{Should take also the combination between last and first variable (var_n - var1)? Default is FALSE.}
  \item{verbose}{Should show also warning messages? The default is FALSE.}
}
\value{
A list of objects "graphs" and "wordreport" which are themselves also lists.
\item{graphs}{The graphs list contains the graph data for each one centrality index.}
\item{wordreport }{The wordreport list contains all centrality indexes for every different word that participates in a combination among wordvars variable vector.}
}
\references{
http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/
}
\author{
Epaminondas Diamantopoulos
}

\seealso{
get.graph.1.n.group.centrality.with.function
}
\examples{
# It is a time consuming function...

\donttest{
allgraphs = get.all.graphs(c("diet1stword", "diet2ndword", "diet3rdword"),
freeassociationdata)

# Get insight in the relation of the centralities for your word set...
cor(allgraphs$wordreport[2:7])

# Plot a particular graph by giving the command
graph_plot(allgraphs$graphs$centrality_closeness)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
\keyword{hplot}
\keyword{graphs}
