\name{get.word.combinations.as.tbl_graph}
\alias{get.word.combinations.as.tbl_graph}
\title{
Calculate all word pairs frequencies.
}
\description{
Calculate all word pairs frequencies among concecutive variables in wordvars vector and
return a tidygraph::as_tbl_graph object in order to be used to create the graph.}
\usage{
get.word.combinations.as.tbl_graph(
                      wordvars,
                      data.df,
                      iscircled = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wordvars}{The vector containing the names of the variables}
  \item{data.df}{The data frame where the variables belong.}
  \item{iscircled}{Should take also the combination between last and first variable (var_n - var1)? Default is FALSE.}
}
\value{
A tidygraph::as_tbl_graph object containing the frequencies of all value pairs between concecutive variables in wordvars vector.
}
\author{
Epaminondas Diamantopoulos
}
\examples{
# It is a time consuming function...
# Thus, for illustrative purposes, an example is provided in a
# small random subset of the original data...

freeassociationdata_part = dplyr::sample_n(freeassociationdata, 10)

df.word.combinations.as.tbl_graph = get.word.combinations.as.tbl_graph(
c("diet1stword", "diet2ndword", "diet3rdword"), freeassociationdata_part)

print(df.word.combinations.as.tbl_graph)
}
