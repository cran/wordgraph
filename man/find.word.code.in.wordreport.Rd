\name{find.word.code.in.wordreport}
\alias{find.word.code.in.wordreport}
\title{Find the index of the word}
\usage{
find.word.code.in.wordreport(
  awordtolookfor,
  wordreporttolookat,
  verbose = FALSE
)
}
\arguments{
\item{awordtolookfor}{The word to look for}

\item{wordreporttolookat}{The wordreport list to look for the centrality of the word (put the wordreport part of allcentralitiesandgraphs object)}

\item{verbose}{Set to TRUE if quiet use is not desired. Default is set to FALSE (do not show warning messages)}
}
\value{
An integer with the index of the word awordtolookfor
}
\description{
Search at wordreporttolookat$name and find the index of the word with name awordtolookfor
The index is the code of the word in the column.

Equivalent to which(grepl(awordtolookfor, allcentralitiesandgraphs$wordreport$name))
}
\author{
Epaminondas Diamantopoulos
}
\examples{
# It is a time consuming function...

\donttest{
allcentralitiesandgraphs = get.all.graphs(c("diet1stword", "diet2ndword", "diet3rdword"),
  freeassociationdata_part)

awordtolookfor = "Diet"
get.word.centrality(awordtolookfor, "centrality_authority", allcentralitiesandgraphs$wordreport)
}
}
\seealso{
Equivalent to which(grepl(awordtolookfor, allcentralitiesandgraphs$wordreport$name))
}
