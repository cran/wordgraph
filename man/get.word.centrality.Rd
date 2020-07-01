\name{get.word.centrality}
\alias{get.word.centrality}
\title{Returns the centrality of the word.}
\usage{
get.word.centrality(
  awordtolookfor,
  centralityfunctionstr,
  wordreporttolookat,
  verbose = FALSE)
}
\arguments{
\item{awordtolookfor}{The word to look for}

\item{centralityfunctionstr}{The centrality function description string}

\item{wordreporttolookat}{The wordreport list to look for the centrality of the word}

\item{verbose}{Set to TRUE if quiet use is not desired. Default is set to FALSE (do not show warning messages)}
}
\value{
A double with the centrality of the word awordtolookfor
}
\description{
Loop into the entries of the wordreporttolookat, find the centrality function described by centralityfunctionstr and returns the centrality of the word.
If there is not a corresponding code then the function return NA
}
\examples{
# It is a time consuming function...
\donttest{
freeassociationdata_part = dplyr::sample_n(freeassociationdata, 10)

  allcentralitiesandgraphs = get.all.graphs(
                    c("diet1stword", "diet2ndword", "diet3rdword"),
                    freeassociationdata_part)

get.word.centrality("Enjoyment", "centrality_authority",
                        allcentralitiesandgraphs$wordreport)
}
}
