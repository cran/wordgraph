\name{get.all.word.pairs.frequency}
\alias{get.all.word.pairs.frequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count all combinations between each concecutive pair of variables.
}
\description{
Use multiple times the function freq.of.all.combinations.df to count all combinations between
each concecutive pair of variables of wordvars ( var1 - var2, var2 - var3, ..., var_{n-1} - var_n).
Then combines all pair frequencies into one data frame with the columns "word1code" "word2code" "word1" "word2" "cumsums"}
\usage{
get.all.word.pairs.frequency(wordvars, data.df, iscircled = FALSE)
}
\arguments{
  \item{wordvars}{The vector containing the names of the variables}
  \item{data.df}{The data frame where the variables belong.}
  \item{iscircled}{Should take also the combination between last and first variable (var_n - var1)? Default is FALSE.}
}
\value{
A data frame contains the columns "word1code" "word2code" "word1" "word2" "cumsums"
}
\author{
Epaminondas Diamantopoulos
}
\examples{
# It is a time consuming function...
# Thus, for illustrative purposes, an example is provided in a small
# random subset of the original data...

freeassociationdata_part = dplyr::sample_n(freeassociationdata, 10)


df.all.pairs.frequency = get.all.word.pairs.frequency(c("diet1stword",
"diet2ndword", "diet3rdword"), freeassociationdata_part)

head(df.all.pairs.frequency)
}
