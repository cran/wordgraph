\name{add.centrality.variables.to.data.frame}
\alias{add.centrality.variables.to.data.frame}
\title{
Main function of the package. Creates the graphs and adds the centrality variables to the data frame.
}
\description{
Creates the graph of all words contained in the wordvars, computes the centrality of each word and for each wordvars variable, a column containing the corresponding centrality score is added to the dataframe.

Supported centrality functions are "centrality_alpha", "centrality_authority", "centrality_betweenness", "centrality_closeness", "centrality_pagerank", "centrality_eigen".

After applying add.centrality.variables.to.data.frame, subsequent quantitative analysis can follow where each word is replaced by its correspondent centrality score.
}
\usage{
add.centrality.variables.to.data.frame(
  wordvars,
  centralityfunctionstr,
  data.df,
  iscircled = FALSE,
  verbose = FALSE)
}
\arguments{
  \item{wordvars}{The vector containing the names of the variables containing the free associated words (from 1_st to n_th)}
  \item{centralityfunctionstr}{The type of the centrality function to use (single string or vector of strings)}
  \item{data.df}{The data frame where wordvars belong.}
  \item{iscircled}{Set to TRUE if the graph is circled (that is, last word is related to the first). Default is FALSE.}
  \item{verbose}{Set to TRUE if warning messages are desired. Default is FALSE.}
}
\value{
The initial data frame with additional columns, one for each variable in wordvars vector, having the correspondiding centrality of the contained words.
}
\author{
Epaminondas Diamantopoulos
}

\examples{
data(freeassociationdata)

# It is a time consuming function...

\donttest{
wordvars = c("diet1stword", "diet2ndword", "diet3rdword")
df.with.centralities1 = add.centrality.variables.to.data.frame(wordvars,
"centrality_eigen", freeassociationdata)

# Variables with the centralities of the words in wordvars was added
# in the end of df.with.centralities1.

# Histogram of eigen centrality index
hist(df.with.centralities1$diet1stword_centrality_eigen)

# One may ask for two centralities...
df.with.centralities2 = add.centrality.variables.to.data.frame(wordvars,
c("centrality_authority", "centrality_betweenness"), freeassociationdata)

# Pearson correlation between authority and betweeness centrality indexes for the 1st word
# concerning diet.

cor(df.with.centralities2[c("diet1stword_centrality_authority",
"diet1stword_centrality_betweenness")], use="complete.obs", method = "pearson")

# ...or even all available centralities.
# Warning: depending on the number of variables and the size of the data frame, it may be a
# time consuming procedure...

df.with.centralities2 = add.centrality.variables.to.data.frame(wordvars,
"all", freeassociationdata)

}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities }% use one of  RShowDoc("KEYWORDS")
