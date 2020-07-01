\name{freq.of.all.combinations.df}
\alias{freq.of.all.combinations.df}
\title{
Count all combinations between the values of column1 and column2.
}
\description{
Count all combinations between the values of column1 and column2 and return a data frame
with the combination of the items and the respecting frequencies.}
\usage{
freq.of.all.combinations.df(column1, column2, data.df, order = 0, removeNA = TRUE)
}
\arguments{
  \item{column1}{The name of the first column}
  \item{column2}{The name of the second column}
  \item{data.df}{The data frame containing the columns column1, column2}
  \item{order}{0 (default): no order, 1: descenting frequancy order, 2: ascenting wordcode order, 3: ascenting word order.}
  \item{removeNA}{Should remove any NA values or count them in their combination? Default is TRUE (remove NA).}
}

\value{
A data frame containing the frequencies of all pairs of values between column1 and column2. It counts all pair occurences independent of position, thus the pairs of (column1, column2): (a, b), (a, b), (b, a) will result to the entry in freq.of.all.combinations.df of (word1 = a, word2 = b, freq = 3).
}
\author{
Epaminondas Diamantopoulos
}

\examples{

# It is a time consuming function...
# Thus, for illustrative purposes, an example is provided in a small
# random subset of the original data...

freeassociationdata_part = dplyr::sample_n(freeassociationdata, 10)


df.with.frequencies = freq.of.all.combinations.df("diet1stword", "diet2ndword",
freeassociationdata_part)

head(df.with.frequencies)
}
