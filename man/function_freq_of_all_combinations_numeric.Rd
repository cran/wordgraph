\name{freq.of.all.combinations.df.numeric}
\alias{freq.of.all.combinations.df.numeric}
\title{
Special case of freq.of.all.combinations.df}
\description{
Count all combinations between the values of column1 and column2 and return a data frame
with the combination of the items and the respecting frequencies. Special case of freq.of.all.combinations.df for numeric column1 and column2, thus no special reason to call.
}
\usage{
freq.of.all.combinations.df.numeric(column1, column2, data.df, order = 0, removeNA = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{column1}{The name of the first column}
  \item{column2}{The name of the second column}
  \item{data.df}{The data frame containing the columns var1, var2}
  \item{order}{0 (default): no order, 1: descenting frequancy order, 2: ascenting wordcode order, 3: ascenting word order.}
  \item{removeNA}{Should remove any NA values or count them in their combination? Default is TRUE (remove NA).}
}
\value{
A data frame containing the frequencies of all pairs of values between column1 and column2. It counts all pair occurences independent of position, thus the pairs of (column1, column2): (a, b), (a, b), (b, a) will result to the entry in freq.of.all.combinations.df of (word1 = a, word2 = b, freq = 3).
}
\author{
Epaminondas Diamantopoulos
}
\seealso{
freq.of.all.combinations.df
}
