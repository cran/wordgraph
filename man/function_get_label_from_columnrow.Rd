\name{get_label_from_columnrow}
\alias{get_label_from_columnrow}
\title{
Returns the label of a value.
}
\description{
Finds the label (string) of a variable specific value at the row.
If there is not a label then the function return the value.
If there is not value or the nrow is not in an acceptable range then it returns NA.}
\usage{
get_label_from_columnrow(
              arow,
              column.of.data.frame,
              verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arow}{The row to look at}
  \item{column.of.data.frame}{The column name to look for value labels}
  \item{verbose}{Set to TRUE if quiet use is not desired. Default is set to FALSE (do not show warning messages}
}
\value{
A string with the label of the value
}
\author{
Epaminondas Diamantopoulos
}
\examples{
get_label_from_columnrow(21, freeassociationdata$gender)
}
