\name{create_a_new_unique_var_name}
\alias{create_a_new_unique_var_name}
\title{
Provides variable name that is not already exists in the data frame data
}
\description{
 Provides a column name like the likewhatvariable that is not already exists in the data frame data.
 }
\usage{
create_a_new_unique_var_name(likewhatvariable, data.df)
}
\arguments{
  \item{likewhatvariable}{The initial name (string)}
  \item{data.df}{The data frame to be added}
}
\value{
A string with the suggested name.
}

\author{
Epaminondas Diamantopoulos
}
\examples{
newvariablename = create_a_new_unique_var_name(
            "diet1stword",
            freeassociationdata)

print(newvariablename)
}
