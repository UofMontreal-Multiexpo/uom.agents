% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_agent_ids}
\alias{get_agent_ids}
\title{Get agent identifiers}
\usage{
get_agent_ids(agent_id = NULL)
}
\arguments{
\item{agent_id}{\code{NULL} or any subset of \code{agents} rownames.}
}
\value{
Values from \code{agent_id} if not \code{NULL}; \code{agents} rownames otherwise.
}
\description{
Check if identifiers are given and extract identifiers of all agents if not.
This internal function allows to use \code{NULL} as default value to consider all
agents.
}
\note{
This function is not exported from the package.
}
\examples{
get_agent_ids()
get_agent_ids(rownames(agents)[6:10])

}
\author{
Gauthier Magnin
}
\keyword{internal}
