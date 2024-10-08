% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{mixie_fr}
\alias{mixie_fr}
\title{MiXie France}
\format{
Character matrix of 661 rows and 2 columns:
\describe{
\item{\code{substance}}{Substance name.}
\item{\code{toxicity_id}}{Identifier of a toxicological class.}
}
}
\source{
Data provided by the \href{http://en.inrs.fr/}{INRS}.

Toxicological class identifiers were created for all provided classes.
}
\usage{
mixie_fr
}
\description{
Substances and toxicological classes from MiXie-FR.
Associate substances with toxicological classes.
}
\details{
It does not contain all substances from MiXie-FR.
}
\seealso{
\code{\link{mixie_qc}}, \code{\link{toxicological_classes}}.
}
\keyword{datasets}
