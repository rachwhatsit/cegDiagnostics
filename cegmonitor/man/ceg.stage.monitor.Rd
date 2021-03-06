% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ceg_stage_monitor.R
\name{ceg.stage.monitor}
\alias{ceg.stage.monitor}
\title{A function to compute the batch monitor of a BN according to the form given in Cowell, et. al. 2007}
\usage{
ceg.stage.monitor(rho = 0.8, t = 500, which.cut, tgt.stg, stage.key,
  stages, df)
}
\arguments{
\item{rho}{double of the probability of situations composing stage changing}

\item{t}{integer ofnumber of records to consider from the data frame}

\item{which.cut}{integer of the cut that the target stage belongs to}

\item{tgt.stg}{string recording what stage to examine}

\item{stage.key}{list of data frames generated by tostagekey()}

\item{stages}{list of all stages in the staged tree}

\item{df}{data frame}
}
\description{
A function to compute the batch monitor of a BN according to the form given in Cowell, et. al. 2007
}
\examples{

}
\keyword{forecast,}
\keyword{monitor}
\keyword{one}
\keyword{stage,}
\keyword{step}
