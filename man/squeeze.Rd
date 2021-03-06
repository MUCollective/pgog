% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/divider.R
\name{squeeze}
\alias{squeeze}
\title{Internal helper function}
\usage{
squeeze(pieces, bounds = bound())
}
\arguments{
\item{pieces}{rectangle specified via l(eft), r(ight), b(ottom), t(op)}

\item{bounds}{rectangle specified via l(eft), r(ight), b(ottom), t(op)}
}
\value{
re-scaled values for piece according to boundaries given by bounds
}
\description{
Squeeze pieces to lie within specified bounds; directly copied from package productplots
}
\author{
Hadley Wickham
}
