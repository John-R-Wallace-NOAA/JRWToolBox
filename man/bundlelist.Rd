\name{bundlelist}
\alias{bundlelist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 function to convert a character vector to a tagged list composed of tagged elements corresponding to the elements of the vector
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bundlelist(charvector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{charvector}{
character vector where each element is currently in .GlobalEnv
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (charvector) 
{
    Return = list()
    for (i in 1:length(charvector)) {
        Tmp = list(get(charvector[[i]]))
        names(Tmp) = charvector[[i]]
        Return = c(Return, Tmp)
    }
    return(Return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
