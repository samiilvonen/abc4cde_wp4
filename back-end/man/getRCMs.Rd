\name{getRCMs}
\alias{getRCMs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{RCM metadata
%%  ~~function to do ... ~~
}
\description{A function that extracts metadata  stored in netCDF files (CF-convention) with regional climate model (RCM) results - CORDEX results.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getRCMs(select = 1:3, varid = "tas")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{select}{Number of files with GCM results to extract
%%     ~~Describe \code{select} here~~
}
  \item{varid}{The variable ID to extract
%%     ~~Describe \code{varid} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Rasmus.Benestad-at-met.no
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{metaxtract}} and \code{\link{getGCM}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (select = 1:3, varid = "tas") 
{
    url <- paste("https://climexp.knmi.nl/CORDEX/EUR-44/mon/", 
        varid, "/", varid, "_EUR-44_cordex_rcp45_mon_00", select, 
        ".nc", sep = "")
    url <- sub("0000", "000", url)
    X <- list()
    for (i in select) X[[as.character(i)]] <- getCM(url = url[i])
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~metadata }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~model-results }% __ONLY ONE__ keyword per line
