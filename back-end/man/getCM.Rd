\name{getCM}
\alias{getCM}
\alias{getatt}
\alias{getGCMs}
\alias{testGCM}
\alias{getRCMs}
\alias{cmip5.urls}
\alias{commonEOFS.gcm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCM(url = "https://climexp.knmi.nl/CMIP5/monthly/tas/tas_Amon_ACCESS1-0_historical_000.nc", destfile = "CM.nc", lon = NULL, lat = NULL,force=FALSE)
getatt(fname)
getGCMs(select=1:3,varid='tas',destfile=rep('CM.nc',3))
testGCM(select=1:3,varid='tas',path='~/storeB/CMIP5.monthly/rcp45/')
getRCMs(select=1:3,varid='tas',destfile=rep('CM.nc',3))
cmip5.urls(experiment='rcp45',varid='tas',
           url="http://climexp.knmi.nl/CMIP5/monthly/", 
           path=NULL,off=FALSE,force=FALSE,verbose=FALSE)
           commonEOFS.gcm(select=1:3,varid='tas',destfile=paste(rep('CM',3),1:3,'.nc',sep=''),it=NULL,is=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{
%%     ~~Describe \code{url} here~~
}
  \item{destfile}{
%%     ~~Describe \code{destfile} here~~
}
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{lat}{
%%     ~~Describe \code{lat} here~~
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
function (url = "https://climexp.knmi.nl/CMIP5/monthly/tas/tas_Amon_ACCESS1-0_historical_000.nc", 
    destfile = "CM.nc", lon = NULL, lat = NULL) 
{
    lok <- try(download.file(url = url, destfile = destfile))
    if (inherits(loc, "try-error")) 
        return()
    X <- retrieve(destfile, lon = lon, lat = lat)
    cid <- getatt(destfile)
    cid$area.mean <- aggregate.area(X, FUN = "mean")
    cid$url <- url
    return(cid)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
