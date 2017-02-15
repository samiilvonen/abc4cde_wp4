## ABC4CDE/DEMC - R-script for prototype tool WP4
## Rasmus.Benestad@met.no  Oslo, Norway, 2017-02-14
##

require(esd) ## This code builds on the esd package: https://github.com/metno/esd

##  Function of an R-package that retrieves data files with CMIP or CORDEX results

getatt <- function(fname) {
  ## Reads and extracts the attribute information in a netCDF files and stores this in a list object## 
  ## This is part of the information stored in the metadatabase
  ncid <- nc_open(fname)
  nc_close(ncid)
  return(ncid)
}

## Generic function to retrieve climate model (CM) file from the KNMI ClimateExplorer
getCM <- function(url='https://climexp.knmi.nl/CMIP5/monthly/tas/tas_Amon_ACCESS1-0_historical_000.nc',destfile='CM.nc',lon=NULL,lat=NULL) {
  ## Retrieves the data 
  lok <- try(download.file(url=url,destfile = destfile))
  if (inherits(loc,"try-error")) return()
  X <- retrieve(destfile,lon=lon,lat=lat)
  ## Collect information stored in the netCDF header
  cid <- getatt(destfile)
  ## Extract a time series for the area mean for 
  cid$area.mean <- aggregate.area(X,FUN='mean')
  cid$url <- url
  return(cid)
}

## Specific function to retrieve GCMs
getGCMs <- function(select=1:3,varid='tas') {
  ## Get the urls
  url <- cmip5.download(varid=varid)[select]
  ## Set up a list variable to contain all the metadata in sub-lists.
  X <- list()
  for (i in select) X[[as.character(i)]] <- getCM(url=url[i])
  return(X)
}

testGCM <- function(select=1:3,varid='tas',path='~/storeB/CMIP5.monthly/rcp45/') {
  fnames <- list.files(path=path,pattern=varid,full.names = TRUE)
  X <- list()
  for (i in select) {
    print(fnames[i])
    x <- retrieve(fnames[i],varid=varid)
    ncid <- nc_open(fnames[i])
    nc_close(ncid)
    ncid$area.mean <- aggregate.area(x,FUN='mean')
    ncid$url <- fnames[i]
    X[[as.character(i)]] <- ncid
  }
  return(X)
}


## Specific model to retrieve RCMs
getRCMs <- function(select=1:3,varid='tas') {
  ## Get the urls
  url <- paste('https://climexp.knmi.nl/CORDEX/EUR-44/mon/',varid,'/',varid,'_EUR-44_cordex_rcp45_mon_00',select,'.nc',sep='')
  url <- sub('0000','000',url)
  ## Set up a list variable to contain all the metadata
  X <- list()
  for (i in select) X[[as.character(i)]] <- getCM(url=url[i])
  return(X)
}



