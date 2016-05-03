revdeps <- function(x) {
  depends <- function( pkg = "Rcpp" ){
    index <- readLines( sprintf("https://cran.rstudio.com//web/packages/%s/index.html",pkg) )
    if( any( grepl("Reverse.*depends", index) ) ){
      x <- index[ grep( "Reverse.*depends", index ) + 1L ]
      gsub( "<.*", "", strsplit( x, "<a href.*?>" )[[1L]] )[-1L]
    } else character(0L)
  }

  seen <- character(0)
  graph <- character(0)

  rec.depends <- function( pkg ){
    dep <- depends(pkg)
    if( !length(dep) ) return(NULL)
    graph <<- c( graph, sprintf( "%s->%s", pkg, dep ) )
    for(p in dep[!dep %in% seen]) rec.depends( p )
    seen <<- c( dep[!dep %in% seen] , seen )
  }

  rec.depends(x)
}

 dd <- revdeps("sp")
## install.packages(dd)

listdata <- function(pkg) {
  data_frame(data = data(package = pkg)$results[, "Item"])
}

listpkgdata <- function(packagedata, type = "Spatial") {
  packagedata$class <- character(nrow(packagedata))
  for (i in seq(nrow(packagedata))) {
    x <- try(local({
      data(eval(packagedata$data[i]), package = packagedata$package[i])
      class(get(packagedata$data[i]))
    }))

    if (!inherits(x, "try-error")) packagedata$class[i] <- x

  }
  packagedata
}



datasets <- dplyr::bind_rows(setNames(lapply(dd, listdata), dd), .id = "package")
datasets1 <- listpkgdata(datasets)
