#!/usr/bin/Rscript

if( file.exists('requests.rds') ) {
    REQUEST_CACHE <- readRDS('requests.rds')
} else
    REQUEST_CACHE <- list()

echo <- function(string='', fill=T)
{
    UseMethod('echo')
}
echo.default <- function(string='', fill=T)
{
    cat(string, fill=fill)
}

fill_data <- function(someObject)
{
    UseMethod('fill_data')
}
fill_data.default <- function(someObject)
{
    echo('fill_data called with unknown object: No Idea what to do.')
}

input <- function(string='')
{
    echo(paste(string, ''), F) # the paste adds a space after string
    stdin <- file('stdin')
    on.exit(close(stdin))
    readLines(stdin, n=1)
}

http_get <- function(url, sleep=2, cache=T)
{
    if( cache && !is.null(REQUEST_CACHE[[url]]) )
    {
        return(REQUEST_CACHE[[url]])
    }
    Sys.sleep(sleep)
    REQUEST_CACHE[[url]] <<- GET(url)
    REQUEST_CACHE[[url]]
}


