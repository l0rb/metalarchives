#!/usr/bin/Rscript

library(httr)
library(xml2)

BASE_URL <- 'https://www.metal-archives.com'

echo <- function(string='', fill=T)
{
    UseMethod('echo')
}
echo.default <- function(string='', fill=T)
{
    cat(string, fill=fill)
}
input <- function(string='')
{
    echo(paste(string, ''), F) # the paste adds a space after string
    stdin <- file('stdin')
    on.exit(close(stdin))
    readLines(stdin, n=1)
}

Band <- function(name = NULL, members = NULL, all_members = NULL)
{
    band <- list(
        name = name,
        members = members,
        all_members = all_members
    )
    class(band) <- "Band"
    band
}
echo.Band <- function(band)
{
    echo(paste(band$name, 'currently has', length(band$members), 'members:'))
    for(member in band$members)
        echo(paste0('   ',xml_text(member)))
    echo()
    echo(paste0('I also have a list of everyone who ever played with ', band$name, ':'))
    for(member in band$all_members)
        echo(paste0('   ',xml_text(member)))
}

get_by_url <- function(url)
{
    response <- GET(url)
    tree <- content(response, type="text/html", encoding='UTF-8')
    members_current <- xml_find_all(tree, "//div[@id='band_tab_members_current']//tr[@class='lineupRow']/td/a")
    members_all <- xml_find_all(tree, "//div[@id='band_tab_members_all']//tr[@class='lineupRow']/td/a")
    Band(members=members_current, all_members=members_all)
}

search_band_name <- function(search_term)
{
    parse_bandlink <- function(band)
    {
        tree <- read_html(band)
        a_element <- xml_find_all(tree, '//a')
        band_name <- xml_text(a_element)
        band_link <- xml_attr(a_element, 'href')
        c(band_link, band_name)
    }
    echo('Doing my best to find bands. Please wait a few seconds.')
    response <- GET(paste(BASE_URL, '/search/ajax-band-search/?field=name&query=', search_term, '&iDisplayStart=0&iDisplayLength=20', sep=''))
    cont <- content(response)
    n <- cont$iTotalDisplayRecords
    if( n < 1 )
    {
        echo('No band found. Maybe try another search term.')
    }
    if( n == 1 )
    {
        echo('Found a band. Please wait while I spy on them.')
        link_and_name = parse_bandlink(cont$aaData[[1]][[1]])
        band <- get_by_url(link_and_name[[1]])
        band$name = link_and_name[[2]]
        echo()
        echo(band)
    }
    else
    {
        echo(paste('Found', n, 'Bands. Please choose one.'))
        for(index in seq_along(cont$aaData))
        {
            link_and_name = parse_bandlink(cont$aaData[[index]][[1]])
            cont$aaData[[index]][[4]] <- link_and_name
            echo(paste0('   [', index, '] ', link_and_name[[2]], ' from ', cont$aaData[[index]][[3]], ' (', cont$aaData[[index]][[2]], ')'))
        }
        echo()
        choice <- as.numeric(input('Enter band number:'))
        if(is.na(choice) || choice > length(cont$aaData) || choice < 1)
        {
            echo('Are you drunk?')
        }
        else
        {
            link_and_name <- cont$aaData[[choice]][[4]]
            band <- get_by_url(link_and_name[[1]])
            band$name = link_and_name[[2]]
            echo()
            echo(band)
        }
    }
}

echo('If you want to quit the program enter "q" as search term.')
while(T)
{
    echo()
    search_term <- input('Search for a band:')
    if(search_term == 'q')
        quit()
    echo()
    search_band_name(search_term)
}
