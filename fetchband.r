#!/usr/bin/Rscript

library(httr)
library(xml2)

BASE_URL <- 'https://www.metal-archives.com'
FULL_DATA <- list()
SEARCH_CACHE <- list()

source_if_exists <- function(file)
{
    if( file.exists(file) )
        source(file)
}

source_if_exists('util.r')

Member <- function(name = NULL, url = NULL, born = NULL, age = NULL)
{
    member <- list(
        name = name,
        born = born,
        url = url,
        age = age
    )
    class(member) <- "Member"
    member
}
echo.Member <- function(member)
{
    string <- trimws(member$name)
    if( !is.null(member$age) )
        string <- paste(string, member$age)
    echo(string)
}
fill_data.Member <- function(member)
{
    response <- http_get(member$url)
    tree <- content(response, type="text/html", encoding='UTF-8')
    elements <- xml_find_all(tree, "//div[@id='member_info']/*/dd")
    lapply(elements, function(elem)
    {
        text <- trimws(xml_text(elem))
        if( grepl("born", text, fixed=T) )
        {
            member$born <<- text
            member$age <<- as.numeric(substr(text, 0, 2))
        }
    })
    member
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
    ages <- sapply(band$members, function(m){if(!is.null(m$age)){m$age}else{NA}})
    mean_age <- mean(ages, na.rm=T)
    echo(paste(band$name, 'currently has', length(band$members), 'members.'))
    echo(paste('  Mean age:', mean_age))
    for(member in band$members)
        echo(member)
    echo()
    echo(paste0('I also have a list of everyone who ever played with ', band$name, ':'))
    for(member in band$all_members)
        echo(member)
}

get_by_url <- function(url)
{
    parse_member <- function(xml_element)
    {
        name <- xml_text(xml_element)
        url <- xml_attr(xml_element, 'href')
        member <- Member(name=name, url=url)
        fill_data(member)
    }
    response <- http_get(url)
    tree <- content(response, type="text/html", encoding='UTF-8')
    members_current <- xml_find_all(tree, "//div[@id='band_tab_members_current']//tr[@class='lineupRow']/td/a")
    members_all <- xml_find_all(tree, "//div[@id='band_tab_members_all']//tr[@class='lineupRow']/td/a")
    members <- list()
    all_members <- list()
    members <- lapply(members_current, parse_member)
    all_members <- lapply(members_all, parse_member)
    band <- Band(members=members, all_members=all_members)
    FULL_DATA[[url]] <<- band
    band
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
    response <- http_get(paste(BASE_URL, '/search/ajax-band-search/?field=name&query=', search_term, '&iDisplayStart=0&iDisplayLength=20', sep=''))
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
    if( n > 1 )
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
    {
        saveRDS(REQUEST_CACHE, "requests.rds")
        quit()
    }
    echo()
    search_band_name(search_term)
}
