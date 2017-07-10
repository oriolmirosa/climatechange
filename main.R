# 1. OBTAIN DATA

# We'll start by scraping a few sites in order to obtain the data needed for
# the project. The sites are: townhall.com, rushlimbaugh.com, foxnews.com

# 1.1. townhall.com

library(rvest)

# Create empty vectors to fill with the links to the relevant pages
links <- vector(mode="character", length=0)
linksBad <- vector(mode="character", length=0)

# Download all the links produced with searches for 'climate change' and
# 'global warming'. We start with 'climate change'

for (year in 2004:2017) {
  i = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22climate+change%22+site%3Atownhall.com%2Fcolumnists&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    links <- c(links, linksnew)
    i = i + 1
    Sys.sleep(sample(seq(15, 60), 1))
  }
}

# We have 927 links at this point:

length(links)
# Let's back these up before we move on so that we don't have to fetch them again

linksTownhallClimateChange <- links

# Now we get the links for 'global warming'

for (year in 2004:2017) {
  i = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22global+warming%22+site%3Atownhall.com%2Fcolumnists&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    links <- c(links, linksnew)
    i = i + 1
    Sys.sleep(sample(seq(15, 60), 1))
  }
}

# We now have a total of #, which means that we got # new links for 'global
# warming'

length(links)
length(links) - length(linksTownhallClimateChange)

# Once more, let's save the links as we have them now in case we need to reuse
# them

linksTownHallRaw <- links

# After studying the form of the links from Google, we will now clean them in
# order to obtain the links necessary to reach the valid articles

# These lines remove the Google specific elements from the URL and
# reconstruct the URL without the encoded symbols
links <- gsub('/url\\?q=', '', links)
links <- gsub("(.+n[0-9]{6,7}).*", "\\1", links)
links <- gsub("%25E2%2580%2599", "’", links)
links <- gsub("%3Fpage%3D1", "", links)
noNumberLinks <- which(!grepl('.+n[0-9]{6,7}$', links))

# These lines remove the links that are not articles (basically the author
# pages for each Townhall author)
for (i in length(links):1) {
  if (!grepl("n[0-9]{6,7}", links[i]) && !grepl(".+/[0-9]{4}/[0-9]{2}/[0-9]{2}/.+", links[i])) {
    linksBad <-append(linksBad, links[i])
    links <- links[-i]
    next
  }
}
links <- gsub('(.+)&sa=.+', '\\1', links)

# We are left with X links

length(links)

# Now we eliminate any duplicates, likely to appear because of the two separate
# searches

links <- unique(links)

# In the end, we have X different articles

length(links)

# Now that we have all the links for the articles, we are ready to go through
# them and extract the relevant data

# Here we set up the tibble where we will introduce all the info that we need

library(tibble)
library(lubridate)
transcript <- tibble(title=character(), author=character(), date=as.Date(character()), body=character())

# Now we go through all the links, extract the info that we need from each
# page, and put it in the 'transcript' tibble

badLinks <- vector(mode='character', length=0)

for (i in 1:length(links)) {
  page <- try(read_html(links[i]))
  
  if (class(page) == "try-error") {
    cat(paste0("Error downloading link #: ", i, "\n"))
    transcript[i,]$title <- "error"
    badLinks <- c(badLinks, links[i])
    next
  }
  
  title <- page %>% html_node("div h1") %>% html_text()
  author <- page %>% html_node("div.contributor.pull-left > a") %>% html_text()
  date <- (page %>% html_nodes("div.contributor.pull-left") %>% html_text())[3]
  date <- paste(unlist(strsplit(date, ' '))[c(2, 3, 4)], collapse=' ')
  body <- page %>% html_nodes("section#article-body > p") %>% html_text()
  
  date <- parse_date_time(date, 'b! d, y')
  
  try(date <- as.Date(date))
  if (class(date) == "try-error") {
    cat(paste0("Error converting date of link #: ", i, "\n"))
    date <- NA
  }
  
  body <- paste(body, collapse="\n")
  
  transcript[i,]$title <- title
  transcript[i,]$author <- author
  transcript[i,]$date <- date
  transcript[i,]$body <- body
}

# There seem to be a few rows where there was a problem. We could fix them
# manually, but given that it is only two of them, we will just delete them
transcript <- transcript[!is.na(transcript$date) | !is.na(transcript$author),]

# Now that we have the tibble, let's see if there are any duplicates
sum(duplicated(transcript))

# We have 14 duplicates, so we will just remove them
transcript <- transcript[!duplicated(transcript),]

# Since we had 790 rows left before this step, we should now have 776. Let's
# verify it
nrow(transcript)

# Yep! Before we move on to other sources, let's quickly see the number of
# articles by year and by author

table(format(transcript$date, '%Y'))

sort(table(transcript$author), decreasing = TRUE)

# As we can see, most of the articles are concentrated in the most recent
# years, and a few authors have a large number of articles, and many authors
# just one. We'll have to keep that into account in our analysis



# 1.2. rushlimbaugh.com

# We will employ the same course of action as with townhall.com, but searching
# on rushlimbaugh.com

links <- vector(mode="character", length=0)
linksBad <- vector(mode="character", length=0)

for (year in 2006:2017) {
  i = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22climate+change%22+site%3Arushlimbaugh.com%2Fdaily&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    links <- c(links, linksnew)
    i = i + 1
    Sys.sleep(sample(seq(15, 30), 1))
  }
}

# Now we get the links for 'global warming'

for (year in 2006:2017) {
  i = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22global+warming%22+site%3Arushlimbaugh.com%2Fdaily&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    links <- c(links, linksnew)
    i = i + 1
    Sys.sleep(sample(seq(15, 30), 1))
  }
}

linksBackUp <- links

links <- gsub('/url\\?q=', '', links)
links <- gsub('(.+)&sa=.+', '\\1', links)

for (i in length(links):1) {
  if (grepl("quick_hits_page", links[i]) || grepl(".+/[0-9]{4}/[0-9]{2}(/[0-9]{2})?/$", links[i])) {
    linksBad <-append(linksBad, links[i])
    links <- links[-i]
    next
  }
}

links <- unique(links)

transcript2 <- tibble(title=character(), author=character(), date=as.Date(character()), body=character())

badLinks <- vector(mode='character', length=0)

for (i in 1:length(links)) {
  page <- try(read_html(links[i]))
  
  if (class(page) == "try-error") {
    cat(paste0("Error downloading link #: ", i, "\n"))
    transcript2[i,]$title <- "error"
    badLinks <- c(badLinks, links[i])
    next
  }
  
  title <- page %>% html_node("div h1") %>% html_text()
  author <- "Rush Limbaugh"
  date <- page %>% html_nodes("p span") %>% html_text()
  body <- page %>% html_nodes("div.entry-content p") %>% html_text()
  
  date <- parse_date_time(date, 'b! d, y')
  
  try(date <- as.Date(date))
  if (class(date) == "try-error") {
    cat(paste0("Error converting date of link #: ", i, "\n"))
    date <- NA
  }
  
  body <- paste(body, collapse="\n")
  
  transcript2[i,]$title <- title
  transcript2[i,]$author <- author
  transcript2[i,]$date <- date
  transcript2[i,]$body <- body
}

transcript2 <- transcript2[!is.na(transcript2$date),]
