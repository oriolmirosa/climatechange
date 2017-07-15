# 1. OBTAIN DATA

# We'll start by scraping a few sites in order to obtain the data needed for
# the project. The sites are: townhall.com, rushlimbaugh.com, foxnews.com

# 1.1. townhall.com

library(rvest)

# Create empty vectors to fill with the links to the relevant pages
linksClimateChange <- vector(mode="character", length=0)
linksBad <- vector(mode="character", length=0)

# Download all the links produced with searches for 'climate change' and
# 'global warming'. We start with 'climate change'
# Notice that, in order to prevent Google from blocking us, we have to
# space our connections for at least a minute. This means that downloading all
# the pages takes a long time, about 12 hours for each search loop

numLinksTotal = 0
for (year in 2004:2017) {
  i = 0
  numLinksYear = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22climate+change%22+site%3Atownhall.com%2Fcolumnists&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    linksClimateChange <- c(linksClimateChange, linksnew)
    numLinksYear = numLinksYear + length(linksnew)
    numLinksTotal = numLinksTotal + length(linksnew)
    cat(paste0('Downloaded ', length(linksnew), ' links from page ', i, ' of year ', year, '\n'))
    cat(paste0('Links downloaded for year ', year, ': ', numLinksYear, '\n'))
    cat(paste0('Total \'global warming\' links downloaded so far: ', numLinksTotal, '\n\n'))
    i = i + 1
    Sys.sleep(sample(seq(60, 75), 1))
  }
}

# We have 7304 links at this point:

length(linksClimateChange)

# Let's back these up before we move on so that we don't have to fetch them again

linksTownhallClimateChange <- linksClimateChange
save(linksTownhallClimateChange, file = 'linksTownhallClimateChange.Rda')

# Now we get the links for 'global warming'

linksGlobalWarming <- vector(mode="character", length=0)

numLinksTotal = 0
for (year in 2004:2017) {
  i = 0
  numLinksYear = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22global+warming%22+site%3Atownhall.com%2Fcolumnists&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    linksGlobalWarming <- c(linksGlobalWarming, linksnew)
    numLinksYear = numLinksYear + length(linksnew)
    numLinksTotal = numLinksTotal + length(linksnew)
    cat(paste0('Downloaded ', length(linksnew), ' links from page ', i, ' of year ', year, '\n'))
    cat(paste0('Links downloaded for year ', year, ': ', numLinksYear, '\n'))
    cat(paste0('Total \'global warming\' links downloaded so far: ', numLinksTotal, '\n\n'))
    i = i + 1
    Sys.sleep(sample(seq(60, 75), 1))
  }
}

# We got 7511 links for 'global warming'

length(linksGlobalWarming)

# Once more, let's save the links as we have them now in case we need to reuse
# them

linksTownhallGlobalWarming <- linksGlobalWarming
save(linksTownhallGlobalWarming, file = 'linksTownhallGlobalWarming.Rda')

# Let's join together the links from the two searches, with a total of 14815
# links

linksTownhall <- c(linksTownhallClimateChange, linksTownhallGlobalWarming)
length(linksTownhall)

# After studying the form of the links from Google, we will now clean them in
# order to obtain the links necessary to reach the valid articles

# These lines remove the Google specific elements from the URL and
# reconstruct the URL without the encoded symbols
linksTownhall <- gsub('/url\\?q=', '', linksTownhall)
linksTownhall <- gsub("(.+n[0-9]{6,7}).*", "\\1", linksTownhall)
linksTownhall <- gsub("%25E2%2580%2599", "’", linksTownhall)
linksTownhall <- gsub("%3Fpage%3D1", "", linksTownhall)
linksTownhall <- gsub("%25C3%25A9", "é", linksTownhall)
linksTownhall <- gsub("%25E2%2580%2599", "’", linksTownhall)

# These lines remove the links that are not articles (basically the author
# pages for each Townhall author)

for (i in length(linksTownhall):1) {
  if (!grepl("n[0-9]{6,7}", linksTownhall[i]) && !grepl(".+/[0-9]{4}/[0-9]{2}/[0-9]{2}/.+", linksTownhall[i])) {
    linksBad <-append(linksBad, linksTownhall[i])
    linksTownhall <- linksTownhall[-i]
    next
  }
}
linksTownhall <- gsub('(.+)&sa=.+', '\\1', linksTownhall)
linksTownhall <- gsub('http://', 'https://', linksTownhall)
linksTownhall <- gsub('https://www\\.', 'https://', linksTownhall)

# At this point, we are left with 14326 links

length(linksTownhall)

# Now we eliminate any duplicates, likely to appear because of the two separate
# searches

linksTownhall <- unique(linksTownhall)

# In the end, we have only 962 different links to articles

length(linksTownhall)

# Townhall links have a number (starting with n and followed by 6 or 7 digits),
# yet the link will work even if the number is absent. If any of our links are
# do not have the number it is possible that they are duplicated and we haven't
# caught them (because they appear both with and without the number on the
# list). Let's see how many links do not have the number

noNumberLinks <- which(!grepl('.+n[0-9]{6,7}$', linksTownhall))
length(noNumberLinks)

# There's 30 links without the number. We could deal with the duplicates
# later, when we have downloaded the data and we can find repeated titles,
# but that means that we could potentially hit the servers to download articles
# more times than necessary, so we'll deal with this now

# Let's see if any of the links without number coincide with the other links

numberLinks <- !seq(1, 976) %in% noNumberLinks

for (i in length(noNumberLinks):1) {
  if (sum(grepl(paste0('^', linksTownhall[noNumberLinks[i]], '.*'), linksTownhall[numberLinks])) > 0) {
    cat(paste0('The link: ', linksTownhall[noNumberLinks[i]], ' is duplicated\n\n'))
  }
}

# We get no answers, so there don't seem to be any duplicates. In any case, we
# will check again later when we have downloaded all articles

# Now that we have all the links for the articles, we are ready to go through
# them and extract the relevant data. But first, let's save these data to disk

save(linksTownhall, file = 'linksTownhall.Rda')

# Here we set up the tibble where we will introduce all the info that we need

library(tibble)
library(lubridate)
transcriptTownhall <- tibble(title=character(), author=character(), date=as.Date(character()), body=character())

# Now we go through all the links, extract the info that we need from each
# page, and put it in the 'transcript' tibble

badLinks <- vector(mode='character', length=0)

for (i in 1:length(linksTownhall)) {
  page <- try(read_html(linksTownhall[i]))
  
  if (class(page)[1] == "try-error") {
    cat(paste0("Error downloading link #: ", i, "\n"))
    transcriptTownhall[i,]$title <- "error"
    badLinks <- c(badLinks, linksTownhall[i])
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
    cat(paste0("Error converting date of link #", i, "\n"))
    date <- NA
  }
  
  body <- paste(body, collapse="\n")
  
  transcriptTownhall[i,]$title <- title
  transcriptTownhall[i,]$author <- author
  transcriptTownhall[i,]$date <- date
  transcriptTownhall[i,]$body <- body
}

# There seem to be a few rows where there was a problem. We could fix them
# manually, but given that it is only two of them, we will just delete them
transcriptTownhall <- transcriptTownhall[!is.na(transcriptTownhall$date) | !is.na(transcriptTownhall$author),]

# Now that we have the tibble, let's see if there are any duplicates
sum(duplicated(transcriptTownhall))

# We have 4 duplicates, so we will just remove them
transcriptTownhall <- transcriptTownhall[!duplicated(transcriptTownhall),]

# We are left with a data frame with 956 rows
nrow(transcriptTownhall)

# The last thing we should do is make sure that all these articles really
# discuss climate change. We will search for 'climate change' and 'global
# warming in the body field of the data frame

sum(!grepl('(climate change|global warming)', transcriptTownhall$body))

# Hmmm... 492 articles where climate change or global warming do not appear,
# more than half of the total we got! Let's visually explore a couple of them
# to confirm

cat(transcriptTownhall$body[sample(which(!grepl('(climate change|global warming)', transcriptTownhall$body)), 2)])

# These don't seem to be related to climate change or global warming at all, so
# we will erase all the rows that don't contain these terms

transcriptTownhall <- transcriptTownhall[-which(!grepl('(climate change|global warming)', transcriptTownhall$body)), ]

# We are, in the end, left with a data frame with 464 rows

nrow(transcriptTownhall)

# Let's save this tibble

save(transcriptTownhall, file = 'transcriptTownhall.Rda')

# Before we move on to other sources, let's quickly see the number of
# articles by year and by author

table(format(transcriptTownhall$date, '%Y'))

sort(table(transcriptTownhall$author), decreasing = TRUE)

# As we can see, most of the articles are concentrated in the most recent
# years, and a few authors have a large number of articles, and many authors
# just one. We'll have to keep that into account in our analysis



# 1.2. rushlimbaugh.com

# We will employ the same course of action as with townhall.com, but searching
# on rushlimbaugh.com

linksClimateChange <- vector(mode="character", length=0)
linksBad <- vector(mode="character", length=0)

numLinksTotal = 0
for (year in 2006:2017) {
  i = 0
  numLinksYear = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22climate+change%22+site%3Arushlimbaugh.com%2Fdaily&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    linksClimateChange <- c(linksClimateChange, linksnew)
    numLinksYear = numLinksYear + length(linksnew)
    numLinksTotal = numLinksTotal + length(linksnew)
    cat(paste0('Downloaded ', length(linksnew), ' links from page ', i, ' of year ', year, '\n'))
    cat(paste0('Links downloaded for year ', year, ': ', numLinksYear, '\n'))
    cat(paste0('Total \'global warming\' links downloaded so far: ', numLinksTotal, '\n\n'))
    i = i + 1
    Sys.sleep(sample(seq(60, 75), 1))
  }
}

# We have 2008 links at this point:

length(linksClimateChange)

# Let's back these up before we move on so that we don't have to fetch them again

linksLimbaughClimateChange <- linksClimateChange
save(linksLimbaughClimateChange, file = 'linksLimbaughClimateChange.Rda')

# Now we get the links for 'global warming'

linksGlobalWarming <- vector(mode="character", length=0)

numLinksTotal = 0
for (year in 2004:2017) {
  i = 0
  numLinksYear = 0
  while (TRUE) {
    searchpage <- paste0("https://www.google.com/search?q=%22global+warming%22+site%3Arushlimbaugh.com%2Fdaily&rlz=1C5CHFA_enUS733US733&source=lnt&tbs=cdr%3A1%2Ccd_min%3A1%2F1%2F", year, "%2Ccd_max%3A12%2F31%2F", year, "&start=", i * 10)
    linkspage <- read_html(searchpage)
    linksnew <- linkspage %>% html_nodes("h3 a") %>% html_attr("href")
    if (length(linksnew) == 0) break
    linksGlobalWarming <- c(linksGlobalWarming, linksnew)
    numLinksYear = numLinksYear + length(linksnew)
    numLinksTotal = numLinksTotal + length(linksnew)
    cat(paste0('Downloaded ', length(linksnew), ' links from page ', i, ' of year ', year, '\n'))
    cat(paste0('Links downloaded for year ', year, ': ', numLinksYear, '\n'))
    cat(paste0('Total \'global warming\' links downloaded so far: ', numLinksTotal, '\n\n'))
    i = i + 1
    Sys.sleep(sample(seq(60, 75), 1))
  }
}

# We got 1861 links for 'global warming'

length(linksGlobalWarming)

# Once more, let's save the links as we have them now in case we need to reuse
# them

linksLimbaughGlobalWarming <- linksGlobalWarming
save(linksLimbaughGlobalWarming, file = 'linksLimbaughGlobalWarming.Rda')

# Let's join together the links from the two searches, with a total of 3869
# links

linksLimbaugh <- c(linksLimbaughClimateChange, linksLimbaughGlobalWarming)
length(linksLimbaugh)

# XXXX

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
