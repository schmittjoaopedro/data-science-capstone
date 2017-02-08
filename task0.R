# Obtaining the data
if(!file.exists("Coursera-SwiftKey.zip")) {
    download.file(
        url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
        file = "Coursera-SwiftKey.zip")
}
# Unziping the data
if(!file.exists("final")) {
    unzip(zipfile = "Coursera-SwiftKey.zip")
}

# Reading the data
twitterData <- readLines("final/en_US/en_US.twitter.txt")
blogsData <- readLines("final/en_US/en_US.blogs.txt")
newsData <- readLines("final/en_US/en_US.news.txt")

# How data is look like
head(twitterData)
head(blogsData)
head(newsData)

# Size of objects
object.size(twitterData)
object.size(blogsData)
object.size(newsData)

# Num lines of files
length(twitterData)
length(blogsData)
length(newsData)

# Max phrases size
max(nchar(readLines("final/en_US/en_US.twitter.txt")))
max(nchar(readLines("final/en_US/en_US.blogs.txt")))
max(nchar(readLines("final/en_US/en_US.news.txt")))

# Min phrases size
min(nchar(readLines("final/en_US/en_US.twitter.txt")))
min(nchar(readLines("final/en_US/en_US.blogs.txt")))
min(nchar(readLines("final/en_US/en_US.news.txt")))