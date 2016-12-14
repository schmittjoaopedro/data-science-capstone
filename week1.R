# Open a connection with the file
connRead <- file("final/en_US/en_US.twitter.txt", "r")
# Read Next
readLines(connRead, 1)
close(connRead)

# Create a new File
if(file.exists("tidy/tidy.twitter.txt")) {
    file.remove("tidy/tidy.twitter.txt")
}
file.create("tidy/tidy.twitter.txt")

# Connection to new file
connWrite <- file("tidy/tidy.twitter.txt", "w")
# Write some data
writeLines(readLines(connRead, 1), connWrite)
close(connWrite)

# Quiz 1
# Q1
file.info("final/en_US/en_US.blogs.txt")$size / 1024^2
# Q2
length(readLines("final/en_US/en_US.twitter.txt"))
# Q3
max(nchar(readLines("final/en_US/en_US.twitter.txt")))
max(nchar(readLines("final/en_US/en_US.blogs.txt")))
max(nchar(readLines("final/en_US/en_US.news.txt")))
# Q4
dataFile <- readLines("final/en_US/en_US.twitter.txt")
loveCount <- length(grep("love", dataFile))
hateCount <- length(grep("hate", dataFile))
loveCount / hateCount
# Q5
dataFile[grep("biostats", dataFile)]
# Q6
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", dataFile))
