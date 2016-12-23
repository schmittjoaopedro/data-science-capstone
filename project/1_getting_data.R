# Valid directories variables
dir_data <- "data"
dir_data_raw <- "data/raw"
dir_data_raw_final <- "data/raw/final"
zip_data_file <- "Coursera-SwiftKey.zip"
zip_data_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

# Obtaining the data
if(!dir.exists("data")) {
    dir.create(data_dir)
}
if(!file.exists(sprintf("%s/%s", dir_data, zip_data_file))) {
    download.file(
        url = zip_data_url, 
        destfile = sprintf("%s/%s", dir_data, zip_data_file))
}
# Unziping the data
if(file.exists(sprintf("%s/%s", dir_data, zip_data_file)) && 
   !dir.exists(sprintf(dir_data_raw_final))) {
    unzip(zipfile = sprintf("%s/%s", dir_data, zip_data_file),
        exdir = dir_data_raw)
}


# Sampling the data
locale_file <- "en_US"
data.file <- c()
for(source in c("blogs", "news", "twitter")) {
    file <- readLines(sprintf("%s/%s/%s.%s.txt", dir_data_raw_final, locale_file, locale_file, source), warn = F)
    file_lines <- length(file)
    file_sample <- ceiling(file_lines * 0.01)
    file <- file[sample(1:file_lines, file_sample, replace = F)]
    print(sprintf("Sample %s of %s with %s", source, file_lines, file_sample))
    data.file <- c(data.file, file)
}

saveRDS(object = data.file, file = sprintf("%s/%s.rds", dir_data_raw, "data_sampled"))
