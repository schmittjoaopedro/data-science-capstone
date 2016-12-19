library(tm)
library(SnowballC)
library(cldr)

dir_data_raw <- "data/raw"
dir_data_clean <- "data/clean"
data_sampled_file <- "data_sampled"
data_clean_file <- "data_clean"

data.file <- readRDS(sprintf("%s/%s.rds", dir_data_raw, data_sampled_file))

# Remove phrases that not are in english
data.file <- data.file[detectLanguage(data_sampled_file)$detectedLanguage == "ENGLISH"]
# Create a corpus
dataVS <- VectorSource(data.file)
dataCorpus <- VCorpus(dataVS)
# Transform to lower
dataCorpus <- tm_map(dataCorpus, content_transformer(tolower))
# Remove ponctuation
dataCorpus <- tm_map(dataCorpus, removePunctuation)
# Remove numbers
dataCorpus <- tm_map(dataCorpus, removeNumbers)
# Remove extra spaces
dataCorpus <- tm_map(dataCorpus, stripWhitespace)

# Save clean data as RDS
if(!dir.exists(dir_data_clean)) dir.create(dir_data_clean)
data.clean <- c()
for(i in 1:length(data.file)) {
    data.clean <- c(data.clean, dataCorpus[[i]]$content)
}
saveRDS(object = data.clean, file = sprintf("%s/%s.rds", dir_data_clean, data_clean_file))