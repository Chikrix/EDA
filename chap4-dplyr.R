library(tidyverse)
library(downloader)

file_path <- "data/chicago"
chicago <- readRDS(file_path)
dim(chicago)
head(chicago)
str(chicago)
names(chicago)[1:3]
(subset <- select(chicago, city:dptp))
## Selects with deleting some rows
(subset <- select(chicago, -(city:dptp)))

## Reorders a df by the given variable (date below)
head(subset <- arrange(chicago, date))
head(subset <- arrange(chicago, desc(date)))

qq <- quantile(chicago$pm25tmean2, seq(0, 1, 0.2), na.rm = TRUE)
qq
