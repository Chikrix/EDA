# The steps in EDA are
# - Fornulate the Question
# - Read your data
# - Check the packaging
# - Run str
# - Look at the top and bottom of your data
# - Check your n's
# - Validate with at least one external data source
# - Try the easy solution first
# - Challenge your solution
# - Follow - up


## Its generally a good idea spending a few mins trying to figure out what question we're interested in, and narrow
## it as specific as possible. 
library(downloader)
library(tidyverse)

## ============= Formulate your question ===============================
# The question we want to ask is, which counties in the US has the highest levels of ambient ozone pollution

## ================ Read in the Data ===================================
url <- "https://github.com/rdpeng/exdata/raw/master/other_data/hourly_44201_2014.csv.bz2"
epa2014 <- paste0("data/", basename(url))
download(url, destfile = epa2014, quiet = FALSE)
## The downloading above isn't really necessary as I can pass in the url string like that and it'll download and decompress
## if necessary, doing this just because I want to :), haha

epa2014 <- read_csv(epa2014, col_types = "ccccinnccccccncnncccccc")
names(epa2014)

## Remove spaces from the column names
names(epa2014) <- make.names(names(epa2014))

## ================= Check the packaging ===============================
# This is about checking the properties of the data before starting to explore it. Things like
dim(epa2014) # 7 million + rows with 23 cols
# We can check the actual file in a text editor to see if we have the right data imported

## ================= Run str() =========================================
str(epa2014)
# str gives us some information and summarisations about the properties of our data

## ================= Look at the top and bottom of your data ===========
head(epa2014)
head(select(epa2014, Sample.Measurement:Date.of.Last.Change))
tail(epa2014)
tail(select(epa2014, Sample.Measurement:Date.of.Last.Change))
# This helps us confirm that the data is properly formatted

## =============== Check your n's =====================================
# This is about checking data coherence with what we expect the data would be
table(epa2014$Time.Local)

## This row is inconsistent with the other rows
epa2014 %>%
  filter(Time.Local == "13:14") %>%
  select(State.Name, County.Name, Date.Local, Date.Local, Time.Local, Sample.Measurement)

## From above, we've seen that some camaras in Franklin county had issues on that day.
## Looking at data from that day
epa2014 %>% 
  filter(State.Code == "36" ,
         County.Code == "033" ,
         Date.Local == "2014-09-30") %>%
  select(Date.Local, Time.Local, Sample.Measurement) %>%
  as.data.frame()

# From the above, we see that this monitor records all its values at odd times. We can ask follow up questions from this

# represented states
epa2014 %>%
  select(State.Name) %>%
  unique %>%
  nrow

##======================== Validate with at least one external data source =====================================
# Ensuring our data matches something from outside can help us confirm that our data is roughly in line with what it should be. 
# This can be as simple as matching a single number against something from ours. 
# “In the U.S. we have national ambient air quality standards, and for ozone, the current standard set in 2008 is that 
# the “annual fourth-highest daily maximum 8-hr concentration, averaged over 3 years” should not exceed 0.075 parts per million (ppm).”
summary(epa2014$Sample.Measurement)
quantile(epa2014$Sample.Measurement, seq(0, 1, 0.1))

# Given the national standard is 0.075, we see from our output, we see that we match in the following ways;
# - our output is in the same order of magnitude (in terms of units, 3 decimal values)
# - The range of the distribution is roughly within what we'll expect

##================== Try the easy solution first =============================
epa2014 %>%
  group_by(State.Name, County.Name, County.Code) %>%
  summarize(ozone = mean(Sample.Measurement),
            n = n()) %>%
  arrange(desc(ozone)) -> ranking

head(ranking, 10)
tail(ranking, 10)

# Note the n column, its good to keep the number in mind, if it makes sense, remember that our dataset is hourly measurement over a year
# for each county, so we should expect at least 8760 n count values for each county to say data was properly taken
# We can look at monthly measurement for the county with the highest ozone level from above
epa2014 %>%
  mutate(Date.Local = as.Date(Date.Local)) %>%
  filter(County.Name == "Mariposa" & State.Name == "California") %>%
  mutate(months = factor(months(Date.Local), levels = month.name)) %>%
  group_by(months) %>%
  summarize(ozone = mean(Sample.Measurement)) -> california_mariposa_ozone

# From the above, I see that only 10 months are covered, of which we notice that the summer months have a higher level compared 
# to the winter months and lower in the winter months.We can do the same for the lowest level county and so on and keep probing

## ==================== Challenge your solution ==================================
# “You should always be thinking of ways to challenge the results, especially if those results comport with your prior expectation.” 
# From our solution to our question above, somethings raised eyebrows, and given those possible issues like this data was only for 
# the year 2014, the missing months, insufficiently sampled months and counties, etc, how would those affect the outcome of the rankings
# if we had that data. Remember that the solution we got are random values. To help us give some credibility to our data, we can resample 
# from our data to simulate taking new samples of all data from the population, and see how much it matches with the result we got above. 
set.seed(1001)
N <- nrow(epa2014)
indices <- sample(N, N, replace = TRUE)
epa2014_2 <- epa2014[indices, ]

# now I've resampled from my data, I'll perform the calculation I did to answer my question
epa2014_2 %>%
  group_by(State.Name, County.Name, County.Code) %>%
  summarize(ozone = mean(Sample.Measurement),
            n = n()) %>%
  arrange(desc(ozone)) -> ranking2

head(ranking2, 10)
head(ranking, 10)
# We'll notice how much it matches with the output from our solution, also notice how close the ozone values are. 
tail(ranking2, 10)
tail(ranking, 10)
# same thing here. This at least adds some credibility to my solution

## ================== Follow up questions ========================
# We might have follow up questions, and we can use this iterative steps to answer these questions and probe out data more. 
# Some follow up questions includes are questions like;
# - do I need more data?
# - do I have the right data?
# - am I asking the right question
# Remember that the goal of EDA is to get us thinking about our data and reasoning about our question, and do what is necessary
# to get the truth for our questions, like getting the necessary data and so on.
