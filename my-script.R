## my script from data carpentry UC Merced
## Julie Lowndes lowndes@nceas.ucsb.edu
## Aug 17-18, 2017...

## download data into my R project ----
# download.file("https://ndownloader.figshare.com/files/2292169",     
#               "data/portal_data_joined.csv")

## read in data into R ----
surveys <- read.csv("data/portal_data_joined.csv")

## explore our data (content)
head(surveys) # show first 6 rows of data
tail(surveys, 12) # show last 12 rows of data

## summaries
str(surveys) # structure of the data
summary(surveys)

## explore size  
dim(surveys)
nrow(surveys)
ncol(surveys)

names(surveys)

## the $ operator for isolating columns
tail(surveys$weight)
str(surveys$weight)
summary(surveys$weight)

## scatterplot: plot(x, y)
plot(surveys$year, surveys$weight)
plot(surveys$hindfoot_length, surveys$weight)

## explore month with histograms
summary(surveys$month)
hist(surveys$month, col='grey', breaks = 12)

## explore factor variable
summary(surveys$taxa)
levels(surveys$taxa)
nlevels(surveys$taxa)
hist(surveys$taxa) ## error

class(surveys$taxa) # it's a factor!
table(surveys$taxa) # turns it into a table
class(table(surveys$taxa)) # now it's a table
barplot(table(surveys$taxa))

## subset in base R ----
## [rows, columns]

## return all the columns for genus Ammodramus
surveys[surveys$genus == 'Ammodramus', ]

## return a few columns
surveys[surveys$genus == 'Ammodramus', 
        c('record_id', 'month', 'weight') ]

## how many observations (rows) are there in January (1) and February (2)?

## one way to do it; the `|` operator says 'or'
nrow(surveys[surveys$month == 1 | surveys$month == 2, ])

## exploring why these didn't work
# nrow(surveys[surveys$month == 1 | 2, ]) # not correct! 
# nrow(surveys[surveys$month == (1 | 2), ]) # this didn't work either

table(surveys$month == 1 | surveys$month == 2)

## what's the length (i.e. how many rows) are month less than 3
length(which(surveys$month < 3))

# Sabah (here you can look in the console and add the 'TRUE' cases together
x <- summary(surveys$month == 1)
(summary(surveys$month == 2))

summary(as.factor(surveys$month))

