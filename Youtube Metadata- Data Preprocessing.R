
#----------------------------------------------------------------------------------#


# DATA PRE-PROCESSING
          ### Transforming our variables
                  # 1- Frequency 
                  # 2- Dislike Percentage
                  # 3- Trending Days
                  # 4- Views Ratio

# The objective is to form a table of unique values based on video_id and append 4 
       # columns with variables for the above attributes [1-4]


### Forming the table with unique values based on video_id such each video will have 
     # just one row with the latest trending date
     # taking in account maximum likes, dislikes and comments
setwd("C:/Users/Mohit/Desktop/ba with r/project")
library(dplyr)

# Reading the dataset
df <- read.csv('CAvideos.csv')
# Sorting as per 'video_id'
dfsort <- arrange(df,video_id)


# Assigning categories to the videos



#library(plyr)
#library(RJSONIO)
#json_file <- json_file <- fromJSON('CA_category_id.json')
#con <- file('CA_category_id.json', "r")
#df  <- ldply(fromJSON(con), data.frame)
#close(con)



dfsort$category_id <- ordered(dfsort$category_id,
levels = c(1, 2, 10, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 30, 31, 32, 33, 34,
           35, 36, 37, 39, 40, 41, 42, 43, 44, 38),
labels = c("Film & Animation", "Autos & Vehicles", "Music", "Pets & Animals", "Sports", "Short Movies", 
            "Travel & Events", "Gaming", "Videoblogging", "People & Blogs", "Comedy", "Entertainment", 
            "News & Politics", "Howto & Style",  "Education", "Science & Technology", "Movies", 
           "Anime/Animation", "Action/Adventure", "Classics", "Comedy", "Documentary", "Drama", 
          "Family","Horror", "Sci-Fi/Fantasy", "Thriller", "Shorts", "Shows", "Trailers", "Foreign"))
View(dfsort)


 #changing the column name of category id to category
colnames(dfsort)[5] <- "category"



# Extracting unique values from sorted dataset
dfnew <- dfsort[order(dfsort$video_id, -abs(dfsort$likes), -abs(dfsort$dislikes), -abs(dfsort$comment_count)), ] 
videoUniq <-  dfnew[ !duplicated(dfnew$video_id), ] 

#----------------------------------------------------------------------------------#

### 1- Frequency of a video [No. of times it trended]

freq.df = count(df, df$video_id)
videoUniq['frequency'] <- freq.df$n 

#----------------------------------------------------------------------------------#

### 2- Dislike Percentage for each video

videoUniq['dislike_percent'] <-  (videoUniq$dislikes) / (videoUniq$likes + videoUniq$dislikes)
 
#----------------------------------------------------------------------------------#

### 3- Days to Trending for each video

# Separating 'trending_date' and 'publish_time' into 2 different columns
y.df <- videoUniq[2]
x.df <- videoUniq[6]

View(y.df)
View(x.df)

# Extracting the date from column 'publish_time' 
#x.df %>%
x.df <- mutate(x.df, publish_time = as.Date(substr(publish_time, 1, 10)))

# Converting the date-format of 'trending_date' from yyddmm to yymmdd
#y.df %>%
y.df <-  mutate(y.df, trending_date= as.Date(trending_date, format = "%y.%d.%m"))

View(y.df)
View(x.df)


# Combining the new dates into one single dataframe
z.df <- videoUniq[1]
z.df['date_of_publishing'] <- x.df$publish_time
z.df['date_of_trending'] <- y.df$trending_date

View(z.df)

# Finding the number of days between the 2 dates
z.df <- mutate(z.df, No_of_days= as.numeric(date_of_trending-date_of_publishing, units = "days") )

View(z.df)

# So, 'z.df' is the dataframe with the columns that can be added to our original dataset

 
videoUniq ['date_of_publishing'] <- z.df$date_of_publishing
videoUniq ['date_of_trending'] <- z.df$date_of_trending
videoUniq ['days_to_trend'] <- z.df$No_of_days

#drpping the original trending date column and publish time

videoUniq <- videoUniq[,-c(2)]
videoUniq <- videoUniq[,-c(5)]
View(videoUniq)


library(caret)
library(tidyverse)
library(forecast)
library(leaps)
library(dplyr)
# Selecting variables for regression
videoRegVar.df <- videoUniq[, c(6,7,8,9,15,16,19)]

# Data partition
set.seed(123)
training.index <- createDataPartition(videoRegVar.df$days_to_trend, p = 0.9, list = FALSE)
videoReg.train.df <- videoRegVar.df[training.index, ]
videoReg.valid.df <- videoRegVar.df[-training.index, ]

### Run regression
video.lm <- lm(days_to_trend ~ ., data = videoReg.train.df)

options(scipen = 999)
summary(video.lm)


###Linear Discriminant Analysis

library(MASS)
library(dplyr)
library(ggplot2)


## Fit the model
video1<- videoRegVar.df[order(videoRegVar.df$days_to_trend),]
lda_1 <- lda(days_to_trend~.,data=video1)

##Compute LDA
lda_1

## Make Predictions
predict_1 <- predict(lda_1,data=video1)

## Model Accuracy
mean(predict_1$class==video1$days_to_trend)

## LDA plot using ggplot 2
lda_1 <- cbind(video1[1:24229, ], predict_1$x)

ggplot(lda_1, aes(LD1, LD2)) +
  geom_point(aes(color = days_to_trend))

# videos having less than 8 days_to_trend
fil <- filter(video1, days_to_trend < 8)

ggplot(aes(x=frequency,y=days_to_trend), data = fil)+
  geom_boxplot()


###VISUALIZATIONS
##Most influential creators
creators.df <- table(videoUniq$channel_title)

barchart(head(sort(creators.df, decreasing = TRUE),15))


##Video Category Distribution
videoUniq$category_id <- ordered(videoUniq$category)
                            
barchart(table(videoUniq$category))


##Time Series Analysis (number of Views over year)
view.ts <- ts(videoUniq$views, start = c(2017, 1), end = c(2018, 6), freq = 12)
plot(view.ts, xlab = "Year", ylab = "Views on Youtube Videos")
##Time Series Analysis (number of likes over year)
like.ts <- ts(videoUniq$likes, start = c(2017, 1), end = c(2018, 6), freq = 12)
plot(like.ts, xlab = "Year", ylab = "Likes on Youtube Videos")
##Time Series Analysis (number of dislikes over year)
dislike.ts <- ts(videoUniq$dislikes, start = c(2017, 1), end = c(2018, 3), freq = 12)
plot(dislike.ts, xlab = "Year", ylab = "Dislikes on Youtube Videos")

## Scatter plot to find out correlation between likes and views for videos
ggplot(videoUniq) + 
  geom_point(aes(x = views, y = likes), color = "navy", alpha = .7) +
  theme_classic()


##Days to trending status
barchart(table(videoUniq$days_to_trend))

# Filled Density Plot (Distribution for dislike percentage)
ggplot(videoUniq, aes(x=dislike_percent)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 colour="black") +
  geom_density(alpha=.2, fill="#FF6666") 

## Not Instant Hits : Success by dislike percentage
ggplot(videoUniq) + 
  geom_point(aes(x = likes, y = dislikes), color = "navy", alpha = .7) +
  theme_classic()



#Data Mining and sentiment analysis

video_uniq_mine <- head(videoUniq,5000)

library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(SnowballC)
library(tm)
library(wordcloud)
library(gridExtra) #viewing multiple plots together

Ca_mining= Corpus(VectorSource(video_uniq_mine$tags))

ab <- paste(unlist(Ca_mining), collapse = '')

View(ab)

# Convert the text to lower case
Ca_mining <- tm_map(Ca_mining, content_transformer(tolower))

# Remove numbers
Ca_mining <- tm_map(Ca_mining, removeNumbers)

# Remove english common stopwords
Ca_mining <- tm_map(Ca_mining, removeWords, stopwords('english'))

# Remove punctuations
Ca_mining <- tm_map(Ca_mining, removePunctuation)

# Eliminate extra white spaces
Ca_mining <- tm_map(Ca_mining, stripWhitespace)

# Text stemming (reduces words to their root form)

Ca_mining <- tm_map(Ca_mining, stemDocument)

View(Ca_mining)

# Remove additional stopwords
Ca_mining <- tm_map(Ca_mining, removeWords, c('clintonemailcom', 'stategov', 'hrod'))

dtm <- TermDocumentMatrix(Ca_mining)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

# Generate the WordCloud
par(bg='mistyrose')
png(file="WordCloud.png",width=1000,height=700, bg="grey30")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Most words in tags", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
dev.off()

#----------------------------------------------------------------------------------#

### 4- Views Ratio for each video






#----------------------------------------------------------------------------------#












