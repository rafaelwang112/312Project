library(tidyverse)
library(moderndive)

# Contributed by Rafael Wang
# converts a csv into a tbl_df type
# ensure tracks_simplified.csv is in pwd
tracks_simplified <- read_csv("tracks_simplified.csv")

# Contributed by Rafael Wang
# creates a new variable called album_release_year and handles
# logic to handle parsing of different formats of the album_release_date
# This new variable was created because we want to analyze industry trends,
# which would require seeing differences through the years
tracks_simplified <- tracks_simplified |>
  mutate(album_release_year = as.numeric(str_sub(album_release_date,-2)))
tracks_simplified <- tracks_simplified |>
  mutate(album_release_year = ifelse(album_release_year <=26,
                                     album_release_year+2000,
                                     album_release_year+1900))
tracks_simplified <- tracks_simplified |>
  mutate(album_release_year = ifelse(album_release_date =="0",
                                     NA, album_release_year))
tracks_simplified <- tracks_simplified |>
  mutate(album_release_year = ifelse(str_sub(album_release_date, 5, 5)=="-",
                                     as.numeric(str_sub(album_release_date, 1,4)),
                                     album_release_year))
# Contributed by Rafael Wang
#Two new variables created. One is title_length, as we want to see if title length
# can predict success. Another is if a title has the word "feat" as we want to see
# if collaboration between artists correlates to greater success
tracks_simplified <- tracks_simplified |>
  mutate (title_length = nchar(name))
tracks_simplified <- tracks_simplified |>
  mutate (feat_flag = str_detect(str_to_lower(name), "feat"))

summary (tracks_simplified)
# Streams has a median of 17450 but its max is 1367372. This suggests that there 
# may be outliers that should be further investigated. Median artist_followers is  
# 2688044 but min is 223, also worth exploring. The variable we created, title_length,
# has mean of 16.31 but max of 291, another interesting variable to further explore.
# track_artists has 5183 NAs, so it is not a variable we should use. 
# Popularity's min and 1st q are both 0,
# perhaps suggesting we should look into another variable (such as streams) as our
# dependent variable as the 1st quarter being 0 means at least 25% of the tracks have 
# 0 popularity, which may make comparisons and analysis challenging. 

#Contributed by Rafael Wang
# Outlier analysis
followers_lower_bound= quantile(tracks_simplified$artist_followers, 0.25, na.rm = TRUE)-1.5*IQR(tracks_simplified$artist_followers, na.rm = TRUE)
followers_upper_bound= quantile(tracks_simplified$artist_followers, 0.75,na.rm = TRUE)+1.5*IQR(tracks_simplified$artist_followers, na.rm = TRUE)
tracks_outlier_followers <- tracks_simplified |> 
  filter (artist_followers<followers_lower_bound | artist_followers>followers_upper_bound)
nrow (tracks_outlier_followers)
# There appears to be some outliers in terms of songs whose artists followers are very 
# large. In terms of outlier treatment, they should be kept, as we should explore if
# having an abnormally large number of followers is related to predicting success.

streams_lower_bound= quantile(tracks_simplified$streams, 0.25)-1.5*IQR(tracks_simplified$streams)
streams_upper_bound= quantile(tracks_simplified$streams, 0.75)+1.5*IQR(tracks_simplified$streams)
tracks_outlier_streams <- tracks_simplified |> 
  filter (streams<streams_lower_bound | streams>streams_upper_bound)
nrow (tracks_outlier_streams)
# Outliers should be explored as popular songs are important for our analysis.
# In terms of outlier treatment, we will keep tracks that have outliers in regards
# to their streams because they are important to our analysis. We want to see if there
# are any independent variables that contribute them to being streamed so much.

length_lower_bound= quantile(tracks_simplified$title_length, 0.25, na.rm = TRUE)-1.5*IQR(tracks_simplified$title_length, na.rm = TRUE)
length_upper_bound= quantile(tracks_simplified$title_length, 0.75, na.rm = TRUE)+1.5*IQR(tracks_simplified$title_length, na.rm = TRUE)
tracks_outlier_length <- tracks_simplified |> 
  filter (title_length<length_lower_bound | title_length>length_upper_bound)
nrow (tracks_outlier_length)
# There are 277 tracks with outlier length, but in terms of outlier treatment,
# they should be kept because we want to explore if these longer names have any correlation
# at predicting success. However, it is also important to note that "name" column 
# exported from the csv file may include more than just the official song name, such 
# as things like "Official Song 2018 FIFA World Cup Russia" or "from "Deadpool 2" Motion Picture Soundtrack"


duration_lower_bound= quantile(tracks_simplified$duration_ms, 0.25)-1.5*IQR(tracks_simplified$duration_ms)
duration_upper_bound= quantile(tracks_simplified$duration_ms, 0.75)+1.5*IQR(tracks_simplified$duration_ms)
tracks_outlier_duration <- tracks_simplified |> 
  filter (duration_ms<duration_lower_bound | duration_ms>duration_upper_bound)
nrow (tracks_outlier_duration)
# Observing tracks_outlier_duration, a few of them are actually 0 duration_ms
# which suggests that it is missing data rather than the song actually being 0 ms long.
# Thus, for outlier treatment, these values should be converted to NA, while keeping those
# with very long durations would be helpful because it can potentially uncover interesting relationships.
tracks_simplified <- tracks_simplified |>
  mutate (duration_ms = ifelse(duration_ms == 0,
                               NA,
                               duration_ms))


# Contributed by Rafael Wang
# correlation table, filtering for ony first row, which is correlation between streams
# and other variables. We decided to use streams as our dependent variable over other
# variables such as popularity because popularity seems to have many values with 0.
tracks_num <- select_if(tracks_simplified, is.numeric)
cor(na.omit(tracks_num))[1,]
# There are no particularly strong variables, so our approach pivots to identifying
# variables that logically may have an interesting relationship with stream
# and develop models to come to conclusions.



# Contributed by Rafael Wang
# variables potentially useful for predicting industry trends
# grouped by year
trends_table <- tracks_simplified |>
  filter (!is.na(album_release_year)) |>
  group_by (album_release_year) |>
  summarize(
    average_duration= mean(duration_ms, na.rm=TRUE),
    average_danceability = mean(danceability),
    average_energy = mean (energy),
    average_acousticness = mean(acousticness),
    average_liveness = mean (liveness),
    average_valence = mean (valence),
    count = n()
  )
# Average danceability and energy appears to have increased over from the beginning years
# to the latest years based on the table, although it is important to note that 
# the early years in the data do not have large sample size. Thus, these 
# varaibles  should be visualized. Average valence appears to have
# remained consistent over the years, this could also be visualized.

# Contributed by Rafael Wang
# proportion table seeing proportion of genre distribution for a given year
genre_table <- table (tracks_simplified$album_release_year, tracks_simplified$genres)
genre_trends_proportion_table <- proportions (genre_table, margin = 1)
genre_trends_proportion_table
# Hip hop seems to be a rising genre, going from less than 10% in 2014 to being over
# 20% from 2016-2020. Pop seems to be popular as starting 2008, the genre makes up
# at least 20-30+% of the total. A large proportion of data appears to be categorized as other/unknown
# for several years.

#Contributed by Rafael Wang
# table of genre compared to average streams. Will be useful to seeing
# if genre can help predict success. From an initial observation,
# hip-hop has the greatest average_streams but also the greatest
# standard deviation, and Soul/R&B has the least average_streams.
success_genre_prediction <- tracks_simplified |>
  group_by(genres) |>
  summarize (
    average_streams = mean(streams),
    std = sd(streams),
    count = n()
  )
success_genre_prediction



