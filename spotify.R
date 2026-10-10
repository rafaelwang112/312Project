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
  mutate(album_release_year = as.numeric(str_sub(album_release_date, -2))) |>
  mutate(album_release_year = ifelse(album_release_year <= 26,
                                     album_release_year + 2000,
                                     album_release_year + 1900)) |>
  mutate(album_release_year = ifelse(album_release_date == "0",
                                     NA, album_release_year)) |>
  mutate(album_release_year = ifelse(str_sub(album_release_date, 5, 5) == "-",
                                     as.numeric(str_sub(album_release_date, 1, 4)),
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

# This table adds an extra criteria of popularity >=62, the 3rd Quartile, to help
# guide the question of whether popular songs remain stable over time.
trends_popular_table <- tracks_simplified |>
  filter (!is.na(album_release_year) & popularity>=62) |>
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
# In the most recent ~5 years, it appears that average danceability has increased, while 
# average energy has remained stable.

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

# Contributed by Cherim Kim
# added duration_min; adjusted unit from ms to minute
tracks_simplified <- tracks_simplified |>
  mutate(duration_min = duration_ms / 60000)

# note: feat_flag uses str_detect(name, "feat), which could also match words like "defeat". This is likely only a few songs, so not adjustment will be necessary.

# Contributed by Cherim Kim
# Q2 Decoding Success
# predictors: artist_followers, tempo, title_length, feat_flag, genres
# used log(streams) and long(artist_followers) because both are extremely right skwed, and will disrupt regression
# rows with 0 are removed (log of 0 is undefined), and rows with missing predictors are removed so every model uses the same rows.
model_data <- tracks_simplified |>
  filter(streams > 0, artist_followers > 0,
         !is.na(tempo), !is.na(title_length), !is.na(feat_flag), !is.na(genres)) |>
  mutate(log_streams   = log10(streams),
         log_followers = log10(artist_followers))

# Artist followers vs streams (expected to be the strongest predictor)
ggplot(model_data, aes(x = log_followers, y = log_streams)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Artists with more followers get more streams",
       x = "log10(Artist followers)", y = "log10(Streams)")

# Tempo vs streams
ggplot(model_data, aes(x = tempo, y = log_streams)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Tempo vs streams",
       x = "Tempo (BPM)", y = "log10(Streams)")

# Title length vs streams
ggplot(model_data, aes(x = title_length, y = log_streams)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Title length vs streams",
       x = "Number of characters in title", y = "log10(Streams)")

# Collaboration (feat) vs streams
ggplot(model_data, aes(x = feat_flag, y = log_streams, fill = feat_flag)) +
  geom_boxplot(show.legend = FALSE) +
  scale_x_discrete(labels = c("FALSE" = "Solo", "TRUE" = "Featuring")) +
  labs(title = "Do collaborations get more streams?",
       x = NULL, y = "log10(Streams)")

# Genre vs streams (visualizes A's success_genre_prediction table)
ggplot(model_data,
       aes(x = fct_reorder(genres, log_streams, .fun = median),
           y = log_streams)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  coord_flip() +
  labs(title = "Streams by genre (ordered by median)",
       x = NULL, y = "log10(Streams)")

# Contributed by Cherim Kim
# regression models
# used model_data to predict streaming number, and adds variable as it moves on
# model 1: predict streams using artist followers only
m1 <- lm(log_streams ~ log_followers, data = model_data)
# model 2: add temp
m2 <- lm(log_streams ~ log_followers + tempo, data = model_data)
# model 3: add title length and collaboration (feat)
m3 <- lm(log_streams ~ log_followers + tempo + title_length + feat_flag, data = model_data)
# model 4: add genre (final model)
m4 <- lm(log_streams ~ log_followers + tempo + title_length + feat_flag + genres,
         data = model_data)

# Contributed by Cherim Kim
# shows if adding variables makes prediction better (adj_r_squared (higher=better), rmse(lower=better)
get_regression_summaries(m1)
get_regression_summaries(m2)
get_regression_summaries(m3)
get_regression_summaries(m4)

# effect of each variable in the final model
get_regression_table(m4)

# Contributed by Cherim Kim
# get each track's predicted value (log_streams_hat) and residual from the final model
#  If the dots are spread evenly above and below the red line with no clear pattern, our model is a good fit.
m4_points <- get_regression_points(m4)

ggplot(m4_points, aes(x = log_streams_hat, y = residual)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs fitted (final model)",
       x = "Fitted log10(Streams)", y = "Residual")

# Contributed by Cherim Kim
# Put songs into 4 groups, from artists with the fewest followers (1) to the most (4).
# For each group, show the typical number of streams for solo songs and "feat" songs.
# This helps us see if more followers and collaborations mean more streams
model_data |>
  mutate(follower_group = ntile(artist_followers, 4)) |>
  group_by(follower_group, feat_flag) |>
  summarize(median_streams = median(streams), count = n())

# Contributed by Cherim Kim
# Q3 Predicting industry trends
# Combine A's two tables (all songs vs popular songs). Only keep years with at least 20 songs, since early years have too few songs for reliable averages.
trends_long <- bind_rows(
  trends_table         |> mutate(group = "All songs"),
  trends_popular_table |> mutate(group = "Popular (popularity >= 62)")
) |>
  filter(count >= 20) |>
  pivot_longer(c(average_duration, average_danceability, average_energy,
                 average_acousticness, average_liveness, average_valence),
               names_to = "feature", values_to = "value")

# Contributed by Cherim Kim
# Song characteristics over time, all songs vs popular songs
ggplot(trends_long, aes(x = album_release_year, y = value, color = group)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ feature, scales = "free_y") +
  labs(title = "Song characteristics over time",
       x = "Album release year", y = "Average value", color = NULL) +
  theme(legend.position = "bottom")

# Contributed by Cherim Kim
# Genre share by year (change 2000 if your group wants a different start year)
ggplot(tracks_simplified |> filter(album_release_year >= 2000),
       aes(x = album_release_year, fill = genres)) +
  geom_bar(position = "fill") +
  labs(title = "Genre share by release year",
       x = "Album release year", y = "Share of tracks", fill = "Genre")


