####################
# IMDB data cleaning
#Sources: 
#https://datasets.imdbws.com/
####################


library(stringr)
library(tidyverse)
library(data.table)

# Import and clean data
names_data <- as.data.frame(fread("name.basics.tsv"))
names_data <- names_data %>%
  filter(str_detect(primaryProfession, "actor|actress")) %>%
  select(c("nconst","primaryName"))

title_data <- as.data.frame(fread("title.akas.tsv"))
title_data <- title_data %>%
  filter(isOriginalTitle=="1") %>%
  select(c("titleId", "title"))

movie_data <- as.data.frame(fread("title.basics.tsv"))
movie_data <- movie_data %>%
  select(c("genres", "runtimeMinutes", "tconst","startYear", "titleType")) %>%
  filter(titleType=="movie" | titleType=="tvSeries") %>%
  separate(genres, c("genre1","genre2","genre3"), sep=",", remove=TRUE)

actor_data <- as.data.frame(fread("title.principals.tsv"))
actor_data <- actor_data %>%
  filter(category == "self" | category == "actor" | category == "actress") %>%
  select(c("tconst","nconst"))

ratings_data <- as.data.frame(fread("title.ratings.tsv"))

profit_data <- read_csv("boxoffice.csv")

oscars_data <- read_csv("oscars.csv")
oscars_data <- oscars_data %>%
  filter(category=="BEST PICTURE") %>%
  rename("oscar_year"=year, "oscar_category"=category, "oscar_win"=winner, "title"=entity) %>%
  mutate(oscar_nom = oscar_win==FALSE)


# Merge data
title_rating <- merge(title_data, ratings_data, by.x="titleId",by.y="tconst")
actor_names <- merge(actor_data, names_data, by.x="nconst", by.y="nconst")
actor_title <- merge(title_rating, actor_names, by.x="titleId",by.y="tconst")
actor_movie <- merge(actor_title, movie_data, by.x="titleId",by.y="tconst")
movie_profit <- merge(actor_movie, profit_data, by.x="title",by.y="title")
merged_data <- merge(movie_profit, oscars_data, by.x="title", by.y="title", all=TRUE)

merged_data <- merged_data %>%
  select(-c("nconst", "titleId", "startYear", "oscar_category")) %>%
  group_by(title) %>%
  mutate(actor=row_number()) %>%
  pivot_wider(values_from=primaryName, names_from=actor, names_prefix="actor") %>%
  select(-c("actor11":"actor80"))


merged_data$runtimeMinutes <- as.numeric(merged_data$runtimeMinutes)
merged_data$oscar_nom <- "No"
merged_data$oscar_nom[!is.na(merged_data$oscar_win)] <- "Yes"

merged_data$oscar_win[is.na(merged_data$oscar_win)] <- "No"
merged_data$oscar_win[merged_data$oscar_win==FALSE] <- "No"
merged_data$oscar_win[merged_data$oscar_win==TRUE] <- "Yes"

#Move actors over to get rid of missing values
i1 <- startsWith(names(merged_data), "actor")
merged_data[i1] <-  t(apply(merged_data[i1], 1, function(x) c(x[!is.na(x)], x[is.na(x)])))


movies <- merged_data[merged_data$titleType=="movie",]
tvshows <- merged_data[merged_data$titleType=="tvSeries",]

write.csv(merged_data, "merged_data.csv")
write.csv(movies, "movies.csv")
write.csv(tvshows, "tvshows.csv")




