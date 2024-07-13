pacman::p_load(tidyverse, tidymodels,readr, skimr,discrim, ggthemes, ggsci, vip, caret, knitr)

spotify_songs_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
spotify_songs <- spotify_songs_raw
skim_without_charts(spotify_songs)

## (Data Cleaning)

total_genre <- spotify_songs %>%
  distinct(track_id,playlist_genre)%>%
  count(track_id, playlist_genre) %>%
  pivot_wider(names_from = playlist_genre, values_from = n, values_fill = 0)%>%
  mutate(total_genre = rock + pop +`r&b`+ latin + edm+rap)%>%
  ggplot(aes(x=total_genre, fill = factor(total_genre)))+ 
  geom_bar(color = 'black')+
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5)+
  labs(x = "Number of genres per song",
       y = "Number of songs")+
  theme_clean()+
  scale_fill_npg(name = "Number of Genres")

#ggsave("figures/Distinct_genres_per_song.png", total_genre)

spotify_songs %>%
  distinct(track_id,playlist_genre)%>%
  count(track_id, playlist_genre) %>%
  pivot_wider(names_from = playlist_genre, values_from = n, values_fill = 0)%>%
  mutate(total_genre = rock + pop +`r&b`+ latin + edm+rap) %>%
  filter(total_genre ==5) %>%
  inner_join(spotify_songs,by="track_id")%>%
  dplyr::select(track_name, track_artist, playlist_name, playlist_genre,playlist_subgenre) %>%
  head() %>%
  kable (caption = "Example of one track with multiple genre")

# Select songs that is only classified as a single genre
spotify_songs_single_genre <-spotify_songs %>%
  distinct(track_id,playlist_genre)%>%
  count(track_id, playlist_genre) %>%
  pivot_wider(names_from = playlist_genre, values_from = n, values_fill = 0)%>%
  mutate(total_genre = rock + pop +`r&b`+ latin + edm+rap) %>%
  filter(total_genre ==1)%>%
  dplyr::select(track_id)

# Clean the data
spotify_songs_cleaned <- spotify_songs %>%
  distinct(track_id, track_popularity,track_album_release_date,playlist_genre,
           danceability,energy,key,loudness,mode,speechiness,acousticness,
           instrumentalness,liveness,valence,tempo,duration_ms)%>%
  mutate (release_year =as.integer(str_sub(track_album_release_date,1,4)),
          playlist_genre = factor(playlist_genre),
          key = factor(key),
          mode = factor(mode))%>%
  dplyr::select(-track_album_release_date)

# Combine to final data
spotify_songs_cleaned <- spotify_songs_cleaned %>%
  inner_join(spotify_songs_single_genre)

# Reduce to 1000 samples per genre
set.seed(1874837)
spotify_songs <- spotify_songs_cleaned %>%
  group_by(playlist_genre) %>%
  slice_sample(n = 1000)%>%
  ungroup()


## (Exploratory Data Analysis)

# Year the song was released

year_by_genre <- spotify_songs %>% 
  ggplot(aes(x = fct_reorder(playlist_genre, release_year), y = release_year, fill = playlist_genre)) + 
  geom_boxplot(alpha = 0.85)+
  labs (x = "Genre",
        y = "Release Year")+
  theme_clean()+
  scale_fill_npg(name = "Genre")

#ggsave("figures/release_year_by_genre.png", year_by_genre)


genre_by_year <- spotify_songs %>%
  filter(release_year > 1970)%>%
  ggplot(aes (x = release_year, fill = playlist_genre)) + 
  geom_bar(position = 'fill',col = "black", alpha = 0.85)+
  labs (x = "Release Year",
        y = "Proportion of songs")+
  theme_clean() +
  scale_fill_npg(name = "Genre")

#ggsave("figures/genre_by_year.png", genre_by_year)

# Speechiness*
  
speechiness_by_genre <- spotify_songs %>%
  ggplot(aes(x = fct_reorder(playlist_genre,speechiness), y = speechiness, fill = playlist_genre))+ 
  geom_boxplot(alpha = 0.85)+
  labs(x = "Genre",
       y = "Speechiness")+
  theme_clean() +
  scale_fill_npg(name = "Genre")

#ggsave("figures/speechiness_by_genre.png", speechiness_by_genre)

# Danceability
  
dancibility_by_genre <- spotify_songs %>%
  ggplot(aes(x = fct_reorder(playlist_genre,danceability), y = danceability, fill = playlist_genre))+ 
  geom_boxplot(alpha = 0.85)+
  labs(x = "Genre",
       y = "Danceability")+
  theme_clean() +
  scale_fill_npg(name = "Genre")

#ggsave("figures/dancibility_by_genre.png", dancibility_by_genre)

# Tempo
  
tempo_by_genre <- spotify_songs %>%
  ggplot(aes(x = fct_reorder(playlist_genre,tempo), y = tempo, fill = playlist_genre))+ 
  geom_boxplot(alpha = 0.85)+
  labs (x = "Genre",
        y = "Tempo (beats per minute)")+
  theme_clean() +
  scale_fill_npg(name = "Genre")

#ggsave("figures/tempo_by_genre.png", tempo_by_genre)

## (Specific Analysis)

# Does the popularity of songs differ between genres?
  
track_popularity_by_genre <- spotify_songs %>%
  ggplot(aes(x = fct_reorder(playlist_genre,track_popularity), y = track_popularity, fill = playlist_genre)) + geom_boxplot(alpha = 0.85)+
  labs(x = "Genre",
       y = "Popularity score")+
  theme_clean()+
  scale_fill_npg(name = "Genre")

#ggsave("figures/track_popularity_by_genre.png", track_popularity_by_genre)

songs_by_genre_popularity <- spotify_songs %>%
  mutate (popularity_rank = factor(case_when(track_popularity >75 ~ 'high: > 75',
                                             track_popularity %in% 25:75 ~ 'medium: 25-75',
                                             track_popularity < 25 ~ 'low: < 25'))) %>%
  ggplot(aes(x= playlist_genre, fill = popularity_rank)) + geom_bar(color = "black", position = "dodge", width = 0.7)+
  labs (x = "Genre",
        y = "Number of songs")+
  theme_clean()+
  scale_fill_npg(name = "Popularity")

#ggsave("figures/songs_by_genre_popularity.png", songs_by_genre_popularity)

# Is there difference in speechiness for each genre
  
speechiness_difference <- spotify_songs %>%
  mutate(year =release_year,
         playlist_genre = ifelse(playlist_genre=='rap', 'rap','other')) %>%
  filter(year >=1970)%>%
  group_by(year,playlist_genre) %>%
  summarise(mean = mean(speechiness))%>%
  ungroup() %>%
  ggplot(aes(x =year, y = mean, color = playlist_genre))+
  geom_line(size = 0.8, alpha = 0.3)+ 
  geom_smooth(size = 1.5,se=FALSE)+
  labs(x = "Release Year",
       y = "Speechiness")+
  theme_clean()+
  scale_color_npg(name = "Genre")

#ggsave("figures/speechiness_difference.png", speechiness_difference)

#How does track popularity change over time?*
  
avg_popularity_trending <- spotify_songs %>%
  mutate(year =release_year) %>%
  filter(year >=1970)%>%
  group_by(year) %>%
  summarise(mean = mean(track_popularity))%>%
  ungroup() %>%
  ggplot(aes(x =year, y = mean))+
  geom_line(size = 0.8, alpha = 0.3, color = pal_npg()(9)[6])+ 
  geom_smooth(size = 1.5,se=FALSE, color = pal_npg()(9)[6])+
  geom_label(color = pal_npg()(9)[9],alpha = 0.9,size = 3,aes(label = ifelse(year %% 10==0,round(mean,1),NA)))+
  labs(x = "Release Year",
       y = "Average Popularity")+
  theme_clean()+
  scale_color_npg()

#ggsave("figures/average_popularity_trending.png", avg_popularity_trending)

avg_popularity_trending_by_genre <- spotify_songs %>%
  mutate(year = release_year) %>%
  filter(year >=1970) %>%
  group_by(year, playlist_genre) %>%
  summarise(mean = mean(track_popularity))%>%
  ungroup() %>%
  ggplot(aes(x =year, y = mean, color = playlist_genre))+
  geom_line(size = 0.5, alpha = 0.2, color = "black")+ 
  geom_smooth(se=FALSE)+
  labs(x = "Release Year",
       y = "Average Popularity")+
  facet_wrap(~playlist_genre)+
  theme_clean()+
  theme(panel.spacing = unit(1.5, "lines"),
        axis.text.x = element_text(size = 6))+
  scale_color_npg(name = "Genre")

#ggsave("figures/average_popularity_trending_by_genre.png", avg_popularity_trending_by_genre)

## (Data Splitting)

set.seed(1874837)
spotify_split <- initial_split(spotify_songs, strata = playlist_genre)
spotify_train <- training (spotify_split)
spotify_test <- testing (spotify_split)
```

## (Data Preprocessing)

spotify_recipe <- recipe (playlist_genre ~., data = spotify_train) %>%
  step_rm(all_string_predictors(), key,liveness) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_predictors()) %>%
  step_corr (all_predictors()) %>%
  prep()

spotify_train_preproc <- juice(spotify_recipe)


## (Model Specifications)

#LDA spec
lda_spec <- discrim_linear(mode ="classification") %>%
  set_engine("MASS")

#K-nearest neighbors
knn_spec <- nearest_neighbor(mode = "classification", 
                             neighbors = tune()) %>%
  set_engine("kknn")

#Random Forest
rf_spec <- rand_forest(mode = "classification",
                       trees = 100,
                       mtry = tune(),
                       min_n = tune()) %>%
  set_engine("ranger", importance ="permutation")


## (Model Tuning)

# Create 10 bootstraps resamples
set.seed(1874837)
spotify_boots <- bootstraps(spotify_train_preproc, times = 10, strata = playlist_genre)

#For k-nearest neighbors
k_grid <- grid_regular(neighbors (range = c(1,100)),
                       levels = 20)

doParallel::registerDoParallel()
knn_tune <- tune_grid(object = knn_spec,
                      preprocessor = recipe(playlist_genre ~ ., data = spotify_train_preproc),
                      resamples = spotify_boots,
                      grid = k_grid)

best_knn_acc <- select_best(knn_tune, "accuracy")

knn_spec_final <- finalize_model(knn_spec,best_knn_acc)

#For random forest
rand_grid <- grid_regular(finalize (mtry(),
                                    spotify_train_preproc %>% dplyr::select(-playlist_genre)),
                          min_n(),
                          levels = 5)
doParallel::registerDoParallel()
set.seed(1874837)
rand_tune <- tune_grid (object = rf_spec,
                        preprocessor = recipe(playlist_genre~., data = spotify_train_preproc),
                        resamples = spotify_boots,
                        grid = rand_grid)

best_rand_acc <- select_best(rand_tune, "accuracy")

rf_spec_final <- finalize_model(rf_spec, best_rand_acc)

## (Model Selection)

# Cross validation
set.seed(1874837)
spotify_cv <-vfold_cv(spotify_train_preproc, v =10, strata = playlist_genre)

# Fit
lda_cv <- fit_resamples(object = lda_spec,
                        preprocessor = recipe (playlist_genre~., data = spotify_train_preproc),
                        resamples = spotify_cv)

knn_cv <- fit_resamples (object = knn_spec_final,
                         preprocessor = recipe (playlist_genre~.,data = spotify_train_preproc),
                         resamples = spotify_cv)

set.seed(1874837)
rf_cv <- fit_resamples (object = rf_spec_final,
                        preprocessor = recipe (playlist_genre~., data = spotify_train_preproc),
                        resamples = spotify_cv)


pct_song_by_genre <- spotify_songs_cleaned %>%
  count(playlist_genre) %>% 
  mutate (prop = round(n/sum(n)*100,2))%>%
  ggplot(aes(x = playlist_genre, y = prop, fill = playlist_genre))+
  geom_col(col = "black",alpha = 0.85)+
  geom_hline(yintercept = 16.67, linetype = "longdash", size = 1, alpha = 0.3, color = "purple")+
  geom_text(aes(label = prop), vjust = -0.3, size = 4)+
  labs(x="Genre",
       y = "Percentage (%)")+
  theme_clean()+
  scale_fill_npg(name = "Genre")

#ggsave("figures/percentage_of_song_by_genre.png",pct_song_by_genre, width = 5)

# Comparing model
lda_cv %>% collect_metrics() %>% bind_cols (model = factor("lda")) %>%
  bind_rows(knn_cv %>% collect_metrics() %>% bind_cols (model = factor("knn")))%>%
  bind_rows(rf_cv %>% collect_metrics() %>% bind_cols (model = factor("ranndom forest")))%>%
  dplyr::select(-.config, -n, -.estimator)%>%
  filter (.metric =="accuracy") 

model_performance <- lda_cv %>% collect_metrics() %>% bind_cols (model = factor("lda")) %>%
  bind_rows(knn_cv %>% collect_metrics() %>% bind_cols (model = factor("knn")))%>%
  bind_rows(rf_cv %>% collect_metrics() %>% bind_cols (model = factor("ranndom forest")))%>%
  dplyr::select(-.config, -n, -.estimator)%>%
  filter (.metric =="accuracy") %>%
  ggplot(aes(x = model, y = mean, fill = model))+
  geom_col(width = 0.5, col = "black")+
  geom_errorbar(aes(ymin = mean-std_err, ymax = mean+std_err), width = 0.1)+
  coord_cartesian(ylim = c(0.4,0.6))+
  labs(x="Model",
       y = "Accuracy")+
  theme_clean()+
  scale_fill_npg()

#ggsave("figures/model_performance.png", model_performance, width = 5)


## (Model Evaluation)

# Preprocess test data
spotify_test_preproc <- bake(spotify_recipe, spotify_test)

# Fit random forest model to training data
spotify_rf <- rf_spec_final %>%
  fit(playlist_genre ~., data = spotify_train_preproc)

# Variable importance plot

variable_importance <- spotify_rf %>% vip()
#ggsave("figures/variable_importance.png", variable_importance, width = 5)

# Use model to predict test data
spotify_preds <- predict(spotify_rf,
                         new_data = spotify_test_preproc) %>%
  bind_cols (spotify_test_preproc %>%
               dplyr::select(playlist_genre))

# Sens and Specs table
conf_mat <- confusionMatrix(data= spotify_preds$.pred_class,reference = spotify_preds$playlist_genre)
conf_mat$byClass %>% as_tibble()%>%
  bind_cols(conf_mat$byClass%>% rownames()) %>%
  dplyr::select(...12,Sensitivity,Specificity) %>%
  rename(Genre = ...12) %>%
  mutate (Genre = str_sub(Genre,8)) %>%
  kable(caption = "Sensitivity and Specificity by Genre")

sens_specs <- conf_mat$byClass %>% as_tibble()%>%
  bind_cols(conf_mat$byClass%>% rownames()) %>%
  dplyr::select(...12,Sensitivity,Specificity) %>%
  rename(Genre = ...12) %>%
  mutate (Genre = str_sub(Genre,8)) %>%
  gather (key = "Metric", value ="Value", Sensitivity:Specificity) %>%
  ggplot (aes (x =Genre, y = Value, fill = Genre))+
  geom_col(col = "black", alpha = 0.85, aes(linetype = Metric))+
  facet_wrap(~Metric) +
  geom_text(aes(label = round(Value,2)), vjust = -0.5)+
  theme_clean()+
  scale_fill_npg()

#ggsave("figures/sensitivity_specificity_by_genre.png", sens_specs, width = 7)

# plot confusion matrix
confusion_matrix <- spotify_preds %>%
  conf_mat(truth = playlist_genre, estimate = .pred_class) %>%
  autoplot(type = "heatmap") + theme_clean()

#ggsave("figures/confusion_matrix.png", confusion_matrix, width = 7)
