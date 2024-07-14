# ML Project in R: Song Genre Prediction

## Introduction

This ML project in R is a supervised classification project aimed at predicting the genre of a song based on various attributes, including:

- Track release year
- Track popularity
- Danceability
- Energy
- Loudness
- Mode
- Speechiness
- Acousticness
- Instrumentalness
- Valence
- Tempo
- Track duration

## Data

The data used for this project is an extensive record of 32,833 songs collated by the Spotify Web Services department. You can access the dataset [here](https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv).

## Models

Three models were examined to find the best model for predicting the song genre:

- Linear Discriminant Analysis (LDA)
- K-Nearest Neighbors (KNN)
- Random Forest

Hyperparameter tuning was performed on KNN and Random Forest. The metric used for training and evaluation is accuracy since the class labels in the dataset are balanced.

## Results

| Metric   | Mean  | Std Error | Model          |
|----------|-------|-----------|----------------|
| Accuracy | 0.501 | 0.00995   | LDA            |
| Accuracy | 0.524 | 0.00771   | KNN            |
| Accuracy | 0.582 | 0.00669   | Random Forest  |

## Limitation and Conclusion

A limitation of the model is that it can only predict songs that belong to a single genre. However, in reality, a song can belong to multiple genres (e.g., pop and R&B) simultaneously, especially with the evolution of the music industry in recent years. In the near future, more genres will be mixed together to create new genres, and the distinctive characteristics of one genre will become less noticeable. Hence, genre classification will become even more challenging.

Future work should consider employing a multi-label classification model instead of a simple multiclass classification model like the one examined in this report. The results presented here could serve as preliminary analysis and thresholds for future work.





