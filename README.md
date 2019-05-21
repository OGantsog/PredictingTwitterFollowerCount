# Predicting Twitter Follower Count

We collected data using twitteR package for accessing twitter API and retrieved 17 features information. We chose Follower count as dependent variable and rest of the features as independent variable. Then we experimented several regressions (Ridge, Linear, Lasso, Boosting, Bagging and RandomForest) and compared error prediction with RMSE. Then in order to improve the prediction we used clustering (KMeans) on accounts and fitted RandomForest regression on each cluster.
