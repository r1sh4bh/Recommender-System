# Recommender System with REST API
Based on user ratings data from the music streaming service LastFM, built the following Recommender Systems:
- Collaborative Filtering
  - User-based
  - Item-based
  - Cluster-based
- Content-based RecSys
- Hybrid Recsys

The models were built in R using user-defined as well as in-built functions.

Created a REST API in Flask (tested using Postman) which uses the results of the above recommender systems to return the top 5 artists for any userID provided as input. 
