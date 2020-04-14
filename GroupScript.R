#MBDA 2019-2020
#Course: Recommendation Tool
#students: Claire Hutin - Rishabh Sharma - Jules Motyl - Brayan Thomas Emil

###########################################################################
##########################DATA PREPARATION#################################

# set the working directory
setwd("C:/Users/rsharma3/OneDrive - IESEG/Recommendation Tools/last_fm")
getwd()

# Read in the data
artist<-read.delim("Artists.dat")
tags<-read.delim("tags.dat")
user_artists<-read.delim("user_artists.dat")
user_taggedartists<-read.delim("user_taggedartists.dat")


#base matrix for content-based recommendation systems: merge user_taggedartists and tags

library(dplyr)
library(tidyr)
library(proxy)
library(reshape2)
library(sqldf)
library(rlang)
library(data.table)
library(tm)

basematrix_contentbased<- merge(user_taggedartists, tags,by="tagID")

basematrix_contentbased <- subset(basematrix_contentbased,select = c("userID","artistID","tagID","tagValue","year"))

hist(basematrix_contentbased$year)

summary(basematrix_contentbased$year)

#Classifying years by decades

basematrix_contentbased$Y1950 <- ifelse(basematrix_contentbased$year < 1960, 1, 0)
basematrix_contentbased$Y1960 <- ifelse(basematrix_contentbased$year >= 1960 & basematrix_contentbased$year < 1970, 1, 0)
basematrix_contentbased$Y1970 <- ifelse(basematrix_contentbased$year >= 1970 & basematrix_contentbased$year < 1980, 1, 0)
basematrix_contentbased$Y1980 <- ifelse(basematrix_contentbased$year >= 1980 & basematrix_contentbased$year < 1990, 1, 0)
basematrix_contentbased$Y1990 <- ifelse(basematrix_contentbased$year >= 1990 & basematrix_contentbased$year < 2000, 1, 0)
basematrix_contentbased$Y2000 <- ifelse(basematrix_contentbased$year >= 2000 & basematrix_contentbased$year < 2010, 1, 0)
basematrix_contentbased$Y2010 <- ifelse(basematrix_contentbased$year >= 2010, 1, 0)

basematrix_contentbased$year <- NULL

length(unique(basematrix_contentbased$artistID))
length(unique(user_artists$artistID))
length(unique(basematrix_contentbased$tagID))

#Standardizing to have correct spellings


removeSpace <- function(x) gsub("\\s","",x)
removeTh <- function(x) gsub(" th","",x)

# Vectorize
source <- VectorSource(tags$tagValue)
all_tags <- VCorpus(source)
all_tags


# Normalize
all_tags <- tm_map(all_tags, content_transformer(tolower))
all_tags <- tm_map(all_tags, removeWords, stopwords("english"))
all_tags <- tm_map(all_tags, removePunctuation)
all_tags <- tm_map(all_tags, stemDocument)
all_tags <- tm_map(all_tags, content_transformer(removeTh))
all_tags <- tm_map(all_tags, stripWhitespace)
all_tags <- tm_map(all_tags, content_transformer(removeSpace))

class(all_tags)

tags_checked<-data.frame(text = sapply(all_tags, as.character), stringsAsFactors = FALSE)
length(unique(tags_checked$text))
length(unique(tags$tagID))

#cbin between our new tags and old ID

newtagsID<-data.frame(cbind(tags$tagID,tags_checked$text))
colnames(newtagsID)<-c("tagID","tagname")

#left join on tagID between user_taggedartists and new names

temp_tag<-merge(x = user_taggedartists, y = newtagsID, by = "tagID", all.x = TRUE)


taggroup<-temp_tag%>%group_by(userID,artistID,tagname)%>%count()

check <- taggroup %>% group_by(tagname) %>% tally(name='count')
taggroup <- merge(x = taggroup, y = check, by = "tagname", all.x = TRUE)

#Deleting rows for tags that appears less than X time
taggroup<-taggroup[!(taggroup$count<50),]

length(unique(taggroup$artistID))

#mutate(basematrix_contentbased, tagCount = count(tagID))

#Tag per User to be used for Content Based RecSys
TagPerArtist <- dcast(taggroup,artistID~tagname, value.var = "tagname")

# Artist per User to be used for Content Based RecSys
ArtPerUserContent <- dcast(taggroup, userID~artistID, value.var = "artistID")

#Artist per User for Collaborative Filtering
ArtistPerUser <- dcast(basematrix_contentbased,userID~artistID, value.var = "artistID")

# product matrix for Content-based
# rename row names of TagPerArtist 
rownames(TagPerArtist) <- TagPerArtist$artistID

# delete artisID column
TagPerArtist$artistID <- NULL

# convert to matrix
TagPerArtistMatrix <- as.matrix(TagPerArtist)


# base matrix for Content-based 
# rename rows of ArtPerUserContent
rownames(ArtPerUserContent) <- ArtPerUserContent$userID

# delete userID column
ArtPerUserContent$userID <- NULL

# convert to matrix
ArtPerUserContentMatrix <- as.matrix(ArtPerUserContent)



#base matrix for cluster based collaborative filtering:
summary(user_artists)

reshape_userartists<-spread(user_artists, key=artistID, value=weight)
reshape_userartists <- as_tibble(reshape_userartists)

rownames(reshape_userartists) = reshape_userartists$userID

reshape_userartists$userID = NULL
reshape_userartists = as.matrix(reshape_userartists)
class(reshape_userartists)

# subset created for testing the function calculations
trainSubset <- reshape_userartists[1:1500, 1:1000]
testSubset <- reshape_userartists[1501:1892, 1:1000]
ContentTestSubset <- ArtPerUserContentMatrix[1:1745, ]



#########################################################################
################ CLUSTER BASED COLLABORATIVE FILTERING ##################

# define the function

ClusterBasedCF <- function(data, N, centers, iter, onlyNew=TRUE){
  
  data2 <- data
  
  # fill with average product rating
  colmeans <- colMeans(data2, na.rm=TRUE)
  
  for (j in colnames(data2)){
    data2[, j] <- ifelse(is.na(data2[ ,j]) | is.nan(data2[ ,j]) | is.infinite(data2[ ,j]), colmeans[j], data2[, j])
    #data2[, j] <- ifelse(is.na(as.numeric(data2[ ,j])), colmeans[j], data2[, j])
  }
  
  km <- kmeans(data2, centers=centers, iter.max=iter)
  
  head(km$cluster)
  head(km$centers)
  
  
  # Statistics of the groups
  tab <- table(km$cluster)
  
  # Assign users to groups
  RES <- cbind(data, as.data.frame(km$cluster))
  
  # Calculate average ratings for everi cluster
  aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
  aggregation <- aggregation[,-1]
  
  # Make a prediction
  users <- as.data.frame(RES$"km$cluster")
  users <- cbind(users, rownames(RES))
  colnames(users) <- c("km$cluster", 'rn')
  
  
  prediction = merge(users, aggregation, by="km$cluster")
  rownames(prediction) <- prediction$rn
  
  prediction  <- prediction[order(rownames(prediction)), -1:-2]
  
  prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction), 
                        dimnames=list(rownames(prediction), colnames(prediction)))
  colnames(prediction2) <- colnames(prediction)
  rownames(prediction2) <- rownames(prediction)
  
  for (u in rownames(prediction)){
    if (onlyNew == TRUE){
      unseen <- names(data[u, is.na(data[u,])])
      
      prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
  }
  
  # TopN
  TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), N))))
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
} 

cl = ClusterBasedCF(reshape_userartists, 3, 10, 20)

cl$topN
cl$prediction



#########################################################################
################ USER BASED COLLABORATIVE FILTERING #####################
###################    CORELATION BASED    ##############################
######################   single user   ##################################

# define the function
UserBasedCFOneUser <- function(dataset, user, N, NN, onlyNew=TRUE){
  
  ### similarity ###
  similarity_vect <- vector(, nrow(dataset))
  names(similarity_vect) <- rownames(dataset)
  for (i in rownames(dataset)){
    if (i != user){
      userMean <- mean(dataset[user, ], na.rm  = TRUE)
      iMean <- mean(dataset[i, ], na.rm = TRUE)
      sim <- sum((dataset[user, ]-userMean)*(dataset[i, ]-iMean), na.rm=TRUE)/
        sqrt(sum((dataset[user, ]-userMean)^2, na.rm=TRUE)*
               sum((dataset[i, ]-iMean)^2, na.rm=TRUE))
      similarity_vect[i] <- sim
    }
  }
  
  ### Nearest Neighbors ###
  crit_val <- -sort(-similarity_vect)[NN]
  similarity_vect <- na.omit(ifelse(similarity_vect >= crit_val, similarity_vect, NA))
  
  ### Prediction ###
  # Prepare
  NN_norm <- dataset[rownames(dataset) %in% names(similarity_vect),]
  CM <- colMeans(dataset, na.rm=TRUE)
  for (l in 1:ncol(NN_norm)){
    NN_norm[,l] <- NN_norm[,l] - CM[l]
  }
  NN_norm[is.na(NN_norm)] <- 0
  
  # Numerator
  Num = similarity_vect %*% NN_norm
  
  #Prediction
  prediction = mean(dataset[user, ], na.rm=TRUE) + (Num/sum(similarity_vect, na.rm=TRUE))
  names(prediction) = colnames(dataset)
  
  if (onlyNew == TRUE){
    unseen <- names(dataset[user, is.na(dataset[user,])])
    prediction <- prediction[names(prediction) %in% unseen]
  }
  TopN <- head(-sort(-prediction), N)
  
  return(TopN)
}

# Run the recommendation function
res <- UserBasedCFOneUser(reshape_userartists, user='1700', N=10, NN=10, onlyNew=TRUE)

#check the results
res

#########################################################################
################ USER BASED COLLABORATIVE FILTERING #####################
###################    CORELATION BASED    ##############################
######################    all users    ##################################

# define the function
UserBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  
  ### similarity matrix ###
  
  similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data), 
                              dimnames = list(rownames(test_data), rownames(train_data)))
  
  # Calculate pearson corelation similarity between all user pairs
  ptm <- proc.time()
  
  for(x in 1:nrow(test_data)) {
    for(y in 1:nrow(train_data)) {
      
      print(x)
      print(y)
      meanX = mean(train_data[x, ], na.rm = TRUE)
      meanY = mean(train_data[y, ], na.rm = TRUE)
      #print(meanX)
      #print(meanY)
      sim <- sum((test_data[x, ]-meanX)*(train_data[y,]-meanY), na.rm=TRUE) / 
        sqrt(sum((test_data[x, ]-meanX)^2, na.rm=TRUE) * sum((train_data[y, ]-meanY)^2, na.rm=TRUE))
      
      similarity_matrix[x,y] <- sim
      
    }
  }
  Time <- (proc.time() - ptm)
  print(Time)
  
  print("similarity calculation done")
  ### Nearest Neighbors ###
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
  }
  
  print("Nearest Neighbor selection done")
  ### Prediction ###
  # Prepare
  prediction <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  
  TopN <- matrix(, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
  ### Numerator ###
  
  for (u in rownames(test_data)){
    
    similarity_vector <- na.omit(similarity_matrix_NN[u, ])
    
    NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]
    
    CM <- colMeans(train_data, na.rm=TRUE)
    for (l in 1:ncol(NN_norm)){
      NN_norm[,l] <- NN_norm[,l] - CM[l]
    }
    NN_norm[is.na(NN_norm)] <- 0
    
    # Numerator
    Num = similarity_vector %*% NN_norm
    
    #Prediction
    prediction[u, ] =  mean(test_data[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
    
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

# run the function on a subset of the data
ResultsUBCF <- UserBasedCF(trainSubset, testSubset, N=5, NN=10, onlyNew=TRUE)

# check the results
UBCFPred <- as.data.frame(ResultsUBCF$prediction)
UBCFtop5 <- as.data.frame(ResultsUBCF$topN)

#########################################################################
################ ITEM BASED COLLABORATIVE FILTERING #####################
###################    CORELATION BASED    ##############################

# define the function
ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
  # Similarity matrix
  
  ptm <- proc.time()
  
  similarity_matrix = matrix(, ncol=ncol(train_data), nrow=ncol(train_data), dimnames = list(colnames(train_data), colnames(train_data)))
  
  for (i in colnames(train_data)){
    for (j in colnames(train_data)){
      if (j > i){
        print(i)
        print(j)
        iMean = mean(train_data[, i], na.rm = TRUE)
        jMean = mean(train_data[, j], na.rm = TRUE)
        sim <- sum((train_data[,i] - iMean)*(train_data[, j] - jMean), na.rm=TRUE)/
          sqrt(sum((train_data[,i] - iMean)^2, na.rm=TRUE) * sum((train_data[, j] - jMean)^2, na.rm=TRUE))
        similarity_matrix[i, j] <- sim
        similarity_matrix[j, i] <- sim
      }
    }
  }
  
  print("Similarity calculation done")
  Time <- (proc.time() - ptm)
  print(Time)
  
  # Nearest Neighbor
  similarity_matrix_NN <- similarity_matrix
  
  for (k in 1:ncol(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
    similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
  }
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  
  train_data[is.na(train_data)] <- 0
  
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

# run the function on a subset of data
ResultsIBCF <- ItemBasedCF(trainSubset, testSubset, N=5, NN=10, onlyNew=TRUE)

# check the results
IBCFPred <- as.data.frame(ResultsIBCF$prediction)
IBCFTop5 <- as.data.frame(ResultsIBCF$topN)


#########################################################################
####################      CONTENT BASED RecSys      #####################

# define the function
ContentBased <- function(product_data, test_data, N, NN, onlyNew=TRUE){
  
  # Similarity calculation
  similarity_matrix <- as.matrix(simil(product_data, method="cosine"))
  
  print("Similarity calculation done")
  
  # Set Nearest neighbors
  similarity_matrix_NN <- similarity_matrix
  
  
  for (k in 1:nrow(similarity_matrix_NN)){
    crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
    similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], 0)
  }
  
  similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
  test_data2 <- test_data
  test_data2[is.na(test_data2)] <- 0
  
  print("Nearest neighbor selection done")
  
  ### Prediction ###
  prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                       dimnames=list(rownames(test_data), colnames(test_data)))
  prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
  TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
  
  for (u in rownames(test_data)){
    # Numerator
    Num <-  test_data2[u, ] %*% similarity_matrix_NN
    
    # Denominator
    Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
    
    # Prediction
    prediction[u, ] <- Num/Denom
    
    if (onlyNew == TRUE){
      unseen <- names(test_data[u, is.na(test_data[u,])])
      prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
    }else{
      prediction2[u, ] <- prediction[u, ]
    }
    
    TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
    
  }
  
  print("Prediction done")
  
  res <- list(prediction, TopN)
  names(res) <- c('prediction', 'topN')
  
  return(res)
}

CB <- ContentBased(TagPerArtistMatrix, ContentTestSubset, 5, 10, onlyNew=T)


#########################################################################
#####################      MODEL EVALUATION      #######################

#Prediction Accuracy: MAE

MAE = function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    RMSE = abs(sum( (prediction - real)^2 , na.rm = TRUE )) / (nrow(prediction) * ncol(prediction))
    return(RMSE)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}


#F1 Metrics
F1BIS = function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      F1 = 2*((Recall*Precision)/(Precision+Recall))
      Class_Thres= list(F1)
      names(Class_Thres) = c("F1")
    }
    if (!is.na(TopN)){
      TP = vector(, length = nrow(prediction))
      FP = vector(, length = nrow(prediction))
      FN = vector(, length = nrow(prediction))
      
      for (u in 1:nrow(prediction)){
        predSorted = names(-sort(-prediction[u, ])[1:TopN])
        realSorted = names(-sort(-real[u, ])[1:TopN])
        TP[u] = length(intersect(predSorted, realSorted))
        FP[u] = length(setdiff(predSorted, realSorted))
        FN[u] = length(setdiff(realSorted, predSorted))
        }
      TPall = sum(TP)
      FPall = sum(FP)
      FNall = sum(FN)
      Recall = TPall/(TPall+FNall)
      Precision = TPall/(TPall+FPall)
      F1=2*((Recall*Precision)/(Precision+Recall))
      #Class_TopN = list(Recall, Precision)
      Class_TopN=list(F1)
      names(Class_TopN) = c("F1")
    }
    
    
    if (!is.na(threshold) & !is.na(TopN)){
      Class = list(Class_Thres, Class_TopN)
      names(Class) = c("Threshold", "TopN")
    }else if (!is.na(threshold) & is.na(TopN)) {
      Class = Class_Thres
    }else if (is.na(threshold) & !is.na(TopN)) {
      Class = Class_TopN
    }else{
      Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
    }
    return(Class)  
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}


#Classification
Classification <- function(prediction, real, threshold=NA, TopN=NA){
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    # Threshold #
    if (!is.na(threshold)){
      TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
      FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
      FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
      Recall = TP/(TP+FN)
      Precision = TP/(TP+FP)
      Class_Thres = list(Recall, Precision)
      names(Class_Thres) = c("Recall", "Precision")
    }
    if (!is.na(TopN)){
      TP = vector(, length = nrow(prediction))
      FP = vector(, length = nrow(prediction))
      FN = vector(, length = nrow(prediction))
      
      for (u in 1:nrow(prediction)){
        threshold_pred = -sort(-prediction[u, ])[TopN]
        threshold_real = -sort(-real[u, ])[TopN]
        TP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
        FP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] < threshold_real, 1, 0), na.rm=T)
        FN[u] = sum(ifelse(prediction[u, ] < threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
      }
      TPall = sum(TP)
      FPall = sum(FP)
      FNall = sum(FN)
      Recall = TPall/(TPall+FNall)
      Precision = TPall/(TPall+FPall)
      Class_TopN = list(Recall, Precision)
      names(Class_TopN) = c("Recall", "Precision")
    }
    
    
    if (!is.na(threshold) & !is.na(TopN)){
      Class = list(Class_Thres, Class_TopN)
      names(Class) = c("Threshold", "TopN")
    }else if (!is.na(threshold) & is.na(TopN)) {
      Class = Class_Thres
    }else if (is.na(threshold) & !is.na(TopN)) {
      Class = Class_TopN
    }else{
      Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
    }
    return(Class)  
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}

#RMSE
RMSE <- function(prediction, real){
  
  if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
    RMSE = sqrt( sum( (prediction - real)^2 , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
    return(RMSE)
  }else{
    return("Dimension of prediction are not equal to dimension of real")
  }
}



#FOR UBCF
MAE(ResultsUBCF$prediction,testSubset)

F1BIS(ResultsUBCF$prediction, testSubset, 5, 10)


#FOR ITCF

MAE(ResultsIBCF$prediction,testSubset)

F1BIS(ResultsIBCF$prediction, testSubset, 5, 10)

#For Content Based

MAE(CB$prediction,ContentTestSubset)

F1BIS(CB$prediction, ContentTestSubset, 5, 10)


#Evaluation of all models (RMSE, Classification)

#ContentBased
RMSE(CB$prediction, ContentTestSubset)
Classification(CB$prediction, ContentTestSubset, threshold=5)

#ItemBasedCF
RMSE(ResultsIBCF$prediction, testSubset)
Classification(ResultsIBCF$prediction, testSubset, threshold=5)

#UserBased CF
RMSE(ResultsUBCF$prediction, testSubset)
Classification(ResultsUBCF$prediction, testSubset, threshold=5)



#########################################################################
######################      HYBRID RecSys      #########################



### Input our individuals recommendation systems results in new variables###
UB <- ResultsUBCF
IB <- ResultsIBCF

### Transform results to lists (to be able to use the rowMeans function) ###
UB_list <- as.list(ResultsUBCF$prediction)
IB_list <- as.list(ResultsIBCF$prediction)

#set our testset used previously
test1<-testSubset

####################
### Compute Mean ###
####################
hybrid <- rowMeans(cbind(as.numeric(UB_list), as.numeric(IB_list)), na.rm=T)

### Transform list back to matrix with correct number of dimensions ###
Hybrid_prediction <- matrix(hybrid, nrow=nrow(test1), ncol=ncol(test1))
rownames(Hybrid_prediction) <- rownames(test1)
colnames(Hybrid_prediction) <- colnames(test1)

### Evaluate ###
# RMSE
RMSE(UB$prediction, test1)
RMSE(IB$prediction, test1)
RMSE(Hybrid_prediction, test1)

# Classification
Classification(UB$prediction, test1, threshold=5)
Classification(IB$prediction, test1, threshold=5)
Classification(Hybrid_prediction, test1, threshold=5)

#########################
### Linear Regression ###
#########################

# Train a linear Regression
### flatten test1 dataset
test_list <- as.list(test1)

### Transform list and matrices to dataframe
test_df <- data.frame(matrix(unlist(test_list), byrow=T))
UB_df <- data.frame(matrix(unlist(UB_list), byrow=T))
IB_df <- data.frame(matrix(unlist(IB_list), byrow=T))

### Combine created dataframes
input <- cbind(test_df, UB_df, IB_df)
colnames(input) <- c('TARGET', 'UB', 'IB')

### Train the linear regression
fit <- lm(TARGET ~ UB + IB, data=input)
summary(fit)

### Score Models
UB2 <- ResultsUBCF
IB2 <- ResultsIBCF
test2<-testSubset

### Matrix to list
test_list2 <- as.list(test2)
UB_list2 <- as.list(UB2$prediction)
IB_list2 <- as.list(IB2$prediction)

### List to dataframe
test_df2 <- data.frame(matrix(unlist(test_list2), byrow=T))
UB_df2 <- data.frame(matrix(unlist(UB_list2), byrow=T))
IB_df2 <- data.frame(matrix(unlist(IB_list2), byrow=T))

### combine dataframes to have an input dataset for linear regression
input2 <- cbind(test_df2, UB_df2, IB_df2)
colnames(input2) <- c('TARGET', 'UB', 'IB')

### Predict using the model calculated on test2 dataset
Hybrid_lin_reg <- predict(fit, input2)

### Transform the list of results to matrix with the correct dimensions
Hybrid_lin_reg <- matrix(Hybrid_lin_reg, nrow=nrow(test2), ncol=ncol(test2))
rownames(Hybrid_lin_reg) <- rownames(test2)
colnames(Hybrid_lin_reg) <- colnames(test2)

# Evaluation
# RMSE
RMSE(UB2$prediction, test2)
RMSE(IB2$prediction, test2)
RMSE(Hybrid_lin_reg, test2)

# Classification
Classification(UB2$prediction, test2, threshold=5)
Classification(IB2$prediction, test2, threshold=5)
Classification(Hybrid_lin_reg, test2, threshold=5)

################################################################################

# Prepare the Top 5 table
# change rownames to UserID column
setDT(UBCFtop5, keep.rownames = "UserID")[]

# gather the columns into Top 5 ArtistIDs 
UBCFTop5Table <- UBCFtop5 %>% gather(key = "Rank", value = "id", -UserID)


# Prepare the weights table
# change the rownames to UserID column 
setDT(UBCFPred, keep.rownames = "UserID")[]

# gather the columns into Top 5 ArtistIDs 
UBCFPredTable <- UBCFPred %>% gather(key = "id", value = "score", -UserID)

# merge the tables to get ArtistID and score in one table
Predictions <- merge(x = UBCFTop5Table, y = UBCFPredTable, by = c("UserID", "id"), all.x = TRUE) 

#export the table in CSV format
write.csv(Predictions, file = "Predictions.csv", col.names = TRUE, row.names = FALSE)
