#install.packages("twitteR")
#install.packages('openssl')
library(twitteR)

# Log in as 'swbrewer'
api_key <- #paste your api key
api_secret <- #paste your api secret 
access_token <- #paste your token
access_token_secret <- #paste your token secret 
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Log in as 'onontsatsal'
api_key <- #paste your api key  
api_secret <- #paste your api secret  
access_token <- #paste your token 
access_token_secret <- #paste your token secret
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

userName <- 'swbrewer'
userName <- 'BigDataGal'


# Get seed user info
user <- getUser(userName)
#friends <- user$getFriends()
#save(list=c('friends'), file=paste0('/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/', user$screenName, '_friends.data'))
load(file=paste0('/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/', userName, '_friends.data'))

# Get Followers
#followers <- user$getFollowers() # Also caused a rate limit, but got 68k followers
#save(list=c('followers'), file=paste0('/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/', user$screenName, '_followers.data'))
load(file=paste0('/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/', userName, '_followers.data'))

# Attach file, get stats
attach(paste0('/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/', userName, '_followers.data'))
attach(paste0('/Users/cymorene/Documents/UIC/2018Spring/IDS575/Project/', userName, '_friends.data'))
search()
#detach(3)

n <- length(friends)
# Initialize columns of user data
id <- rep(NA, n)
screenName <- rep(NA, n)
frCount <- rep(NA, n)
statCount <- rep(NA, n)
favCount <- rep(NA, n)
createdDate <- rep(NA, n)
folCount <- rep(NA, n)
verifiedBool <- rep(NA, n)
protectedBool <- rep(NA, n)


# Build user data frame of friends
for (i in 1:n) {
  #afriend <- getUser(friends[i]) # Don't use - calls twitter api resulting in rate limit after ~1000 calls
  id[i] <- friends[[i]]$getId()
  screenName[i] <- friends[[i]]$screenName
  frCount[i] <- friends[[i]]$friendsCount
  folCount[i] <- friends[[i]]$followersCount
  statCount[i] <- friends[[i]]$statusesCount
  favCount[i] <- friends[[i]]$favoritesCount
  createdDate[i] <- friends[[i]]$created
  verifiedBool[i] <- friends[[i]]$verified
  protectedBool[i] <- friends[[i]]$protected
}

# Combine columns into data frame. Note: cbind was specifically avoided due to being bad practice
df <- data.frame(id=id,
                 screenName=screenName,
                 frCount=frCount,
                 folCount=folCount,
                 statCount=statCount,
                 favCount=favCount,
                 createdDate=createdDate,
                 verifiedBool=verifiedBool,
                 protectedBool=protectedBool,
                 stringsAsFactors = FALSE)
write.csv(df, file='/Users/cymorene/Documents/Share.nosync/BigDataGal_friends.csv', row.names = FALSE)

# Build user data frame of followers
n <- length(followers)
for (i in 1:n) {
  #afollower <- getUser(followers[i])  # Don't use - calls twitter api resulting in rate limit after ~1000 calls
  id[i] <- followers[[i]]$id
  screenName[i] <- followers[[i]]$screenName
  frCount[i] <- followers[[i]]$friendsCount
  folCount[i] <- followers[[i]]$followersCount
  statCount[i] <- followers[[i]]$statusesCount
  favCount[i] <- followers[[i]]$favoritesCount
  createdDate[i] <- followers[[i]]$created
  verifiedBool[i] <- followers[[i]]$verified
  protectedBool[i] <- followers[[i]]$protected
}
# Combine columns into data frame. Note: cbind was specifically avoided due to being bad practice
df <- data.frame(id=id,
                 screenName=screenName,
                 frCount=frCount,
                 folCount=folCount,
                 statCount=statCount,
                 favCount=favCount,
                 createdDate=createdDate,
                 verifiedBool=verifiedBool,
                 protectedBool=protectedBool,
                 stringsAsFactors = FALSE)
write.csv(df, file='/Users/cymorene/Documents/Share.nosync/BigDataGal_followers.csv', row.names = FALSE)
# I separated out the fave and retweet extraction loop to be run after the df is created from the friends and followers lists
numToPull <- 50
favSum <- rep(NA, n)
rtSum <- rep(NA, n)

#seq1 <- 1:1000
#seq2 <- 1001:2000
seq3 <- 2001:3000
#seq4 <- 3001:4000
#seq5 <- 4001:5000
#seq6 <- 5001:6000
#seq7 <- 6001:7000
#seq8 <- 7001:8000
#seq9 <- 8001:9000
#seq10 <- 9001:10000
#seq11 <- 10001:11000
#seq12 <- 11001:12000
#seq13 <- 12001:13000
#seq14 <- 13001:14000
#seq15 <- 14001:15000
for (i in seq3) {
  # Initilize sums, then loop through afriend twts - This may causeget a rate limit!! Must wait 15 min to clear
  # https://developer.twitter.com/en/docs/basics/rate-limiting
  twts <- userTimeline(df$id[i], n=numToPull)
  tmpFavSum <- 0
  tmpRtSum <- 0
  for (twt in twts){
    tmpFavSum <- tmpFavSum + twt$favoriteCount
    tmpRtSum <- tmpRtSum + twt$retweetCount
  }
  favSum[i] <- tmpFavSum
  rtSum[i] <- tmpRtSum
}

tmpdf <- data.frame(favSum=favSum,rtSum=rtSum)
write.csv(tmpdf, file='/Users/cymorene/Documents/Share.nosync/BigDataGal_friend_rt&favSums.csv', row.names = FALSE)

# Reload data
data_fr <- read.csv('/Users/cymorene/Documents/Share.nosync/BigDataGal_friends.csv')
data_fo <- read.csv('/Users/cymorene/Documents/Share.nosync/BigDataGal_followers.csv')

# Tag source before combine (probably not important, but just in case)
data_fr$BDG_status <- 'friend'
data_fo$BDG_status <- 'follower'

# Combine and write
df <- rbind(data_fr[,2:11], data_fo[,2:11])
write.csv(df, file='/Users/cymorene/Documents/Share.nosync/BigDataGal_combo.csv', row.names = FALSE)

# Input data
data <- read.csv(file='/Users/cymorene/Documents/Share.nosync/BigDataGal_combo.csv')

# Check for duplicates by screen name
dupes <- duplicated(data$screenName)
sum(dupes)

# Strip out dupes by screen name
# Used ono's account who knows she's a friend and follower to check behavior of duplicated() output
rows <- which(data$screenName=='onontsatsal')
dupes[rows]
# It flags occurences after first with TRUE
data <- data[-which(dupes),]

# Remove id, sceen name, and friend/follower tag.
# Convert dates from seconds since 1/1/70 to days since 1/1/70
# Transfrom large range field by log(x+1) (to account fo zeros)
df <- data[,3:9]
df$createdDate <- as.numeric(as.Date(as.POSIXct(df$createdDate, origin='1970-1-1')))
df.base <- df # Storing df before transforming it


# Declare functions for transformations and error calcs 
log.plus1 <- function(baseval) log(baseval+1) 

exp.minus1 <- function(logxplus1val) exp(logxplus1val)-1

rmse <- function(actual_val, pred_val) mean(sqrt((actual_val-pred_val)^2))


#df$createdDate <- log(df$createdDate+1)
df$frCount <- log.plus1(df$frCount)
df$folCount <- log.plus1(df$folCount)
df$statCount <- log.plus1(df$statCount)
df$favCount <- log.plus1(df$favCount)


summary(df)
# Based in ranges it likely that most variables will need log transform

# Create train and test row vectors
set.seed(1)
train <- sample(1:nrow(df), 0.7*nrow(df))
test <- -train

# Multi Variable Linear Regression

# No Transforms
lm.fit.base <- lm(folCount~., data = df.base[train,])
summary(lm.fit.base)

lm.pred.base <- predict(lm.fit.base, newdata = df.base[test,])
rmse.lm.base <- rmse(df.base$folCount[test], lm.pred.base)

# Transformed LR
lm.fit <- lm(folCount~., data = df[train,])
summary(lm.fit)
lm.pred <- predict(lm.fit, newdata = df[test,])
rmse.lm <- rmse(df$folCount[test], lm.pred)

# Ridge Regression
library(glmnet)
x <- model.matrix(folCount~., df)[,-1]
y <- df$folCount
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=0)
bestlam <- cv.out$lambda.min
ridge.fit <- glmnet(x[train,], y[train], alpha=0)
ridge.pred <- predict(ridge.fit, s=bestlam, newx=x[test,])
rmse.ridge <- rmse(df$folCount[test], ridge.pred)


# LASSO Regression
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
bestlam <- cv.out$lambda.min
lasso.fit <- glmnet(x[train,], y[train], alpha=1)
lasso.pred <- predict(lasso.fit, s=bestlam, newx=x[test,])
rmse.lasso <- rmse(df$folCount[test], lasso.pred)

library(randomForest)
library(gbm)

# Bagging - Long!
mtry <- ncol(df) - 1
bag.fit <- randomForest(folCount~., data=df[train,], mtry=mtry, ntree=500)
bag.pred <- predict(bag.fit, newdata=df[test,])
rmse.bag <- rmse(df$folCount[test], bag.pred)

# Random Forest, use default mtry - Long!
#mtry <- floor(sqrt(ncol(df)))
#mtry <- max(floor(ncol(df)/3), 1)
rf.fit <- randomForest(folCount~., data=df[train,], ntree=500)
rf.pred <- predict(rf.fit, newdata=df[test,])
rmse.rf <- rmse(df$folCount[test], rf.pred)

# As an example, I converted the rf prediction back to reality and compared to your follower count
# The error is down to 308 vs ~400 for lm!
rf.pred.ono <- predict(rf.fit, newdata=df[964,])
rf.ono.err <- data$folCount[964] - exp.minus1(rf.pred.ono)

# Now same, but for max folCount user
rf.pred.max <- predict(rf.fit, newdata=df[which.max(df$folCount),])
rf.max.err <- data$folCount[which.max(df$folCount)] - exp.minus1(rf.pred.max)
# Error of 3,468,095 from actual of 4,620,130 so not good at all! I experimented with this more using the lm
# It seems like the error at the extremes just get way out of hand - Do you recall any models that help with this?

# Boosting
lambdas <- seq(0.01,1,0.01)
rmse.vals <- vector(mode='numeric',length=length(lambdas))
function.boost <- folCount~.-verifiedBool-protectedBool+I(as.factor(verifiedBool))+I(as.factor(verifiedBool))
for (i in seq_along(lambdas)){
  boost.fit <- gbm(function.boost, data=df[train,], distribution='gaussian',
                   n.trees=50, interaction.depth=1, shrinkage=lambdas[i])
  boost.pred <- predict(boost.fit, newdata=df[test,], n.trees=50)
  rmse.vals[i] <- rmse(df$folCount[test], boost.pred)
}
plot(lambdas, rmse.vals, type='b', main='Boosting Test RMSE for Range of Shrinkage Values')
rmse.boost <- min(rmse.vals)
boost.lambda <- lambdas[which(boost.rmse==rmse.vals)]

# Just curious, let's try finding tree stabilization point
tree.cnt <- seq(500,5000,500)
rmse.vals <- vector(mode='numeric',length=length(tree.cnt))
function.boost <- folCount~.-verifiedBool-protectedBool+I(as.factor(verifiedBool))+I(as.factor(verifiedBool))
for (i in seq_along(tree.cnt)){
  boost.fit <- gbm(function.boost, data=df[train,], distribution='gaussian',
                   n.trees=tree.cnt[i], interaction.depth=1, shrinkage=boost.lambda)
  boost.pred <- predict(boost.fit, newdata=df[test,], n.trees=tree.cnt[i])
  rmse.vals[i] <- rmse(df$folCount[test], boost.pred)
}
plot(tree.cnt, rmse.vals, type='b', main='Boosting Test RMSE for Range of Tree Counts')
boost.ntree <- tree.cnt[which.min(rmse.vals)]

# Now re-run with more trees using best lambda and tree count
boost.fit <- gbm(function.boost, data=df[train,], distribution='gaussian',
                 n.trees=boost.ntree, interaction.depth=1, shrinkage=boost.lambda)
boost.pred <- predict(boost.fit, newdata=df[test,], n.trees=boost.ntree)
rmse.boost <- rmse(df$folCount[test], boost.pred)



#lm.rmse.all.base <- rmse(exp.minus1(df$folCount), exp.minus1(predict(lm.fit, newdata = df)))