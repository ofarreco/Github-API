install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Access_GitHub",
                   key = "1ab488ceeadc6a9bc3a5",
                   secret = "ee2e58fd88e104b16b90da3a4825bfe8c40d94b7")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 
#install all needed packages
install.packages("jsonlite")
install.packages("httr")
install.packages("httpuv")
install.packages("ggplot2")
install.packages("plotly")
install.packages('devtools')
library(plotly)
library(jsonlite)
library(httpuv)
library(httr)
library(devtools)
library(magrittr)
library(dplyr)
library(plotly)

#Choose github as the endpoint
oauth_endpoints("github")


#Enter my personal github details
myApplication <- oauth_app(appname = "Rstudio",
                           key = "467fb84caca2c6e217d2",
                           secret = "29bfddb0d3f3bc73fae1fe006c5782705bea5b8e")


#Set and get my github token 
githubToken <- oauth2.0_token(oauth_endpoints("github"), myApplication)
getToken <- config(token = githubToken)



#Returns a dataframe with information on the Current Users Followers, current user is Dirk Wetter 
getFollowers <- function(drwetter)
{
  i <- 1
  x <- 1
  followersDF <- data_frame()
  while(x!=0)
  {
    followers <- GET( paste0("https://api.github.com/users/", username, "/followers?per_page=100&page=", i),getToken)
    followersContent <- content(followers)
    currentFollowersDF <- lapply(followersContent, function(x) 
    {
      df <- data_frame(user = x$login, userID = x$id, followersURL = x$followers_url, followingURL = x$following_url)
    }) %>% bind_rows()
    i <- i+1
    x <- length(followersContent)
    followersDF <- rbind(followersDF, currentFollowersDF)
  }
  return (followersDF)
}

#Returns a dataframe with information on the Current Users repositoryitories, current user Dirk Wetter
getrepositoryitory <- function(drwetter)
{
  i <- 1
  x <- 1
  repositoryitoryDF <- data_frame()
  while(x!=0)
  {
    repository <- GET( paste0("https://api.github.com/users/", username, "/repository?per_page=100&page=", i),getToken)
    repositoryContent <- content(repository)
    currentrepositoryitoryDF <- lapply(repositoryContent, function(x) 
    {
      df <- data_frame(repo = x$name, id = x$id, commits = x$git_commits_url, language = x$languages) #language = x$language)
    }) %>% bind_rows()
    i <- i+1
    x <- length(repositoryContent)
    repositoryDF <- rbind(repositoryDF, currentrepositoryDF)
  }
  return (repositoryDF)
}
