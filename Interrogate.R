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

#Used account of Dirk Wetter to produce plots.
#Used his account as it has more followers and would produce more accurate results.
#Big sample size produces better results.
#Username is drwetter.

#Began to interrogate Dirk Wetter's account to produce graphs, by first looking at his followers.
myData = GET("https://api.github.com/users/drwetter/followers?per_page=100;", getToken)
stop_for_status(myData)
extract = content(myData)
#Converts into dataframe
githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))
githubDB$login

#Retrieve a list of usernames
id = githubDB$login
user_ids = c(id)

#Create an empty vector and data.frame
users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)

#Loops through users and adds to list
for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingReq = GET(followingURL, getToken)
  followingCont = content(followingReq)
  
  #Doesn't add users if they don't have any followers
  if(length(followingCont) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingCont))
  followingLogin = followingDF$login
  
  #Loops through 'following' users
  for (j in 1:length(followingLogin))
  {
    #Checks for duplicate users
    if (is.element(followingLogin[j], users) == FALSE)
    {
      #Adds user to the current list
      users[length(users) + 1] = followingLogin[j]
      
      #Obtains info from each user
      followingURLTwo = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      followingTwo = GET(followingURLTwo, getToken)
      followingContTwo = content(followingTwo)
      followingDFTwo = jsonlite::fromJSON(jsonlite::toJSON(followingContTwo))
      
      #Retrieves who the user is following
      followingNumber = followingDFTwo$following
      
      #Retrieves the users followers
      followersNumber = followingDFTwo$followers
      
      #Retrieves how many repositories the user has 
      reposNumber = followingDFTwo$public_repos
      
      #Retrieves the year in which each user joined Github
      yearJoined = substr(followingDFTwo$created_at, start = 1, stop = 4)
      
      #Adds the users data to a new row in dataframe
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearJoined)
      
    }
    next
  }


