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
 #Stops when there are more than 155 users
  if(length(users) > 155)
  {
    break
  }
  next
}

#Creates a link to plotly to produce graphs.
Sys.setenv("plotly_username"="ofarreco")
Sys.setenv("plotly_api_key"="Yny1ifZbRIf2iTexoNnh")

#Plot one graphs repositories vs followers by year using a scatter plot.
#Takes into account 155 of Dirk Wetter's followers.
#X-axis displays 'repositories' which shows the no. of repositories per user.
#Y-axis displays 'followers' which shows the no. of followers of each each of Dirk Wetter's followers.
plotOne = plot_ly(data = usersDB, x = ~repos, y = ~followers, text = ~paste("Followers: ", followers, "<br>Repos: ", repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plotOne
#Sends graph to plotly
api_create(plotOne, filename = "Repos vs Followers")
#Plot can be viewed on plotly for more interactive visualisation of the data: https://plot.ly/~ofarreco/1/#/

#Plot two graphs following vs followers by year using a scatter plot.
#Takes into account 150 of Dirk Wetter's followers.
#X-axis displays 'following' which shows the no. of users followed by each of Sebastien's followers.
#Y-axis displays 'followers' which shows the no. of followers of each of Dirk Wetter's followers.
plotTwo = plot_ly(data = usersDB, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated)
plotTwo
#Sends graph to plotly
api_create(plotTwo, filename = "Following vs Followers")
#Plot can be viewed on plotly for more interactive visualisation of the data: https://plot.ly/~ofarreco/3/

#Below code is to produce plot 3.
#Graph the 10 most popular languages used by Dirk Wetter's 155 followers.
#Same 155 users from two previous plots are used.
languages = c()

for (i in 1:length(users))
{
  ReposUrl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repos = GET(ReposUrl, getToken)
  ReposCont = content(Repos)
  ReposDF = jsonlite::fromJSON(jsonlite::toJSON(ReposCont))
  ReposNames = ReposDF$name
  
  #Loop through all the repositories of an individual user
  for (j in 1: length(ReposNames))
  {
    #Find all repositories and save in data frame
    ReposURLTwo = paste("https://api.github.com/repos/", users[i], "/", ReposNames[j], sep = "")
    ReposTwo = GET(ReposURLTwo, getToken)
    ReposContTwo = content(ReposTwo)
    ReposDFTwo = jsonlite::fromJSON(jsonlite::toJSON(ReposContTwo))
    language = ReposDFTwo$language
    
    #Removes repositories containing no specific languages
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}
#Puts 10 most popular languages in table 
allLanguages = sort(table(languages), increasing=TRUE)
topTenLanguages = allLanguages[(length(allLanguages)-9):length(allLanguages)]

#Converts to dataframe
languageDF = as.data.frame(topTenLanguages)

#Plot three shows the 10 most popular languages used by Dirk Wetter's 150 followers.
#The data is represented by a bar chart.
#X-axis displays 'languages'.
#Y-axis displays 'number of users'.
#Ruby is the most popular, followed by JavaScript.
#C and Rust were determined to be the least popular languages.
plotThree = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
plotThree

Sys.setenv("plotly_username"="ofarreco")

Sys.setenv("plotly_api_key"="Yny1ifZbRIf2iTexoNnh")
#Sends graph to plotly
api_create(plotThree, filename = "10 Most Popular Languages")
#Plot can be viewed on plotly for more interactive visualisation of the data: https://plot.ly/~ofarreco/5/#/



