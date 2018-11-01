library(RMySQL)
library(rvest)#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest use for scraping data html
library(purrr)#function map to get unique element html
# lapply(dbListConnections(MySQL()), dbDisconnect)

#geração da rede social

create.data.network <-function(){
  projects <- read.csv(file = "input/repositories.csv", sep = ",")
  for(i in 1:nrow(projects)){
    create.social.network(projects[i,]$name, projects[i,]$owner)
  }
}

create.metric.network <- function(){
  name = "freeCodeCamp";
  owner = "freeCodeCamp";
  
  issues = getInteractions(name, owner, getQueryIssue(name, owner));
  
  interactions = issues;
  
  days <- seq(from=as.Date(max(interactions$CREATEDAT)), to=as.Date(min(interactions$CREATEDAT)),by='days' )
  for ( i in seq_along(days) )
  {
    print(paste(days[i],"T12:00:00", sep=""))
  }
}

