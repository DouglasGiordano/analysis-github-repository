library(RMySQL)
library(rvest)#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest use for scraping data html
library(purrr)#function map to get unique element html
# lapply(dbListConnections(MySQL()), dbDisconnect)

#geração da rede social



getInteractions <- function(name, owner, query){
  
  rs = dbSendQuery(mydb, query)
  rows = fetch(rs, n=-1)
  rows$CREATEDAT = as.Date(rows$CREATEDAT)
  return(na.omit(rows))
}

getNetwork <- function(){
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

setMentions <- function(){
  name = "freeCodeCamp";
  owner = "freeCodeCamp";
  
  issues = getInteractions(name, owner, getQueryIssue(name, owner));
  issuescomment = getInteractions(name, owner, getQueryIssueComment(name, owner));
  interactions = rbind(issues, issuescomment);
  for(row in 1:nrow(issues)){
    id = issues[row]$ID;
    mentions = issues[row]$mention
    print(id);
  }
}

