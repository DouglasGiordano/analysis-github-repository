getQueryIssue <- function(name, owner){
  return(paste("SELECT 
               i.id as ID, 
               i.author as AUTHOR,
               i.createdAt as CREATEDAT,
               i.id as PARENT
               FROM
               issue as i
               WHERE name=\"",name, "\" and owner=\"",owner,"\";", sep=""));
}

getQueryPull <- function(name, owner){
  return(paste("SELECT 
               i.id as ID, 
               i.author as AUTHOR,
               i.createdAt as CREATEDAT,
               i.id as PARENT
               FROM
               pull as i
               WHERE name=\"",name, "\" and owner=\"",owner,"\";", sep=""));
}

getQueryIssueComment <- function(name, owner){
  return(paste("SELECT 
               i.id as ID, 
               i.author as AUTHOR,
               i.createdAt as CREATEDAT,
               i.issue as PARENT
               FROM
               issuecomment as i
               WHERE name=\"",name, "\" and owner=\"",owner,"\";", sep=""));
}

getQueryPullComment <- function(name, owner){
  return(queryPullComment <- paste("SELECT 
                                   i.id as ID, 
                                   i.author as AUTHOR,
                                   i.createdAt as CREATEDAT,
                                   i.pull as PARENT
                                   FROM
                                   pullcomment as i
                                   WHERE name=\"",name, "\" and owner=\"",owner,"\";", sep=""));
}