library(RMySQL)
library(rvest)#https://www.datacamp.com/community/tutorials/r-web-scraping-rvest use for scraping data html
library(purrr)#function map to get unique element html
# lapply(dbListConnections(MySQL()), dbDisconnect)
#get connection for bd

#first
create.data.network <-function(){
  source(file = "CreateSocialNetwork.r")
  source(file ="Querys.r")
  projects = search.projects()
  mydb = get.connection.bd()
  for(i in 1:nrow(projects)){
    owner = projects[i,]$owner
    name = projects[i,]$name
    status = get.status.processing(mydb, owner, name)
    if(is.na(status$createNetwork)){
      message.i(owner, name, "Creating social network...")
      create.social.network(mydb, projects[i,]$name, projects[i,]$owner)
      update.status.create.network(mydb, owner, name)
      message.i(owner, name,"Complete...")
    }
  }
}

message.i <- function (owner, name, message){
  message(Sys.time(), " - (", owner, " ", name,") ", message)
}


create.data.sentiment <- function(){
  source(file = "SentiMetric.r")
  source(file ="Querys.r")
  source(file = "TextProcessing.R")
  projects = search.projects()
  mydb = get.connection.bd()
  n.projects = nrow(projects)
  for(i in 1:n.projects){
    owner = projects[i,]$owner
    name = projects[i,]$name
    message.i(owner, name, paste0(i, " of ", n.projects))
    status = get.status.processing(mydb, owner, name)
    if(is.na(status$createSentimentText)){
      message.i(owner, name, "Creating sentiment for text... ")
      
      #issue
      issue = query.issue.project(mydb, owner, name)
      if(nrow(issue) != 0){
        for(j in 1:nrow(issue)){
          issue.now = issue[j,]
          text = issue.now$text
          result = get.sentiment.metric(text)
          update.text.sentiment.issue(mydb, issue.now$id, result[1], result[2])
        }
        
        #issue comment
        issue.comment = query.issue.comment.project(mydb, owner, name)
        for(j in 1:nrow(issue.comment)){
          issue.comment.now = issue.comment[j,]
          text = text.clear.text(issue.comment.now$text)
          result = get.sentiment.metric(text)
          update.text.sentiment.issue.comment(mydb, issue.comment.now$id, result[1], result[2])
        }
      }
      
      #pull
      pull = query.pullrequest.project(mydb, owner, name)
      if(nrow(pull) != 0){
        for(j in 1:nrow(pull)){
          pull.now = pull[j,]
          text = pull.now$text
          result = get.sentiment.metric(text)
          update.text.sentiment.pull(mydb, pull.now$id, result[1], result[2])
        }
        #pull comment
        pull.comment = query.pullrequest.comment.project(mydb, owner, name)
        for(j in 1:nrow(pull.comment)){
          pull.comment.now = pull.comment[j,]
          text = text.clear.text(pull.comment.now$text)
          result = get.sentiment.metric(text)
          update.text.sentiment.pull.comment(mydb, pull.comment.now$id, result[1], result[2])
        }
      }
      
      update.status.create.sentiment.text(mydb, owner, name)
      message.i(owner, name, "Complete...")
    }
  }
}

get.status.processing <- function(mydb, owner, name){
  status = get.status(mydb, owner, name)
  if (dim(status)[1] == 0) {
    print("Not find status.")
    save.status(mydb, owner, name)
    status = get.status(mydb, owner, name)
  }
  return(status)
}

#second
create.metric.network <- function(project, mydb){
    edges = query.edge.project(mydb, project$owner, project$name)
    edges = na.omit(edges)
    users = unique(c(as.list(edges$source), as.list(edges$target)))
    users = na.omit(users)
    edges$time = as.POSIXct(edges$time, "UTC", "%Y-%m-%d %H:%M:%S")
    max.edges = as.Date(max(edges$time))
    min.edges = as.Date(min(edges$time))
    message(min.edges, " ", max.edges)
    
    #create sequence days for looping in graph and get temporal metrics
    sequence.days = seq(from=min.edges, to=max.edges, by='days' )
    
    #create graph witch edges and vertices
    graph = graph.data.frame(edges, directed = T)
    #graph <- simplify(graph, remove.multiple = F, remove.loops = T, edge.attr.comb=list(weight="sum","ignore"))
    vertices = V(graph)$name

    #get and save metric of repository
    metric = get.network.metric(graph)
    metric$owner = project$owner
    metric$name = project$name
    save.metric.project(metric, mydb)
    
    #get and save metric of vertices
    metric.links = get.network.metric.links(vertices, graph)
    
    return(metric.links)
}


#github,fetch teste
create.metric.sentiment <- function(project, mydb){
  owner = as.character(project$owner)
  name = as.character(project$name)
  interactions <- na.omit(query.interaction.project.sentiment(mydb, owner, name))
  
  user = na.omit(unique(interactions$user))
  
  #get and save metric of vertices
  metrics.senti = get.sentiment.median.metric(interactions, user)
  return(metrics.senti)
}

create.metric.count <- function(project, mydb){
  owner = as.character(project$owner)
  name = as.character(project$name)
  interactions <- na.omit(query.interaction.project(mydb, owner, name))
  
  user = na.omit(unique(interactions$user))
  
  #get and save metric of vertices
  metrics = get.count.metric(interactions, user)
  return(metrics)
}

create.metric.turnover <- function(project, mydb){
    owner = as.character(project$owner)
    name = as.character(project$name)
    interactions <- na.omit(query.interaction.project(mydb, owner, name))
    
    user = na.omit(unique(interactions$user))
    
    #get and save metric of vertices
    metric.turnover = as.numeric(get.turnover.metric(interactions, user))
    return(metric.turnover)
}


create.metrics <- function(){
  library(igraph)
  library(RColorBrewer)
  source(file = "NetworkMetric.R")
  source(file = "TurnoverMetric.R")
  source(file = "SentiMetric.R")
  source(file = "CounterMetric.R")
  source(file ="Querys.r")
  projects = search.projects()
  mydb = get.connection.bd()
  n.projects = nrow(projects)
  for(i in 1:n.projects){
    owner = as.character(projects[i,]$owner)
    name = as.character(projects[i,]$name)
	  message.i(owner, name, paste0(i, " of ", n.projects))
    status = get.status.processing(mydb, owner, name)
    metrics = NULL
    if(is.na(status$createMetricNetwork)){
      message.i(owner, name,"Calculating graph metrics... ")
      metric.links = create.metric.network(projects[i,], mydb)
      metrics = merge.metrics(metrics, metric.links)
      update.status.create.metric.network(mydb, owner, name)
    }
    
    if(is.na(status$createMetricTurnover)){
      message.i(owner, name,"Calculating turnover metrics... ")
      metric.turnover = create.metric.turnover(projects[i,], mydb)
      metrics = merge.metrics(metrics, metric.turnover)
      update.status.create.metric.turnover(mydb, owner, name)
    }
    
    if(is.na(status$createMetricSentiment)){
      message.i(owner, name,"Calculating sentiment metrics... ")
      metric.sentiment = create.metric.sentiment(projects[i,], mydb)
      metrics = merge.metrics(metrics, metric.sentiment)
      update.status.create.metric.sentiment(mydb, owner, name)
    }
    
    if(is.na(status$createMetricCount)){
      message.i(owner, name,"Calculating count metrics... ")
      metric.count = create.metric.count(projects[i,], mydb)
      metrics = merge.metrics(metrics, metric.count)
      update.status.create.metric.count(mydb, owner, name)
    }
    if(!is.null(metrics)){
      metrics$owner = owner
      metrics$name = name
      save.metric.user(metrics, mydb)
      message.i(owner, name,"Complete... ")
    }
  }
}


merge.metrics <- function(metrics, data){
  if(is.null(metrics)){
    return(data)
  } else {
    return(merge(metrics, data, by="user", all = T))
  }
}