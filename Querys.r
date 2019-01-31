
search.projects <- function(){
  projects <- read.csv(file = paste0("input/repositories.csv"), sep = ",")
  return(projects[i:f,])
}

get.connection.bd <- function(){
  mydb = dbConnect(MySQL(), user='root', password='root', dbname='simple_github', host='127.0.0.1')
  return(mydb)
}

query.issue.project <- function(mydb, owner, name){
  rsI = dbSendQuery(mydb, paste0("select  id, title as text, author, createdat, id as parent 
                                 from issue 
                                 where owner = '",owner,"' and name = '",name,"'"))
  issues = fetch(rsI, n=-1)
  return(issues)
}

query.issue.comment.project <- function(mydb, owner, name){
  rsIC = dbSendQuery(mydb, paste0("select id, bodyhtml as text, author, createdat, issue as parent 
                     from issuecomment 
                                  where owner = '",owner,"' and name = '",name,"'"))
  comments = fetch(rsIC, n=-1)
  return(comments)
}

query.pullrequest.project <- function(mydb, owner, name){
  rsP = dbSendQuery(mydb, paste0("select id, title as text, author, createdat, id as parent 
                    from pullrequest
                                 where owner = '",owner,"' and name = '",name,"'"))
  pulls = fetch(rsP, n=-1)
  return(pulls)
}

query.pullrequest.comment.project <- function(mydb, owner, name){
  rsPC = dbSendQuery(mydb, paste0("select id, bodyhtml as text, author, createdat, pull as parent 
                     from pullcomment 
                                  where owner = '",owner,"' and name = '",name,"'"))
  pullcomments = fetch(rsPC, n=-1)
  return(pullcomments)
}

query.edge.project <- function(mydb, owner, name){
  rs = dbSendQuery(mydb, paste0("select user_source as source, user_target as target, date_time as time
                     from edge 
                                  where project_owner = '",owner,"' and project_name = '",name,"' and user_source != user_target order by date_time asc"))
  edges = fetch(rs, n=-1)
  return(edges)
}

query.metricusers <- function(mydb){
  rs = dbSendQuery(mydb, paste0("select * from metricuser;"))
  metrics = fetch(rs, n=-1)
  return(metrics)
}

query.metricusers.with.significance <- function(mydb){
  rs = dbSendQuery(mydb, paste0("select * from metricuser where significance > 0;"))
  metrics = fetch(rs, n=-1)
  return(metrics)
}

query.interaction.project <- function(mydb, owner, name){
  rs = dbSendQuery(mydb, paste0("SELECT 
                                commit.author AS user,
                                commit.authoredDate AS date
                                FROM
                                commit 
                                WHERE commit.owner = '",owner,"' and name = '",name,"'
                                UNION SELECT 
                                issue.author AS user,
                                issue.createdAt AS date
                                FROM
                                issue 
                                WHERE issue.owner = '",owner,"' and issue.name = '",name,"'
                                UNION SELECT 
                                pullrequest.author AS user,
                                pullrequest.createdAt AS date
                                FROM
                                pullrequest 
                                WHERE pullrequest.owner = '",owner,"' and pullrequest.name = '",name,"'
                                UNION SELECT 
                                pullcomment.author AS user,
                                pullcomment.createdAt AS date
                                FROM
                                pullcomment 
                                WHERE pullcomment.owner = '",owner,"' and pullcomment.name = '",name,"'
                                UNION SELECT 
                                issuecomment.author AS user,
                                issuecomment.createdAt AS date
                                FROM
                                issuecomment 
                                WHERE issuecomment.owner = '",owner,"' and issuecomment.name = '",name,"'"))
  edges = fetch(rs, n=-1)
  return(edges)
}


query.interaction.project.sentiment <- function(mydb, owner, name){
  rs = dbSendQuery(mydb, paste0("SELECT 
                                issue.author AS user,
                                issue.positive AS positive,
                                issue.negative AS negative,
                                issue.id AS source
                                FROM
                                issue 
                                WHERE issue.owner = '",owner,"' and issue.name = '",name,"'
                                UNION SELECT 
                                pullrequest.author AS user,
                                pullrequest.positive AS positive,
                                pullrequest.negative AS negative,
                                pullrequest.id AS source
                                FROM
                                pullrequest 
                                WHERE pullrequest.owner = '",owner,"' and pullrequest.name = '",name,"'
                                UNION SELECT 
                                pullcomment.author AS user,
                                pullcomment.positive AS positive,
                                pullcomment.negative AS negative,
                                pullcomment.pull AS source
                                FROM
                                pullcomment 
                                WHERE pullcomment.owner = '",owner,"' and pullcomment.name = '",name,"'
                                UNION SELECT 
                                issuecomment.author AS user,
                                issuecomment.positive AS positive,
                                issuecomment.negative AS negative,
                                issuecomment.issue AS source
                                FROM
                                issuecomment 
                                WHERE issuecomment.owner = '",owner,"' and issuecomment.name = '",name,"'"))
  edges = fetch(rs, n=-1)
  return(edges)
}

# Metrics *****************************************************************************
get.status <- function(mydb, owner, name){
  rs = dbSendQuery(mydb, paste0("SELECT * FROM statusmetric WHERE  owner = '", owner, "' and name= '", name,"'"))
  status = fetch(rs, n=-1)
  return(status)
}

save.status <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("INSERT INTO statusmetric ( owner, name) VALUES ('",owner,"','",name,"')"))
}

update.status.create.network <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE statusmetric SET createNetwork = 1 WHERE  owner = '", owner, "' and name= '", name,"'"))
}

update.status.create.sentiment.text <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE statusmetric SET createSentimentText = 1 WHERE  owner = '", owner, "' and name= '", name,"'"))
}

update.status.create.network.negative <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE statusmetric SET createNetwork = NULL WHERE  owner = '", owner, "' and name= '", name,"'"))
}


#### status metrics
update.status.create.metric.network <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE statusmetric SET createMetricNetwork = 1 WHERE  owner = '", owner, "' and name= '", name,"'"))
}

update.status.create.metric.turnover <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE statusmetric SET createMetricTurnover = 1 WHERE  owner = '", owner, "' and name= '", name,"'"))
}

update.status.create.metric.sentiment <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE statusmetric SET createMetricSentiment = 1 WHERE  owner = '", owner, "' and name= '", name,"'"))
}

update.status.create.metric.count <- function(mydb, owner, name){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE statusmetric SET createMetricCount = 1 WHERE  owner = '", owner, "' and name= '", name,"'"))
}
#### status metrics

save.metric.project <-function(metrics, mydb){
  dbWriteTable(mydb, "metricproject", metrics, append = TRUE,row.names=FALSE)
}

save.metric.user <-function(metrics, mydb){
  dbWriteTable(mydb, "metricuser", metrics, append = TRUE,row.names=FALSE)
}

# Metrics sentiments ************************

update.text.sentiment.issue <- function(mydb, id, positive, negative){
  query = paste0("UPDATE issue SET negative = ",negative,", positive = ",positive," WHERE  id = '", id, "';")
  rsInsert = dbSendQuery(mydb, query)
}

update.text.sentiment.issue.comment <- function(mydb, id, positive, negative){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE issuecomment SET negative = ",negative,", positive = ",positive," WHERE  id = '", id, "';"))
}

update.text.sentiment.pull <- function(mydb, id, positive, negative){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE pullrequest SET negative = ",negative,", positive = ",positive," WHERE  id = '", id, "';"))
}

update.text.sentiment.pull.comment <- function(mydb, id, positive, negative){
  rsInsert = dbSendQuery(mydb, paste0("UPDATE pullcomment SET negative = ",negative,", positive = ",positive," WHERE  id = '", id, "';"))
}


#metrics turnover

update.metric.days.no.interaction <- function(mydb, user, owner, name, days_no_interaction){
  if(user != ""){
    user = dbEscapeStrings(mydb, user)
    query <- sprintf('UPDATE metricuser SET days_no_interaction = %f WHERE owner = "%s" and name = "%s" and user = "%s";', days_no_interaction, owner, name, user)
    rsInsert = dbSendQuery(mydb, query)
  }
  }
