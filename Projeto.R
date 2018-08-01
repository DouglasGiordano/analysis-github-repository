library(RMySQL)
calculaMetricas <- function(){
  mydb = dbConnect(MySQL(), user='root', password='root', dbname='github_extract', host='127.0.0.1')
  projects = getProjects(mydb)
  for (row in 1:nrow(projects)) {
    print(projects[row,'name'])
    name <- projects[row,'name']
    pulls <- getPull(projects[row,'id'], mydb)
    issues <- getIssue(projects[row,'id'], mydb)
    issuesComment <- getIssueComments(projects[row,'id'], mydb)
    pullsComment <- getPullComments(projects[row,'id'], mydb)
    commits <- getCommits(projects[row,'id'], mydb)
    messages <- rbind(pulls, issues,issuesComment,pullsComment)
    interactions <- rbind(pulls, issues,issuesComment,pullsComment, commits)
    users <- unique(as.numeric(messages$USER))
    edges <- getEdges(messages, users)
    lastProject <- max(interactions$CREATEDAT)
    users <- getMeasures(edges, users)
    users$turnover <- NULL
    users$n.pull <- NULL
    users$n.issue <- NULL
    users$n.commit <- NULL
    users$n.pull.c <- NULL
    users$n.issue.c <- NULL
    users$n.interactions <- NULL
    users$p <- NULL
    users$n <- NULL
    users$s <- NULL
    for (row in 1:nrow(users)) {
      user_id <- as.numeric(users[row, "users"])
      messages_user = filter(interactions, USER == user_id)
      users$turnover[row] <- isTurnover(messages_user, lastProject)
      senti <- getSenti(messages_user)
      users$p[row] <- as.numeric(senti$p)
      users$n[row] <- as.numeric(senti$n)
      users$s[row] <- as.numeric(senti$s)
      users$n.pull[row] <- nrow(filter(pulls, USER == user_id))
      users$n.issue[row] <- nrow(filter(issues, USER == user_id))
      users$n.issue.c[row] <- nrow(filter(issuesComment, USER == user_id))
      users$n.pull.c[row] <- nrow(filter(pullsComment, USER == user_id))
      users$n.commit[row] <- nrow(filter(commits, USER == user_id))
      users$n.interactions[row] <- nrow(filter(interactions, USER == user_id))
    }
    print(users)
    write.csv(as.matrix(users), file = paste("users_",name,".csv"))
  }
}

getPull <-function(project, mydb){
  rs = dbSendQuery(mydb, paste("SELECT ID, ID as PARENT,
                   user_id as USER,project_id as PROJECT,CREATEDAT,UPDATEDAT,HTMLURL as URL,n as N,p as P,s as S,sc as SC
                   FROM issue WHERE htmlurl LIKE ('%pull%') and project_id=",project, ";", sep=""))
  pulls = fetch(rs, n=-1)
  return(pulls)
}

getCommits <-function(project, mydb){
  rs = dbSendQuery(mydb, paste("SELECT 
    commit.SHA as ID,
                               commit.SHA as PARENT,
                               commit.AUTHOR_ID as USER,
                               commit.project_id as PROJECT,
                               commit.date_author_commit as CREATEDAT,
                               commit.date_commiter_commit as UPDATEDAT,
                               commit.URL as URL,
                               commit.n as N,
                               commit.p as P,
                               commit.s as S,
                               commit.sc as SC
                               FROM
                               commit
                               WHERE
                               PROJECT_ID =",project, ";", sep=""))
  commits = fetch(rs, n=-1)
  return(commits)
}


getIssue <-function(project, mydb){
  rs = dbSendQuery(mydb, paste("SELECT ID, ID as PARENT,
                   user_id as USER,project_id as PROJECT,CREATEDAT,UPDATEDAT,HTMLURL as URL,n as N,p as P,s as S,sc as SC
                               FROM issue WHERE htmlurl LIKE ('%issue%') and project_id=",project, ";", sep=""))
  issues = fetch(rs, n=-1)
  return(issues)
}

getIssueComments <- function(project, mydb){
  rs = dbSendQuery(mydb, paste("SELECT 
                   issue_comment.ID as ID,
                               issue.id as PARENT,
                               issue_comment.user_id as USER,
                               issue.project_id as PROJECT,
                               issue_comment.CREATEDAT as CREATEDAT,
                               issue_comment.UPDATEDAT as UPDATEDAT,
                               issue_comment.URL as URL,
                               issue_comment.n as N,
                               issue_comment.p as P,
                               issue_comment.s as S,
                               issue_comment.sc as SC
                               FROM
                               issue
                               INNER JOIN issue_comment ON issue_comment.ISSUE_ID = issue.ID
                               WHERE
                               htmlurl LIKE ('%issue%') and project_id=",project, ";", sep=""))
  comments = fetch(rs, n=-1)
  return(comments)
}


getPullComments <- function(project, mydb){
  rs = dbSendQuery(mydb, paste("SELECT 
                   issue_comment.ID as ID,
                   issue.id as PARENT,
                   issue_comment.user_id as USER,
                   issue.project_id as PROJECT,
                   issue_comment.CREATEDAT as CREATEDAT,
                   issue_comment.UPDATEDAT as UPDATEDAT,
                   issue_comment.URL as URL,
                   issue_comment.n as N,
                   issue_comment.p as P,
                   issue_comment.s as S,
                   issue_comment.sc as SC
                   FROM
                   issue
                   INNER JOIN issue_comment ON issue_comment.ISSUE_ID = issue.ID
                   WHERE
                   htmlurl LIKE ('%pull%') and project_id=",project, ";", sep=""))
  comments = fetch(rs, n=-1)
  return(comments)
}

getProjects <- function(mydb){
  rs = dbSendQuery(mydb, "SELECT id, name, language, forks, htmlurl,createdat, updatedat, size  
  FROM github_extract.project 
  where htmlurl IN('https://github.com/angular/angular.js',
                   'https://github.com/ANGULAR/ANGULAR-CLI',
                   'https://github.com/ANSIBLE/ANSIBLE',
                   'https://github.com/atom/atom',
                   'https://github.com/FORTAWESOME/FONT-AWESOME',
                   'https://github.com/CMS-SW/CMSSW',
                   'https://github.com/d3/d3',
                   'https://github.com/django/django',
                   'https://github.com/elastic/elasticsearch',
                   'https://github.com/electron/electron',
                   'https://github.com/impress/impress.js',
                   'https://github.com/ionic-team/ionic',
                   'https://github.com/jekyll/jekyll',
                   'https://github.com/jquery/jquery',
                   'https://github.com/KUBERNETES/KUBERNETES',
                   'https://github.com/laravel/laravel',
                   'https://github.com/moby/moby',
                   'https://github.com/DOCKER/DOCKER',
                   'https://github.com/nodejs/node',
                   'https://github.com/NPM/NPM',
                   'https://github.com/nwjs/nw.js',
                   'https://github.com/OPENSHIFT/ORIGIN',
                   'https://github.com/pallets/flask',
                   'https://github.com/rails/rails',
                   'https://github.com/RUST-LANG/RUST',
                   'https://github.com/SERVO/SERVO',
                   'https://github.com/TENSORFLOW/MODELS',
                   'https://github.com/TENSORFLOW/TENSORFLOW',
                   'https://github.com/TGSTATION/TGSTATION',
                   'https://github.com/twbs/bootstrap',
                   'https://github.com/VUEJS/VUE');")
  projects = fetch(rs, n=-1)
  return(projects)
}

getEdges <- function(messages, users){
  
  edges <- data.frame(source= numeric(0), 
                                 target= numeric(0), 
                                 message = numeric(0))
  message.id <- as.list(unique(messages$PARENT))
  #print(message.id)
  for(i in 1:length(message.id)){
    message <- filter(messages, PARENT == message.id[i])
    message_user <- unique(as.numeric(message$USER))
    edge_user <- NULL
    if(length(message_user) > 1){
      edge_user <- permutations(n=length(message_user),r=2,v=message_user,repeats.allowed=F)
      colnames(edge_user) <- c("source", "target")
      edge_user <- as.data.frame(edge_user)
      edge_user["message"] <- message.id[i]
      edges <- rbind(edge_user, edges)
      #print(edge_user)
    } 
  }
  return(edges)
}

getMeasures <- function(edges, users){
  g <- graph.data.frame(edges,directed=F, vertices = users)
  
  gt <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
  
  is_simple(gt)
  ops<-list(sym=c(FALSE), complex=c(FALSE))
  print("altera objeto usuarios")
  users <- data.frame(users)
  print("inicia calculo de métricas")
  users$evcent <- as.list(centralization.evcent(gt, directed=FALSE, scale=TRUE, options=ops)$vector)
  users$in.degree <- as.list(centr_degree(gt, mode=c("out"))$res)
  users$out.degree <- as.list(centr_degree(gt, mode=c("in"))$res)
  users$total.degree <- as.list(centr_degree(gt, mode=c("total"))$res)
  users$betweenness <- as.list(centr_betw(gt, directed=FALSE, normalized=TRUE)$res)
  users$transitivity <- transitivity(gt, type="local")
  users$coreness <- graph.coreness(gt, mode="all")
  return(users)
}

getMessagesForYear <- function(messages){
  dates <- format(as.Date(messages$CREATEAT, format="%d/%m/%Y"),"%m/%Y")
  messages$MONTH <- dates
  sum <- append(sum, mean(aggregate(ID ~ MONTH, messages, FUN="length")$ID))
  return(sum)
}

isTurnover <- function(messages, lastProject){
  first <- min(messages$CREATEDAT)
  last <- max(messages$CREATEDAT)
  darysnointeraction <- as.numeric(difftime(lastProject, as.Date(last), units = "days"))
  lengthInteraction <- as.numeric(diff(sort(as.Date(messages$CREATEDAT))))
  result <- NA
  if(length(lengthInteraction) > 3){
    result <- ((as.numeric(darysnointeraction) - mean(lengthInteraction))/sd(lengthInteraction))
  }
  ninteractions <- length(lengthInteraction)
  if(is.na(result)){
      return(FALSE)
  } else {
    weight <- filter(significance, n == ninteractions)
    if(nrow(weight) != 0){
      if(result > weight$value){
        return(TRUE)
      } else {
        return(FALSE)
      } 
    } else if(result > max(significance$value)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

getSenti <- function(messages, users){
  p <- mean(as.numeric(messages$P))
  n <- mean(as.numeric(messages$N))
  s <- mean(as.numeric(messages$S))
  return(data.frame(p, n, s))
}