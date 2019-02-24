Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-10.0.1')#location jre java
library(rJava)
.jinit('.')
.jaddClassPath('input/SentiStrength.jar')#location .jar sentistregth
jsentistrength <- .jnew('uk/ac/wlv/sentistrength/SentiStrength')
jType <- .jnew('java/lang/String', "sentidata")
jDataLocation <- .jnew('java/lang/String', "input/data_2015/")#location files input sentistregth
#jTypeReturn <- .jnew('java/lang/String', "explain")
jparams <- .jarray(list(jType, jDataLocation), contents.class = "java/lang/String")
.jcall(jsentistrength, 'V', 'initialise', jparams)


get.sentiment.metric <- function(text){
  tryCatch(
    {
      jtext <- .jnew('java/lang/String', text)
      result <- .jcall(jsentistrength, 'Ljava/lang/String;', 'computeSentimentScores', jtext)
      result = as.list(strsplit(result, ' ')[[1]])
      return(result)
    },
    error=function(error_message) {
      message("Error analysis sentiment: ",text," - ", error_message)
    }
  )

}

get.sentiment.median.metric <-function(interactions, users){
  mean.positive = c()
  mean.negative = c()
  received.negative = c()
  received.positive = c()
  for (row in 1:length(users)) {
    user = users[row]
    user.interactions = interactions[interactions$user == user,]
    source.interactions = unique(user.interactions$source)
    received.interactions = interactions[interactions$user != user & interactions$source %in% source.interactions,]
    
    mean.positive = c(mean.positive, mean(user.interactions$positive))
    mean.negative = c(mean.negative, mean(user.interactions$negative))
    received.negative = c(received.negative, mean(received.interactions$negative))
    received.positive = c(received.positive, mean(received.interactions$positive))
  }
  
  metrics = data.frame(matrix(NA, nrow = length(users), ncol = 1))
  metrics$user = users
  metrics$mean_positive = mean.positive
  metrics$mean_negative = mean.negative
  metrics$received_negative = received.negative
  metrics$received_positive = received.positive
  
  return(metrics[2:6])
}

get.sentiment.median.metric.last <-function(interactions, users){
  interactions$date  = gsub("[A-Za-z]"," " , interactions$date ,ignore.case = TRUE)
  interactions$date = substring(interactions$date, 1, 19)
  interactions$date = as.Date(as.POSIXct(interactions$date, "UTC", "%Y-%m-%d %H:%M:%S"))
  mean.positive = c()
  mean.negative = c()
  received.negative = c()
  received.positive = c()
  
  for (row in 1:length(users)) {
    user = users[row]
    user.interactions = interactions[interactions$user == user,]
    #filter for 30 days befores last interation
    end = max(user.interactions$date)
    start = end - 30
    user.interactions = interactions[interactions$date >= start & interactions$date <= end & interactions$user == user,]
    source.interactions = unique(user.interactions$source)
    received.interactions = interactions[interactions$user != user & interactions$source %in% source.interactions,]
    
    mean.positive = c(mean.positive, mean(user.interactions$positive))
    mean.negative = c(mean.negative, mean(user.interactions$negative))
    received.negative = c(received.negative, mean(received.interactions$negative))
    received.positive = c(received.positive, mean(received.interactions$positive))
  }
  
  metrics = data.frame(matrix(NA, nrow = length(users), ncol = 1))
  metrics$user = users
  metrics$mean_positive = mean.positive
  metrics$mean_negative = mean.negative
  metrics$received_negative = received.negative
  metrics$received_positive = received.positive
  
  return(metrics[2:6])
}