
getActor <- function(name = NA, email = NA) {
  actor = list(objectType = "Agent")
  if (!is.na(actor)){actor[["name"]]<-name}
  if (!is.na(email)){actor[["mbox"]]<-paste0("mailto:",email)}
  actor
}

getVerb <- function(id,name) {
  list(id = id, display = list(en = name))
}

getObject <- function(objectId,objectName) {
  objectId2 = ifelse(is.na(objectId),paste0("https://https://iledaueflrs.es/xapi/object/",slugify(objectName)),objectId)
  list(id=objectId2, objectType = "Activity", definition=list(name=list(en = objectName)))
}

getResult <- function(completion = NA, success = NA, response = NA, scaled = NA, raw = NA, duration = NA){
  result = list()
  if (!is.na(completion)){result[["completion"]] <- as.logical(completion)}
  if (!is.na(success)){result[["success"]] <- as.logical(success)}
  if (!is.na(response)){result[["response"]] <- response}
  if (!is.na(scaled)|!is.na(raw)){
    result[["score"]] <- list()
    if (!is.na(scaled) & !is.na(as.numeric(scaled))){result[["score"]][["scaled"]] <- as.numeric(scaled)}
    if (!is.na(raw) & !is.na(as.numeric(raw))){result[["score"]][["raw"]] <- as.numeric(raw)}
  }
  if (!is.na(duration)){result[["duration"]] <-  as.character(duration)}
  result
}

getContext <- function (contextid1, contextid2, contextname1="Unknown", contextname2="Unknown"){
  contextid1a = ifelse(is.na(contextid1),paste0("https://https://iledaueflrs.es/xapi/object/",slugify(contextname1)),contextid1)
  contextid2a = ifelse(is.na(contextid1),paste0("https://https://iledaueflrs.es/xapi/object/",slugify(contextname2)),contextid2)
  list(contextActivities = list(parent = list(id = contextid1, definition = list(name = list( en = contextname1))), 
                                grouping = list(id = contextid2, definition = list(name = list( en  = contextname2)))))
}

getxAPIStatement <- function(actor,verb,object, context, result) {
  list(actor=actor, verb=verb, object=object, context=context, result=result)
}

sendToLRS<- function(endpoint,user,password,body) {
  res = httr::POST(endpoint,
                   add_headers(`X-Experience-API-Version` = "1.0.1"), 
                   encode="raw" ,body=(body),
                   config = authenticate(user,password),
                   content_type("application/json") )
  # writeLines(body, "file_path.txt")
  
  
  res
}

jsonify <- function(xapidata) {
  df = xapidata  %>% dplyr::rowwise() %>%  
    dplyr::mutate(timestamp =  parsedate::format_iso_8601(timestamp),
                  actor = toJSON(getActor(actor_name, actor_email),auto_unbox = T),
                  object = toJSON(getObject(activity_id, activity_name),auto_unbox = T),
                  verb = toJSON(getVerb(verb_id,verb_display),auto_unbox = T),
                  result = toJSON(getResult(result_completion,result_success,result_response,result_scaled,result_raw,result_duration),auto_unbox = T),
                  context = toJSON(getContext(context_id1,context_id2,context_name1,context_name2),auto_unbox = T)
    ) %>% 
    dplyr::select(any_of(c("timestamp", "actor", "verb", "object","result","context")))
  df <- df[,colSums(is.na(df))<nrow(df)]
  df
}