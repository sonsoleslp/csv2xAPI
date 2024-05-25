
getActor <- function(name = NA, email = NA) {
  actor = list(objectType = "Agent")
  if (!is.na(name) & !is.na(email)){
    actor[["mbox"]]<-paste0("mailto:",email)
    actor[["name"]]<-name
  } else {

    if (!is.na(name)){
      actor[["account"]] <- list();
      actor[["account"]][["homePage"]]<-"https://sonsoleslp.shinyapps.io/csv2xapi/";
      actor[["account"]][["name"]] <- name;
    }
    if (!is.na(email)){actor[["mbox"]]<-paste0("mailto:",email)}
  }
  actor
}

getVerb <- function(id,name="unknown") {
  verb2 = ifelse(is.na(id)| is.null(id),paste0("https://https://iledaueflrs.es/xapi/object/",slugify(name)),id)
  list(id = verb2, display = list(en = name))
}

getObject <- function(objectId = NA,objectName = "Unknown") {
  objectId2 = ifelse(is.na(objectId)| is.null(objectId),paste0("https://https://iledaueflrs.es/xapi/object/",slugify(objectName)),objectId)
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

getContext <- function (contextid1= NA, contextid2= NA, contextname1="Unknown", contextname2="Unknown"){
  contextid1a = ifelse(is.na(contextid1) | is.null(contextid1),paste0("https://https://iledaueflrs.es/xapi/object/",slugify(contextname1)),contextid1)
  contextid2a = ifelse(is.na(contextid2) | is.null(contextid2),paste0("https://https://iledaueflrs.es/xapi/object/",slugify(contextname2)),contextid2)
  list(contextActivities = list(parent = list(id = contextid1a, definition = list(name = list( en = contextname1))), 
                                grouping = list(id = contextid2a, definition = list(name = list( en  = contextname2)))))
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
    dplyr::select(any_of(c("timestamp", "actor", "verb", "object","result","context"))) %>% dplyr::ungroup()
  df <- df[,colSums(is.na(df))<nrow(df)]
  df
}