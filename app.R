library(shiny)
library(promises)
library(tidyr)
library(shinyBS)
library(jsonlite)
library(stringi)
library(httr)
library(sjmisc)
library(DT)
library(shinyStorePlus)
library(log4r)
loggerDebug <- create.logger()
logfile(loggerDebug) <- './debugData.log'
level(loggerDebug) <- 'INFO'

source("slugify_charmap.R")
source("xapi.R")
source("ui.R")

# Define server logic
server <- function(input, output, session) {  # Include session argument here
  global_data = reactiveVal(data.frame())
  xapified_data = reactiveVal(data.frame())
  # Function to read uploaded CSV file
  data <- reactive({
    req(input$file)
    df <- rio::import(input$file$datapath,quote = "\"") %>% 
      dplyr::mutate_all(\(x) stringr::str_replace_all(x,"\\\"","\""))
    updateCheckboxGroupInput(session, "columns",
                             choices = names(df),
                             selected = NULL)
    global_data(df)
    xapified_data(df)
    updateAllDrops(df, session)
    return(df)
  })
   
  # Select all columns
  observeEvent(input$select_all_button, {
    if (input$select_all_button %% 2 == 1) {
      updateCheckboxGroupInput(session, "columns",
                               selected = names(data()))
    } else {
      updateCheckboxGroupInput(session, "columns",
                               selected = NULL)
    }
  })  
  observeEvent(input$lrs_send, {
    jsonified_all = xapified_data()
    pagesize = 10
    pages = ceiling(nrow(jsonified_all) / 10)
    outputs <- ""
    withProgress(message = 'Sending to LRS', value = 0, {
      for (j in 1:(pages)){
        jsonified<-jsonified_all[((j-1)*10+1):min(nrow(pages),j*10),]
        timestamps = jsonified$timestamp
        actors = jsonified$actor
        objects = jsonified$object
        contexts = jsonified$context
        results = jsonified$result
        verbs = jsonified$verb
        objs = list()
        for (i in 1:(nrow(jsonified))) {
          newobj = list()
          if(isFALSE(is.null(timestamps)) & !isTRUE(is.na(timestamps))){
            timestamp <- (timestamps[i])
            if (!is_empty(timestamp)) {
              newobj[["timestamp"]] <- (timestamp)
            }
          }
          if(isFALSE(is.null(actors)) & !isTRUE(is.na(actors))){
            actor <- (actors[i])
            if (!is_empty(actor)) {
              newobj[["actor"]] <- fromJSON(actor)
            }
          }
          
          if(isFALSE(is.null(objects)) & !isTRUE(is.na(objects))){
            object <- (objects[i])
            if (!is_empty(object)) {
              newobj[["object"]] <- fromJSON(object)
            }
          }
          
          if(isFALSE(is.null(verbs)) & !isTRUE(is.na(verbs))){
            verb <- (verbs[i])
            if (!is_empty(verb)) {
              newobj[["verb"]] <- fromJSON(verb)
            }
          }
          
          if(isFALSE(is.null(contexts)) & !isTRUE(is.na(contexts))){
            context <- (contexts[i])
            if (!is_empty(context)) {
              newobj[["context"]] <- fromJSON(context)
            }
          }
          
          if(isFALSE(is.null(results)) & !isTRUE(is.na(results))){
            result <- (results[i])
            if (!is_empty(result)) {
              newobj[["result"]] <- fromJSON(result)
            }
          }
          
          objs[[i]] <- newobj
        }
        parsed = toJSON(objs, flatten = F, auto_unbox = T, null = 'null')  
         
        printRes <- function(resultfromlrs) {
          if (resultfromlrs$status_code > 299) {
            outputs <- c(paste0("Batch ", j, ": "), content(resultfromlrs),  outputs)
            output$sent <- renderText("Something went wrong")
            output$GETresponse <- renderPrint(outputs)
            # error(loggerDebug, resultfromlrs)
            
          } else {
            output$sent <- renderText(paste0("Sent ", nrow(jsonified_all), " statements in ", pages," batches successfully"))
            # info(loggerDebug, "SENT OK")
            
          }
         
        }
        incProgress(1/pages, detail = paste("Sending batch", j))
        future_promise({sendToLRS(input$lrs_endpoint, input$lrs_user, input$lrs_password, parsed)}) %...>% printRes
      }
    })
    output$sent <- renderText(paste0("Sending ", nrow(jsonified_all), " statements in ", pages," batches"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      rio::export(data(), file, qmethod="escape")
    }
  )
  
  # Convert wide to long format
  observeEvent(input$convert2xapi, {
    
    actor_email <- input$actor_email
    actor_name <- input$actor_name
    verb_id <- input$verb_id
    verb_display <- input$verb_display
    activity_name <- input$activity_name
    activity_id <- input$activity_id
    result_response <- input$result_response
    result_scaled <- input$result_scaled
    result_raw <- input$result_raw
    result_success <- input$result_success
    result_completion <- input$result_completion
    result_duration <- input$result_duration
    context_id1 <- input$context_id1
    context_name1 <- input$context_name1
    context_id2 <- input$context_id2
    context_name2 <- input$context_name2
    timestamp <- input$timestamp
    
    global_data_2 = global_data()
    
    xapi_data = global_data_2 %>% dplyr::select(id = 1)
    
    for (i in 1:nrow(global_data_2)) {
    xapi_data[i,"actor_email"] <- ifelse(actor_email != "Empty", global_data_2[i,actor_email], NA);
    xapi_data[i,"actor_name"] <- ifelse(actor_name != "Empty", global_data_2[i,actor_name], NA);
    xapi_data[i,"verb_id"] <- ifelse(verb_id != "Empty", global_data_2[i,verb_id], NA);
    xapi_data[i,"verb_display"] <- ifelse(verb_display != "Empty", global_data_2[i,verb_display], NA);
    xapi_data[i,"activity_name"] <- ifelse(activity_name != "Empty", global_data_2[i,activity_name], NA);
    xapi_data[i,"activity_id"] <- ifelse(activity_id != "Empty", global_data_2[i,activity_id], NA);
    xapi_data[i,"result_response"] <- ifelse(result_response != "Empty", global_data_2[i,result_response], NA);
    xapi_data[i,"result_scaled"] <- ifelse(result_scaled != "Empty", global_data_2[i,result_scaled], NA);
    xapi_data[i,"result_raw"] <- ifelse(result_raw != "Empty", global_data_2[i,result_raw], NA);
    xapi_data[i,"result_success"] <- ifelse(result_success != "Empty", global_data_2[i,result_success], NA);
    xapi_data[i,"result_completion"] <- ifelse(result_completion != "Empty", global_data_2[i,result_completion], NA);
    xapi_data[i,"result_duration"] <- ifelse(result_duration != "Empty", global_data_2[i,result_duration], NA);
    xapi_data[i,"context_id1"] <- ifelse(context_id1 != "Empty", global_data_2[i,context_id1], NA);
    xapi_data[i,"context_name1"] <- ifelse(context_name1 != "Empty", global_data_2[i,context_name1], NA);
    xapi_data[i,"context_id2"] <- ifelse(context_id2 != "Empty", global_data_2[i,context_id2], NA);
    xapi_data[i,"context_name2"] <- ifelse(context_name2 != "Empty", global_data_2[i,context_name2], NA);
    xapi_data[i,"timestamp"] <- ifelse(timestamp != "Empty", lapply(global_data_2[i,timestamp],parsedate::parse_date), NA);
    }
    xapi_parsed <- xapi_data 
    
    
    jsonified = jsonify(xapi_parsed)  
    xapified_data(jsonified)
    
    output$xapi_dataset <- renderDT(jsonified, options = list(
      pageLength = 5, scrollX = TRUE
    ))
    output$additional_operations_output2 <- renderDT(jsonified, options = list(
      pageLength = 5, scrollX = TRUE
    ))  
    
  })
  observeEvent(input$pivot_button, {
    req(input$file, input$columns)
    df <- data()
    selected_cols <- input$columns
    df_long <- pivot_longer(df, cols = selected_cols, values_transform = as.character,
                            names_to = ifelse(is.na(input$name_column),"Variable",input$name_column) , 
                            values_to = ifelse(is.na(input$value_column),"Value",input$value_column))
  
    output$original_data <- renderDT(df_long, options = list(
      pageLength = 5, scrollX = TRUE
    ))
  
    global_data(df_long)
    updateAllDrops(global_data(), session)
    
    output$additional_operations_output <- renderDT(df_long, options = list(
      pageLength = 5, scrollX = TRUE
    ))
    
    output$additional_operations_output2 <- renderDT(df_long, options = list(
      pageLength = 5, scrollX = TRUE
    ))
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        rio::export(global_data(), file, qmethod="escape")
      }
    )
  })
  
  output$downloadDataxAPI <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      jsonified = xapified_data()
      rio::export(jsonified, file, qmethod="escape")
    }
  )
  # Display original data
  output$original_data <- renderDT(data(), options = list(
    pageLength = 5, scrollX = TRUE
  ))
  
  output$additional_operations_output <- renderDT(data(), options = list(
    pageLength = 5, scrollX = TRUE
  ))
  
  output$additional_operations_output2 <- renderDT(data(), options = list(
    pageLength = 5, scrollX = TRUE
  ))
  
  observeEvent(input$return_to_original, {
    req(input$file)
    output$original_data <- renderDT(data(), options = list(
      pageLength = 5, scrollX = TRUE
    ))
    global_data(data())
    output$additional_operations_output <- renderDT(global_data(), options = list(
      pageLength = 5, scrollX = TRUE
    ))    
    output$additional_operations_output2 <- renderDT(global_data(), options = list(
      pageLength = 5, scrollX = TRUE
    ))
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        rio::export(global_data(), file, qmethod="escape")
      }
    )
    output$downloadDataxAPI <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        rio::export(global_data(), file, qmethod="escape")
      }
    )
    updateAllDrops(global_data, session)
  })
  observeEvent(input$return_to_last, {
    req(input$file)
     
    xapified_data(global_data())
    output$additional_operations_output <- renderDT(global_data(), options = list(
      pageLength = 5, scrollX = TRUE
    ))    
    output$additional_operations_output2 <- renderDT(global_data(), options = list(
      pageLength = 5, scrollX = TRUE
    ))
    output$downloadDataxAPI <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        rio::export(global_data(), file, qmethod="escape")
      }
    )
  })
  
  appid = "application-shiny"
  setupStorage(appId = appid,inputs = TRUE)
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
