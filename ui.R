updateAllDrops <- function(global_data, session){
  updateSelectInput(session, 'timestamp', choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, 'actor_email', choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "actor_name", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "verb_id", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "verb_display", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "activity_name", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "activity_id", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "result_response", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "result_scaled", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "result_raw", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "result_success", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "result_completion", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "result_duration", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "context_id1", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "context_name1", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "context_id2", choices= c("Empty",names(global_data)), selected = "Empty")
  updateSelectInput(session, "context_name2", choices= c("Empty",names(global_data)), selected = "Empty")
}

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$script("
      
    "),
    tags$style(HTML("
         .panel-heading .panel-title a.collapsed:after {
      transform: rotate(180deg);
      transition: .5s ease-in-out;
    }
    .panel-heading .panel-title a:after {
      content:'⏶';
      text-align: right;
      float:right;
      transition: .5s ease-in-out;
    }
    .panel-heading .panel-title a:not([class]):after {
      transform: rotate(180deg);
    }
    #select_all_button {
      float: right;
    }
     
        "))),
  titlePanel(
    div(
      img(src = "isila-logo.jpg", height = "40px", style = "margin-right: 10px;vertical-align:sub;"),
      "csv2xAPI"
    ),
  ),
  tabsetPanel(
    tabPanel("Import data",
             sidebarLayout(
               sidebarPanel( 
                            fileInput("file", "Choose CSV File",
                                      accept = c(".csv",".xlsx",".xls",".tsv",".RDS",".sav",".psv",".feather",".parquet")),
                            bsButton("select_all_button", "Select All", style = "primary"),
                            checkboxGroupInput("columns", "Select Columns to Transform:",
                                               choices = NULL),
                            textInput("name_column", "Name column:", value = "Variable"),
                            textInput("value_column", "Value column:", value = "Value"),
                            downloadLink("downloadData", "Download", class="btn btn-warning"),
                            bsButton("return_to_original", "Return to original", style = "info"),
                            bsButton("pivot_button", "Transform to Long Format", style = "success")
               ),
               mainPanel(width = 9,
                         dataTableOutput("original_data")
               )
             )
    ),
    tabPanel("Convert to xAPI",
             sidebarLayout(
               sidebarPanel(
                 selectInput("timestamp", label = "Timestamp", choices = list("Empty")),
                 bsCollapse(id = "collapseExample", open = "Actor",
                            bsCollapsePanel("Actor", 
                                            selectInput(inputId = "actor_name", label = "Name", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "actor_email", label = "Email", choices =  list("Empty"), selected = "Empty"),
                                            style = "info"),
                            bsCollapsePanel("Verb", 
                                            selectInput(inputId = "verb_id", label = "Verb id", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "verb_display", label = "Verb name", choices =  list("Empty"), selected = "Empty"),
                                            style = "info"),
                            bsCollapsePanel("Object",  
                                            selectInput(inputId = "activity_name", label = "Object name", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "activity_id", label = "Object id", choices =  list("Empty"), selected = "Empty"),
                                            style = "info"),
                            bsCollapsePanel("Result",  
                                            selectInput(inputId = "result_response", label = "Response", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "result_scaled", label = "Scaled score", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "result_raw", label = "Raw score", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "result_success", label = "Success", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "result_completion", label = "Complertion", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "result_duration", label = "Duration", choices =  list("Empty"), selected = "Empty"),
                                            style = "info"), 
                            bsCollapsePanel("Context",  
                                            selectInput(inputId = "context_id1", label = "Activity parent id", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "context_name1", label = "Activity parent name", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "context_id2", label = "Context id", choices =  list("Empty"), selected = "Empty"),
                                            selectInput(inputId = "context_name2", label = "Context name", choices =  list("Empty"), selected = "Empty"),
                                            style = "info")),
                 downloadLink("downloadDataxAPI", "Download", class="btn btn-warning"),
                 bsButton("return_to_last", "Return to previous state", style = "info"),
                 bsButton("convert2xapi", "Convert columns", style = "success")
               ),
               mainPanel(
                 h4("Data to convert"),
                 dataTableOutput("additional_operations_output"),
                 h4("Result"),
                 dataTableOutput("xapi_dataset")
               )
             )
    ),
    tabPanel("Send to LRS",
             sidebarLayout(
               sidebarPanel(
                 initStore(),
                 h4("LRS Auth"),
                 textInput("lrs_endpoint", "Endpoint:"),
                 textInput("lrs_user", "Client name:"),
                 textInput("lrs_password", "Client secret:"),
                 bsButton("lrs_send", "Send to LRS", style = "success")
               ),
               mainPanel(
                 h4("Server response"),
                 textOutput('sent'),
                 verbatimTextOutput('GETresponse'),
                 h4("Sending the following statements"),
                 dataTableOutput("additional_operations_output2")
               )
             )
    )
  )
)

