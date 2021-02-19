options(shiny.maxRequestSize = 30 * 1024 ^ 2)

BASE_URI = "http://statistics.gov.uk/"

N <- 1

incrementor <- function(x, N) {
  string = paste(x, N, sep="_")
  return(string)
}

pathify <- function(name) {
   stringr::str_replace(janitor::make_clean_names(name), "_", "-")
}

default_uris <- list(
  dimension = "{BASE_URI}{input$dataset_id}#dimension/{pathify(name)}",
  measure = "{BASE_URI}{input$dataset_id}#measure/{pathify(name)}"
)


ui <-
  fluidPage(# App title ----
            titlePanel("CSVWMapper"),
              inputPanel(
                  # Input: Select a file ----
                  fileInput(
                    "FileInput",
                    "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                
                checkboxGroupInput("header", "Header", list("Dataset has headers" = TRUE)),
                
                radioButtons(
                         "sep",
                         "Separator",
                         choices = c(
                           Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"
                         ),
                         selected = ","
                       ),
                
                radioButtons(
                         "quote",
                         "Quote",
                         choices = c(
                           None = "",
                           "Double Quote" = '"',
                           "Single Quote" = "'"
                         ),
                         selected = '"'
                       )
              ),
              
              fluidRow(column(6,
                              textInput(
                                inputId = incrementor("dataset_id", N),
                                label = "Dataset ID",
                                value = "example-title"
                              ),
                              # Data Table ----
                              DT::dataTableOutput("table")),
                       
                       column(6, uiOutput("info")))
            )

server <-
  function(input, output, session) {
    datasetInput <- eventReactive(input$FileInput, {
      infile <- input$FileInput
      readr::read_csv(infile$datapath, col_names = TRUE)
    })
    
    observeEvent(datasetInput(), {
      x <- names(datasetInput())
      updateSelectInput(session,
                        "inSelect",
                        choices = x,
                        selected = head(x, 1))
    })
    
    output$table <- DT::renderDataTable(datasetInput(), rownames=FALSE)
    
    output$info <- renderUI({
      name <- names(datasetInput())
      purrr::map(name,
                 function(name) {
                   panel <- inputPanel(
                     textInput(
                       inputId = incrementor("title", N),
                       label = "Title",
                       value = name
                     ),
                     textInput(
                       inputId = incrementor("name", N),
                       label = "Name",
                       value = janitor::make_clean_names(name)
                     ),
                     selectInput(
                       selectize = FALSE,
                       inputId = incrementor("component_type", N),
                       label = "Component Type",
                       choices = c("Dimension", "Measure", "Attribute")
                     ),
                     selectInput(
                       selectize = FALSE,
                       inputId = incrementor("datatype", N),
                       label = "Datatype",
                       selected = "string",
                       choices = list(
                         common = c(
                           "string",
                           "decimal",
                           "integer",
                           "date",
                           "datetime",
                           "time",
                           "boolean"
                         ),
                         other = c(
                           "number",
                           "binary",
                           "any",
                           "xml",
                           "html",
                           "json",
                           "anyAtomicType",
                           "anyURI",
                           "base64Binary",
                           "dateTimeStamp",
                           "decimal",
                           "long",
                           "int",
                           "short",
                           "byte",
                           "nonNegativeInteger",
                           "positiveInteger",
                           "unsignedLong",
                           "unsignedInt",
                           "unsignedShort",
                           "unsignedByte",
                           "nonPositiveInteger",
                           "negativeInteger",
                           "double",
                           "duration",
                           "dayTimeDuration",
                           "yearMonthDuration",
                           "float",
                           "gDay",
                           "gMonth",
                           "gYear",
                           "gYearMonth",
                           "hexBinary",
                           "QName",
                           "normalizedString",
                           "token",
                           "language",
                           "Name",
                           "NMTOKEN"
                         )
                       )
                     ),
                     textInput(
                       inputId = incrementor("component_uri", N),
                       label = "Component URI",
                       value = glue::glue("{BASE_URI}"),
                       placeholder = glue::glue(default_uris$dimension)
                     ),
                     textInput(
                       inputId = incrementor("value_uri", N),
                       label = "Value URI",
                       placeholder = glue::glue("{BASE_URI}")
                     ),
                     checkboxInput(
                       inputId = incrementor("has_codelist", N), 
                       label = "This column has a codelist", 
                       value = FALSE
                     ),
                     conditionalPanel(
                       condition = glue::glue("input.has_codelist_{N} == true"),
                       textInput(
                         inputId = incrementor("codelist_filename", N),
                         label = "Codelist Filename",
                         placeholder = glue::glue("{BASE_URI}")
                       ),
                       textInput(
                         inputId = incrementor("codelist_uri", N),
                         label = "Codelist URI",
                         placeholder = glue::glue("{janitor::make_clean_names(name)}.csv")
                       )
                     )
                   )
                   N <<- N + 1
                   return(panel)
                   })
    })
    
  }

shinyApp(ui, server)