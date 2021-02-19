library(dplyr)
library(stringr)

query <- function(query_string, 
                  endpoint = "https://staging.gss-data.org.uk/sparql") {

  response <- httr::POST(url=endpoint,
                         httr::accept("application/json"),
                         body=list(query=query_string),
                         encode="form")
  
  parsed <- jsonlite::fromJSON(httr::content(response, encoding="UTF-8", "text"), simplifyVector = T)
  return(parsed$results$bindings)

}

update_raw_data <- function() {
  
  query_string <- "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX qb: <http://purl.org/linked-data/cube#>
    
    SELECT DISTINCT * {
      {
      SELECT * 
      WHERE {
        BIND(qb:DimensionProperty as ?type) .
        ?component a ?type .
        ?component rdfs:label ?label .
        }
      } UNION {
         BIND(qb:MeasureProperty as ?type) .
        ?component a ?type .
        ?component rdfs:label ?label .   
      } UNION {
         BIND(qb:AttributeProperty as ?type) .
        ?component a ?type .
        ?component rdfs:label ?label .   
      }
    }
  "

  result <- query(query_string)
  
  df <- purrr::map_df(result, function(x) {x$value})
  names(df) <- names(result)
  
  df <- df %>%
    filter(!str_detect(component, "http:\\/\\/gss-data\\.org\\.uk\\/data\\/gss_data\\/[\\w\\/-]*#dimension\\/\\w*")) %>%
    rowwise() %>%
    mutate(family = case_when(
      type == "http://purl.org/linked-data/cube#DimensionProperty" ~ tail(str_split(str_extract(component, "http:\\/\\/gss-data.org.uk\\/def\\/[\\w\\-]*\\/"), "/")[[1]], 2)[1],
      TRUE ~ NA_character_)
      )
  
  return(df)
}

y <- update_raw_data()

readr::write_csv(y, "./data-raw/components.csv")

