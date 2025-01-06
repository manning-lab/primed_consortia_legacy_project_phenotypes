
find_drugs_by_ATC_class_concept_id <- function(class_concept_id) {
  connectionDetails <- createConnectionDetails(dbms="postgresql", 
                                               user="primed", password="primed", 
                                               server="localhost/primed", port=5432, 
                                               pathToDriver="./JdbcDrivers/")
  
  sql <- render("SELECT  d.concept_id, d.concept_code, d.concept_name, 
        d.domain_id, d.concept_class_id, d.standard_concept,
        d.vocabulary_id,
        v.vocabulary_name
 FROM primed.concept_ancestor ca
        JOIN primed.concept d on d.concept_id = ca.descendant_concept_id
        JOIN primed.concept a on a.concept_id = ca.ancestor_concept_id
        JOIN primed.vocabulary v on d.vocabulary_id = v.vocabulary_id
 WHERE  ca.ancestor_concept_id = @class_concept_id
   AND  a.vocabulary_id = 'ATC'
   AND  d.domain_id = 'Drug'
   AND  (getdate() >= d.valid_start_date)
   AND  (getdate() <= d.valid_end_date);",
                class_concept_id=class_concept_id)
  #print(sql)
  sql <- translate(sql,targetDialect = "postgresql")
  conn <- connect(connectionDetails)
  results <- querySql(conn,sql)
  disconnect(conn)
  return(results)
}

brand_name_search <- function(x) {
  #  print(paste("Searching for brand names of",x["CONCEPT_NAME"], " with ATC code ", x["CONCEPT_CODE"]," and OMOP Concept ID ",x["CONCEPT_ID"]))
  to_return <- find_drugs_by_ATC_class_concept_id(x["CONCEPT_ID"]) %>% 
    filter(grepl("Brand",CONCEPT_CLASS_ID)) %>% #& VOCABULARY_ID=="RxNorm") %>% 
    mutate(brand_name = str_extract(CONCEPT_NAME, "\\[.*\\]"),
           brand_name = str_replace_all(brand_name,"\\[",""),
           brand_name = str_replace_all(brand_name,"\\]",""),
           brand_name = str_to_lower(brand_name),
           ATC_5th=x["CONCEPT_CODE"],
           ATC_name=x["CONCEPT_NAME"]) %>%
    select(ATC_5th,ATC_name,brand_name,CONCEPT_ID,CONCEPT_CODE,CONCEPT_NAME,DOMAIN_ID,CONCEPT_CLASS_ID,STANDARD_CONCEPT,VOCABULARY_ID,VOCABULARY_NAME)
  return(to_return)
}