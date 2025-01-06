
relationships_to_exclude <- c('Is a','Has clinical course', 'Followed by','Occurs after','Has status','Has Module','Has finding site', 'Has pathology','Has asso morph','Has occurrence','Has interpretation','Contained in panel','Has causative agent','Has due to')

find_descendants_and_mappings_for_a_given_concept_id <- function(concept_ids,file_label) {
  results_list <- list()
  
  connectionDetails <- createConnectionDetails(dbms="postgresql", 
                                               user="primed", password="primed", 
                                               server="localhost/primed", port=5432, 
                                               pathToDriver="./JdbcDrivers/")
  for(concept_id in concept_ids){
    print(paste("Concept ID:",concept_id))
    sql <- render(
      "SELECT
  a.concept_id	    AS target_concept_id,
  a.concept_code  AS target_concept_code,
  a.concept_name	AS	target_concept_name,
  a.vocabulary_id AS target_vocab_id,
  cr.relationship_id   AS relationship_id,
  d.concept_id         AS concept_id,
  d.concept_name       AS concept_name,
  d.concept_code       AS concept_code,
  d.concept_class_id   AS concept_class_id,
  d.vocabulary_id      AS concept_vocab_id
FROM primed.concept_relationship AS cr
  JOIN primed.concept AS a ON cr.concept_id_1 = a.concept_id
  JOIN primed.concept AS d ON cr.concept_id_2 = d.concept_id
WHERE 
  (a.concept_id = @concept_id OR
  a.concept_id in ( -- this subquery gets all descendants of the concept
    SELECT
      c.concept_id               AS descendant_concept_id
    FROM primed.concept_ancestor AS a
      JOIN primed.concept AS c ON a.descendant_concept_id = c.concept_id
    WHERE 
      a.ancestor_concept_id != a.descendant_concept_id AND 
      a.ancestor_concept_id = @concept_id
      )) 
  ORDER BY cr.concept_id_1
;",
      concept_id=concept_id)
    
    sql <- translate(sql,targetDialect = "postgresql")
    conn <- connect(connectionDetails)
    results <- querySql(conn,sql)
    results_unique <- results %>% filter(! RELATIONSHIP_ID %in% relationships_to_exclude & !duplicated(CONCEPT_ID))
    disconnect(conn)
    print(paste("Number of results returned:", nrow(results),"; Unique Concept IDs:", nrow(results_unique)))
    print("CONCEPT CLASSES vs VOCABULARIES:")
    print(table(CONCEPT_CLASS=results_unique$CONCEPT_CLASS_ID, VOCABULARY=results_unique$CONCEPT_VOCAB_ID))
    results_list[[concept_id]] <- results
  }
  # concatenate all results in results_list and write to file    
  results <- do.call(rbind,results_list) 
  results_unique <- results %>% filter(! RELATIONSHIP_ID %in% relationships_to_exclude & !duplicated(CONCEPT_ID))
    
  write.table(results,file=paste0(file_label,"_ALL_MAPPINGS.csv"),sep=",",quote=FALSE,row.names=FALSE)
  
  ## TO DO -- FILTER OUT THESE ROWS
  # 'Is a','Has clinical course', 'Followed by','Occurs after','Has status','Has Module','Has finding site', 'Has pathology','Has asso morph','Has occurrence','Has interpretation','Contained in panel','Has causative agent','Has due to'
  
  print(paste("Query results written to file:",paste0(file_label,"_ALL_MAPPINGS.csv")))
  print(paste("Unique Concept IDs written to file:",paste0(file_label,"_UNIQUE_CODES.csv")))
  
  for(vocab in unique(results_unique$CONCEPT_VOCAB_ID)){
    system(paste0("mkdir -p ",vocab))
    print(paste("Vocabulary:",vocab))
    for(concept_class in unique(results_unique$CONCEPT_CLASS_ID[results_unique$CONCEPT_VOCAB_ID==vocab])){

      print(paste("Vocabulary:",vocab,"; Concept Class:",concept_class))
      print(table(results_unique$CONCEPT_CLASS_ID[results_unique$CONCEPT_VOCAB_ID==vocab & results_unique$CONCEPT_CLASS_ID==concept_class]))
      # replace spaces and any special characters in concept_class
      concept_class <- gsub("[^[:alnum:]]", "_", concept_class)
      
      write.table(results_unique[which(results_unique$CONCEPT_VOCAB_ID==vocab & results_unique$CONCEPT_CLASS_ID==concept_class),
                               c("CONCEPT_ID","CONCEPT_NAME","CONCEPT_CLASS_ID","CONCEPT_VOCAB_ID")],
                  file=paste0(vocab,"/",file_label,"_",vocab,"_",concept_class,"_UNIQUE_CODES.csv"),sep=",",quote=FALSE,row.names=FALSE)
      print(paste0("File written: ",vocab,"/",file_label,"_",vocab,"_",concept_class,"_UNIQUE_CODES.csv"))
    }
  }
  
  return(results)
}
