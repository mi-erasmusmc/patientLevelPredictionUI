
library(SqlRender)

runSQL <- function(file, cohortsDatabaseSchema,  cdmDatabaseSchema, connectionDetails) {
  
  # run the sql
  sql <- readSql(file)
  sql <- renderSql(sql,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   cohortsDatabaseSchema =  cohortsDatabaseSchema,
                   post_time = 30,
                   pre_time = 365)$sql
  sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  connection <- connect(connectionDetails)
  executeSql(connection, sql) 
  
  # get number of events per type
  sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
               "FROM @cohortsDatabaseSchema.rehospitalization",
               "GROUP BY cohort_definition_id")
  sql <- renderSql(sql, cohortsDatabaseSchema = cohortsDatabaseSchema)$sql
  sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  result<-querySql(connection, sql)
}
