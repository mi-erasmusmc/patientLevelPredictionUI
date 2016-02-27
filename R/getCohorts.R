
# to be finalized still with correct SQL. Might be good to return also list that can be used in the dropdown boxes

getCohorts<-function(cohortsDatabaseSchema,  cdmDatabaseSchema, connectionDetails) {
    # all the cohorts from the database
    sql <- paste("ADD correct SQL here")
    sql <- renderSql(sql, cohortsDatabaseSchema = cohortsDatabaseSchema)$sql
    sql <- translateSql(sql, targetDialect = connectionDetails$dbms)$sql
    result<-querySql(connection, sql)
}
