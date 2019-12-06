"""Copies values between SQLite tables

Copies variables from one sqlite table column and puts them into a column in \
another table, which could be in a different database and could have a different name. \
Columns can be renamed. The mapping is defined in an input yaml configuration file. \
Each table needs an index column.

$$$$$$ currently assumes that the yaml file and databases are in the working directory. \
This should be altered. Might need to specify directories in the links yaml file.

"""
#region Imports and settings 
import os
import sys
import yaml  # for reading in parameter files
import sqlite3

# Global Variables
Links = []
ProjectDir =''
FileDb = "Sparrow.db" # can be overidden in configuration file
modelHasInitialized = False
modelHasRun = False
closeInterpreterOnExit = False

# function definitions

def GetColumnNames(conn,tablename,schemaname='main'):
    '''get list of columns in a sqlite table'''
    sqlstring =  'PRAGMA '+ schemaname + '.TABLE_INFO(' + tablename + ')'
    curs = conn.cursor()
    curs.execute(sqlstring)
    ColumnNameList = [tup[1] for tup in curs.fetchall()]
    return ColumnNameList # list of column names

def GetColumnTypes(conn,tablename,schemaname='main'):
    '''get list of data type in a sqlite table table'''
    sqlstring =  'PRAGMA '+ schemaname + '.TABLE_INFO(' + tablename + ')'
    curs = conn.cursor()
    curs.execute(sqlstring)
    ColumnTypes = [tup[2] for tup in curs.fetchall()]
    return ColumnTypes # list of column names

def SQLexecute(conn,query):
    '''execute sqlite query and report error if it fails'''
    try:
        conn.execute(query)
        conn.commit()
    except Exception as err:
        conn.close()
        print('SQL query failed. Exiting. Query string and exception error are:\n %s\nError: %s' % (query, str(err)))
        sys.exit(0)

#pylint:disable=E1101     
# above disables the pylint no member error, as it was coming up inappropriately

#endregion

#************************************************************
#region     initialise
#************************************************************
def initialize(config_file):
    '''Implements the BMI initialize function'''
    global Links, modelHasInitialized, closeInterpreterOnExit

    # Read control and parameter files
    InitialiseDir=''
    Linksyaml = 'SqliteAdapterConfig.yaml'
    if len(config_file) > 0:
        Linksyaml = config_file

    # Read parameters as yaml file
    try:
        with open(InitialiseDir + Linksyaml, 'r') as f:
            config = yaml.safe_load(f)
            if isinstance(config, list):
                Links = config
            else:
                Links = config["Links"]
                # Check for interpreter shutdown instruction.
                # This is strictly a testing thing
                try:
                    if config["CloseInterpreterOnExit"]:
                        closeInterpreterOnExit = True
                except:
                    pass
    except:
        return -1
    modelHasInitialized = True
    # permit the program to run multiple times with fresh initialization
    modelHasRun = False
    return 0
#endregion

#************************************************************
#region           update
#************************************************************
def update(dt):
    '''Implements the BMI update function.'''
    global modelHasInitialized, modelHasRun, Links

    if modelHasRun or not modelHasInitialized:
        return

    for link in Links: # each link has mapping from one table to one other table. Can be multiple variables.
        #link = Links[0]
        conn = sqlite3.connect(link['Sourcedb']) #

        Sourcedb = link['Sourcedb']
        Destinationdb = link['Destinationdb']
        SourceTable = link['SourceTable']
        DestinationTable = link['DestinationTable']
        SourceKeyColumn = link['SourceKeyColumn']
        DestinationKeyColumn = link['DestinationKeyColumn']  

        if Sourcedb == Destinationdb:
            DestinationSchemaName = 'main'
        else:    # destination database is different from the source database
            if not os.path.isfile(Destinationdb): # create destination database if it doesn't exist already
                conntemp = sqlite3.connect(Destinationdb); conntemp.close()# creates a db if the connection can't be made.   
                print('Warning: created a destination database ' + Destinationdb + ' because it does not exist: ')
            DestinationSchemaName = 'DestDB' # Set up an schema name with arbitrary name DestDB for a database to be attached
            sqlstring = "attach database '" + Destinationdb + "' as " + 'DestDB'
            SQLexecute(conn,sqlstring) 
        DestinationTableFull =  DestinationSchemaName + '.' + DestinationTable # destination table name with schema name prepended

        SourceTableColumnNames = GetColumnNames(conn,SourceTable)
        DestinationTableColumnNames = GetColumnNames(conn, DestinationTable, schemaname=DestinationSchemaName) # list of column names already in the destination table

        SourceTableTypes = GetColumnTypes(conn,SourceTable)

        if  Sourcedb == Destinationdb and SourceTable == DestinationTable: #Not appropriate to have source table same as destination table.
            print (" Source and destination tables are the same. Sourcedb: " + Sourcedb + ' .SourceTable: ' +  SourceTable)
            sys.exit(0)

        # create destination table if it doesn't exist, and populate it with the key column from the source table.
        sqlstring = 'create table if not exists ' + DestinationTableFull + \
                ' as select ' + SourceKeyColumn + ' as ' + DestinationKeyColumn + ' from ' + SourceTable
        SQLexecute(conn,sqlstring)

        # create index for destination table if it doesn't exist
        sqlstring = 'create index if not exists ' + DestinationSchemaName +'.'+ DestinationKeyColumn + \
                ' on ' + DestinationTable + ' ( ' + DestinationKeyColumn + ' )'
        SQLexecute(conn,sqlstring)

        # copy each variable across to the new table
        for variable in link['Variables']:
            SourceColumn = variable['SourceColumn']
            # check that the source column exists
            if SourceColumn not in SourceTableColumnNames:
                print('Column ' + SourceColumn + ' does not exist in source table ' + SourceTable + '. Exiting programme')
                sys.exit(0) 
            DestinationColumn = variable['DestinationColumn']
            # create the destination column if it doesn't already exist in the destination table
            if DestinationColumn not in DestinationTableColumnNames:
                typecol = SourceTableTypes[SourceTableColumnNames.index(SourceColumn)]
                sqlstring = 'alter table ' + DestinationTableFull + ' add column ' + DestinationColumn + ' ' + typecol
                SQLexecute(conn,sqlstring)

            sqlselstring = '(select ' + SourceColumn + ' from ' + SourceTable + ' where ' + SourceTable + \
                        '.' + SourceKeyColumn + ' = ' + DestinationTableFull + '.' + DestinationKeyColumn + ')'      
            sqlstring = 'update ' + DestinationTableFull + ' set ' + DestinationColumn + ' = ' + sqlselstring + \
                    ' where exists ' + sqlselstring
            SQLexecute(conn,sqlstring)
   
        conn.close()
        # finished processing link
#endregion

if __name__ == "__main__":
    initialize(sys.argv)
    update(0)
