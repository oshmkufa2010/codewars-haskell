module SimpleSQLEngine where

    import Text.Parsec
    import Data.Char
    import Control.Monad

    -- ENBF grammar of sql:
    -- query         =  select, from, [ { ws, join } ],  [ ws, where ] ;
    -- select        =  "SELECT ", column-id, [ { ", ", column-id } ] ;
    -- from          =  "FROM ", table-name ;
    -- join          =  "JOIN ", table-name, " on ", value-test ;
    -- where         =  "WHERE ", value-test ;
    -- value-test    =  value, comparison, value;
    -- column-id     =  table-name, ".", column-name ;
    -- table-name    = ? a valid SQL table name ? ;
    -- column-name   = ? a valid SQL column name ? ;
    -- value         =  column-id | const
    -- comparison    =  " = " | " > " | " < " | " <= " | " >= " | " <> " ;
    -- const         =  ? a number ? | ? a SQL single-quoted string ? ;
    -- ws            = " " | "\n" | ws, ws ;

    type ColumnName = String
    type TableName = String
    type Const = String

    data QueryNode = QueryNode SelectNode FromNode [JoinNode] WhereNode deriving (Show)
    data SelectNode = SelectNode [ColumnIDNode] deriving (Show)
    data FromNode = FromNode TableName deriving (Show)
    data JoinNode = JoinNode TableName ValueTestNode deriving (Show)
    data WhereNode = WhereNode ValueTestNode | EmptyWhereNode deriving (Show)
    data ValueTestNode = ValueTestNode Value Comparison Value deriving (Show)
    data ColumnIDNode = ColumnIDNode TableName ColumnName deriving (Show)
    data Value = ColumnIDValue ColumnIDNode | ConstValue Const deriving (Show)
    data Comparison = Eq | Gt | Lt | LtOrEq | GtOrEq | NotEq deriving (Show)

    stringWithoutCaseSensitive :: String -> Parsec String () String
    stringWithoutCaseSensitive = mapM $ \c -> liftM toLower $ satisfy $ (== toLower c) . toLower

    tableNameOrColumnNameP :: Parsec String () String 
    tableNameOrColumnNameP = many1 $ liftM toLower $ letter <|> digit <|> char '_'

    columnIDP :: Parsec String () ColumnIDNode
    columnIDP = do
        tableName <- tableNameOrColumnNameP        
        char '.'
        columnName <- tableNameOrColumnNameP
        return $ ColumnIDNode tableName columnName 

    wsP = many1 $ char ' ' <|> char '\n'

    queryP :: Parsec String () QueryNode
    queryP = do
        selectNode <- selectP
        wsP
        fromNode <- fromP

        (joinNodes, whereNode) <- option ([], EmptyWhereNode) $ wsP >> do
            joinNodes' <- sepEndBy joinP wsP
            whereNode' <- option EmptyWhereNode whereP
            return (joinNodes', whereNode')

        return $ QueryNode selectNode fromNode joinNodes whereNode

    selectP :: Parsec String () SelectNode
    selectP = do
        stringWithoutCaseSensitive "select "
        columnIDs <- sepBy1 columnIDP $ string ", "
        return $ SelectNode columnIDs
    
    fromP :: Parsec String () FromNode
    fromP = do
        stringWithoutCaseSensitive "from "
        tableName <- tableNameOrColumnNameP 
        return $ FromNode tableName
    
    joinP :: Parsec String () JoinNode
    joinP = do
        stringWithoutCaseSensitive "join "
        tableName <- tableNameOrColumnNameP
        char ' '
        stringWithoutCaseSensitive "on"
        char ' '
        valueTestNode <- valueTestP
        return $ JoinNode tableName valueTestNode
        
    whereP :: Parsec String () WhereNode
    whereP = do
        stringWithoutCaseSensitive "where "
        valueTestNode <- valueTestP
        return $ WhereNode valueTestNode

    valueTestP :: Parsec String () ValueTestNode 
    valueTestP = do
        value1 <- valueP 
        comparison <- between (char ' ') (char ' ') $ do
            (string "=" >> return Eq)
            <|> (string ">" >> return Gt)
            <|> (string "<" >> return Lt)
            <|> (string ">=" >> return GtOrEq)
            <|> (string "<=" >> return LtOrEq)
            <|> (string "<>" >> return NotEq)
        value2 <- valueP 
        return $ ValueTestNode value1 comparison value2
    
    valueP :: Parsec String () Value
    valueP = (constP >>= return . ConstValue) <|> do
        tableName <- tableNameOrColumnNameP
        char '.'
        columnName <- tableNameOrColumnNameP
        return $ ColumnIDValue $ ColumnIDNode tableName columnName

    constP :: Parsec String () Const
    constP = between (char '\'') (char '\'' >> notFollowedBy (char '\'')) $ do
        let strP = many $ noneOf ['\n', '.', '\'']
            in chainl1 strP $ do
                    quotedValue <- between (try $ string "''") (try $ string "''") strP
                    return $ \a b -> a ++ "'" ++ quotedValue ++ "'" ++ b

    sqlEngine :: [(String,[[(String,String)]])] -> String -> [[(String,String)]]
    sqlEngine database = execute where
      execute :: String -> [[(String,String)]]
      execute query = undefined
