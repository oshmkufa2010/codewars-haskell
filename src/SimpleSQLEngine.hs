module SimpleSQLEngine where

    import Text.Parsec
    import Data.Char
    import Control.Monad
    import Data.List
    import Data.Maybe
    import Data.Either

    -- ENBF grammar of sql:
    -- query         =  select, from, [ { ws, join } ],  [ ws, where ] ;
    -- select        =  "SELECT ", column-id, [ { ",", column-id } ] ;
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
    newtype SelectNode = SelectNode [ColumnIDNode] deriving (Show)
    newtype FromNode = FromNode TableName deriving (Show)
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

    wsP = many1 space

    queryP :: Parsec String () QueryNode
    queryP = do
        spaces
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
        stringWithoutCaseSensitive "select"
        wsP
        columnIDs <- sepBy1 columnIDP $ try $ spaces >> char ',' >> spaces
        return $ SelectNode columnIDs
    
    fromP :: Parsec String () FromNode
    fromP = do
        stringWithoutCaseSensitive "from"
        wsP
        tableName <- tableNameOrColumnNameP 
        return $ FromNode tableName
    
    joinP :: Parsec String () JoinNode
    joinP = do
        stringWithoutCaseSensitive "join"
        wsP
        tableName <- tableNameOrColumnNameP
        wsP
        stringWithoutCaseSensitive "on"
        wsP
        valueTestNode <- valueTestP
        return $ JoinNode tableName valueTestNode
        
    whereP :: Parsec String () WhereNode
    whereP = do
        stringWithoutCaseSensitive "where"
        wsP
        valueTestNode <- valueTestP
        return $ WhereNode valueTestNode

    valueTestP :: Parsec String () ValueTestNode 
    valueTestP = do
        value1 <- valueP 
        spaces
        comparison <- do
            (char '=' >> return Eq)
            <|> (char '>' >> option Gt (char '=' >> return GtOrEq))
            <|> (char '<' >> option Lt ((char '>' >> return NotEq) <|> (char '=' >> return LtOrEq)))
        spaces
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
        let strP = many $ noneOf ['\n', '\'']
            in chainl1 strP $ do
                    try (string "''")
                    return $ \a b -> a ++ "'" ++ b
    
    type Row = [(ColumnName, Const)]
    type Relation = [(TableName, Row)]
    type Table = (TableName, [Row])
    type Database = [Table] 

    findTable :: Database -> TableName -> Table
    findTable database tableName = fromJust $ find ((== tableName) . (map toLower) . fst) database

    findColumnValue :: Relation -> TableName -> ColumnName -> Const
    findColumnValue relation tableName columnName =
        let (_, row) = fromJust $ find ((== tableName) . (map toLower) . fst) relation
            in snd $ fromJust $ find ((== columnName) . (map toLower) . fst) row 
    
    getValue :: Relation -> Value -> Const
    getValue relation v =
        case v of
            ColumnIDValue (ColumnIDNode tableName columnName) -> findColumnValue relation tableName columnName
            ConstValue constValue -> constValue
    
    evalValueTest :: Relation -> ValueTestNode -> Bool
    evalValueTest relation (ValueTestNode v1 op v2) =
        case op of
            Eq -> v1' == v2'
            Gt -> v1' > v2'
            GtOrEq -> v1' >= v2'
            Lt -> v1' < v2'
            LtOrEq -> v1' <= v2'
            NotEq -> v1' /= v2'
        where
            v1' = getValue relation v1
            v2' = getValue relation v2

    evalJoin :: Database -> [Relation] -> JoinNode -> [Relation]
    evalJoin database relations (JoinNode tableName valueTestNode) = 
        do
            relation <- relations
            row <- rows
            guard $ evalValueTest (relation ++ [(tableName, row)]) valueTestNode
            return $ relation ++ [(tableName, row)]
        where (_, rows) = findTable database tableName

    -- type Row = [(ColumnName, Const)]
    -- type Relation = [(TableName, Row)]
    -- type Table = (TableName, [Row])
    -- type Database = [Table] 
    -- data SelectNode = SelectNode [ColumnIDNode] 
    -- data ColumnIDNode = ColumnIDNode TableName ColumnName

    evalWhere :: [Relation] -> WhereNode -> [Relation]
    evalWhere relations whereNode = 
        case whereNode of
            EmptyWhereNode -> relations
            WhereNode valueTestNode -> do
                relation <- relations
                guard $ evalValueTest relation valueTestNode
                return relation

    evalSelect :: [Relation] -> [ColumnIDNode] -> [Row]
    evalSelect relations columnNodes = do
        relation <- relations

        return $ do
            (ColumnIDNode selectTableName selectColumnName) <- columnNodes

            (tableName, row) <- relation

            (columnName, value) <- row

            guard $ map toLower columnName == selectColumnName && map toLower tableName == selectTableName 

            return (tableName ++ "." ++ columnName, value)

    evalQuery :: QueryNode -> Database -> [Row]
    evalQuery (QueryNode (SelectNode columnNodes) (FromNode primaryTableName) joinNodes whereNode) database =
        evalSelect relations' columnNodes
        where
            (_, tableRows) = findTable database primaryTableName
            relations = foldl (evalJoin database) (map (\row -> [(primaryTableName, row)]) tableRows) joinNodes
            relations' = evalWhere relations whereNode

    sqlEngine :: Database -> String -> [Row]
    sqlEngine database query = 
        case parse queryP "" query of
            Right queryNode -> evalQuery queryNode database
            Left e -> error $ show e
