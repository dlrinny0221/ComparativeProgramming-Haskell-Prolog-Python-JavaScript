{-
 - Program: hw5.hs
 - Authors: Yunyi Ding and Brian Lin
 - On this homework, we worked together for 20 hours,
 - Yunyi worked independently for 10 hours,
 - and Brian worked independently for 10 hours.
-}

-- Necessary imports
import Control.Applicative ((<$>),liftA,liftA2)
import Data.Map
import Data.Maybe
import Control.Monad
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P


--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                   -- e.g. x = 23
               | If Expression Statement Statement -- if e then s1 else s2 end
               | While Expression Statement        -- while e do s end
               | Sequence Statement Statement      -- s1; s2
               | Skip                              -- no-op
               | For Variable Expression Expression Statement

-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = (P.reservedOp lexer name) >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

varParser :: Parser Variable
varParser = P.identifier lexer

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
          <|> do
              P.reserved lexer "for"
              var <- varParser
              P.reserved lexer "in"
              from <- exprParser
              P.reserved lexer "to"
              to <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (For var from to body)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)

-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case (parse stmtParser "" input) of
    Left err -> print err
    Right x  -> print x

-- Parse and run the given WHILE programs
run :: (Show v) => (Parser n) -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case (parse parser "" input) of
    Left err -> print err
    Right x  -> print (eval x empty)

-- Question 1
instance Show Value where
    show (IntVal i) = show i
    show (BoolVal b) = show b

instance Show Op where
    show Plus         = " + "
    show Minus        = " - "
    show Times        = " * "
    show GreaterThan  = " > "
    show Equals       = " = "
    show LessThan     = " < "

instance Show Expression where
    show (Var v)          = v
    show (Val v)          = show v
    show (BinOp o e1 e2)  = (show e1) ++ (show o) ++ (show e2)
    show (Assignment v e) = v ++ " = " ++ (show e)
    

instance Show Statement where
    show (Expr e)         = show e
    show (If e s1 s2)     = "if " ++ (show e) ++ " then " ++ (show s1) ++ " else " ++ (show s2) ++ " end"
    show (While e s)      = "while " ++ (show e) ++ " do " ++ (show s) ++ " end"
    show (Sequence s1 s2) = (show s1) ++ "; " ++ (show s2)
    show Skip             = "no-op"
    show (For v e1 e2 s)  = "for " ++ (show v) ++ " in " ++ (show e1) ++ " to " ++ (show e2) ++ " do " ++ (show s) ++ " end"

-- Question 2
evalE :: Expression -> Store -> (Value, Store)
evalE (Var x) s = (fromJust (Data.Map.lookup x s), s)
evalE (Val v) s = (v, s)
evalE (BinOp o a b) s = (applyOp o a' b', s'')
    where (a', s')  = evalE a s
          (b', s'') = evalE b s'
evalE (Assignment x e) s = (e', insert x e' s')
    where (e', s') = evalE e s

-- Question 3
evalS :: Statement -> Store -> Store
evalS (Expr e) s         = snd (evalE e s)
evalS (If e s1 s2) s     = case (evalE e s) of
                            (BoolVal True, s')  -> evalS s1 s'
                            (BoolVal False, s') -> evalS s2 s'
                            _                   -> error "Condition must be BoolVal"
evalS w@(While e s1) s = case (evalE e s) of
                          (BoolVal True, s')  -> let s'' = evalS s1 s' in evalS w s''
                          (BoolVal False, s') -> s'
                          _                  -> error "Condition must be a BoolVal"
evalS (Sequence s1 s2) s = evalS s2 (evalS s1 s)
evalS Skip s             = s
evalS (For v e1 e2 s1) s = evalS (While e' s1') s'
    where (_, s') = evalE (Assignment v e1) s
          e'  = BinOp LessThan (Var v) e2'
          e2' = BinOp Plus e2 (Val (IntVal 1))
          s1' = Sequence s1 s2'
          s2' = Expr (Assignment v (BinOp Plus (Var v) (Val (IntVal 1))))

-- Question 4
evalE_maybe :: Expression -> Store -> Maybe (Value, Store)
evalE_maybe (Var x) s          = case (Data.Map.lookup x s) of
                                  Nothing -> Nothing
                                  Just v -> return (v, s)
evalE_maybe (Val v) s          = return (v, s)
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Assignment x e) s = do (e', s') <- evalE_maybe e s
                                    return (e', insert x e' s')

evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe (Expr e) s         = case (evalE_maybe e s) of
                                  Nothing -> Nothing
                                  Just (v, s') -> return s'
evalS_maybe (If e s1 s2) s     = case (evalE_maybe e s) of
                                  Nothing -> Nothing
                                  Just (BoolVal True, s')  -> evalS_maybe s1 s'
                                  Just (BoolVal False, s') -> evalS_maybe s2 s'
                                  _                        -> Nothing
evalS_maybe w@(While e s1) s   = case (evalE_maybe e s) of
                                   Nothing -> Nothing
                                   Just (BoolVal True,s')  -> case (evalS_maybe s1 s') of
                                                                Just s'' -> evalS_maybe w s''
                                                                Nothing  -> Nothing
                                   Just (BoolVal False,s') -> return s'
                                   _                       -> Nothing
evalS_maybe (Sequence s1 s2) s = case (evalS_maybe s1 s) of
                                   Nothing -> Nothing
                                   Just s' -> evalS_maybe s2 s'
evalS_maybe Skip s             = return s
evalS_maybe (For v e1 e2 s1) s = evalS_maybe (While e' s1') s'
    where Just (_, s') = evalE_maybe (Assignment v e1) s
          e'  = BinOp LessThan (Var v) e2'
          e2' = BinOp Plus e2 (Val (IntVal 1))
          s1' = Sequence s1 s2'
          s2' = Expr (Assignment v (BinOp Plus (Var v) (Val (IntVal 1))))

-- Uncomment the following function for question #5 and #6
-- Parse and run the given WHILE programs using monads
runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing

-- Question 5
newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- (runImperative b) s
                                   runImperative (f v1) s1)
    fail _ = Imperative (\s -> Nothing)

instance Functor Imperative where
    fmap f a = a >>= \v -> return $ f v

evalE_monad :: Expression -> Imperative Value
evalE_monad (Var x) = getVar x
evalE_monad (Val v) = return v
evalE_monad (BinOp o a b) = do a' <- evalE_monad a
                               b' <- evalE_monad b
                               return (applyOp o a' b')
evalE_monad (Assignment x e) = do v <- (evalE_monad e)
                                  setVar x v

evalS_monad :: Statement -> Imperative ()
evalS_monad (Expr e)         = void $ (evalE_monad e) :: Imperative ()
evalS_monad (If e s1 s2)     = do v <- evalE_monad e
                                  case (v) of
                                   BoolVal True  -> void $ (evalS_monad s1) :: Imperative ()
                                   BoolVal False -> void $ (evalS_monad s2) :: Imperative ()
                                   _ -> fail "Condition must be BoolVal"
evalS_monad w@(While e s1)   = do v <- evalE_monad e
                                  case (v) of
                                   BoolVal True  -> do evalS_monad s1
                                                       evalS_monad w
                                   BoolVal False -> return ()
                                   _ -> fail "Condition must be BoolVal"
evalS_monad (Sequence s1 s2) = do _ <- evalS_monad s1
                                  evalS_monad s2
evalS_monad Skip             = return ()
evalS_monad (For v e1 e2 s1) = do evalE_monad (Assignment v e1)
                                  evalS_monad (While e' s1')
    where e'  = BinOp LessThan (Var v) e2'
          e2' = BinOp Plus e2 (Val (IntVal 1))
          s1' = Sequence s1 s2'
          s2' = Expr (Assignment v (BinOp Plus (Var v) (Val (IntVal 1))))

getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> ((Data.Map.lookup var store) >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, Data.Map.insert var val store))

miniprog :: Imperative Value
miniprog = do
            setVar "x" (IntVal 2)
            setVar "y" (IntVal 3)
            a <- getVar "x"
            b <- getVar "y"
            return (applyOp Plus a b)
