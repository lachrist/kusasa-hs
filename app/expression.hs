module Expression (Expression(Constant, Variable, Abstraction, Application, Block, Assignment, Definition, Branch), Expression.parser) where

import Text.ParserCombinators.Parsec
import Data.List
import JSON
import Text.JSON
import Data

data Expression
  = Constant Data
  | Variable String
  | Abstraction [String] Expression
  | Application Expression [Expression]
  | Block Expression Expression [Expression]
  | Assignment String Expression
  | Definition String Expression
  | Branch Expression Expression Expression
  deriving Eq

----------
-- JSON --
----------

instance JSON Expression where
  showJSON expr = JSString $ toJSString $ show expr
  readJSON (JSString jss) = parsec expression jss
  readJSON _ = Error "expects a string"

-------------------
-- Show & Parsec --
-------------------

instance Show Expression where
  show (Constant dta) = show dta
  show (Variable str) = str
  show (Abstraction ids expr) = "(lambda ("++(intercalate " " ids)++") "++(show expr)++")"
  show (Application expr exprs) = "("++(show expr)++(concatMap ((' ':).show) exprs)++")"
  show (Block expr1 expr2 exprs) = "(begin "++(show expr1)++" "++(show expr2)++(concatMap ((' ':).show) exprs)++")"
  show (Assignment str expr) = "(set! "++str++" "++(show expr)++")"
  show (Definition str expr) = "(define "++str++" "++(show expr)++")"
  show (Branch expr1 expr2 expr3) = "(if "++(show expr1)++" "++(show expr2)++" "++(show expr3)++")"

eat = many (space <|> tab <|> newline)

identifier = eat >> (many1 $ noneOf " \t\n()")

parser = expression

expression = choice [try abstraction, try block, try assignment, try definition, try branch, try application, try constant, try variable]

constant = eat >> Data.parser >>= (return . Constant)

variable = identifier >>= (return . Variable)

abstraction = do eat
                 char '('
                 eat
                 string "lambda"
                 eat
                 char '('
                 params <- many identifier
                 eat
                 char ')'
                 body <- expression
                 eat
                 char ')'
                 return $ Abstraction params body


block = do eat
           char '('
           eat
           string "begin"
           expr1 <- expression
           expr2 <- expression
           exprs <- many expression
           eat
           char ')'
           return $ Block expr1 expr2 exprs


assignment = do eat
                char '('
                eat
                string "set!"
                param <- identifier
                body <- expression
                eat
                char ')'
                return $ Assignment param body

definition = do eat
                char '('
                eat
                string "define"
                param <- identifier
                body <- expression
                eat
                char ')'
                return $ Definition param body

branch = do eat
            char '('
            eat
            string "if"
            pred <- expression
            cons <- expression
            alt <- expression
            eat
            char ')'
            return $ Branch pred cons alt

application = do eat
                 char '('
                 proc <- expression
                 args <- many expression
                 eat
                 char ')'
                 return $ Application proc args
