module ModuloFuncional where

import System.IO (hFlush, stdout)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)
import Data.Maybe (fromMaybe)
import qualified Text.Parsec as Parsec
import Prelude hiding (log)
import qualified Prelude (log)

data Expr
     = Number Double  -- Representa números
    | Var String     -- Representa variáveis
    | UnaryMinus Expr  -- Representa `-x`
    | Add Expr Expr
    | Sub Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Sin Expr
    | Cos Expr
    | Logaritmo Expr
    | Factorial Expr
    | Derivada Expr
    deriving (Show, Eq)


lexer = Token.makeTokenParser haskellDef

number :: Parser Expr
number = do
    sign <- optionMaybe (char '-')
    num <- Token.naturalOrFloat lexer
    return $ case sign of
        Just _  -> Number (- extractNumber num)  -- Se houver "-", inverte o valor
        Nothing -> Number (extractNumber num)

extractNumber :: Either Integer Double -> Double
extractNumber (Left intVal) = fromIntegral intVal
extractNumber (Right dblVal) = dblVal

variable :: Parser Expr
variable = Var <$> many1 letter

parens :: Parser Expr -> Parser Expr
parens = Token.parens lexer

table :: OperatorTable String () Identity Expr
table =
    [ [Prefix (Token.reservedOp lexer "-" >> return UnaryMinus)]
    , [Infix (Token.reservedOp lexer "^" >> return Pow) AssocRight]
    , [Infix (Token.reservedOp lexer "*" >> return Mult) AssocLeft,
       Infix (Token.reservedOp lexer "/" >> return Div) AssocLeft]
    , [Infix (Token.reservedOp lexer "+" >> return Add) AssocLeft,
       Infix (Token.reservedOp lexer "-" >> return Sub) AssocLeft]
    ]

whiteSpace = Token.whiteSpace lexer

exprParser :: Parser Expr
exprParser = buildExpressionParser table factor

factor :: Parser Expr
factor = try (do
        _ <- char '-'
        expr <- factor
        return $ UnaryMinus expr)
    <|> try (do
        base <- parens exprParser <|> number <|> variable 
        _ <- char '!'
        return $ Factorial base )    
    <|> parens exprParser 
    <|> functionParser
    <|> number
    <|> variable
   

functionParser :: Parser Expr
functionParser =
    try (string "sin" *> spaces *> (Sin <$> parens exprParser)) <|>
    try (string "cos" *> spaces *> (Cos <$> parens exprParser)) <|>
    try (string "log" *> spaces *> (Logaritmo <$> parens exprParser))

avaliar :: Expr -> (String -> Double) -> Double
avaliar (Number c) _ = c
avaliar (Var x) env = env x
avaliar (UnaryMinus e) env = - (avaliar e env)
avaliar (Add e1 e2) env = avaliar e1 env + avaliar e2 env
avaliar (Sub e1 e2) env = avaliar e1 env - avaliar e2 env
avaliar (Mult e1 e2) env = avaliar e1 env * avaliar e2 env
avaliar (Div e1 e2) env = avaliar e1 env / avaliar e2 env
avaliar (Pow e1 e2) env = avaliar e1 env ** avaliar e2 env
avaliar (Sin e) env = sin (avaliar e env)
avaliar (Cos e) env = cos (avaliar e env)
avaliar (Logaritmo e) env = Prelude.log (avaliar e env)
avaliar (Factorial e) env = 
    let valor = avaliar e env
        inteiro = round valor :: Integer -- Convertendo para inteiro
    in if valor < 0 || valor /= fromIntegral inteiro
        then error "Factorial não é definido para números negativos ou não inteiros"
        else fromIntegral (factorial inteiro) -- Convertendo Integer para Double
  where
    factorial 0 = 1
    factorial n = n * factorial (n - 1)

-- Função auxiliar para verificar se a variável "x" está presente na expressão
contemVariavelX :: Expr -> Bool
contemVariavelX (Var "x") = True
contemVariavelX (UnaryMinus e) = contemVariavelX e
contemVariavelX (Add e1 e2) = contemVariavelX e1 || contemVariavelX e2
contemVariavelX (Sub e1 e2) = contemVariavelX e1 || contemVariavelX e2
contemVariavelX (Mult e1 e2) = contemVariavelX e1 || contemVariavelX e2
contemVariavelX (Div e1 e2) = contemVariavelX e1 || contemVariavelX e2
contemVariavelX (Pow e1 e2) = contemVariavelX e1 || contemVariavelX e2
contemVariavelX (Sin e) = contemVariavelX e
contemVariavelX (Cos e) = contemVariavelX e
contemVariavelX (Logaritmo e) = contemVariavelX e
contemVariavelX (Factorial e) = contemVariavelX e
contemVariavelX _ = False  -- Caso seja um número, não tem variável


-- Implementação da derivada
derivar :: Expr -> Expr
derivar (Number _) = Number 0  -- A derivada de uma constante é 0
derivar (Var "x") = Number 1  -- d(x)/dx = 1
derivar (Add e1 e2) = Add (derivar e1) (derivar e2)  -- (f + g)' = f' + g'
derivar (Sub e1 e2) = Sub (derivar e1) (derivar e2)  -- (f - g)' = f' - g'
derivar (Mult e1 e2) = Add (Mult (derivar e1) e2) (Mult e1 (derivar e2))  -- (fg)' = f'g + fg'
derivar (Div e1 e2) = Div (Sub (Mult (derivar e1) e2) (Mult e1 (derivar e2))) (Pow e2 (Number 2))  -- (f/g)' = (f'g - fg') / g^2
derivar (Pow e (Number n)) = Mult (Number n) (Mult (Pow e (Number (n - 1))) (derivar e))  -- (x^n)' = n * x^(n-1)
derivar (Sin e) = Mult (Cos e) (derivar e)  -- (sin f)' = cos f * f'
derivar (Cos e) = Mult (Number (-1)) (Mult (Sin e) (derivar e))  -- (cos f)' = -sin f * f'
derivar (Logaritmo e) = Div (derivar e) e  -- (ln f)' = f' / f
derivar (Factorial _) = error "Derivada de fatorial não definida!"  -- Fatorial não tem derivada
derivar _ = error "Operação não suportada para derivação"

mostrarExpressao :: Expr -> String
mostrarExpressao (Number 0) = "0"
mostrarExpressao (Number 1) = "1"
mostrarExpressao (Number n) = show n
mostrarExpressao (Var x) = x
mostrarExpressao (Add e1 (Number 0)) = mostrarExpressao e1
mostrarExpressao (Add (Number 0) e2) = mostrarExpressao e2
mostrarExpressao (Add e1 e2) = "(" ++ mostrarExpressao e1 ++ " + " ++ mostrarExpressao e2 ++ ")"
mostrarExpressao (Mult _ (Number 0)) = "0"
mostrarExpressao (Mult (Number 0) _) = "0"
mostrarExpressao (Mult e1 (Number 1)) = mostrarExpressao e1
mostrarExpressao (Mult (Number 1) e2) = mostrarExpressao e2
mostrarExpressao (Mult e1 e2) = "(" ++ mostrarExpressao e1 ++ " * " ++ mostrarExpressao e2 ++ ")"
mostrarExpressao (Pow e1 (Number 1)) = mostrarExpressao e1
mostrarExpressao (Pow e1 e2) = "(" ++ mostrarExpressao e1 ++ "^" ++ mostrarExpressao e2 ++ ")"
mostrarExpressao (Sub e1 e2) = "(" ++ mostrarExpressao e1 ++ " - " ++ mostrarExpressao e2 ++ ")"
mostrarExpressao (Div e1 e2) = "(" ++ mostrarExpressao e1 ++ " / " ++ mostrarExpressao e2 ++ ")"
mostrarExpressao (Sin e) = "sin(" ++ mostrarExpressao e ++ ")"
mostrarExpressao (Cos e) = "cos(" ++ mostrarExpressao e ++ ")"
mostrarExpressao _ = error "Expressão não reconhecida na função mostrarExpressao"

calcularDerivada :: IO ()
calcularDerivada = do
    putStr "Digite a função matemática para derivar (ex: x^2 + sin(x)): "
    hFlush stdout
    entrada <- getLine
    case parse exprParser "" entrada of
        Left err -> print err 
        
        Right funcao -> do
            let derivada = derivar funcao
            putStrLn $ "Derivada da função: " ++ mostrarExpressao (derivar funcao)


somatorio :: [Int] -> Int
somatorio = sum

-- Soma dois números
soma :: Int -> Int -> Int
soma a b = a + b

-- Fatorial de um número
fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n-1)

-- Fatorial duplo
duploFatorial :: Int -> Int
duploFatorial 0 = 1
duploFatorial 1 = 1
duploFatorial n = n * duploFatorial (n-2)

-- Potência de um número
potencia :: Int -> Int -> Int
potencia _ 0 = 1       
potencia b 1 = b
potencia b e  = b * potencia b (e-1)

-- Sequência de Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- Sucessor e antecessor de um número
suc :: Int -> Int
suc x = x + 1

ant :: Int -> Int
ant x = x - 1

-- Quadrado da soma de dois números
quadradoDaSoma :: Integer -> Integer -> Integer
quadradoDaSoma a b = (a + b) ^ 2

-- Cubo de um número
cubo :: Int -> Int
cubo x = x ^ 3

-- Produto de dois números
produto :: Int -> Int -> Int
produto x y = x * y

-- Índice de massa corporal
imc :: Float -> Float -> Float
imc _ 0 = -1 
imc p a = p / (a ^ 2)

-- Soma dos quadrados
totalQuadrado :: Int -> Int -> Int
totalQuadrado a b = a * a + b * b

-- Máximo divisor comum
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

-- multiplicação por soma
multiplicacaoPorSoma :: Int -> Int -> Int
multiplicacaoPorSoma _ 0 = 0
multiplicacaoPorSoma m n = if n > 0 then m + multiplicacaoPorSoma m (n-1) else - multiplicacaoPorSoma m (-n)

-- Soma de 1 a N
somaDe1aN :: Int -> Int
somaDe1aN 1 = 1
somaDe1aN 0 = 0
somaDe1aN n = somaDe1aN (n-1) + n   

-- Sequência de Pell
pell :: Int -> Int
pell 0 = 0
pell 1 = 1
pell n = pell (n-2) + 2 * pell (n-1)

-- Derivada de um polinômio
derivada :: [Integer] -> [Integer]
derivada = zipWith (*) [1..] . tail

-- Composição de funções

-- Filtrar números pares de uma lista
filtrarPares :: [Integer] -> [Integer]
filtrarPares = filter even

-- Mapear lista para dobrar elementos
mapearDobra :: [Integer] -> [Integer]
mapearDobra = map (* 2)

resolverExpressao :: Double -> Double -> Double
resolverExpressao x y = (x + y) * (x - y)  -- Apenas um exemplo de expressao matematica
