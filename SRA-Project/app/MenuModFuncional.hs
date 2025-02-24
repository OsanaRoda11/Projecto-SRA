module MenuModFuncional where

import ModuloFuncional
import Cores
import Estatisticas (registrarFuncaoMatematica)
import System.IO (hFlush, stdout)
import Text.Parsec (parse)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)



resolverFuncaoMatematica :: IO ()
resolverFuncaoMatematica = do
    putStr "Digite a funcao matematica (ex: x^2 + sin(x) + 3*x): "
    hFlush stdout
    entrada <- getLine
    case parse exprParser "" entrada of
        Left err -> print err
        Right funcao -> do
            putStrLn $ "Função interpretada: " ++ show funcao
            if contemVariavelX funcao
                then do
                    putStr "Digite um valor para x (ex: 2.5): "
                    hFlush stdout
                    x <- fmap (fromMaybe 0 . (readMaybe :: String -> Maybe Double)) getLine
                    putStrLn $ "Resultado: " ++ show (avaliar funcao (\_ -> x))
                else
                    putStrLn $ "Resultado: " ++ show (avaliar funcao (\_ -> 0))  -- Não precisa de x


menuModuloFuncional :: IO ()
menuModuloFuncional = do
    registrarFuncaoMatematica  -- Atualiza a estatística
    putStrLn "----------------------------------------------------------"
    putStrLn (roxo ++ "Modulo Funcional - Calculo de Funcoes Matematicas" ++ reset)
    putStrLn "----------------------------------------------------------"
    putStrLn "Escolha a operacao desejada:"
    putStrLn "0  - Voltar ao Menu Principal"
    putStrLn "1  - Calcular Fatorial"
    putStrLn "2  - Resolver Expressao Matematica"
    putStrLn "3  - Calcular Somatorio"
    putStrLn "4  - Calcular Duplo Fatorial"
    putStrLn "5  - Calcular Potencia"
    putStrLn "6  - Sequencia de Fibonacci"
    putStrLn "7  - Quadrado da soma de dois numeros"
    putStrLn "8  - Cubo de um numero"
    putStrLn "9  - Indice de Massa Corporal (IMC)"
    putStrLn "10 - Maximo Divisor Comum (MDC)"
    putStrLn "11 - Multiplicacao por soma"
    putStrLn "12 - Produto de dois numeros"
    putStrLn "13 - Soma de 1 a N"
    putStrLn "14 - Sequencia de Pell"
    putStrLn "15 - Derivada de um polinomio"
    putStrLn "17 - Filtrar numeros pares de uma lista"
    putStrLn "18 - Mapear lista para dobrar elementos"
    putStrLn "19 - Avaliar uma função matemática digitada"
    putStrLn "----------------------------------------------------------"
    putStr "Digite o numero da opcao desejada: "
    hFlush stdout
    opcao <- getLine
    processarOpcao2 opcao

processarOpcao2 :: String -> IO ()
processarOpcao2 opcao = case opcao of
    "0"  -> putStrLn (verde ++ "Voltando ao menu principal..." ++ reset)
    "1"  -> registrarEExecutar "Digite um numero: " fat "Fatorial"
    "2"  -> registrarEExecutar2 "Digite dois numeros para resolver a expressao: " resolverExpressao "Resultado da expressao"
    "3"  -> registrarEExecutarLista "Digite uma lista de numeros separados por espaco: " somatorio "Somatorio"
    "4"  -> registrarEExecutar "Digite um numero: " duploFatorial "Duplo Fatorial"
    "5"  -> registrarEExecutar2 "Digite a base e o expoente: " potencia "Potencia"
    "6"  -> registrarEExecutar "Digite a posicao na sequencia de Fibonacci: " fibonacci "Numero de Fibonacci"
    "7"  -> registrarEExecutar2 "Digite dois numeros: " quadradoDaSoma "Quadrado da soma"
    "8"  -> registrarEExecutar "Digite um numero: " cubo "Cubo"
    "9"  -> registrarEExecutar2 "Digite peso e altura: " imc "Indice de Massa Corporal (IMC)"
    "10" -> registrarEExecutar2 "Digite dois numeros: " mdc "Maximo Divisor Comum (MDC)"
    "11" -> registrarEExecutar2 "Digite dois numeros: " multiplicacaoPorSoma "Multiplicacao por soma"
    "12" -> registrarEExecutar2 "Digite dois numeros: " produto "Produto"
    "13" -> registrarEExecutar "Digite um numero: " somaDe1aN "Soma de 1 a N"
    "14" -> registrarEExecutar "Digite a posicao na sequencia de Pell: " pell "Numero de Pell"
    "15" -> calcularDerivada
    "17" -> registrarEExecutarLista "Digite uma lista de numeros: " filtrarPares "Lista com numeros pares"
    "18" -> registrarEExecutarLista "Digite uma lista de numeros: " mapearDobra "Lista com elementos dobrados"
    "19" -> resolverFuncaoMatematica
    _    -> do
        putStrLn (vermelho ++ "Opcao invalida! Tente novamente." ++ reset)
        menuModuloFuncional

registrarEExecutar :: (Read a, Show b) => String -> (a -> b) -> String -> IO ()
registrarEExecutar mensagem func descricao = do
    registrarFuncaoMatematica
    solicitarNumero mensagem func descricao

registrarEExecutar2 :: (Read a, Read b, Show c) => String -> (a -> b -> c) -> String -> IO ()
registrarEExecutar2 mensagem func descricao = do
    registrarFuncaoMatematica
    solicitarDoisNumeros mensagem func descricao

registrarEExecutarLista :: (Read a, Show b) => String -> ([a] -> b) -> String -> IO ()
registrarEExecutarLista mensagem func descricao = do
    registrarFuncaoMatematica
    solicitarLista mensagem func descricao

solicitarNumero :: (Read a, Show b) => String -> (a -> b) -> String -> IO ()
solicitarNumero mensagem func descricao = do
    putStr mensagem
    hFlush stdout
    n <- readLn
    putStrLn $ descricao ++ ": " ++ show (func n)
    menuModuloFuncional

solicitarDoisNumeros :: (Read a, Read b, Show c) => String -> (a -> b -> c) -> String -> IO ()
solicitarDoisNumeros mensagem func descricao = do
    putStrLn mensagem
    putStr "Primeiro numero: "
    hFlush stdout
    x <- readLn
    putStr "Segundo numero: "
    hFlush stdout
    y <- readLn
    putStrLn $ descricao ++ ": " ++ show (func x y)
    menuModuloFuncional

solicitarLista :: (Read a, Show b) => String -> ([a] -> b) -> String -> IO ()
solicitarLista mensagem func descricao = do
    putStr mensagem
    hFlush stdout
    input <- getLine
    let numeros = map read (words input)
    putStrLn $ descricao ++ ": " ++ show (func numeros)
    menuModuloFuncional
