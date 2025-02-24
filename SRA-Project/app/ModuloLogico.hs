{-# LANGUAGE ScopedTypeVariables #-}
module ModuloLogico where
import Cores

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Exception (catch, SomeException)
import System.IO (hFlush, stdout)
import Estatisticas (registrarFuncaoLogica, registrarErro)  --  Correção: Importação correta

-- Função principal para executar o módulo lógico
executarModuloLogico :: IO ()
executarModuloLogico = do
    registrarFuncaoLogica  -- Atualiza a estatística
    putStrLn "----------------------------------------------------------"
    putStrLn (roxo ++ "Modulo Logico - Problemas de Logica Proposicional" ++ reset)
    putStrLn "----------------------------------------------------------"
    putStrLn "\nEscolha uma opção:"
    putStrLn "1- Avaliar Expressão"
    putStrLn "2- Verificar se é uma Tautologia"
    putStrLn "3- Inferir Proposição e Explicação"
    putStrLn "4- Voltar ao Menu Principal"
    putStr "Opção: "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> avaliarExpressao
        "2" -> verificarTautologia
        --"3" -> inferirProposicao
        "4" -> return ()
        _   -> putStrLn "Opção inválida. Tente novamente." >> executarModuloLogico

-- Função para avaliar a expressão lógica
avaliarExpressao :: IO ()
avaliarExpressao = do
    putStrLn "\nDigite uma expressao logica"
    putStrLn "Operadores disponiveis: and, or, not, implies, iff"
    putStrLn "Exemplo: and(true, or(false, not(true)))"
    putStr "\nExpressao: "
    hFlush stdout
    expr <- getLine
    if null expr
        then do 
            putStrLn (vermelho ++ "Expressao vazia. Tente novamente." ++ reset)
            registrarErro  --  Agora funciona
            avaliarExpressao  -- Permite tentar novamente
        else do
            registrarFuncaoLogica 
            resultado <- avaliarExpressaoLogica expr
            case resultado of
                Left erro -> do 
                    putStrLn (vermelho ++ "Erro: " ++ erro ++ reset)
                    registrarErro  --  Agora funciona
                Right valor -> do
                    putStrLn (verde ++"Resultado: " ++ valor ++ reset)
            executarModuloLogico  -- Retorna ao menu após a execução

-- Função para avaliar a expressao logica usando Prolog
avaliarExpressaoLogica :: String -> IO (Either String String)
avaliarExpressaoLogica expr = do
    let query = "consult('logica.pl'), avaliar(" ++ expr ++ ", R), write(R), halt."
    callProlog query
    where
        callProlog :: String -> IO (Either String String)
        callProlog q = 
            (do
                (exitCode, stdout, stderr) <- readProcessWithExitCode "swipl" ["-q", "-g", q] ""
                case exitCode of
                    ExitSuccess -> 
                        let result = limparSaida stdout
                        in return $ case result of
                            "true" -> Right ("Verdadeiro")
                            "false" -> Right ( "Falso")
                            _ -> Left $ "Resultado inesperado: " ++ result
                    ExitFailure _ -> do  --  Correção: `do` necessário
                        registrarErro  --  Agora funciona
                        return $ Left "Erro de sintaxe. Use o formato: and(true, false)"
            ) `catch` \(e :: SomeException) -> do
                registrarErro  -- Agora funciona
                return $ Left "Erro ao executar Prolog. Verifique se o SWI-Prolog esta instalado."

-- Remove caracteres indesejados da saída
limparSaida :: String -> String
limparSaida = takeWhile (/= '\n') . filter (/= '\r')


-- Função para verificar se uma proposição é uma tautologia
verificarTautologia :: IO ()
verificarTautologia = do
    putStrLn "\nDigite uma proposição para verificar se é uma tautologia:"
    putStrLn "Operadores disponíveis: and, or, not, implies, iff"
    putStrLn "Exemplo: implies(P, P) (deve ser verdadeiro para qualquer P)"
    putStr "\nProposição: "
    hFlush stdout
    prop <- getLine
    if null prop
        then do
            putStrLn (vermelho ++ "Proposição vazia. Tente novamente." ++ reset)
            registrarErro
        else do
            registrarFuncaoLogica
            resultado <- avaliarTautologia prop
            case resultado of
                Left erro -> do
                    putStrLn (vermelho ++ "Erro: " ++ erro ++ reset)
                    registrarErro
                Right valor -> do
                    putStrLn (verde ++ "Resultado: " ++ valor ++ reset)

-- Função para chamar Prolog e verificar se uma proposição é uma tautologia
avaliarTautologia :: String -> IO (Either String String)
avaliarTautologia prop = do
    let query = "consult('logica.pl'), eh_tautologia(" ++ prop ++ "), write('Resultado: '), write(Result), halt."
    callProlog query
    where
        callProlog :: String -> IO (Either String String)
        callProlog q = 
            (do
                (exitCode, stdout, stderr) <- readProcessWithExitCode "swipl" ["-q", "-g", q] ""
                case exitCode of
                    ExitSuccess -> 
                        let result = limparSaida stdout
                        in return $ if result == "true"
                            then Right "É uma tautologia"
                            else Right "Não é uma tautologia"
                    ExitFailure _ -> do
                        registrarErro
                        return $ Left "Erro de sintaxe ou Proposição inválida."
            ) `catch` \(e :: SomeException) -> do
                registrarErro
                return $ Left "Erro ao executar Prolog. Verifique se o SWI-Prolog está instalado."
