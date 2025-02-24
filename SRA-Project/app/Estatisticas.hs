module Estatisticas where

import System.IO
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Cores

-- Variáveis globais para estatísticas
totalFuncoesLogicas :: IORef Int
totalFuncoesLogicas = unsafePerformIO (newIORef 0)

totalFuncoesMatematicas :: IORef Int
totalFuncoesMatematicas = unsafePerformIO (newIORef 0)

totalErros :: IORef Int
totalErros = unsafePerformIO (newIORef 0)

-- Funcao para registrar uma nova funcao logica processada
registrarFuncaoLogica :: IO ()
registrarFuncaoLogica = modifyIORef totalFuncoesLogicas (+1)

-- Função para registrar uma nova função matemática processada
registrarFuncaoMatematica :: IO ()
registrarFuncaoMatematica = modifyIORef totalFuncoesMatematicas (+1)

-- Função para registrar um erro
registrarErro :: IO ()
registrarErro = modifyIORef totalErros (+1)

-- Função para obter estatísticas
obterEstatisticas :: IO String
obterEstatisticas = do
    logicas <- readIORef totalFuncoesLogicas
    matematicas <- readIORef totalFuncoesMatematicas
    erros <- readIORef totalErros
    return $ "----------------------------------------------------------\nEstatisticas do Sistema\n----------------------------------------------------------\n" ++
             "Total de funcoes logicas executadas: " ++ show logicas ++ "\n" ++
             "Total de funcoes matematicas executadas: " ++ show matematicas ++ "\n" ++
             "Total de erros encontrados: " ++ show erros ++ "\n"
