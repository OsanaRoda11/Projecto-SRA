module Prolog where

import System.Process
import System.IO

-- Função que executa Prolog e retorna o resultado

executarProlog :: String -> IO String
executarProlog expressao = do
    (Just stdin, Just stdout, _, _) <- createProcess (proc "swipl" ["-q", "-f", "Modulo_Logico.pl"])
        { std_in = CreatePipe, std_out = CreatePipe }
    
    hPutStrLn stdin ("eval(" ++ expressao ++ ", R), write(R), nl.")
    hFlush stdin
    resultado <- hGetContents stdout
    return resultado


