import System.IO
import MenuModFuncional
import ModuloLogico
import Cores

main :: IO ()
main = do
    hSetEncoding stdout utf8  -- Definir saida padrao para UTF-8
    hSetEncoding stdin utf8   -- Definir entrada padrao para UTF-8
    hSetEncoding stderr utf8  -- Definir erro padrao para UTF-8
    putStrLn "---------------------------------------------------"
    putStrLn " Bem-vindo ao Sistema de Raciocinio Automatico (SRA) "
    putStrLn "---------------------------------------------------"
    putStrLn "Por favor, informe o seu nome e sobrenome:"
    nome <- getLine
    putStrLn ("Ola, " ++ nome ++ "! Seja bem-vindo ao SRA.")
    menu

-- Funcao para exibir o menu principal e processar a opcao do usuario em loop
menu :: IO ()
menu = do
    putStrLn "\nO que deseja fazer?"
    putStrLn (azul++"1- Executar funcoes logicas"++reset)
    putStrLn (azul++"2- Executar funcoes matematicas"++reset)
    putStrLn (azul++"3- Ver estatisticas de execucao"++reset)
    putStrLn (amarelo++"4- Sair do programa"++reset)
    putStr "Escolha uma opcao: "
    hFlush stdout
    opcao <- getLine
    if opcao == "4"
        then putStrLn (verde ++ "Saindo do programa. Ate logo!" ++ reset)
        else do
            processarOpcao opcao
            menu -- Volta a exibir o menu apos processar a opcao

-- Funcao para processar a escolha do usuario
processarOpcao :: String -> IO ()
processarOpcao "1" = executarModuloLogico
processarOpcao "2" = menuModuloFuncional
processarOpcao "3" = putStrLn (azul ++ "Estatisticas ainda nao implementadas." ++ reset)
processarOpcao _   = putStrLn (vermelho ++ "Opcao invalida. Tente novamente!" ++ reset)
