module Cores where

-- Códigos ANSI para cores no terminal
reset   = "\x1b[0m"    -- Resetar cor
vermelho = "\x1b[31m"
verde    = "\x1b[32m"
amarelo  = "\x1b[33m"
azul     = "\x1b[34m"
roxo     = "\x1b[35m"
ciano    = "\x1b[36m"
branco   = "\x1b[37m"

-- Estilos de texto
negrito  = "\x1b[1m"
sublinhado = "\x1b[4m"
inverter  = "\x1b[7m"
