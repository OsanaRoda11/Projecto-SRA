# Sistema de Raciocínio Automático (SRA)

## Introdução
O Sistema de Raciocínio Automático (SRA) é um projeto que combina os paradigmas de programação funcional e lógica para resolver problemas de lógica proposicional e funções matemáticas. Ele foi desenvolvido utilizando Haskell para cálculos matemáticos e SWI-Prolog para resolução de problemas lógicos.

## Instalação e Execução

### Requisitos
- [GHC e Stack](https://www.haskell.org/downloads/) para compilar e executar o código Haskell.
- [SWI-Prolog](https://www.swi-prolog.org/download/stable) para execução das funções lógicas.

### Como Rodar
1. Clone o repositório:
   ```sh
   git clone https://github.com/seu-usuario/sra-project.git
   cd sra-project
   ```
2. Execute o sistema usando Stack:
   ```sh
   stack runghc Main.hs
   ```
3. O menu principal será exibido e você poderá escolher entre as funcionalidades disponíveis.

## Estrutura do Projeto

```
SRA-Project/
├── app/
│   ├── Main.hs          # Arquivo principal que integra os módulos
│   ├── modulo_funcional.hs  # Módulo para operações matemáticas em Haskell
│   ├── modulo_logico.pl  # Módulo para inferência lógica em Prolog
│   ├── logica.pl        # Definição de regras lógicas em Prolog
│   └── utils.hs         # Funções auxiliares
├── README.md          # Este documento
├── stack.yaml         # Configuração do Stack para Haskell
├── package.yaml       # Dependências do projeto
```

## Exemplos de Uso

### Módulo Lógico (Prolog)

O usuário insere expressões proposicionais usando operadores como and, or, not, implies, e iff.
A função avaliarExpressaoLogica recebe uma expressão lógica digitada pelo usuário, a envia para o Prolog, e retorna a avaliação da expressão
```prolog
?- or(false, not(true))

false.

?- eh_tautologia(implies(true, true)).
true.
```

### Módulo Funcional (Haskell)
Cálculo de uma função matemática:
```haskell
main> calcularFuncao "x^3 + 2*x - 5" 3
Resultado: 28
```

## Possíveis Erros e Soluções
- **Erro ao executar Prolog:** Certifique-se de que o SWI-Prolog está instalado e configurado corretamente.
- **Erro ao rodar o sistema com Stack:** Verifique se o Stack está instalado e execute `stack setup` antes de rodar o programa.

## Contribuição
Sinta-se à vontade para contribuir! Abra uma issue ou envie um pull request.

## Licença
Este projeto está sob a licença MIT.


