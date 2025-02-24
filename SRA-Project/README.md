# Sistema de Raciocínio Automático (SRA)

# Integrantes do grupo:
   Constantino da Costa Afonso
   Gildo Gomes Quissunge
   Nicklânder Kiakuenda Amândio Metusal Bueia
   Osana Roda António Manuel


## Descrição
O Sistema de Raciocínio Automático (SRA) é um projeto que combina os paradigmas de programação funcional e lógica para resolver problemas de lógica proposicional e funções matemáticas. Ele foi desenvolvido utilizando Haskell para cálculos matemáticos e SWI-Prolog para resolução de problemas lógicos.

## Instalação e Execução

### Requisitos
- [GHC e Stack](https://www.haskell.org/downloads/) para compilar e executar o código Haskell.
- [SWI-Prolog](https://www.swi-prolog.org/download/stable) para execução das funções lógicas.

### Como Rodar
1. Clone o repositório:
   ```sh
   git clone https://github.com/OsanaRoda11/sra-project.git
   cd sra-project
   ```
2. Execute o sistema usando Stack:
   ```sh
   stack runghc Main.hs
   ```
3. O menu principal será exibido e você poderá primeiramente inserir o seu nome e em seguida escolher entre as funcionalidades disponíveis.

## Estrutura do Projeto

```
SRA-Project/
├── app/
│   ├── Main.hs          # Arquivo principal que integra os módulos
│   ├── Modulo_Logico.pl  # Módulo para inferência lógica em Prolog
│   ├── ModuloFuncional.hs  # Módulo para operações matemáticas em Haskell
│   ├── MenuModFuncional.hs  # Menu das operações matemáticas em Haskell
│   ├── logica.pl        # Definição de regras lógicas em Prolog
|   |__ Estatisticas.hs  # Arquivo onde integra as estatisticas de execução
│   └── Cores.hs         # Arquivo onde consta códigos ANSI para cores no terminal
|   
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

```

### Módulo Funcional (Haskell)
Este módulo dá suporte a várias funções matemáticas. Dentre elas, calculo da sequencia de Fibonacci, seqência de Pell, Maximo divisor comum (Algoritmo de Euclides), Derivada de um polinómio e a avaliação de função matemática digitada pelo utilizador.
Exemplos de execução:
```
14 - Sequencia de Pell
Digite o numero da opcao desejada: 14
Digite a posicao na sequencia de Pell: 8
Numero de Pell: 408
```
10 - Maximo Divisor Comum (Algoritmo de Euclides)
Digite dois numeros: 
Primeiro numero: 54
Segundo numero: 13
Maximo Divisor Comum (MDC): 1
```
15 - Derivada de um polinomio
Digite a função matemática para derivar (ex: x^2 + sin(x)): x^2 + sin(x)
Derivada da função: ((2.0 * x) + cos(x))
```
19 - Avaliar uma função matemática digitada
Digite a funcao matematica (ex: x^2 + sin(x) + 3*x): x^2 + sin(x) + x^3
Função interpretada: Add (Add (Pow (Var "x") (Number 2.0)) (Sin (Var "x"))) (Pow (Var "x") (Number 3.0))
Digite um valor para x (ex: 2.5): 2
Resultado: 12.909297426825681
```

```
## Estatisticas do sistema
   Permite visualizar a quantidade de funções lógicas, matemáticas e o registrar a quantidade de erros durante a execução do programa.

## Possíveis Erros e Soluções
- **Erro ao executar Prolog:** Certifique-se de que o SWI-Prolog está instalado e configurado corretamente.
- **Erro ao rodar o sistema com Stack:** Verifique se o Stack está instalado e execute `stack setup` antes de rodar o programa.

## Contribuição
Sinta-se à vontade para contribuir no nosso sistema! Abra uma issue ou envie um pull request.

## Licença
Este projeto está sob a licença MIT.


