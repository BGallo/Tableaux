# Tableaux
Sistema de Tableaux para a Lógica Clássica Proposicional em linguagem funcional.

## Guia de instalação:
### Opção 1:
1. Faça o download do código fonte [aqui](https://github.com/BGallo/Tableaux/archive/refs/heads/main.zip).
2. Extraia o arquivo .zip e entre na pasta do projeto.
3. Compile o código fonte com o [GHC](https://www.haskell.org/ghc/). 
    - (Windows) Ex.:  `ghc -o main.exe .\main.hs`.
    - (Linux) Ex.: `ghc main.hs `
    // É possivel que ocorra um erro de link por faltar a biblioteca gmp,para corrigir esse erro basta instalar o pacote dessa biblioteca para sua distribuição
     Para Ubuntu/Debian/Mint : sudo apt-get install libgmp3-dev
     Para Fedora : sudo dnf install gmp-devel
### Opção 2:
1. Faça o download to programa pré-compilado para Windows 10 [aqui](https://github.com/BGallo/Tableaux/releases).
2. Extraia o arquivo .zip e entre na pasta do programa.

## Informações de uso:
- Ao executar o programa, o usuário deve entrar com a fórmula pelo próprio terminal.
- A fórmula deve ser da lógica proposicional clássica.
- Todas as fórmulas devem seguir a notação prefixa.
- Não é permitido o uso de parênteses.
- Deve haver 1 espaço entre cada operador ou variável.
- As variáveis podem ser um caractere que não tenha sido definido como operador.
- Os operadores disponíveis estão na tabela abaixo:

| Nome  | Operador |
| ------------- | ------------- |
| E  | ^  |
| Ou  | v  |
| Se, Então  | ->  |
| Não  | ~  |

- Exemplos de fórmulas:
    1. `-> v p ^ q r ^ v p q v p r`
    2. `-> a -> a -> b a`
    3. `-> b ^ a v b a`
