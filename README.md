# Tableaux
Sistema de Tableaux para a Lógica Clássica Proposicional em linguagem funcional.

## Guia de instalação:
1. Faça o download do código fonte [aqui](https://github.com/BGallo/Tableaux/archive/refs/heads/main.zip).
2. Extraia o arquivo .zip e entre na pasta do projeto.
3. Compile o código fonte com o [GHC](https://www.haskell.org/ghc/). 
    - (Windows) Ex.:  `ghc -o main.exe .\main.hs`.
    - (Linux) Ex.: `ghc main.hs `
    // É possivel que ocorra um erro de link por faltar a biblioteca gmp,para corrigir esse erro basta instalar o pacote dessa biblioteca para sua distribuição
     Para Ubuntu/Debian/Mint : sudo apt-get install libgmp3-dev
     Para Fedora : sudo dnf install gmp-devel

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
    1. `-> a b`
    2. `-> v a b v a b`
    3. `v ^ a b ^ c d`
    4. `~ ~ -> ^ a b v a b` x
    5. `-> a -> a -> b a` x
    6. `~ ^ a b`
    7. `^ a ~ a`
    8. `v ~ a v a ~ a`
    9. `~ ~ ^ a ^ a ~ b`
    10. `~ ^ ^ a b ^ c b`
