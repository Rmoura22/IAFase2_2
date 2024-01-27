# Manual de Utilizador - Fase 2 - Jogo do Cavalo

## Inteligência Artificial 23/24

Prof. Joaquim Filipe

Eng. Filipe Mariano

Realizado por:

Mateus Cerejo - **202100250**

Ricardo Moura - **202003000**

## Introdução

Este documento tem como objetivos introduzir e guiar o utilizador ao Jogo do Cavalo, desenvolvido em Common Lisp.

### Conteudo

1. Regras
2. Estrutura e Instalação
3. Configuração
4. Menus

## Regras

- O Jogo do cavalo é um jogo por turnos, num tabuleiro 10x10.
- Cada casa é inicialmente preenchida por números inteiros, únicos e aleatórios de 0 a 99, que representam o valor da casa.
- Cada jogador controla um cavalo, o jogador com cavalo branco inicia o jogo, ao colocar o seu cavalo na casa com valor mais alto da primeira linha do tabuleiro, contabilizando os pontos e tornando-a inacessível futuramente.
- Se o número da casa for duplo (ex.: 0 11 22 99) é removido do tabuleiro o número duplo mais alto, caso ainda exista.
- Se o número da casa não for duplo (ex.: 2 13 87) deve ser removido do tabuleiro o número oposto a esse (ex.: 2=>20 13=>31 87=>78).
- O jogador com o cavalo preto joga asseguir aplicando-se as mesmas regras, mas na ultima linha.
- Agora que os cavalos estão em campo, começa o ciclo do jogo onde cada jogador, no seu respetivo turno, movimenta o cavalo tal como no xadrez, para uma casa livre, ganhando os seus pontos e aplicando as regras de remoção, similarmente às indicadas para o primeiro movimento.

## Instalação

A aplicação precisa de um compilador de Common Lisp.

O sistema do Jogo do Cavalo foi implementado LISP e foi desenvolvido com auxilio do IDE LispWorks. O projeto é composto por 3 ficheiros:

- **interact.lisp** - Interação com o utilizador.
- **jogo.lisp** - Implementação da resolução do problema incluindo seletores, operadores heuristicas e outras funcõess auxiliares.
- **algoritmo.lisp** - Implementação do algoritmos MiniMax com cortes Alpha Beta e outras funções auxiliares.

## Configuração

### Ficheiro de estatisticas

Para aceder às estatísticas do jogo é preciso primeiro alterar o caminho do ficheiro na função que se encontra no topo do ficheiro da interface.lisp.

```Lisp
(defun get-path ()
 "put full path to file here"
)
```

Após a configuração da path, compilar os três ficheiros.

## Main Menu

Para iniciar o programa usa-se a função (start).

O menu principal mostra as opções iniciais:

1. Player VS AI - Modo de jogo onde o utilizador pode jogar contra uma inteligência artificial.

2. AI VS AI - Modo de jogo onde o utilizador pode ver o jogo entre duas AI.

3. Exit - Termina o programa.

```txt
            |
            |
            + \
            \.G_.*=.
             `(#'/.\|
              .>' (_--.
           _=/d   ,^\
          ~ \)-'   '
             / |   
            '  '        
          Wellcome to
        the Knight Game

Choose the game mode
 1 - Player VS AI 
 2 - AI VS AI 
 3 - Exit 

```

## Player VS AI

Modo de jogo onde o utilizador pode jogar contra uma inteligência artificial.

### Definir parâmetros do jogo 1

Aqui o jogador pode definir o seu nome, definir o tempo de que a AI tem para escolher a sua jogada e o tabuleiro aleatório.

### Exemplo 1

```txt
Insert your name: Mateus

Choose pieces
 1 - White pieces
 2 - Black pieces

>1

Define the AI's time limit per play (1000 to 5000ms): 4200

      AA   BB   CC   DD   EE   FF   GG   HH   II   JJ

 1 |  57   14   95   76   32   97   99   16   28   48 

 2 |  30   20    6    5   47   58   67   71   10    3 

 3 |  70   24   53   38   23   46   11   12   39   56 

 4 |  35    2   25   41   22   94   88   34    8   19 

 5 |  17   79   45    4   50   62   40   55   69   49 

 6 |  72   90   18    7   21   26   84   86   36   87 

 7 |  83   77   42   92   82   29   44   66   93   98 

 8 |  59   75   63   78   74   73    1   54   68   33 

 9 |  15   61   85   51   60   52   37   31   96   64 

10 |  91   43    0   81   89   27   13    9   80   65 



Choose this table?

1 - yes 
2 - no (reroll) 
```

A primeira jogada é automática, os cavalos são colocados nas casas correspondentes de acordo com as regras. O cavalo branco é representado por WK e o cavalo preto por BK.

```txt
Mateus's turn:
      AA   BB   CC   DD   EE   FF   GG   HH   II   JJ

 1 |  57   14   95   76   32   97   WK   16   28   48 

 2 |  30   20    6    5   47   58   67   71   10    3 

 3 |  70   24   53   38   23   46   11   12   39   56 

 4 |  35    2   25   41   22   94   --   34    8   -- 

 5 |  17   79   45    4   50   62   40   55   69   49 

 6 |  72   90   18    7   21   26   84   86   36   87 

 7 |  83   77   42   92   82   29   44   66   93   98 

 8 |  59   75   63   78   74   73    1   54   68   33 

 9 |  15   61   85   51   60   52   37   31   96   64 

10 |  BK   43    0   81   89   27   13    9   80   65 
```

### Jogar

Depois de definir os parametros o jogo começa. No turno do jogador é pedida a linha e coluna para a qual o seu cavalo deve movimentar. Caso a jogada seja inválida é requirido outra vez.

### Exemplo 2

```txt
Insert line (1-10): 3
Insert Column (A-J): c
```

## AI VS AI

Modo de jogo onde o utilizador pode ver o jogo entre duas AI.

### Definir parâmetros do jogo 2

Aqui o jogador pode definir o seu nome, definir o tempo de que a AI tem para escolher a sua jogada e o tabuleiro aleatório.

### Exemplo 3

```txt
Define the AI's time limit per play (1000 to 5000ms): 4200
```
