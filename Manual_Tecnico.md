# Manual Técnico - Fase 2 - Jogo do Cavalo

## Inteligência Artificial 23/24

Prof. Joaquim Filipe

Eng. Filipe Mariano

Realizado por:

Mateus Cerejo - **202100250**

Ricardo Moura - **202003000**

## Indice

1. Introdução
2. Arquitetura do programa
3. Algoritmo Minimax com cortes Alpha Beta
4. Resultados

## Introdução

Este documento tem como objetivos documentar e explicar o funcionamento interno do Jogo do Cavalo implementado em Common Lisp.

Para as regras e funcionamento de alto nível do Jogo, por favor consultar o **Manual de Utilizador**.

## Arquitetura

O sistema do Jogo do Cavalo foi implementado em Common LISP e foi desenvolvido com auxilio do IDE LispWorks. O projeto é composto por 3 ficheiros:

- **interact.lisp** - Interação com o utilizador.
- **jogo.lisp** - Implementação da resolução do problema incluindo seletores, operadores heuristicas e outras funcõess auxiliares.
- **algoritmo.lisp** - Implementação do algoritmos MiniMax com cortes Alpha Beta e outras funções auxiliares.

### Tabuleiro  

Lista de listas, que representa o tabuleiro do jogo.

```Lisp

  67   94   78   33   61   79   91   18    5   40 

  44   13   32   95   26   68   96   29   64   82 

  19   81   37   50   53   55   71   14   47   16 

   9    8   30   23   58   89   45   39   62   11 

  36   84   99   88   59   74   52   21   57    1 

  70   56   35   98   54    4    0   72   27   48 

   3   31   20   22    6   76   25   28    7   75 

  41   66   51   90   80   93   97   12   10   42 

  87   34   43   24   65   77   60   49   83   69 

  15   46   17    2   73   85   38   86   63   92 

```

### Nó

O nó representa o estado do jogo, contendo todas as informações relevantes sobre o jogo num determidado turno.

#### Lista de atributos

- 💥
- 💥
- 💥
- 💥

### Regra do Simétrico e do Duplo

A regra do **Simétrico** faz com que simetrico do número no qual o cavalo calhou (ex.: 2 -> 20, 13 -> 31) seja marcado como nil.

A regra do **Duplo** faz com que o número duplo mais alto seja marcado como nil, caso o cavalo calhe numa casa de número nil (ex.: Calhar em 22 pode eleminar também a casa 77 desde que esta exista e seja o maior duplo no momento).

```Lisp
(defun removeOther (table num)
"Remove o oposto ou maior duplo dependendo se o numero é duplo."
)
  ```

### Operadores

O cavalo pode fazer, na melhor das condições, 8 movimentos, menos se estiver perto das paredes. Os operadores usam a função move que devolve nil se não for possivel mover para um certo sítio ou o nó após o movimento.

Os movimentos são feitos pelas linhas e colunas, x e y para simplificar.

#### Ex: operador1(x=1, y=2) -> 1 para baixo, 2 para a direita  

```Lisp
(defun get-operators ()
"💥"
 (list
  'operator1
  'operator2
  'operator3
  'operator4
  'operator5
  'operator6
  'operator7
  'operator8
 )
)

(defun Operator1 ("💥")
"💥"
)

(defun move ("💥")
"💥"
)

```

### Movimento

```Lisp

(defun move ("💥")
"💥"
)

```

### Sucessores

Os sucessores ou nós filhos são gerados pela função successores. Se a posição do cavalo for nil então chama-se a firstSuccessors caso contrário chama-se todos os operadores e remove-se aqueles que devolveram nil.

```Lisp
(defun successors ("💥")
"💥"
) 
```

## MiniMax com cortes Alpha Beta

### Inserir nós ordenados

Insere os nós de forma ordenada para facilitar a procura do melhor nó.

```Lisp

(defun insertOrderedDecHeuristic (Node Nodes)
 (cond 
  ((null Nodes) (list Node))
  ((< (sixth Node) (sixth (car Nodes))) (cons Node Nodes))
  (t (cons (first Nodes) (insertOrderedDecHeuristic Node (cdr Nodes))))
 )
)
```

### Heurística

## Resultados
