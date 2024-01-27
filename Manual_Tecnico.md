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

- **pontuacao1** - Pontuação do jogador1
- **pontuacao2** - Pontuação do jogador2
- **posicao1**   - Posição do jogador1
- **tabuleiro**  - Tabuleiro do jogo
- **profundidade** - Profundidade do nó
- **curJogador** - Jogador atual
- **pai** - Lista de operações até ao nó
- **heuristica** - Heuristica do nó
- **posicao2** - Posição do jogador2

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
  (if (equal line 1)
    (case (+ line col)
      (3 'Operator1)
      (-1 'Operator2)
      (-3 'Operator5)
      (1 'Operator6)
      (t nil)
    )
    (case (+ line col)
      (3 'Operator3)
      (1 'Operator4)
      (-3 'Operator7)
      (-1 'Operator8)
      (t nil)
    )
  )
)

(defun Operator1 (node)
  (move node (+ (horsePosLine node) 1) (+ (horsePosColumn node) 2) 'operator1)
)


```

### Movimento

```Lisp
(defun move (node line column name)
  (let ((value (cel (first node) line column)))
  (cond
   ((or (null value) (equal value 'WK) (equal value 'BK)) nil)
   (t (list 
        (removeOther (replaceInTableIndex (replaceInTableIndex (first node) (first 
        (get-player-pos (get-node-curPlayer node))) (second (get-player-pos 
        (get-node-curPlayer node)))) line column (cel (get-node-table node) (first 
        (get-player-pos (get-node-curPlayer node))) (second (get-player-pos 
        (get-node-curPlayer node))))) value)
        (third node);invert cur with next
        (upadate-player (second node) (list line column) value)
        (1+(fourth node))
        nil
        nil
      )
   )
  )
 )
)

```

### Sucessores

Os sucessores ou nós filhos são gerados pela função successores. Se a posição do cavalo for nil então chama-se a firstSuccessors caso contrário chama-se todos os operadores e remove-se aqueles que devolveram nil.

```Lisp
(defun successors (node funcs)
  (remove-if #'null (successors-rec node funcs))
)

(defun successors-rec (node funcs)
  (cond 
    ((null funcs) nil)
    (t (insertOrderedDecHeuristic (apply (car funcs) (list node)) (successors-rec node (cdr funcs))))
  )
)
```

## MiniMax com cortes Alpha Beta

### Ordenar nós

Consiste em ordenar os nós com o mesmo pai pelo valor da função de avaliação/heuristica para facilitar os cortes dos nós no algoritmo alfabeta.

A função order-nodes recebe a lista de nós com o mesmo pai e ordena-os de forma crescente se for nó MIN ou decrescente se for nó MAX.

```Lisp
(defun order-nodes (nodes jogador-max jogador)
  (cond
    ((eq jogador-max jogador) (max-node-sort nodes jogador-max))
    (t (min-node-sort nodes jogador-max))
  )
)
```

Função max-node-sort coloca o nó de maior valor heuristica no inicio da fila e retorna a lista de nós ordenados por valor de heuristica decrescente.

```Lisp
(defun max-node-sort (nodes jogador-max)
  "Retorna uma lista de nÃ³s ordenados por heuristica decrescente"
  (cond
    ((null nodes) nil)
    (t (let* ((min-node (find-min-node nodes jogador-max))
           (rest-nodes (remove min-node nodes)))
      (append  (list min-node) (max-node-sort rest-nodes jogador-max)
             ))))
)
```

Função encontra o nó com maior valor heuristico na lista de nós.

```Lisp
(defun find-max-node (nodes jogador-max)
  "Encontra o nÃ³ com melhor heuristica na lista de nÃ³s"
  (reduce #'(lambda (a b)
              (if (> (heuristic-minmax a jogador-max) (heuristic-minmax b jogador-max))
                a
                b))
          nodes)
)
```

Função min-node-sort coloca o nó de menot valor heuristica no inicio da fila e retorna a lista de nós ordenados por valor de heuristica crescente.

```Lisp
(defun min-node-sort (nodes jogador-max)
  "Retorna uma lista de nÃ³s ordenados por heuristica crescente"
  (cond
    ((null nodes) nil)
    (t (let* ((max-node (find-max-node nodes jogador-max))
           (rest-nodes (remove max-node nodes)))
      (append  (list max-node) (min-node-sort rest-nodes jogador-max)
             ))))
)
```

Função encontra o nó com menor valor heuristico na lista de nós.

```Lisp
(defun find-min-node (nodes jogador-max)
  "Encontra o nÃ³ com pior heuristica na lista de nÃ³s"
  (reduce #'(lambda (a b)
              (if (< (heuristic-minmax a jogador-max) (heuristic-minmax b jogador-max))
                a
                b))
          nodes)
)
```

### Heurística

Função calcula o valor de heuristica/função de avaliação do algoritmo alfabeta ao calcular a diferença de pontuação entre os dois jogadores.

A diferença é feita a partir da pontuação do jogador 1 se ele for o JogadorMax

Senão é feita a paritr da pontuação do jogador 2
```Lisp
(defun heuristic-minmax (node jogador-max)
"FunÃ§Ã£o retorna o valor da heuristica da funÃ§Ã£o de avaliaÃ§Ã£o do minmax"
  (cond
    ;JogadorMax Ã© o Jogador1
    ((eq jogador-max *jogador1*) (- (first node) (second node)))
    ;JogadorMax Ã© o Jogador2
    (t (- (second node) (first node)))
  )
)
```

## MinMax 
MinMax é um algoritmo de decisão que simula a decisão de um jogador encontrar a jogada ideal, o algoritmo também presume que o jogador oposto também escolherá a melhor jogada.

Existem dois jogadores no MinMax:

- JogadorMax procura o valor de heuristica maior possivel, neste caso a pontuação mais alta

- JogadorMin procura o valor de heuristica menor possivel, neste caso a pontuação mais baixa

A pesquisa é muitas vezes representada numa estrutura de dados árvore.

A função é eficaz, no entanto é necessário percorrer todos os nós o que a torna enificiente e cara. Por isso é usado uma versão mais avançada do algoritmo: **alfabeta**

## Alfabeta
O alfabeta consiste em reduzir os nós pesquisados ao cortá-los quando é encontrado um nó melhor diminuindo os recursos computacionais usados.
Esta versão do algoritmo requer 2 valores adicionais: **alfa** e **beta**


- Alfa é o melhor valor heuristico que o jogadorMax atualmente pode garantir nesse nível ou acima.

- Beta é o melhor valor heuristico que o jogadorMin atualmente pode garantir nesse nível ou acima.

- Nó max com alfa > beta, então faz corte os nós do nivel acima dos nós terminais e devolve o Beta

- Nó min com beta < alfa, então faz corte os nós do nivel acima dos nós iniciais e devolve o Alfa.



### Funções
Função de algoritmo alfabeta retorna o valor heuristico do nó a partir da função heuristic-minmax se:
 - Nó tiver na profundidade limite
- Nó for nulo
- Nó for nó folha
- Tiver passado +90% do tempo atribuido para a jogada

Ou executa a função alfa-beta-max para nó Max, ou executa a função alfa-beta-max para nó Min


```Lisp
(defun alfa-beta (node profundidade-max jogador-max timestamps &optional (alfa *minimum-value*) (beta *maximum-value*))
  (cond
    ((or (eq (fifth node) profundidade-max)
         (null node) 
         (null (convert-all-to-alfa-beta-nodes (successors node (get-operators))))
         (has-passed-90-percent (first timestamps) (second timestamps)))             
                            (heuristic-minmax node jogador-max))  ; 
    (t (let ((sucessores (order-nodes (convert-all-to-alfa-beta-nodes (successors node (get-operators))) (sixth node) jogador-max)))
                         (cond
                           ;NÃ³ Max
                           ((eq (sixth node) jogador-max)                             
                                (alfa-beta-max sucessores profundidade-max 
                                               jogador-max timestamps alfa beta)) 
                           ;NÃ³ Min  
                           (t                                                         
                                (alfa-beta-min sucessores profundidade-max 
                                               jogador-max timestamps alfa beta))))
  ))
)
```

#### Caso de nó Max

Função retorna alfa se não houverem sucessores, com sucessores a função verifica o máximo entre beta e o valor alfa do sucessor.


Corta o ramo se beta for menor, caso contrário continua a seguir o ramo.

``` Lisp
(defun alfa-beta-max (sucessores profundidade-max jogador-max timestamps alfa beta)
  (if (null sucessores)
      alfa
      (let* ((child-result (alfa-beta (first sucessores) profundidade-max jogador-max timestamps alfa beta))
             (novo-alfa (max alfa child-result)))
        (cond 
          ((> novo-alfa beta) 
              
              beta)
          (t 
           (setf *jogada* (first sucessores))
           (max novo-alfa (alfa-beta-max (rest sucessores) profundidade-max 
                                          jogador-max timestamps novo-alfa beta))))))
)
```

#### Caso de nó Min

Função retorna beta se não houverem sucessores, com sucessores a função verifica o máximo entre alfa e o valor beta do sucessor.

Corta o ramo se beta for menor, caso contrário continua a seguir o ramo.

``` Lisp
(defun alfa-beta-min (sucessores profundidade-max jogador-max timestamps alfa beta)
  (if (null sucessores)
       beta 
       (let* ((child-result (alfa-beta (first sucessores) profundidade-max jogador-max timestamps alfa beta))
             (novo-beta (min beta child-result)))
         (cond 
           ((< novo-beta alfa)  
               
               alfa)
           (t  
            (setf *jogada* (first sucessores))
            (min novo-beta (alfa-beta-min (rest sucessores) profundidade-max 
                                           jogador-max timestamps alfa novo-beta))))))
)
```

## Temporizador
Para jogadores computadores foi adicionado uma condição de tempo adicional definido pelo utilizador para limitar o tempo que o computador tem para fazer uma jogada.

Esta condição é usada no algoritmo MinMax alfabeta com a variável ***timestamps*** para impedir a geração de novos nós quando o computador estiver perto do tempo limite.

### Funções
Função predicado recebe a data e hora em que o temporizador começou e o tempo limite atribuido para  jogada. Compara o tempo passado e retorna True se já tiver passado 90% do tempo atribuido.

``` Lisp
(defun has-passed-90-percent (startTimestamp timeLimit)
  "Verifica se pelo menos 90% do tempo passou desde o inicio da contagem"
  (let* ((currentTime (get-internal-real-time)))
    (>= (- currentTime startTimestamp) (* 0.9 timeLimit))
  )
)
```


## Resultados


