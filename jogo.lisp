

(defpackage "Logic")


;;;;
;;;; Constantes:
;;;;
(defvar *jogador1* -1)
(defvar *jogador2* -2)
(defvar *jogada* nil)



;--------------------- OPERADORES -----------------------
(defun trocar-jogador (jogador)
  "Troca a peca de um jogador para a peca de outro jogador."
  (- -3 jogador)
)


(defun get-operators ()
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

(defun Operator1 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) 1) (+ (horsePosColumn posicao-jogador) 2) 'operator1 heur))
)

(defun Operator2 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) 1) (+ (horsePosColumn posicao-jogador) -2)'operator2 heur))
)

(defun Operator3 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) 2) (+ (horsePosColumn posicao-jogador) 1)'operator3 heur))
)

(defun Operator4 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) 2) (+ (horsePosColumn posicao-jogador) -1)'operator4 heur))
)

(defun Operator5 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) -1) (+ (horsePosColumn posicao-jogador) -2)'operator5 heur))
)

(defun Operator6 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) -1) (+ (horsePosColumn posicao-jogador) 2)'operator6 heur))
)

(defun Operator7 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) -2) (+ (horsePosColumn posicao-jogador) -1)'operator7 heur))
)

(defun Operator8 (node &optional (heur nil))
  (let ((posicao-jogador (if (eq (sixth node) -1) 
                           (third node) 
                           (ninth node))))
  (move node (+ (horsePosLine posicao-jogador) -2) (+ (horsePosColumn posicao-jogador) 1)'operator8 heur))
)




;--------------------- Movimento -----------------------
;; Nó -  (pontos1 pontos2 position1 table profundidade jogador pai heuristica position2)
(defun move (node line column name &optional (heur nil))
 (let ((points (cel (fourth node) line column)))
  (cond
   ((null points) nil)
   (t (let ((newNode (list
           (if (eq (sixth node) -1) 
               (+ (first node)  points)
               (first node)
           )
           (if (eq (sixth node) -2) 
                (+ (second node)  points)
                (second node)
           ) 
           (if (eq (sixth node) -1) 
               (list line column)
               (third node)
           )
           (removeOther (replaceInTableIndex (fourth node) line column (sixth node)) points)
           (1+ (fifth node))
           (trocar-jogador (sixth node))
           (append (seventh node) (list name))
           nil
           (if (eq (sixth node) -2) 
                (list line column)
                (ninth node)
           )
         )))
        (if (null heur) newNode (funcall heur newNode 400))
      )
   )
  )
 )
)

(defun removeOther (table num)
"Removes the oposite number or the highst dub"
  (cond 
   ((doubleNumP num) (replaceInTable table (findHighstDubInTable table)))
   (t (replaceInTable table (reverseNum num)))

  )
)

(defun doubleNumP (num) (zerop (mod num 11)))


(defun reverseNum (num) (+ (* (mod num 10) 10) (/ (- num (mod num 10)) 10)))


(defun replaceInTable (table num &optional (value nil))
 (cond
  ((null table) nil)
  ((find num (car table)) (cons (replaceInList (car table) num value) (cdr table)))
  (t (cons (car table) (replaceInTable (cdr table) num value)))
 )
)

(defun replaceInList (lista num &optional (value nil))
 (cond
  ((null lista) nil)
  ((equal (car lista) num) (cons value (cdr lista)))
  (t (cons (car lista)(replaceInList (cdr lista) num value)))
 )
)

(defun replaceInTableIndex (table line column &optional (value nil))
 (append (subseq table 0 line) (list (replaceInLineIndex (nth line table) column value)) (subseq table (1+ line)))
)

(defun replaceInLineIndex (lista index &optional (value nil))
 (append (subseq lista 0 index) (list value) (subseq lista (1+ index) ))
)

(defun findHighstDubInTable (tabuleiro &optional (max-duplicate 0))
  "Encontra e retorna o maior nÃºmero duplo no tabuleiro"
  (cond
    ((null tabuleiro) max-duplicate)
    (t (findHighstDubInTable 
        (cdr tabuleiro) 
        (findHighstDubInList (car tabuleiro) max-duplicate)
       )
    )
  )
)

(defun findHighstDubInList (lista max-duplicate)
  "Encontra e retorna o maior nÃºmero duplo numa linha"
  (cond
    ((null lista) max-duplicate)
    ((and (not (null (car lista))) (> (first lista) max-duplicate) (doubleNumP (first lista))) 
     (findHighstDubInList (cdr lista) (first lista)))
    (t (findHighstDubInList (cdr lista) max-duplicate))
  )
)


(defun horsePosLine (table)
 "Linha em que o cavalo se encontra dado uma tabela"
 (first table)
)

(defun horsePosColumn (table)
 "Coluna em que o cavalo se encontra dado uma tabela"
 (second table)
)

(defun cel (table line column)
 (cond
  ((or (< line 0) (< column 0) (> line 9) (> column 9)) nil)
  (t (nth column (nth line table)))
 )
)


;--------------------- Data Structure -----------------------


;;Representação de um nó
; Contém ~ 1- Pontos do jogador 1
;          2- Pontos do jogador 2 
;          3- Posição jogador1
;          4- Tabuleiro 
;          5- Profundidade 
;          6- Jogador  
;          7- Pai
;          8- Heurística
;          9- Posição jogador2
(defun node (pontos1 pontos2 position1 table profundidade curjogador pai heuristica position2)
 (list pontos1 pontos2 position1 table profundidade curjogador pai heuristica position2)
)


(defun successors (node funcs &optional (maxDepth 100) (heur nil))
 (cond

  ((null (third node)) (remove-if #'null (firstsuccessors node heur)))
  ((> (1+ (fifth node)) maxdepth) nil)
  (t (remove-if #'null (successors-rec node funcs maxdepth heur))
  )
 )
)

(defun successors-rec (node funcs &optional (maxDepth 100) (heur nil))
 (cond
  ((null funcs) nil)
  (t
   (cons (apply (car funcs) (list node heur)) (successors-rec node (cdr funcs) maxdepth heur))
  )
 ) 
)

(defun firstSuccessors (node &optional (heur nil) (counter 0))
"Generate the first nodes, for when there is no knight on the board"
 (cond 
  ((= counter 9) (list (move node 0 counter 'firstsuccessors heur)))
  (t (cons (move node 0 counter (list 0 counter) heur) (firstSuccessors node heur (1+ counter))))
 )
)



#|
 ******************** HeurÃ­sticas ********************
|#
;; Nó -  (pontos1 pontos2 position table profundidade jogador-max pai heuristica)
;Função retorna o valor da heuristica da função de avaliação do minmax
(defun heuristic-minmax (node jogador-max)
  (cond
    ;JogadorMax é o Jogador1
    ((eq jogador-max *jogador1*) (- (first node) (second node)))
    ;JogadorMax é o Jogador2
    (t (- (second node) (first node)))
  )
)



#|
 ******************** Tests ********************
|#


(defun tabuleiro-teste1 ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (94 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 49 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
    )
)

(defun tabuleiro-teste2 ()
"Tabuleiro de teste sem nenhuma jogada realizada"
  '(
    (nil 25 54 89 21 8 36 14 41 96) 
    (78 47 56 23 5 nil 13 12 26 60) 
    (0 27 17 83 34 93 74 52 45 80) 
    (69 9 77 95 55 39 91 73 57 30) 
    (24 15 22 86 1 11 68 79 76 72) 
    (81 48 32 2 64 16 50 37 29 71) 
    (99 51 6 18 53 28 7 63 10 88) 
    (59 42 46 85 90 75 87 43 20 31) 
    (3 61 58 44 65 82 19 4 35 62) 
    (33 70 84 40 66 38 92 67 98 97)
   )
)


;; Nó -  (pontos1 pontos2 position table profundidade jogador-max pai heuristica)

(defun makeNode (table)
 (list 0 0 nil table 0 *jogador1* nil nil)
)

(defun no-teste1 ()
  (list 0 0 nil (tabuleiro-teste1) 0 *jogador1* nil nil)
)

(defun no-teste2 ()
  (list 0 0 '(0 0) (tabuleiro-teste2) 0 *jogador1* nil 2)
)

(defun no-teste3 ()
  (list 12 '(1 2) (tabuleiro-teste2) 1 nil 1)
)

; (successors (no-teste4) (get-operators) 5)
(defun no-teste4 ()
  (list 0 0 '(0 0) (tabuleiro-teste2) 0 *jogador1* nil nil)
)

; (successors (no-teste5) (get-operators) 5)
(defun no-teste5 ()
  (list 0 0 '(0 0) (tabuleiro-teste2) 0 *jogador1* nil nil '(9 5))
)

