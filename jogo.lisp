;Logic for the Knight Tour Project

(defun convert-to-alfa-beta-node (interacNode)
"It was too hard to change everything to the same node architecture"
  (list
    (get-player-points (get-Node-CurPlayer interacNode))
    (get-player-points (get-Node-NextPalyer interacNode))
    (get-player-pos (get-Node-CurPlayer interacNode))
    (get-Node-Table interacNode)
    (get-Node-Depth interacNode)
    *jogador1*
    nil
    nil
    (get-player-pos (get-Node-NextPalyer interacNode))
  )
)

(defun convert-to-interact-node (algoNode)
"It was too hard to change everything to the same node architecture"
  (list
    (fourth algoNode)
    (if (equal *jogador1* (sixth algoNode)) (list (third algoNode) (first algoNode)) (list (ninth algoNode) (second algoNode)))
    (if (equal *jogador1* (sixth algoNode)) (list (ninth algoNode) (second algoNode)) (list (third algoNode) (first algoNode)))
    (fifth algoNode)
    nil
    nil 
  )
)

(defun invert-players (algoNode)
  (list
    (second algoNode)
    (first algoNode)
    (ninth algoNode)
    (fourth algoNode)
    (fifth algoNode)
    (sixth algoNode)
    nil
    nil
    (third algoNode)
  )
)

(defun convert-all-to-alfa-beta-nodes (interacNodes)
  (cond
    ((null interacNodes) nil)
    (t (cons (invert-players (convert-to-alfa-beta-node (car interacNodes))) (convert-all-to-alfa-beta-nodes (cdr interacNodes))))
  )
)


(defun successors (node funcs)
  (remove-if #'null (successors-rec node funcs))
)

(defun successors-rec (node funcs)
  (cond 
    ((null funcs) nil)
    (t (insertOrderedDecHeuristic (apply (car funcs) (list node)) (successors-rec node (cdr funcs))))
  )
)

(defun insertOrderedDecHeuristic (Node Nodes)
 (cond 
  ((null (car nodes)) (list Node))
  ((null (sixth Node)) (append nodes (list Node)))
  ((< (sixth Node) (sixth (car Nodes))) (cons Node Nodes))
  (t (cons (first Nodes) (insertOrderedDecHeuristic Node (cdr Nodes))))
 )
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

(defun get-operator (line col)
  
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

(defun Operator2 (node)
 (move node (+ (horsePosLine node) 1) (+ (horsePosColumn node) -2)'operator2)
)

(defun Operator3 (node)
 (move node (+ (horsePosLine node) 2) (+ (horsePosColumn node) 1)'operator3)
)

(defun Operator4 (node)
 (move node (+ (horsePosLine node) 2) (+ (horsePosColumn node) -1)'operator4)
)

(defun Operator5 (node)
 (move node (+ (horsePosLine node) -1) (+ (horsePosColumn node) -2)'operator5)
)

(defun Operator6 (node)
 (move node (+ (horsePosLine node) -1) (+ (horsePosColumn node) 2)'operator6)
)

(defun Operator7 (node)
 (move node (+ (horsePosLine node) -2) (+ (horsePosColumn node) -1)'operator7)
)

(defun Operator8 (node)
 (move node (+ (horsePosLine node) -2) (+ (horsePosColumn node) 1)'operator8)
)


#|
 ******************** Movement ********************
|#

(defun move (node line column name)
 (let ((value (cel (first node) line column)))
  (cond
   ((or (null value) (equal value 'WK) (equal value 'BK)) nil)
   (t (list 
        (removeOther (replaceInTableIndex (replaceInTableIndex (first node) (first (get-player-pos (get-node-curPlayer node))) (second (get-player-pos (get-node-curPlayer node)))) line column (cel (get-node-table node) (first (get-player-pos (get-node-curPlayer node))) (second (get-player-pos (get-node-curPlayer node))))) value)
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

(defun upadate-player (player newPos points)
  (list newPos (+ (second player) points))
)


(defun removeOther (table num)
"Remove o oposto ou maior duplo dependendo se o numero é duplo."  
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
  "Encontra e retorna o maior número duplo no tabuleiro"
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
  "Encontra e retorna o maior número duplo numa linha"
  (cond
    ((null lista) max-duplicate)
    ((null (first lista)) (findHighstDubInList (cdr lista) max-duplicate))
    ((and (numberp (car lista)) (> (first lista) max-duplicate) (doubleNumP (first lista)))
     (findHighstDubInList (cdr lista) (first lista)))
    (t (findHighstDubInList (cdr lista) max-duplicate))
  )
)


(defun horsePosLine (node)
 "Linha em que o cavalo se encontra dado um no"
 (first (first (second node)))
)

(defun horsePosColumn (node)
 "Coluna em que o cavalo se encontra dado um no"
 (second (first (second node)))
)

(defun cel (table line column)
 (cond
  ((or (< line 0) (< column 0) (> line 9) (> column 9)) nil)
  (t (nth column (nth line table)))
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

(defun makeNode (table)
 (list 0 nil table 0 nil nil)
)

(defun no-teste1 ()
  (list 0 nil (tabuleiro-teste1) 0 nil 3)
)

(defun no-teste2 ()
  (list 0 '(0 0) (tabuleiro-teste2) 0 nil 2)
)

(defun no-teste3 ()
  (list 12 '(1 2) (tabuleiro-teste2) 1 nil 1)
)