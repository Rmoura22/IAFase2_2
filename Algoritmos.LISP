#|
Implementação dos métodos de procura e com a implementação das métricas de
análise de eficiência, i.e. a parte do programa que é independente do domínio de aplicação
Nó - (pontos posCavalo tabela profundidade pai heuristica)
|#

(defpackage "Algorithms")

;(bfs1 (no-teste2) 'issolution (get-operators))
(defun BFS1 (root pred operators objective)
 ""
 ;openList - Lista de abertos
 ;closedList - Lista de fechados
 ;sucs - Sucessores, é atualizado em BFS-rec
 ;solution - Resultado da função any, se nil então ainda não encontrou solução
  (let* ((openList (list root)) (closedList nil) (sucs nil) (solution nil))
   (labels (
    (BFS-rec ()
     (cond 
      ((null openList) "This shit aint work")
      ;((node-exists (car openList) closedList 'bfs) ())
      (t
        (setf sucs (remove-if #'null (funcall 'successors (car openList) operators)))
        (setf solution (any pred sucs objective))
        (cons (car openList) closedList)
        (setf openList (appendToOpenList sucs (cdr openList) closedList 'bfs))
        (if solution 
         solution 
         (BFS-rec)
        )
      )
     )
    ))
   (BFS-rec)
   )
  ) 
)

;(bfs2 (no-teste2) 'issolution 'successors (get-operators))
(defun BFS (root pred sucFunc operators objective &optional (openList (list root)) (closedList nil))
  (cond 
      ;Se lita de abertos vazia falha
   ((null openList) "No Solution found :(((")
   ; Gerar sucessores e verificar se algum é solução
   (t (let* ((sucs (funcall sucFunc (car openList) operators)) (solution (any pred sucs objective)))
        (cond   
           ;Se algum é solução retorna      
         ((not (null solution)) (list solution openList closedList))
           ;Se não encontrou solução adiciona os sucessores a lista de abertos e repete
         (t (BFS root pred sucFunc operators objective (append (cdr openList) sucs) (cons (car openList) closedList)))
         )
        )
      )
   )
)

;(dfs (no-teste2) 'issolution 'successors (get-operators) :maxDepth 40)
(defun DFS (root pred sucFunc operators objective &optional (openList (list root)) (closedList nil) (maxDepth 10))
  (cond 
      ;Se lita de abertos vazia falha
   ((null openList) "No Solution found :(((")
   ; Gerar sucessores e verificar se algum é solução
   (t (let* ((sucs (funcall sucFunc (car openList) operators maxDepth)) (solution (any pred sucs)))
        (cond   
           ;Se algum é solução retorna      
         ((not (null solution)) solution)
           ;Se não encontrou solução adiciona os sucessores ao inicio da lista de abertos e repete
         (t (DFS root pred sucFunc operators objective (append sucs (cdr openList)) (cons (car openList) closedList)))
        )
      )
   )
  )
)

;(a-star (no-teste2) 'issolution 'successors (get-operators) 'calcheuristic)
(defun A-Star (root predFunc sucFunc operators heuristicFunc objective &optional (openList (list root)) (closedList nil))
 (cond
  ((null openList) "Keine Lösung gefunden")
  (t (let* ((sucs (funcall sucFunc (car openList) operators 100 heuristicfunc)) (solution (any predFunc sucs)))

       (cond   
           ;Se algum é solução retorna      
         ((not (null solution)) solution)
           ;Se não encontrou solução adiciona os sucessores ao inicio da lista de abertos e repete
         (t (A-Star root predFunc sucFunc operators heuristicfunc (appendToOpenList sucs (cdr openList) (cons (car openList) closedList) 'aStar) (cons (car openList) closedList)))
        )
      )
  )
 )
)

(defun insertOrderedDecHeuristic (Node Nodes)
 (cond 
  ((null Nodes) (list Node))
  ((< (sixth Node) (sixth (car Nodes))) (cons Node Nodes))
  (t (cons (first Nodes) (insertOrderedDecHeuristic Node (cdr Nodes))))
 )
)

(defun appendToOpenList (sucs openList closedList alg)
 (cond 
  ((null sucs) openList)
  ((node-exists (car sucs) closedList alg) (appendToOpenList (cdr sucs) openList closedList alg))
  ((equal alg 'aStar) 
   (insertOrderedDecHeuristic (car sucs) (appendToOpenList (cdr sucs) openList closedList alg))
  )
  ((equal alg 'bfs)
   (append (appendToOpenList (cdr sucs) openList closedList alg) (list(car sucs)))
  )
  ((equal alg 'dfs)
   (cons (car sucs) (appendToOpenList (cdr sucs) openList closedList alg))
  )
 )
)

(defun calcHeuristic (node objective)
  (append (subseq node 0 5) (list(heuristic1 node objective)))
)


(defun any (pred lista objective)
 (cond
  ((null lista) nil)
  ((funcall pred  (car lista) objective) (car lista))
  (t (any pred (cdr lista) objective))
 )
)

(defun node-exists (node nodes alg)
"Returns t if the give node exists in the nodes list otherwise nil"
 (cond 
  ((null nodes) nil)

  ((equal alg 'bfs)
   (cond 
    ((equal (third node) (third (first nodes))) t)
    (t (node-exists node (cdr nodes) alg))
   )
  )

  ((equal alg 'dfs)
   (cond 
    ((and (> (fourth node) (fourth (first nodes))) (equal (third node) (third (first nodes)))) t)
    (t (node-exists node (cdr nodes) alg))
   )  )
 )
)

(defun isSolution (node objective)
 (>= (first node) objective)
)