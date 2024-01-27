;; Nó -  (pontos1 pontos2 position1 table profundidade curJogador pai heuristica position2)

;(minmax (no-teste5) 2 *jogador1*)
;(minmax (no-teste5) 3 *jogador1*)

(defun minmax (node profundidade-max jogador-max)
  (format t "Profundidade: ~A~%" (fifth node))
  (format t "Jogador: ~A~%" (sixth node))
  (format t "Pontuação1: ~A~%" (first node))
  (format t "Pontuação2: ~A~%" (second node))
  (format t "Heurística: ~A~%" (heuristic-minmax node))
  (format t "Melhor Nó: ~A~%" *jogada*)
  (format t "Pai: ~A~%" (seventh node))
  (format t "\n")
  (printTable (fourth node))
  (let ((result (cond 
    ((or (eq (fifth node) profundidade-max) 
         (null (successors node (get-operators) profundidade-max nil)))               ;Nó raiz ou folha
                            (heuristic-minmax node))  ; (cons node (heuristic-minmax node)))   (heuristic-minmax node))
    (t (let ((sucessores (successors node (get-operators) profundidade-max nil)))
                         (cond
                           ((eq (sixth node) jogador-max)                             ;Nó Max
                                (minmax-max sucessores profundidade-max jogador-max))   
                           (t                                                         ;Nó Min
                                (minmax-min sucessores profundidade-max jogador-max))))))))
  
  result)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; FASE2  ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Constantes:
;;;;


(defvar *maximum-value* 10000)
(defvar *minimum-value* -10000)
(defvar *cortes-alfa* 0)
(defvar *cortes-beta* 0)



;(minmax-test (minmax (no-teste5) 3 *jogador1*))
(defun minmax-test (function)
  (format t "Resultado: ~A~%" function)
  (format t "Melhor Nó: ~A~%" *jogada*)
  (format t "Cortes Alfa: ~A~%" *cortes-alfa*)
  (format t "Cortes Beta: ~A~%" *cortes-beta*)
)


(defun minmax-max (sucessores profundidade-max jogador-max)
  (max-value-recursive sucessores profundidade-max jogador-max most-negative-fixnum)); (cdr result)


(defun max-value-recursive (sucessores profundidade-max jogador-max current-max)
  (if (null sucessores)
      current-max
      (let ((child-result (minmax (first sucessores) profundidade-max jogador-max))
             (profundidade (fifth (first sucessores)))) ;varriável para teste
        (cond 
          ((>= child-result current-max) 
           (max-value-recursive (rest sucessores) profundidade-max jogador-max child-result))
          (t 
           (setf *jogada* (first sucessores))
           (min-value-recursive (rest sucessores) profundidade-max jogador-max current-max))))))


(defun minmax-min (sucessores profundidade-max jogador-max)
  (min-value-recursive sucessores profundidade-max jogador-max most-positive-fixnum)) ; (cdr result)


(defun min-value-recursive (sucessores profundidade-max jogador-max current-min)
  (if (null sucessores)
      current-min
      (let ((child-result (minmax (first sucessores) profundidade-max jogador-max))
            (profundidade (fifth (first sucessores)))) ;variável para teste)
        (cond 
          ((<= child-result current-min) 
           (min-value-recursive (rest sucessores) profundidade-max jogador-max child-result))
          (t 
           (setf *jogada* (first sucessores))
           (min-value-recursive (rest sucessores) profundidade-max jogador-max current-min))))))




;(alfa-beta (no-teste5) 3 *jogador1*)
(defun alfa-beta (node profundidade-max jogador-max timestamps &optional (alfa *minimum-value*) (beta *maximum-value*) (stats '(0 0 0 0 0)))
  (format t "Profundidade: ~A~%" (fifth node))
  (format t "Jogador: ~A~%" (sixth node))
  (format t "Pontuação1: ~A~%" (first node))
  (format t "Pontuação2: ~A~%" (second node))
  (format t "Heurística: ~A~%" (heuristic-minmax node))
  (format t "Melhor Nó: ~A~%" *jogada*)
  (format t "Pai: ~A~%" (seventh node))
  (format t "Nós gerados: ~A~%" (first stats))
  (format t "Nós expandidos: ~A~%" (second stats))
  (format t "Nós avaliados: ~A~%" (third stats))
  (format t "Nós cortes-alfa: ~A~%" (fourth stats))
  (format t "Nós cortes-beta: ~A~%" (fifth stats))
  (printTable (fourth node))
  (format t "\n")
  (let* ((result (cond
    ((or (eq (fifth node) profundidade-max)
         (null node) 
         (null (successors node (get-operators) profundidade-max nil))
         (has-passed-90-percent (first timestamps) (second timestamps)))               ;Nó raiz ou folha
                            (heuristic-minmax node))  ; (cons node (heuristic-minmax node)))   (heuristic-minmax node))
    (t (let ((sucessores (order-nodes (successors node (get-operators) profundidade-max nil) (sixth node) jogador-max))) ; (ordenar (|# successors node (get-operators) profundidade-max nil #|) (sixth node) jogador-max)
                         
                         (cond
                           ((eq (sixth node) jogador-max)                             ;Nó Max
                                (alfa-beta-max sucessores profundidade-max 
                                               jogador-max timestamps alfa beta (update-stats stats '(0 1 0 0 0))))   
                           (t                                                         ;Nó Min
                                (alfa-beta-min sucessores profundidade-max 
                                               jogador-max timestamps alfa beta (update-stats stats '(0 1 0 0 0))))))
  ))))
  
  result)
)

;;Caso de nó MAX
;;Função retorna alfa se não houverem sucessores, 
;;com sucessores a função verifica o máximo entre beta e o valor alfa do sucessor
;;corta o ramo se beta for menor, caso contrário continua a seguir o ramo
(defun alfa-beta-max (sucessores profundidade-max jogador-max timestamps alfa beta stats)
  (if (null sucessores)
      alfa
      (let* ((child-result (alfa-beta (first sucessores) profundidade-max jogador-max timestamps alfa beta stats))
             (profundidade (fifth (first sucessores))) ;varriável para teste
             (novo-alfa (max alfa child-result)))
        (cond 
          ((> novo-alfa beta) 
              (setf *cortes-alfa* (+ *cortes-alfa* 1))
              (update-stats stats '(0 0 0 1 0))
              beta)
          (t 
           (setf *jogada* (first sucessores))
           (max novo-alfa (alfa-beta-max (rest sucessores) profundidade-max 
                                          jogador-max timestamps novo-alfa beta (update-stats stats '(0 0 1 0 0))))))))
)


;;Caso de nó MIN
;;Função retorna beta se não houverem sucessores, 
;;com sucessores a função verifica o mínimo entre alfa e o valor beta do sucessor
;;corta o ramo se alfa for maior, caso contrário continua a seguir o ramo
(defun alfa-beta-min (sucessores profundidade-max jogador-max timestamps alfa beta stats)
  (if (null sucessores)
       beta 
       (let* ((child-result (alfa-beta (first sucessores) profundidade-max jogador-max timestamps alfa beta stats))
             (profundidade (fifth (first sucessores))) ;varriável para teste
             (novo-beta (min beta child-result)))
         (cond 
           ((< novo-beta alfa)  
               (setf *cortes-beta* (+ *cortes-beta* 1))
               (update-stats stats '(0 0 0 0 1))    
               alfa)
           (t  
            (setf *jogada* (first sucessores))
            (min novo-beta (alfa-beta-min (rest sucessores) profundidade-max 
                                           jogador-max timestamps alfa novo-beta (update-stats stats '(0 0 1 0 0))))))))
)



;(alfa-beta-test (alfa-beta (no-teste5) 2 *jogador1*))
;(alfa-beta-test (alfa-beta (no-teste5) 3 *jogador1* (list (get-internal-real-time) 5000))); 5 segundos
(defun alfa-beta-test (function)
  (format t "Resultado: ~A~%" function)
  (format t "Melhor Nó: ~A~%" *jogada*)
  (format t "Nós cortes-alfa: ~A~%" *cortes-alfa*)
  (format t "Nós cortes-beta: ~A~%" *cortes-beta*)
  (setf *cortes-alfa* 0)
  (setf *cortes-beta* 0)
)

#|

(defun minmax-max (sucessores profundidade-max jogador-max)
  (max-value-recursive sucessores profundidade-max jogador-max *minimum-value*))

(defun max-value-recursive (sucessores profundidade-max jogador-max current-max)
  (if (null sucessores)
      current-max
      (let ((child-value (minmax (first sucessores) profundidade-max (not jogador-max))))
        (max-value-recursive (rest sucessores) profundidade-max jogador-max (max child-value current-max)))))

(defun minmax-min (sucessores profundidade-max jogador-max)
  (min-value-recursive sucessores profundidade-max jogador-max *maximum-value*))

(defun min-value-recursive (sucessores profundidade-max jogador-max current-min)
  (if (null sucessores)
      current-min
      (let ((child-value (minmax (first sucessores) profundidade-max (not jogador-max))))
        (min-value-recursive (rest sucessores) profundidade-max jogador-max (min child-value current-min)))))

|#
;(defun minmax-max (sucessores profundidade-max jogador-max)
;  ()
;)

;(defun minmax-min (sucessores profundidade-max jogador-max))


#|
 ******************** Statistics ********************
|#

; stats: (nos-gerados nos-expandidos nos-avaliados cortes-alfa cortes-beta)
(defun update-stats (stats values)
  (mapcar #'(lambda (s v) (+ s v)) stats values)
)

(defun has-passed-90-percent (startTimestamp timeLimit)
  "Check if at least 90% of the time has passed since the start timestamp."
  (let* ((currentTime (get-internal-real-time)))
    (>= (- currentTime startTimestamp) (* 0.9 timeLimit))
  )
)

(defun ordenar-nos-alfa-beta (lista-de-nos jogador-max)
  "Função para ordenar nós no Alfa-Beta com base no jogador (Max ou Min)."
  (sort lista-de-nos
        #'(lambda (a b)
            (if (eql jogador-max 'Max) ; Verifique se o jogador atual é Max
                (> (terceiro a) (terceiro b)) ; Ordene por valor decrescente
                (< (terceiro a) (terceiro b)))))) ; Ordene por valor crescente


(defun max-node-sort (nodes)
  "Retorna uma lista de nós ordenados por heuristica decrescente"
  (cond
    ((null nodes) nil)
    (t (let* ((min-node (find-min-node nodes))
           (rest-nodes (remove min-node nodes)))
      (append  (list min-node) (max-node-sort rest-nodes)
             ))))
)

(defun find-max-node (nodes)
  "Encontra o nó com melhor heuristica na lista de nós"
  (reduce #'(lambda (a b)
              (if (> (heuristic-minmax a) (heuristic-minmax b))
                a
                b))
          nodes)
)


(defun min-node-sort (nodes)
  "Retorna uma lista de nós ordenados por heuristica crescente"
  (cond
    ((null nodes) nil)
    (t (let* ((max-node (find-max-node nodes))
           (rest-nodes (remove max-node nodes)))
      (append  (list max-node) (min-node-sort rest-nodes)
             ))))
)

(defun find-min-node (nodes)
  "Encontra o nó com pior heuristica na lista de nós"
  (reduce #'(lambda (a b)
              (if (< (heuristic-minmax a) (heuristic-minmax b))
                a
                b))
          nodes)
)



#|
 ******************** Tests ********************
|#


(defun printTable (table)
  (cond 
   ((null table) (format t "~%"))
   (t (printList (car table)) (printTable (cdr table)))
  )
)


(defun printList (lista)
  (cond
   ((null lista) (format t "~%~%"))
   (t (if (null (car lista)) (format t " -- ") (format t " ~2,' d " (car lista))) (printList (cdr lista)))
  )
)