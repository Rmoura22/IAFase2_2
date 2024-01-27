;;;;
;;;; Constantes:
;;;;


(defvar *maximum-value* 10000)
(defvar *minimum-value* -10000)

;(alfa-beta (no-teste5) 3 *jogador1* (list (get-internal-real-time) 5000))

;; Função de algoritmo alfabeta
; Retorna o valor heuristico do nó a partir da função heuristic-minmax se:
;  - Nó tiver na profundidade limite
;  - Nó for nulo
;  - Nó for nó folha
;  - Nó for nulo
;  - Tiver passado +90% do tempo atribuido
; Ou execute a função alfa-beta-max para nós Max
; Ou execute a função alfa-beta-max para nós Min
(defun alfa-beta (node profundidade-max jogador-max timestamps &optional (alfa *minimum-value*) (beta *maximum-value*))
  (cond
    ((or (eq (fifth node) profundidade-max)
         (null node) 
         (null (successors node (get-operators) profundidade-max nil))
         (has-passed-90-percent (first timestamps) (second timestamps)))             
                            (heuristic-minmax node jogador-max))  ; 
    (t (let ((sucessores (order-nodes (successors node (get-operators) profundidade-max nil) (sixth node) jogador-max)))
                         (cond
                           ;Nó Max
                           ((eq (sixth node) jogador-max)                             
                                (alfa-beta-max sucessores profundidade-max 
                                               jogador-max timestamps alfa beta)) 
                           ;Nó Min  
                           (t                                                         
                                (alfa-beta-min sucessores profundidade-max 
                                               jogador-max timestamps alfa beta))))
  ))
)

;;Caso de nó MAX
;;Função retorna alfa se não houverem sucessores, 
;;com sucessores a função verifica o máximo entre beta e o valor alfa do sucessor
;;corta o ramo se beta for menor, caso contrário continua a seguir o ramo
(defun alfa-beta-max (sucessores profundidade-max jogador-max timestamps alfa beta)
  (if (null sucessores)
      alfa
      (let* ((child-result (alfa-beta (first sucessores) profundidade-max jogador-max timestamps alfa beta))
             (novo-alfa (max alfa child-result)))
        (cond 
          ((> novo-alfa beta) 
              (setf *cortes-alfa* (+ *cortes-alfa* 1))
              beta)
          (t 
           (setf *jogada* (first sucessores))
           (max novo-alfa (alfa-beta-max (rest sucessores) profundidade-max 
                                          jogador-max timestamps novo-alfa beta))))))
)


;;Caso de nó MIN
;;Função retorna beta se não houverem sucessores, 
;;com sucessores a função verifica o mínimo entre alfa e o valor beta do sucessor
;;corta o ramo se alfa for maior, caso contrário continua a seguir o ramo
(defun alfa-beta-min (sucessores profundidade-max jogador-max timestamps alfa beta)
  (if (null sucessores)
       beta 
       (let* ((child-result (alfa-beta (first sucessores) profundidade-max jogador-max timestamps alfa beta))
             (novo-beta (min beta child-result)))
         (cond 
           ((< novo-beta alfa)  
               (setf *cortes-beta* (+ *cortes-beta* 1))
               alfa)
           (t  
            (setf *jogada* (first sucessores))
            (min novo-beta (alfa-beta-min (rest sucessores) profundidade-max 
                                           jogador-max timestamps alfa novo-beta))))))
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
 ******************** Statistics ********************
|#

; stats: (nos-gerados nos-expandidos nos-avaliados cortes-alfa cortes-beta)
; Lista de estatisticas
(defun update-stats (stats values)
  (mapcar #'(lambda (s v) (+ s v)) stats values)
)

#|
 ******************** Extras ********************
|#

;Função predicado é True se já tiver passado pelo menos 90% do tempo desde o inico da contagem
(defun has-passed-90-percent (startTimestamp timeLimit)
  "Verifica se pelo menos 90% do tempo passou desde o inicio da contagem"
  (let* ((currentTime (get-internal-real-time)))
    (>= (- currentTime startTimestamp) (* 0.9 timeLimit))
  )
)

;Função ordena os nós Max atrvés da função max-node-sort 
; ou nós Min através da função min-node-sort
(defun order-nodes (nodes jogador-max jogador)
  (cond
    ((eq jogador-max jogador) (max-node-sort nodes jogador-max))
    (t (min-node-sort nodes jogador-max))
  )
)

;Função retorna a lista de nós ordenados por valor de heuristica decrescente
(defun max-node-sort (nodes jogador-max)
  "Retorna uma lista de nós ordenados por heuristica decrescente"
  (cond
    ((null nodes) nil)
    (t (let* ((min-node (find-min-node nodes jogador-max))
           (rest-nodes (remove min-node nodes)))
      (append  (list min-node) (max-node-sort rest-nodes jogador-max)
             ))))
)

;Função o nó com melhor valor de heuristica na lista de nós
(defun find-max-node (nodes jogador-max)
  "Encontra o nó com melhor heuristica na lista de nós"
  (reduce #'(lambda (a b)
              (if (> (heuristic-minmax a jogador-max) (heuristic-minmax b jogador-max))
                a
                b))
          nodes)
)

;Função retorna a lista de nós ordenados por valor de heuristica crescente
(defun min-node-sort (nodes jogador-max)
  "Retorna uma lista de nós ordenados por heuristica crescente"
  (cond
    ((null nodes) nil)
    (t (let* ((max-node (find-max-node nodes jogador-max))
           (rest-nodes (remove max-node nodes)))
      (append  (list max-node) (min-node-sort rest-nodes jogador-max)
             ))))
)

;Função o nó com pior valor de heuristica na lista de nós
(defun find-min-node (nodes jogador-max)
  "Encontra o nó com pior heuristica na lista de nós"
  (reduce #'(lambda (a b)
              (if (< (heuristic-minmax a jogador-max) (heuristic-minmax b jogador-max))
                a
                b))
          nodes)
)



#|
 ******************** Tests ********************
|#

