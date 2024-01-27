;;;;
;;;; Constantes:
;;;;


(defvar *maximum-value* 10000)
(defvar *minimum-value* -10000)

;(alfa-beta (no-teste5) 3 *jogador1* (list (get-internal-real-time) 5000))

;; Fun��o de algoritmo alfabeta
; Retorna o valor heuristico do n� a partir da fun��o heuristic-minmax se:
;  - N� tiver na profundidade limite
;  - N� for nulo
;  - N� for n� folha
;  - N� for nulo
;  - Tiver passado +90% do tempo atribuido
; Ou execute a fun��o alfa-beta-max para n�s Max
; Ou execute a fun��o alfa-beta-max para n�s Min
(defun alfa-beta (node profundidade-max jogador-max timestamps &optional (alfa *minimum-value*) (beta *maximum-value*))
  (cond
    ((or (eq (fifth node) profundidade-max)
         (null node) 
         (null (successors node (get-operators) profundidade-max nil))
         (has-passed-90-percent (first timestamps) (second timestamps)))             
                            (heuristic-minmax node jogador-max))  ; 
    (t (let ((sucessores (order-nodes (successors node (get-operators) profundidade-max nil) (sixth node) jogador-max)))
                         (cond
                           ;N� Max
                           ((eq (sixth node) jogador-max)                             
                                (alfa-beta-max sucessores profundidade-max 
                                               jogador-max timestamps alfa beta)) 
                           ;N� Min  
                           (t                                                         
                                (alfa-beta-min sucessores profundidade-max 
                                               jogador-max timestamps alfa beta))))
  ))
)

;;Caso de n� MAX
;;Fun��o retorna alfa se n�o houverem sucessores, 
;;com sucessores a fun��o verifica o m�ximo entre beta e o valor alfa do sucessor
;;corta o ramo se beta for menor, caso contr�rio continua a seguir o ramo
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


;;Caso de n� MIN
;;Fun��o retorna beta se n�o houverem sucessores, 
;;com sucessores a fun��o verifica o m�nimo entre alfa e o valor beta do sucessor
;;corta o ramo se alfa for maior, caso contr�rio continua a seguir o ramo
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
  (format t "Melhor N�: ~A~%" *jogada*)
  (format t "N�s cortes-alfa: ~A~%" *cortes-alfa*)
  (format t "N�s cortes-beta: ~A~%" *cortes-beta*)
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

;Fun��o predicado � True se j� tiver passado pelo menos 90% do tempo desde o inico da contagem
(defun has-passed-90-percent (startTimestamp timeLimit)
  "Verifica se pelo menos 90% do tempo passou desde o inicio da contagem"
  (let* ((currentTime (get-internal-real-time)))
    (>= (- currentTime startTimestamp) (* 0.9 timeLimit))
  )
)

;Fun��o ordena os n�s Max atrv�s da fun��o max-node-sort 
; ou n�s Min atrav�s da fun��o min-node-sort
(defun order-nodes (nodes jogador-max jogador)
  (cond
    ((eq jogador-max jogador) (max-node-sort nodes jogador-max))
    (t (min-node-sort nodes jogador-max))
  )
)

;Fun��o retorna a lista de n�s ordenados por valor de heuristica decrescente
(defun max-node-sort (nodes jogador-max)
  "Retorna uma lista de n�s ordenados por heuristica decrescente"
  (cond
    ((null nodes) nil)
    (t (let* ((min-node (find-min-node nodes jogador-max))
           (rest-nodes (remove min-node nodes)))
      (append  (list min-node) (max-node-sort rest-nodes jogador-max)
             ))))
)

;Fun��o o n� com melhor valor de heuristica na lista de n�s
(defun find-max-node (nodes jogador-max)
  "Encontra o n� com melhor heuristica na lista de n�s"
  (reduce #'(lambda (a b)
              (if (> (heuristic-minmax a jogador-max) (heuristic-minmax b jogador-max))
                a
                b))
          nodes)
)

;Fun��o retorna a lista de n�s ordenados por valor de heuristica crescente
(defun min-node-sort (nodes jogador-max)
  "Retorna uma lista de n�s ordenados por heuristica crescente"
  (cond
    ((null nodes) nil)
    (t (let* ((max-node (find-max-node nodes jogador-max))
           (rest-nodes (remove max-node nodes)))
      (append  (list max-node) (min-node-sort rest-nodes jogador-max)
             ))))
)

;Fun��o o n� com pior valor de heuristica na lista de n�s
(defun find-min-node (nodes jogador-max)
  "Encontra o n� com pior heuristica na lista de n�s"
  (reduce #'(lambda (a b)
              (if (< (heuristic-minmax a jogador-max) (heuristic-minmax b jogador-max))
                a
                b))
          nodes)
)



#|
 ******************** Tests ********************
|#

