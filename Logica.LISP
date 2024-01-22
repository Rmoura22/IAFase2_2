#|
 
|#

(defpackage "Logic")


(defun startAlgorithms (problem algorithms)
 (cond 
  ((null algorithms) nil)
  (t (cons (startAlgorithm problem (car algorithms)) (startAlgorithms problem (cdr algorithms))))
 )
)

(defun startAlgorithm (problem algorithm)
 (let* ((start (get-internal-real-time))
 		(result 
 			(ecase algorithm
 			 ('bfs (funcall algorithm (makeNode (third problem)) 'isSolution 'successors (get-operators) (second problem)))	
 			 ('dfs (funcall algorithm (makeNode (third problem)) 'isSolution 'successors (get-operators)))	
 			 ('a-star (funcall algorithm (makeNode (third problem)) 'isSolution 'successors (get-operators)) 'calcHeuristic)	
 			)
 		) 
 		(end (get-internal-real-time)))
 	(list
 		algorithm
 		(third problem)
 		(first result)
 		(list
 			(+ (length (second result)) (length (third result))) 
 			(length (third result)) 
 			(penetrancia (+ (length (second result)) (length (third result))) (fourth (first result)))
 			0
 			(- end start)
 		)
 	)
 )
)

(defun penetrancia (generated needed)
 (/ needed generated)
)


#|
 ******************** Heurísticas ********************
|#

(defun heuristic1 (node objective)
 (+ (fourth node )(/ (- objective (first node)) (MedianTable (third node))))
)

(defun MedianTable (table)
 (cond
  ((null table) 0)
  (t (/ (CelSum table) (CelCount table)))
 )
)

(defun CelSum (table)
  (reduce #'+
    (mapcar #'(lambda (lista)
      (reduce #'+
        lista
        :key #'(lambda (num) (if (null num) 0 num))
      ))
      table
    )
  )
)

(defun CelCount (table)
  (reduce #'+
    (mapcar #'(lambda (lista)
      (reduce #'+
        lista
        :key #'(lambda (num) (if (null num) 0 1))
      ))
      table
    )
  )
)


#|
 ******************** Sucessores ********************
|#

(defun successors (node funcs &optional (maxDepth 100) (heur nil))
 (cond

  ((null (second node)) (remove-if #'null (firstsuccessors node heur)))
  ((> (1+ (fourth node)) maxdepth) nil)
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
 (move node (+ (horsePosLine (second node)) 1) (+ (horsePosColumn (second node)) 2) 'operator1 heur)
)

(defun Operator2 (node &optional (heur nil))
 (move node (+ (horsePosLine (second node)) 1) (+ (horsePosColumn (second node)) -2)'operator2 heur)
)

(defun Operator3 (node &optional (heur nil))
 (move node (+ (horsePosLine (second node)) 2) (+ (horsePosColumn (second node)) 1)'operator3 heur)
)

(defun Operator4 (node &optional (heur nil))
 (move node (+ (horsePosLine (second node)) 2) (+ (horsePosColumn (second node)) -1)'operator4 heur)
)

(defun Operator5 (node &optional (heur nil))
 (move node (+ (horsePosLine (second node)) -1) (+ (horsePosColumn (second node)) -2)'operator5 heur)
)

(defun Operator6 (node &optional (heur nil))
 (move node (+ (horsePosLine (second node)) -1) (+ (horsePosColumn (second node)) 2)'operator6 heur)
)

(defun Operator7 (node &optional (heur nil))
 (move node (+ (horsePosLine (second node)) -2) (+ (horsePosColumn (second node)) -1)'operator7 heur)
)

(defun Operator8 (node &optional (heur nil))
 (move node (+ (horsePosLine (second node)) -2) (+ (horsePosColumn (second node)) 1)'operator8 heur)
)


#|
 ******************** Movement ********************
|#

(defun move (node line column name &optional (heur nil))
 (let ((points (cel (third node) line column)))
  (cond
   ((null points) nil)
   (t (let ((newNode (list 
           (+ (first node)  points) 
           (list line column)
           (removeOther (replaceInTableIndex (third node) line column) points)
           (1+ (fourth node))
           (append (fifth node) (list name))
           nil
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

#|
(defun teste (amount)
  (cond ((zerop amount) nil) (t (reverseNum2 45) (teste (- amount 1))))
)

(defun reverseNum2 (num)
  (parse-integer (reverse (write-to-string num)))
)
|#



(defun bisection-with-Newton-Raphson
       (f
        f-prime
        x-left
        x-right
        &key
        (accuracy (* 10.0 single-float-epsilon))
        (maximum-number-of-iterations 100))
  "
  from http://faculty.washington.edu/dbp/SAPACLISP-1.x/basic-math.lisp
  given
   [1] f (required)
       ==> a function with a single argument
   [2] f-prime (required)
       ==> another function with a single argument,
           this one being the first derivative of f
   [3] x-left (required)
       ==> left-hand bracket for the desired root;
           i.e., left-hand bracket <= desired root
   [4] x-right (required)
       ==> right-hand bracket for the desired root;
           i.e., desired root <= right-hand bracket
   [5] accuracy (keyword; (* 10.0 single-float-epsilon))
       ==> desired relative accuracy for computed rood
   [6] maximum-number-of-iterations (keyword; 100)
       ==> maximum number of iterations
returns
   [1] a root of f in [x-left, x-right];
       i.e., a value x in that interval
       such that f(x) = 0
   [2] the number of iterations required
---
Note: this function is based loosely on rtsafe,
Section 9.4, Numerical Recipes, Second Edition"
  (let ((f-low (funcall f x-left))
        (f-high (funcall f x-right))
        df f-new
        x-low x-high
        rtsafe dxold dx temp)
    (when (>= (* f-low f-high) 0.0)
      (cond ((zerop f-low)
             (values x-left 0))
            ((zerop f-high)
             (values x-right 0))
            (t
             (error "root not bracketed"))))
    (cond ((< f-low 0.0)
           (setf x-low x-left
                 x-high x-right))
          (t
           (setf x-high x-left
                 x-low x-right)))
    (setf rtsafe (* 0.5 (+ x-left x-right))
          dxold (abs (- x-right x-left))
          dx dxold
          f-new (funcall f rtsafe)
          df (funcall f-prime rtsafe))
    (dotimes (j maximum-number-of-iterations
                (if (not (cerror "returns solution so far"
                                 "exceeding maximum number of iterations"))
                  (values rtsafe maximum-number-of-iterations)))
      (cond ((or (>= (* (- (* (- rtsafe x-high) df) f-new)
                        (- (* (- rtsafe x-low) df) f-new))
                     0.0)
                 (> (abs (* 2.0 f-new)) (abs (* dxold df))))
             (setf dxold dx
                   dx (* 0.5 (- x-high x-low))
                   rtsafe (+ x-low dx))
             (when (= x-low rtsafe) (return (values rtsafe (1+ j)))))
            (t
             (setf dxold dx
                   dx (/ f-new df)
                   temp rtsafe
                   rtsafe (- rtsafe dx))
             (when (= temp rtsafe) (return (values rtsafe (1+ j))))))
      (when (< (abs dx) accuracy) (return (values rtsafe (1+ j))))
      (setf f-new (funcall f rtsafe)
            df (funcall f-prime rtsafe))
      (if (< f-new 0.0)
        (setf x-low rtsafe)
        (setf x-high rtsafe)))))