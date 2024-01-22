;Interação com o utilizador e para proceder à escrita e leitura de ficheiros.
(defpackage "Interface")

(defun begin ()
;Inicia o programa
  (startASCII)
  (mainMenu)
  (endASCII)
)


(defun mainMenu ()
;Menu incial para escolher o tipo de problema
  (print_MainMenu)
  (ecase (readChoice (list 1 2 3))
    (1 (probFromFileMenu))
    (2 (randomProbMenu))
    (3 "byeeeeee.")
  )
 )

(defun print_MainMenu ()
;Print das opções do menu inicial
  (format t "~%")
  (format t "1 - Resolver problema de ficheiro ~%")
  (format t "2 - Problema aleat?io ~%")
  (format t "3 - Sair ~%~%>")
)


(defun probFromFileMenu ()
;Menu para resolvel um problema do ficheiro
  (print_ProbFromFileMenu)
  (let* 
  	(
     (fileName "problems")
     (problems (getProblemsFromFile fileName))
     (options (getOptions problems))
    )

    (startAlgorithms (chooseProblem problems options))
  )
)

(defun chooseProblem (problems options)
 (let* 
  (
   (problemName (progn (displayAvailableProbs problems) (format t "~%Insira o nome do problema. ~%~%>") (readChoice options)))
   (problem (getProblemFromProblems problemName problems))
  )
  (printProblem problem)
  (format t "~%")
  (format t "O que pretende fazer? ~%~%")
  (format t "1 - Resolver problema ~%")
  (format t "2 - Voltar a trás ~%~%>")

  (ecase (readChoice (list 1 2))
  	(1 (ChooseAlgorithm problem))
  	(2 (chooseProblem problems options))
  )
 )
)

(defun ChooseAlgorithm (problem &optional (choosen nil))
 (cond 
  ((null (elements-not-in-list2 (get-algorithms) choosen)) (startAlgorithms choosen))
  (t 
   (format t "~%")
   (format t "Escolha os algorítmos ~%~%")
   (format t "Escolhidos: ~%")
   (printPossibleAlgoritmhs choosen)
   (format t "Para escolher: ~%>" )
   (printPossibleAlgoritmhs (elements-not-in-list2 (get-algorithms) choosen))

   (ecase (readChoice (list 1 (elements-not-in-list2 (get-algorithms) choosen)))
   	(1 (if (null choosen) (ChooseAlgorithm problem choosen) (startAlgorithms choosen)))
   	('bfs (ChooseAlgorithm problem (cons 'bfs choosen)))
   	('dfs (ChooseAlgorithm problem (cons 'dfs choosen)))
   	('A-Star (ChooseAlgorithm problem (cons 'A-Star choosen)))
   )
  ) 
 )
 
)

(defun elements-not-in-list2 (list1 list2)
  (reverse (set-difference list1 list2 :test #'equal))
)

(defun printPossibleAlgoritmhs (parameters)
 (cond 
  ((null parameters) nil)
  (t (format t "~a ~%"(car parameters)) (printPossibleAlgoritmhs (cdr parameters)))
 )
)

(defun get-algorithms ()
 (list 'Bfs 'Dfs 'A-Star)
)

(defun print_ProbFromFileMenu ()
;Print das opções do menu de problema do ficheiro
  (printHeader "Resolver problema de ficheiro")
  (format t "~%")
)

(defun getProblemsFromFile (fileName)
;Recebe o nome do ficheiro e vai buscar o nome, objetivo e tabela de cada problema nesse ficheiro
 (let* 
  (
   (in (open (format nil "D:/IPS/IA/Projeto/~a.dat" fileName) :if-does-not-exist nil))
   (tables nil)
   (table nil)
  )
  (when in
	(loop for line = (read in nil)
		while (not (equal '--- line))
		do 
			(cond ((equal (length table) 102) (setf tables (append tables (list table))) (setf table nil)))
			(cond
		 		((or (equal 'Problema line) (equal 'Objetivo line))
		  		(setf table (append table (list (read in nil))))
		 		)
		 		(t (setf table (append table (list line))))
			)
		finally (setf tables (append tables (list table))) (close in)
	)
  )
   tables
 )
)

(defun getOptions (tables)
;Recebe a as tabelas (c/ nome e objetivo), dá print e devolve os nomes
  (getTablesNames tables)
)

(defun displayAvailableProbs (tables)
;Recebe a as tabelas (c/ nome e objetivo), dá print do Nome - Objetivo
  (cond
  	((null tables) nil)
  	(t (printNameScore (car tables)) (displayAvailableProbs (cdr tables)))
  )
)

(defun getTablesNames (tables)
;Recebe a as tabelas (c/ nome e objetivo), devolve os nomes
  (cond
  	((null tables) nil)
  	((equal (length tables) 1) (list(car (car tables))))
  	(t (cons (car (car tables)) (getTablesNames (cdr tables))))
  )
)

(defun printNameScore (tables)
  (format t "~a - ~dpts~%" (first tables) (second tables))
)

(defun getProblemFromProblems (name problems)
;Gets the problem named name from the list of problems problems. Nil if not exist
  (cond 
  	((null problems) nil)
  	((equal name (car (car problems))) (list (first (car problems)) (second (car problems)) (listToTable (cddr(car problems)))))
  	(t (getProblemFromProblems name (cdr problems)))
  )
)


(defun printProblem (problem)
  (format t "~%Problema ~a~%" (first problem))
  (format t "Objetivo ~a~%" (second problem))
  (printTable (tableToList(cddr problem)))
)

(defun printTable (table)
  (cond 
   ((null table) (format t "~%"))
   (t (printList (car table)) (printTable (cdr table)))
  )
)

(defun printList (lista)
  (cond
   ((null lista) (format t "~%~%"))
   (t (if (null (car lista)) (format t "  -- ") (format t " ~3,' d " (car lista))) (printList (cdr lista)))
  )
)

(defun listToTable (lista)
; Recebe uma lista (sem nome e objetivo) com a tabela em forma de lista e transforma numa tabela 
 (cond 
  ((null lista) nil)
  ((equal (mod (length lista) 10) 0) (cons (subseq lista 0 10) (listToTable (subseq lista 10 (length lista)))))
 )
)

(defun tableToList (table)
	(cond 
		((null table) nil)
		(t (append (car table) (tableToList (cdr table))))
	)
)


(defun randomProbMenu ()
;
 (print_RandomProbMenu)
 (startAlgorithms (getRandomProblem))
)

(defun random-goal ()
 (random 2500)
)

(defun getRandomProblem ()
 (let ((problem (list 'f (random-goal) (listtotable(baralhar (lista-numeros))))))
 	(printProblem problem)

	(format t "~%")
	(format t "O que pretende fazer? ~%~%")
	(format t "1 - Resolver problema ~%")
	(format t "2 - Reroll ~%~%>")

    (ecase (readChoice (list 1 2))
     (1 problem)
     (2 (getRandomProblem))
	)
 )
)

(defun lista-numeros (&optional (n 100))
  (cond
    ((= n 1) (list 0)) 
    (t (append (list (1- n)) (lista-numeros (1- n))))
  )
)

;;Baralha uma lista
(defun baralhar (lista)
  (cond
    ((null lista) nil)
    (t (let* ((random-element (nth (random (length lista)) lista)))
        (cons random-element (baralhar (remover-se #'(lambda (x) (= x random-element)) lista)))    
       )
    )
  )
)

(defun remover-se(pred lista)
  "Remove todos os elmentos que correspondem ao predicado no argumento"
  (cond ((null lista) NIL) 
        ((funcall pred (car lista)) (remover-se pred (cdr lista)))
        (T (cons (car lista) (remover-se pred (cdr lista))))))

(defun print_RandomProbMenu ()
;
  (printHeader "Resolver problema aleatório")
  (format t "~%")
)


(defun readChoice (escolhas)
;Recieves a list of possible choices, waits for the user to choose a valid option 
  (let ((escolha (read)))
   (if (find escolha escolhas) 
    (return-from readChoice escolha)
    (progn (format t "Escolha inválida, tente outra vez.~%~%>") (readChoice escolhas))
   )
  )
)


(defun printHeader (header)
 (format t "_.oº| ~a |ºo._ ~%" header)
)


(defun startASCII ()
  (format t "
            |
            |
            + \\
            \\\.G_.*=.
             `(#'/.\\|
              .>' (_--.
           _=/d   ,^\\
          ~~ \\)-'   '
             / |   
            '  '        
          Bem vindo
      ao jogo do cavalo
")
)

(defun endASCII ()
 
)
