;Interface for the Knight Tour Project

(defun start ()
  (greeting)
  (main-menu)
  (farewell)
)

(defun main-menu ()
  "Where you can choose the game mode."

  (format t "~%") 
  (format t "Choose the game mode~%") 
  (format t " 1 - Player VS AI ~%")
  (format t " 2 - AI VS AI ~%")
  (format t " 3 - Exit ~%~%>")

  (ecase (read-choice (list 1 2 3))
    (1 (player-vs-AI))
    (2 (AI-vs-AI))
    (3 "")
  )
 )

(defun player-vs-AI ()
  "Player vs AI game mode"

  (format t "~%") 

  (let (
        (playerName (get-player-name))
        (playerPieces (choose-pieces))
        (AITimeLimit (choose-AI-time-limit))
        (table (choose-random-table))
      )
    (if (equal playerPieces 'white)
     (play table 'player-turn 'AI-turn AITimeLimit playerName "AI")
     (play table 'AI-turn 'player-turn AITimeLimit "AI" playerName)
    )
  )
)

(defun AI-vs-AI ()
  "AI vs AI game mode"

  (format t "~%") 

  (play (random-table) 'AI-turn 'AI-turn (choose-AI-time-limit) "AI-White" "AI-Black")
)

(defun choose-random-table ()
  "Returns the random table that the player chooses."
 (let ((table (random-table)))

  (format t "~%")
  (print-table table)
  (format t "~%")
  (format t "Choose this table?~%~%")
  (format t "1 - yes ~%")
  (format t "2 - no (reroll) ~%~%>")

  (ecase (read-choice (list 1 2))
   (1 table)
   (2 (get-random-table))
  )
 )
)

(defun random-table ()
  "Makes a random table with numbers from 99 to 0"
  (list-to-table(randomize (numbers-list)))
)

(defun numbers-list (&optional (n 100))
  "Returns a list with numbers from 99 to 0"
  (cond
    ((= n 1) (list 0)) 
    (t (append (list (1- n)) (numbers-list (1- n))))
  )
)

(defun randomize (lista)
  "Randomizes the elements of a list"
  (cond
    ((null lista) nil)
    (t (let* ((random-element (nth (random (length lista)) lista)))
        (cons random-element (randomize (remove-if #'(lambda (x) (= x random-element)) lista)))    
       )
    )
  )
)

(defun list-to-table (lista) 
  "Gets a list with 100 elements and returns a 10x10 list of lists"
 (cond 
  ((null lista) nil)
  ((equal (mod (length lista) 10) 0) (cons (subseq lista 0 10) (list-to-table (subseq lista 10 (length lista)))))
 )
)

(defun print-table (table)
  (cond 
   ((null table) (format t "~%"))
   (t (print-list (car table)) (print-table (cdr table)))
  )
)

(defun print-list (lista)
  (cond
   ((null lista) (format t "~%~%"))
   (t (if (null (car lista)) (format t "  -- ") (format t " ~3,' d " (car lista))) (print-list (cdr lista)))
  )
)

(defun choose-AI-time-limit ()
  "Lets the player choose the time limit of every AI play"

  (format t "Define the AI's time limit per play (1000 to 5000ms): ")

  (let ((number (read)))
    (if (and (integerp number) (>= number 1000) (<= number 5000))
        number
        (progn
          (format t "Error, please enter a number between 1000 and 5000.~%~%")
          (get-AI-time-limit)
        )
    )
  )
)

(defun choose-pieces ()

  (format t "Choose pieces~%")
  (format t " 1 - White pieces~%")
  (format t " 2 - Black pieces~%~%>")

  (ecase (read-choice (list 1 2))
    (1 'white)
    (2 'black)
  )
)

(defun get-player-name ()
  (format t "Insert your name: ")
  (read-line)
)

(defun read-choice (options)
  "Recieves a list of options, for the user to choose from, outputs the users first valid option."
  (let ((choice (read)))
   (if (find choice options) 
    choice
    (progn (format t "Error, ~a is not a valid choice, try again.~%~%>" choice) (read-choice options))
   )
  )
)

(defun greeting ()
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
          Wellcome to
        the Knight Tour
")
)

(defun farewell ()
   (format t "Thank you for playing!")
)


















(defun play (table whiteFunc blackFunc &optional (timeLimit 3000) (whiteName "white") (blackName "black"))
  "Main function of the program, where the game happens."
  (let* ((result (place-knights table)))
    (play-cycle (first result) (second result) (third result) whiteFunc blackFunc :writeAnalitics t)
  )
)

(defun place-knights (table)
  "Places the white knight and then the black knight, returns (table, (whitePos, whitePoints), (blackPos, blackPoints))"
)


(defun play-cycle (table whitePlayer blackPlayer whiteFunc blackFunc &optional (turn 'white) (turnNum 0) (writeAnalitics nil))
  "Each call to this function represents the turn of a player. whitePlayer and blackPlayer represent (pos, points)"
  (let* (()))
  (cond
    ((not (or (can-move table (get-player-pos whitePlayer)) (can-move table (get-player-pos blackPlayer)))) nil)
    (t (play-cycle table ))
  )
)

(defun get-player-pos (player)
  (first player)
)

(defun get-player-points (player)
  (second player)
)

(defun player-turn (table playerPos)
  "Where the player makes their move."
)

(defun AI-turn (table playerPos)
  "Where the AI makes their move."
)

(defun write-analitics (analised-nodes-num pruned-nodes-num time-used table)
  "Writes all the analytics to a file without erasing the content and creates the file if it does not exist."
  (with-open-file (file "C:\\Users\\mateu\\OneDrive\\Ambiente de Trabalho\\teste.txt"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format file "Analysed Nodes: ~a~%Pruned Nodes: ~a~%Time Used: ~a~%Table: ~a~%"
            analised-nodes-num pruned-nodes-num time-used table)
  )
)

(defun move (table playerPos)


)

(defun player-choose-movement ()
  (let ((movements (get-possible-movements table))))

  (cond
    (nil (can))

  )
)