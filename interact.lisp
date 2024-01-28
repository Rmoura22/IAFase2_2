;Interface for the Knight Game 

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
     (play (initial-node table) 'player-turn 'AI-turn :timeLimit AITimeLimit :whiteName playerName :blackName "AI")
     (play (initial-node table) 'AI-turn 'player-turn :timeLimit AITimeLimit :whiteName "AI" :blackName playerName)
    )
  )
)

(defun initial-node (table)
  (place-knights table)
)

(defun AI-vs-AI ()
  "AI vs AI game mode"

  (format t "~%") 

  (play (initial-node (random-table)) 'AI-turn 'AI-turn :timeLimit (choose-AI-time-limit) :whiteName "AI-White" :blackName "AI-Black")
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
   (2 (choose-random-table))
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
   ((equal (length table) 10) (format t "      AA   BB   CC   DD   EE   FF   GG   HH   II   JJ~%~%") (format t "~2,' d |" (- 11 (length table)))  (print-list (car table)) (print-table (cdr table)))   
   (t (format t "~2,' d |" (- 11 (length table))) (print-list (car table)) (print-table (cdr table)))
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
          (choose-AI-time-limit)
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
        the Knight Game
")
)

(defun farewell ()
   (format t "Thank you for playing!")
)














;Node -> (table curPlayer nextPlayer depth parent heurValue)
  ;Player# -> (playerPos PlayerPoints)
  ;Parent -> List of operators used till we get to this node

(defun play (node whiteFunc blackFunc &key (turn 'white) (turnNum 0) (timeLimit 3000) (whiteName "white") (blackName "black"))
  "Main function of the program, where the game happens."
  (cond
    ((game-over node whiteName blackName turn) (get-winner node turn whiteName blackName))
    ((equal turn 'white)
      (print-play node whiteName blackName turn)
      (play 
        (funcall whiteFunc node timeLimit)
        whiteFunc 
        blackFunc 
        :turn 'black 
        :turnNum (1+ turnNum)
        :timeLimit timeLimit
        :whiteName whiteName
        :blackName blackName
      )
    )
    (t 
      (print-play node whiteName blackName turn)
      (play 
        (funcall blackFunc node timeLimit) 
        whiteFunc 
        blackFunc 
        :turn 'white 
        :turnNum (1+ turnNum)
        :timeLimit timeLimit
        :whiteName whiteName
        :blackName blackName
      )
    )
  )
)

(defun get-winner (node turn whiteName blackName)
  "Returns (table winnerName winnerPoints loserPoints)"
  (cond 
    ((> (get-player-points (get-Node-CurPlayer node)) (get-player-points (get-Node-NextPalyer node)))
      (list 
        (get-Node-Table node)
        (if (equal turn 'white) whiteName blackName)
        (get-player-points (get-Node-CurPlayer node))
        (get-player-points (get-Node-NextPalyer node))
      )
    )
    ((< (get-player-points (get-Node-CurPlayer node)) (get-player-points (get-Node-NextPalyer node)))
      (list 
        (get-Node-Table node)
        (if (equal turn 'white) whiteName blackName)
        (get-player-points (get-Node-CurPlayer node))
        (get-player-points (get-Node-NextPalyer node))
      )
    )
    (t 
      (list 
        (get-Node-Table node)
        nil
      )
    )
  )
)

(defun print-play (node whiteName blackName turn)
  (format t "~%~a's turn:~%" (if (equal turn 'white) whiteName blackName))
  (print-table (get-Node-Table node))
  (format t "~%~a's points: ~a~%" (if (equal turn 'white) whiteName blackName)(get-player-points (get-Node-CurPlayer node)))
  (format t "~%~a's points: ~a~%" (if (equal turn 'white) blackName whiteName)(get-player-points (get-Node-NextPalyer node)))
)
#|
(defun play-cycle (node whiteFunc blackFunc &key (turn 'white) (turnNum 0) (writeAnalitics nil))
  "Each call to this function represents the turn of a player. whitePlayer and blackPlayer represent (pos, points)"
)
|#

(defun place-knights (table)
  "Places the white knight and then the black knight, returns the node"
  (let* ((whitePoints (apply #'max (car table))) (blackPoints (funcall #'max-in-list (car (last (removeOther (cons (replaceInList (car table) whitePoints 'WK) (cdr table)) whitePoints))))))
    (list
      (removeOther (removeOther (cons (replaceInList (car table) whitePoints 'WK) (append (cdr (butlast table)) (list (replaceInList (car (last table)) blackPoints 'BK)))) whitePoints) blackPoints)
      (list (list 0 (get-Index-line (car table) whitePoints)) whitePoints)
      (list (list 9 (get-Index-line (car (last table)) blackPoints)) blackPoints)
      0
      nil
      nil
    )
  )
)

(defun max-in-list (lst)
  "Find the maximum value in a list that may contain nil or numbers."
  (if (null lst)
    nil
    (let ((filtered-list (remove nil lst)))
      (if filtered-list
        (apply #'max filtered-list)
        nil
      )
    )
  )
)

(defun get-Index-line (line num)
  (cond
    ((null line) nil)
    ((equal num (car line)) (- 9 (- (length line) 1)))
    (t (get-Index-line (cdr line) num))
  )
)

(defun replaceInList (lista num &optional (newValue nil))
 (cond
  ((null lista) nil)
  ((equal (car lista) num) (cons newValue (cdr lista)))
  (t (cons (car lista)(replaceInList (cdr lista) num newValue)))
 )
)

(defun get-Node-Table (Node)
  (first Node)
)
(defun get-Node-CurPlayer (Node)
  (second Node)
)
(defun get-Node-NextPalyer (Node)
  (third Node)
)
(defun get-Node-Depth (Node)
  (fourth Node)
)
(defun get-Node-Parent (Node)
  (fifth Node)
)
(defun get-Node-heurValue (Node)
  (sixth Node)
)

(defun get-player-pos (player)
  (first player)
)

(defun get-player-points (player)
  (second player)
)

(defun player-turn (node &optional (timeLimit))
  "Where the player makes their move."
  (cond 
    ((not (can-move node)) (swap-players node))
    (t 
      (let* ((movement (Read-Player-Move node (get-player-pos (get-Node-CurPlayer node)))) (nodee (move node (first movement) (second movement) (get-operator (first movement) (second movement)))))
        (print nodee)
        nodee
      )
    )
  )
)

(defun Read-Player-Move (node playerPos)
  (format t "Insert line (1-10): ")
  (let ((line (- (read-choice '(1 2 3 4 5 6 7 8 9 10)) 1)))
    (format t "Insert Column (A-J): ")
    (let ((column (char-to-number (symbol-to-char (read-choice '(A B C D E F G H I J))))))
      (cond
        ((and (numberp (cel (get-Node-Table node) line column)) (not (null (get-operator (- line (first playerPos))(- column (second playerPos)))))) (list line column))
        (t (Print "Invalid play, try again") (Read-Player-Move node playerPos))
      )
    )
  )
)

(defun char-to-number (letter)
  (let ((num (char-code letter)))
    (if (and (>= num (char-code #\A)) (<= num (char-code #\J)))
        (- num 65)
        (error "Invalid letter"))))

(defun symbol-to-char (symbol)
  (if (symbolp symbol)
      (if (= (length (symbol-name symbol)) 1)
          (char (symbol-name symbol) 0)
          (error "Symbol must be a single-character symbol"))
      (error "Input is not a symbol")))


(defun AI-turn (node &optional (timeLimit))
  "Where the AI makes their move."
  (cond 
    ((not (can-move node)) (swap-players node))
    (t 
      (alfa-beta (convert-to-alfa-beta-node node) 20 *jogador1* (list (get-internal-real-time) timeLimit))
      (convert-to-interact-node (invert-players *jogada*))
    )
  )
)

(defun write-analitics (analised-nodes-num pruned-nodes-num time-used table)
  "Writes all the analytics to a file without erasing the content and creates the file if it does not exist."
  (with-open-file (file "~/OneDrive/Ambiente de trabalho/teste.txt"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format file "Analysed Nodes: ~a~%Pruned Nodes: ~a~%Time Used: ~a~%Table: ~a~%"
            analised-nodes-num pruned-nodes-num time-used table)
  )
)

(defun write-analitics2 (teste)
  "Writes all the analytics to a file without erasing the content and creates the file if it does not exist."
  (with-open-file (file "~/OneDrive/Ambiente de trabalho/teste.txt"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
    (format file "~a"  teste)
  )
)

(defun game-over (node curName nextName turn)
  (let ((isOver (and (not (can-move node)) (not (can-move (swap-players node))))))
    (cond
      (isOver (print "Its over")(format t "Winner is ~a~%" (if (and (equal turn 'white) (> (get-player-points (get-Node-CurPlayer node)) (get-player-points (get-Node-NextPalyer node)))) curName nextName )) t)
      nil
    )
  )
)

(defun can-move (node)
  (not(null (successors node (get-operators))))
)

(defun swap-players (node)
  (list
    (first node)
    (third node)
    (second node)
    (fourth node)
    (fifth node)
    (sixth node)
  )
)



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