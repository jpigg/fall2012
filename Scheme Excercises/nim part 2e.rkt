;Preset boards
(define 4rows '(1 3 5 7))
(define 3rows '(4 3 5))

;begins the game and runs the turns
(define (playNim board computerOption)
  (if (= (totalSticks board 0) 0)
      (display "Player 1 Wins!")
      (begin
        (display "\nPlayer 1's Turn\n\n")
        ;(printBoard board 1)
        (makeMove board 1 computerOption)
        )
  
      )
  )

;(define (playNimPlayer2 board computerOption)
;  (if (= (totalSticks board 0) 0)
;      (display "Computer Wins!")
;      (begin
;        (display "Computer's Turn\n")
;        (printBoard board 1)
;        (makeMove board 2 computerOption)
;        )
;  
;      )
;  )

(define (playNimPlayer2 board computerOption)
  (if (= (totalSticks board 0) 0)
      (display "Computer Wins!")
      (begin
        (display "\nComputer's turn\n")
        (printBoard board 1)
        (apply (eval computerOption) (list board computerOption))
        )
      )
  )

(define (randomComputer board computerOption)
  ;row then column
  ;(playNim (removeSticks board (read) (read) 2 computerOption) computerOption)
  (getRandRow board computerOption (random (length board)))
  )

(define (getRandRow board computerOption attempt)
  (if (= 0 (list-ref board attempt))
      ;invalid row
      (begin
        (getRandRow board computerOption (random (length board)))
        )
      ;valid row
      (begin
        (display "Computer selected row ")
        (display (+ 1 attempt))
        (display ".\n")
        (playNim (removeSticks board (+ 1 attempt) (+ 1 (random (list-ref board attempt))) 2 computerOption) computerOption)
        )
      )
  
  )

(define (smartComputer board computerOption)
  ;(display "smartComputer")
  (getSmartRow board computerOption (random (length board)))
  )

(define (getSmartRow board computerOption attempt)
  (if (= 0 (list-ref board attempt))
      ;invalid row
      (getSmartRow board computerOption (random (length board)))
      ;valid row
      (playNim (kernelMove board board (+ 1 attempt) (+ 1 (random (list-ref board attempt))) computerOption) computerOption)
      )
  )
          
          
          

          
          
(define (kernelMove newBoard oldBoard row num computerOption)
       (if (<= row 1)
           (begin
                 (cons (- (car newBoard) num) (cdr newBoard))
             
           
           )
           ;Need to continue to the correct row
           (if (= 0 (getParity (cons (car newBoard) (removeSticks (cdr newBoard) (- row 1) num 2 computerOption))))
               ;parity is 0
               (removeSticks newBoard row num 2 computerOption)
               ;parity is not 0
               (smartComputer oldBoard computerOption)
               )
           
       )
     
  )

(define (getParity board)
  (if (equal? board '())
      ;end of list
      0
      ;more to list
      (bitwise-xor (car board) (getParity (cdr board)))
  )
  )





(define (totalSticks board sum)
  (if (null? board)
      sum
      (+ sum (totalSticks (cdr board) (car board)))
      )
  )

;gets a valid move
(define (makeMove board playNum computerOption)
  (if (= playNum 1)
      (begin
        ;Human Player
        (display "Which row would you like to remove sticks from?\n")
        (display "And how many sticks would you like to remove?\n")
        (printBoard board 1)
        (playNimPlayer2 (removeSticks board (read) (read) playNum computerOption) computerOption)
        )
      (begin
        ;Computer Player
        (playNim (removeSticks board (read) (read) playNum computerOption) computerOption)
        )
      
      )
  )

(define (removeSticks board row num playNum computerOption)
  (if(validMove board row num)
     ;validMove
     (begin
       (if (<= row 1)
           (begin
             (if (= playNum 2)
                 (begin
                   (display "Computer removed ")
                   (display num)
                   (display " X's.\n")
                   (cons (- (car board) num) (cdr board))
                 )
                 (cons (- (car board) num) (cdr board))
             )
           
           )
           ;Need to continue to the correct row
           (cons (car board) (removeSticks (cdr board) (- row 1) num playNum computerOption))
           )
       )
     ;invalid move
     (begin
       (if (= playNum 1)
           (makeMove board playNum computerOption)
           (apply (eval computerOption) (list board computerOption))
           )
       )
     )
  )

(define (validMove board row num)
  ;(if (<= row 1)
  (if (<= 4 1)
      ;This row is the one selected to remove sticks from
      (begin
        ;row number error checking
        (if (equal? board '())
            ;Invalid row
            #f
            (begin
              ;number of sticks error checking
              (if (< (- (car board) num) 0)
                  ;Displayed if selected number of sticks is not valid
                  #f
                  ;returns the modified value in the list if the selection is valid
                  #t
                  )
              )
            )
        )
      ;Need to continue to the correct row
      ;(cons (car board) (validMove (cdr board) (- row 1) num))
      #t
      )
  )

;Prints the entire board
(define (printBoard board rowNum)
  (if (null? board)
      (display "\n")
      (begin
        (display "Row ")
        (display rowNum)
        (display ": ")
        (disp (car board))
        (display "\n")
        (printBoard (cdr board) (+ rowNum 1))
        )
      )
  )

;Displays the number of X's given as input
(define (disp x)
  (if (> x 0)
      (begin
        (display "X ")
        (disp (- x 1))
        )
      )
  )

