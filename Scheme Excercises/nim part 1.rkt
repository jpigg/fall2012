;Preset boards
(define 4rows '(1 3 5 7))
(define 3rows '(4 3 5))

;begins the game and runs the turns
(define (playNim board)
  (if (= (totalSticks board 0) 0)
      (display "Player 1 Wins!")
      (begin
        (display "Player 1's Turn\n")
        (printBoard board 1)
        (makeMove board 1)
        )
  
      )
  )

(define (playNimPlayer2 board)
  (if (= (totalSticks board 0) 0)
      (display "Player 2 Wins!")
      (begin
        (display "Player 2's Turn\n")
        (printBoard board 1)
        (makeMove board 2)
        )
  
      )
  )

(define (totalSticks board sum)
  (if (null? board)
      sum
      (+ sum (totalSticks (cdr board) (car board)))
      )
  )

;gets a valid move
(define (makeMove board playNum)
  (display "Which row would you like to remove sticks from?\n")
  (display "And how many sticks would you like to remove?\n")
  (if (= playNum 1)
      (playNimPlayer2 (removeSticks board (read) (read) playNum))
      (playNim (removeSticks board (read) (read) playNum))
      
      )
  )

(define (removeSticks board row num playNum)
  (if (<= row 1)
      ;This row is the one selected to remove sticks from
      (begin
        ;row number error checking
        (if (equal? board '())
            ;Invalid row
            (begin
              (display "invalid row selected\n");
              (makeMove board playNum)
              )
            (begin
              ;number of sticks error checking
              (if (< (- (car board) num) 0)
                  ;Displayed if selected number of sticks is not valid
                  (begin
                    (display "Invalid number of sticks selected.\n")
                    (makeMove board playNum)
                    )
                  ;returns the modified value in the list if the selection is valid
                  (cons (- (car board) num) (cdr board))
                  )
              )
            )
        )
      ;Need to continue to the correct row
      (cons (car board) (removeSticks (cdr board) (- row 1) num playNum))
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

