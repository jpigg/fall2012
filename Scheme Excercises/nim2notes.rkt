(define (test funct x y)
  (apply (eval funct) (list x y))
  )

(define (plus x y)
  (+ x y)
  )



(define (playNimPlayer2 board computerOption)
  (if (= (totalSticks board 0) 0)
      (display "Computer Wins!")
      (begin
        (display "Computer's Turn\n")
        (printBoard board 1)
        (makeMove board 2 computerOption)
        )
  
      )
  )

(define (ifOne x)
  (if (test x)
      (display "yes")
      (display "no")
      )
  )

(define (test x)
  (if (= x 1)
      #t
      #f
      )
  )