#lang racket

(require "../main.rkt")

(emit (mix (list (sequence 1 (list (note 'C 5 1)
                                   '(#f . 1)
                                   (note 'C 5 1)
                                   '(#f . 1)
                                   (note 'A# 4 1)
                                   '(#f . 1)
                                   (note 'C 5 1)
                                   '(#f . 3)
                                   (note 'G 4 1)
                                   '(#f . 3)

                                   (note 'G 4 1)
                                   '(#f . 1)
                                   (note 'C 5 1)
                                   '(#f . 1)
                                   (note 'F 5 1)
                                   '(#f . 1)
                                   (note 'E 5 1)
                                   '(#f . 1)
                                   (note 'C 5 1)
                                   '(#f . 9)
                                   )
                           380 sine-wave)
                 1)
           (list (drum 4 '(O #f #f #f X #f #f #f) 380)
                 1))
      "funky-town.wav")