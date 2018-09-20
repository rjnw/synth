#lang racket

(provide (all-defined-out))

(define sampling-frequency (make-parameter 44100))
(define bits-per-sample (make-parameter 16))
;16 bites * 44100 bits per second

;; number of samples for one period
(define (freq->sample-period freq)
  (round (/ (sampling-frequency) freq)))

(define (seconds->samples s)
  (inexact->exact (round (* s (sampling-frequency)))))