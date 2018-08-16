#lang racket

(require "../sham/main.rkt")

(define sampling-frequence (make-parameter 44100))
(define bits-per-sample (make-parameter 16))

(define (freq->sample-period freq)
  (round (/ (sampling-frequence)
            freq)))
(defien (seconds->samples s)
  (inexact->exact (round (* s fs))))

(define (sine-sym freq)
  (string->symbol (format "sine-wave-~a" freq)))
(define (sine-wave-function freq)
  (s$:dfunction (void) (sine-sym freq)
                (list 'x) (list s$:i32) s$:f32
                (s$:ret (s$:app^ (s$:rs 'sin)
                                 (s$:app^ (s$:rs 'mul-nfw)
                                          (s$:f32 (exact->inexact (/ (* freq 2.0 pi)
                                                                     (sampling-frequency))))
                                          (s$:rs 'i32->f32 (s$:v 'x)))))
                ))

(define (sawtooth-sym freq)
  (string->symbol (format "sawtooth-wave-~a" freq)))
(define (sawtooth-wave-function freq)
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (define sample-period/4 (quotient sample-period 4))
  (s$:dfunction (void) (sawtooth-sym freq)
                (list 'x) (list s$:i32) s$:f32
                (s$:ret (s$:app^ (s$:rs '-)
                                 (s$:app^ (s$:rs '/)
                                          (... (exact->inexact (modulo x sample-period)))
                                          sample-period/2)
                                 (s$:f32 1.0)))))

(define (sequence-signals)
  (s$:dfunction (void) 'sequence '(pattern tempo wave)))

(define synthesize-note
  (s$:dfunction (void) 'synthesize-note
                '(note-freq nsamples wave)
                (list s$:f32 s$:i32 (s$:tfun s:f32 s:i32 s$:f32))))
