#lang racket
(require "../sham/private/ast-utils.rkt"
         "signals.rkt"
         "prelude.rkt"
         "note.rkt"
         "wave-params.rkt"
         "wave-sham.rkt")
(provide (all-defined-out))

;; sham function: (frequency f32) (nsamples i32) (wavef (f32 i32 -> f32)) (output f32*) (offset i32)
;; starting at i 0->nsamples, output[i+offset] = wavef(i)
(define (build-sequence n pattern tempo wave total-weight (id (gensym 'sequence)))
  ;; (pretty-print pattern)
  (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
  ;; (define our-sn (if #t
  ;;                    (syntwave)
  ;;                    ;; (let ((sn-sham (sn (lambda (x y) (app^ wave x y)))))
  ;;                    ;;   (lambda (f n t o of) (app^ sn-sham f n t o of)))
  ;;                    (lambda (f n t o of) (app^ synthesize-note-orig f n t wave o of))
  ;;                    ))
  (sham-function
   [,id (output : f32*) (offset : i32)] : tvoid

   (slet^ ([ni (ui32 0) : i32])
          (while-ule^ ni (ui32 n)
                      (block (for/fold ([ofst offset]
                                        [apps '()]
                                        #:result (reverse apps))
                                       ([p pattern])
                               (match-define (signal:chord notes beats) p)
                               (define nsamples (* samples-per-beat beats))
                               (values (add-nuw ofst (ui32 nsamples))
                                       (cond [(false? notes) apps]
                                             [(list? notes)
                                              (append (for/list ([n notes])
                                                        (synthesize-note-simple
                                                         #:specialize '(3)
                                                         ;; #:try-inline 'wave
                                                         (fl32 (note-freq n))
                                                         (ui32 nsamples)
                                                         (fdiv (fl32 total-weight)
                                                               (fl32 (exact->inexact (length notes))))
                                                         wave
                                                         output
                                                         ofst))
                                                      apps)]))))
                      (set!^ ni (add-nuw ni (ui32 1)))))
   ret-void))

(define (build-mix ssfs (id (gensym 'mix)))
  (sham-function [,id (output : f32*) (offset : i32)] : tvoid
              (block (map (λ (f) (f output offset)) ssfs))
              ret-void))

(define (build-drum n pattern tempo total-weight (id (gensym 'drum)))
  ;; (pretty-print pattern)
  (define samples-per-beat (quotient (* (sampling-frequency) 60) tempo))
  (define beat-samples (seconds->samples 0.05))
  ;; bass-drum-array snare-array
  (define (copy-array to ofst from)
    (slet^  ([i (ui32 0) : i32])
               (while-ule^
                i (ui32 beat-samples)
                (add-signal! to
                             (add-nuw i ofst)
                             (fmul (load-signal (ptrcast from (etype f32*)) i)
                                   (fl32 total-weight)))
                (set!^ i (add-nuw i (ui32 1))))))

  (sham-function
   (,id [output : f32*] [offset : i32]) :  tvoid
   (slet^
    ([w (ui32 0) : i32])
    (while-ule^ w (ui32 n)
                (block
                 (for/fold ([ofst offset]
                            [apps '()]
                            #:result (reverse apps))
                           ([p pattern])
                   (values
                    (add-nuw ofst (ui32 samples-per-beat))
                    (cons
                     (match p
                       ['X (copy-array output ofst bass-drum-array)]
                       ['O (copy-array output ofst snare-array)]
                       [#f (svoid)])
                     apps))))
                (set!^ w (add-nuw w (ui32 1)))
                (set!^ offset (add-nuw offset (ui32 (* samples-per-beat (length pattern)))))))
   ret-void))

(define (build-main entry-signal memory-block nsamples)
  (sham-function (,(gensym 'main)) : tvoid
                 (entry-signal #:specialize '(0) (ptrcast memory-block (etype f32*)) (ui32 0))
                 (map-s->i
                  #:specialize '(0)
                  (ptrcast memory-block (etype f32*)) (fl32 0.3) (ui32 nsamples))
                 ret-void))
