#lang racket

;; syntax

(struct eps ()
  #:transparent)

(struct chr
  (symbol)
  #:transparent)

(struct var
  (name)
  #:transparent)

(struct cat
  (left right)
  #:transparent)

(struct choice
  (left right)
  #:transparent)

(struct star
  (exp)
  #:transparent)

(struct nott
  (exp)
  #:transparent)

(define g1 (list (cons 'a(chr #\a))))

(define (run-var g v s)
  (match (assv v g)
    [#f (raise 'undefinedvar)]
    [(cons v1 e) (run g e s)]))

(define (run-eps s)
  (cons '() s))

(define (run-chr c s)
  (match s
    ['() '()]
    [(cons c1 s1) (if (eq? c1 c)
                      (cons (list c) s1)
                      '())]))

(define (run-cat g e1 e2 s)
  (match (run g e1 s)
    ['() '()]
    [(cons pr sf)
     (match (run g e2 sf) 
       ['() '()]
       [(cons pr1 sf1)
        (cons (append pr pr1)
              sf1)])]))

(define (run-choice g e1 e2 s)
  (match (run g e1 s)
    [(cons e1 s1) (cons (list e1) s1)]
    ['() (match (run g e2 s)
      ['() '()]
      [(cons e2 s1) (cons (list e2) s1)])]))

(define (run-star g t s)
  (match (run g t s)
    ['() (cons '() s)]
    [(cons pr sf) (match (run g (star t) sf)
      ['() (cons pr sf)]
      [(cons pr1 sf1) (cons (append pr pr1) sf1)])]))

(define (run-not g n s)
  (match (run g n s)
    ['() (cons '() s)]
    [(cons pr sf) '()]))

(define (run g e s)
  (match e
    [(eps)   (run-eps s)]
    [(chr c) (run-chr c s)]
    [(cat e1 e2) (run-cat g e1 e2 s)]
    [(choice e1 e2) (run-choice g e1 e2 s)]
    [(star t) (run-star g t s)]
    [(nott n) (run-not g n s)]))

(run g1 (eps) (string->list "abc"))

(run g1 (chr #\a) (string->list "abc"))

(run g1 (cat (chr #\a) (chr #\b)) (string->list "abc"))

(run g1 (choice (chr #\a) (chr #\b)) (string->list "abc"))

(run g1 (star (chr #\c)) (string->list "ccabc"))

(run g1 (nott (chr #\a)) (string->list "abc"))

(run-var g1 'a (string->list "abc"))
