#lang racket

(require rackunit)

(provide return-id bind-id return-maybe bind-maybe fail-maybe do)

(define return-id (λ (a) a))
(define bind-id (λ (ma f) (f ma)))

(define plus-id
  (λ (a b)
     (bind-id
       (return-id (+ a b))
       (λ (x) (return-id x)))))

(check-equal? (plus-id 2 3) 5)

(define return-maybe
  (λ (a)
     `(Just ,a)))

(define bind-maybe
  (λ (ma f)
     (cond
       [(eq? (car ma) 'Just) (f (cadr ma))]
       [(eq? (car ma) 'Nothing) '(Nothing)])))

(define fail-maybe
  (λ ()
     '(Nothing)))

(define divide-maybe
  (λ (a b)
     (if (zero? b)
       (fail-maybe)
       (return-maybe (/ a b)))))

(check-equal? (divide-maybe 10 0) '(Nothing))
(check-equal? (divide-maybe 10 5) '(Just 2))

(check-equal?
  (bind-maybe
  (return-maybe (+ 7 8))
  (λ (x)
     (bind-maybe
       (divide-maybe x 4)
       (λ (x^)
          (return-maybe x^)))))
  '(Just 15/4))

(define-syntax do
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e0) e e* ...)
     (bind e0 (λ (v) (do bind e e* ...))))
    ((_ bind e0 e e* ...)
     (bind e0 (λ (_) (do bind e e* ...))))))

(check-equal?
  (do bind-maybe
      (x <- (return-maybe (+ 7 8)))
      (x^ <- (divide-maybe x 4))
      (return-maybe x^))
  '(Just 15/4))
