#lang racket

(require rackunit)

(provide Nothing Just Maybe whenJust orElse)

(define (Nothing)
  (lambda () #f))

(define (Just a)
  (lambda () a))

(let ([maybeA (Nothing)]
      [maybeB (Just 10)])
  (check-equal? (maybeB) 10)
  (check-equal? (maybeA) #f))

(define (Maybe default fun maybe)
  (if (equal? Nothing maybe)
    default
    (fun (maybe))))

(check-equal? (Maybe #f odd? (Just 3)) #t)
(check-equal? (Maybe #f odd? Nothing) #f)
(check-equal? (Maybe 0 + (Just 10)) 10)
(check-equal? (Maybe 0 + Nothing) 0)

(define (whenJust f maybe)
  (if (equal? Nothing maybe)
    maybe
    (Just (f (maybe)))))

(define (add-five n)
  (+ n 5))

(check-equal? (whenJust add-five Nothing) Nothing)
(check-equal? ((whenJust add-five (Just 10))) 15)
(check-equal? ((whenJust add-five
                         (whenJust add-five (Just 10))))
              20)
(check-equal? (whenJust add-five
                        (whenJust add-five Nothing))
              Nothing)

(define (orElse b maybe)
  (if (equal? Nothing maybe)
    b
    (maybe)))

(check-equal? (orElse 10 Nothing) 10)
(check-equal? (orElse 5 (Just 10)) 10)

(define (safeDivide a b)
  (if (zero? b)
    Nothing
    (Just (/ a b))))

(check-equal? (safeDivide 10 0) Nothing)
(check-equal? ((safeDivide 10 2)) 5)
(check-equal? (orElse 0 (safeDivide 10 0)) 0)
(check-equal? (orElse 0 (safeDivide 10 2)) 5)
