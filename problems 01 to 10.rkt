#lang racket
#|
P01 (*) Find the last box of a list.
    Example:
    * (my-last '(a b c d))
    (D)
|#
(define (my-last lst)
  (if (> (length lst) 1)
      (my-last (cdr lst))
      (car lst)))
#|
P02 (*) Find the last but one box of a list.
    Example:
    * (my-but-last '(a b c d))
    (C D)
|#
(define (my-but-last lst)
  (if (> (length lst) 2)
      (my-but-last (cdr lst))
      lst))
#|
P03 (*) Find the K'th element of a list.
    The first element in the list is number 1.
    Example:
    * (element-at '(a b c d e) 3)
    C
|#
(define (element-at lst x)
  (if (> x 1)
      (element-at (cdr lst) (- x 1))
      (car lst)))
#|
P04 (*) Find the number of elements of a list.
|#
(define (my-len lst [acc 0])
  (if (null? lst)
      acc
      (my-len (cdr lst) (+ acc 1))))
#|
P05 (*) Reverse a list.
|#
(define (my-reverse lst [acc '()])
  (if (null? lst)
      acc
      (my-reverse (cdr lst) (cons (car lst) acc))))
#|
P06 (*) Find out whether a list is a palindrome.
    A palindrome can be read forward or backward; e.g. (x a m a x).
|#
(define (palindrome? lst)
  (if (equal? lst (my-reverse lst)) #t #f))
#|
P07 (**) Flatten a nested list structure.
    Transform a list, possibly holding lists as elements into a 'flat'
    list by replacing each list with its elements (recursively).
    Example:
    * (my-flatten '(a (b (c d) e)))
    (A B C D E)
    Hint: Use the predefined functions list and append.
|#
(define (my-flatten lst) ; Satisfies (my-flatten '((a) b (c (d) . e) ()))
  (cond
    [(null? lst) lst]
    [(not (pair? lst)) (list lst)]
    [else (append (my-flatten (car lst)) (my-flatten (cdr lst)))]))
#|
P08 (**) Eliminate consecutive duplicates of list elements.
    If a list contains repeated elements they should be replaced with a single
    copy of the element. The order of the elements should not be changed.
    Example:
    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)
|#
(define (compress lst [acc '()])
  (if (>= (length lst) 2)
      (if (equal? (car lst) (cadr lst))
          (compress (cdr lst) acc)
          (compress (cdr lst) (append acc (list (car lst)))))
      (append acc lst)))
#|
P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.
    Example:
    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))
|#
(define (neck lst [acc '()])
  (if (< (length lst) 2)
      acc
      (neck (cdr lst) (append acc (list (car lst))))))
(define (add-to-end lst x) (append (neck lst) (list (append (last lst) (list x)))))
(define (pack lst [acc '()])
  (cond
    [(null? lst) acc]
    [(null? acc) (pack (cdr lst) (list (list (car lst))))]
    [(equal? (car lst) (car (last acc))) (pack (cdr lst) (add-to-end acc (car lst)))]
    [else (pack (cdr lst) (append acc (list (list (car lst)))))]))
#|
P10 (*) Run-length encoding of a list.
    Use the result of problem P09 to implement the so-called run-length encoding data compression method.
    Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
    Example:
    * (encode '(a a a a b c c a a d e e e e))
    ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
|#
(define (encode-packed lst [acc '()])
  (if (null? lst)
      acc
      (encode-packed (cdr lst) (append acc (list (cons (length (car lst)) (list (caar lst))))))))
(define (encode lst) (encode-packed (pack lst)))