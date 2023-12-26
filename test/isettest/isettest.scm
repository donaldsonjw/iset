(module isettest
   (library iset srfi64))


(define (isettest-error msg . args)
   (error "iset test" msg args))

(define (add1 v)
   (+ v 1))

(test-begin "iset")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bit-vectors

(test (bit-vector-shift '#u8(32) 0) '#u8(32))
(test (bit-vector-shift '#u8(32) 1) '#u8(64))
(test (bit-vector-shift '#u8(32) -1) '#u8(16))
(test (bit-vector-shift '#u8(7) 3) '#u8(56))
(test (bit-vector-shift '#u8(7) 11) '#u8(0 56))
(test (bit-vector-shift '#u8(32 64) 1) '#u8(64 128))
(test (bit-vector-shift '#u8(17 23) 3) '#u8(#x88 #xb8))
(test (bit-vector-shift '#u8(1 2 4 8) 3) '#u8(8 16 32 64))
(test (bit-vector-shift '#u8(2 4 8 0) 5) '#u8(64 128 0 1))
(test (bit-vector-shift '#u8(1 2 4 8) -3) '#u8(64 128 0 1))
(test (bit-vector-shift '#u8(2 4 8 16) -1) '#u8(1 2 4 8))
(test (bit-vector-shift '#u8(2 0) -1) '#u8(1 0))
(test (bit-vector-shift '#u8(128 2) -1) '#u8(64 1))
(test (bit-vector-shift '#u8(128 1) -1) '#u8(192 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; isets

(let ((tests
       `((() (+ 99) (u 3 50) (? 99))
         (() (u 1) (u 1000) (u -1000) (u 3) (u -1))
         ((17 29) (u 7 29))
         ((1 5 9) (= 9 5 1))
         ((2 3 4) (u 1 2 3 4 5))
         ((1 2 3 4 5) (u 2 3 4))
         ((0) (z #f) (- 0) (z))
         ((0 1 2) (- 1) (- 2) (? 0))
         ((23 29 31) (m ,add1) (? 30) (? 31 #f))
         ((1 2 3 1000 2000) (u 1 4))
         ((1 2 3 1000 1005))
         ((1 128 127))
         ((129 2 127))
         ((1 -128 -126))
         ((12354 12356 12358 12360 12362) (i 12354 12356 12362) (= 12354 12356 12362))
         ((12354 12356 12358 12360 12362) (d 12354 12356 12362) (= 12358 12360))
         )))
  (for-each
   (lambda (tst)
     (let* ((ls (car tst))
            (is (list->iset ls)))
       ;; initial creation and sanity checks
       (test (sort (iset->list is) <) (sort ls <))
       (test (iset-size is) (length ls))
       (for-each
        (lambda (x) (test-assert (iset-contains? is x)))
        ls)
       (test (iset-contains? is 42) (member 42 ls))
       ;; additional operations
       (for-each
        (lambda (op)
          (case (car op)
            ((+) (iset-adjoin! is (cadr op)) (test-assert (iset-contains? is (cadr op))))
            ((-) (iset-delete! is (cadr op)) (test-assert (not (iset-contains? is (cadr op)))))
            ((?) (test (if (pair? (cddr op)) (caddr op) #t) (iset-contains? is (cadr op))))
            ((=) (test-assert (iset= is (list->iset (cdr op)))))
            ((>) (test-assert (iset<= is (list->iset (cdr op)))))
            ((<) (test-assert (iset>= is (list->iset (cdr op)))))
            ((a) (test-assert (and (iset-any (cadr op) is) #t)))
            ((d)
             (set! is (iset-difference! is (list->iset (cdr op))))
             (for-each (lambda (x) (test-assert (not (iset-contains? is x)))) (cdr op)))
            ((e) (test-assert (and (iset-every (cadr op) is) #t)))
            ((i) (set! is (iset-intersection! is (list->iset (cdr op)))))
            ((m) (set! is (iset-map (cadr op) is)))
            ((s) (test (iset-size is) (cadr op)))
            ((u)
             (set! is (iset-union! is (list->iset (cdr op))))
             (for-each (lambda (x) (test-assert (iset-contains? is x))) (cdr op)))
            ((x) (set! is (iset-xor! is (list->iset (cdr op)))))
            ((z) (test (iset-empty? is) (if (pair? (cdr op)) (cadr op) #t)))
            (else (isettest-error "unknown operation" (car op)))))
        (cdr tst))
       ))
   tests))

;; loop through  a bit-vector represented intset
(define s1 (list->iset '(3 5 7 9)))
(let ((c (iset-cursor s1)))
   (test "cursor1" (iset-ref s1 c) 3)
   (iset-cursor-next s1 c)
   (test "cursor2" (iset-ref s1 c) 5)
   (iset-cursor-next s1 c)
   (test "cursor3" (iset-ref s1 c) 7)
   (iset-cursor-next s1 c)
   (test "cursor4" (iset-ref s1 c) 9)
   (iset-cursor-next s1 c)
   (test "cursor5" (end-of-iset? c) #t))

;; loop through a range represented intset 
(define s2 (make-iset 3 7))
(let ((c (iset-cursor s2)))
   (test "cursor6" (iset-ref s2 c) 3)
   (iset-cursor-next s2 c)
   (test "cursor7" (iset-ref s2 c) 4)
   (iset-cursor-next s2 c)
   (test "cursor8" (iset-ref s2 c) 5)
   (iset-cursor-next s2 c)
   (test "cursor9" (iset-ref s2 c) 6)
   (iset-cursor-next s2 c)
   (test "cursor10" (iset-ref s2 c) 7)
   (iset-cursor-next s2 c)
   (test "cursor11" (end-of-iset? c) #t))

(test-end "iset")

