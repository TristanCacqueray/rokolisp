;; FACT :: Natural -> Natural
(λx
  ;; Natural
  (let ZERO  (λf n n)
  (let ONE   (λf n (f n))

  ;; Bool
  (let TRUE  (λx y x)
  (let FALSE (λx y y)
  (let IS_ZERO (λn (n (λx FALSE) TRUE))

  ;; Pair
  (let CONS (λa b s (s a b))
  (let CAR (λp (p TRUE))
  (let CDR (λp (p FALSE))

  ;; Natural algebra
  (let INCR (λn f s (f (n f s)))
  (let MUL (λm n f (m (n f)))

  ;; Gadget for decr
  (let T (λp (CONS (INCR (CAR p)) (CAR p)))
  (let DECR (λn (CDR (n T (CONS ZERO ZERO))))

  ;; Factorial definition
  (let R  (λf x (IS_ZERO x ONE (MUL x (f (DECR x)))))

  ;; Y combinator
  (let Y (λg ((λx (g (x x))) (λx (g (x x)))))

  ;; Main
  (let F (Y R)
    (church_numeral_decode (F (church_numeral_encode x)))
))))))))))))))))
