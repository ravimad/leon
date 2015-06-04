(set-logic LIA)

(declare-var ifres!0 Int)
(declare-var r!4 Int)
(declare-var arg!1 Int)
(declare-var res!23 Int)
(declare-var r!3 Int)
(declare-var n!0 Int)
(declare-var arg!0 Int)
(declare-var ifres!1 Int)
(synth-fun a?!0 () Int ((Start Int ((Constant Int)))))
(synth-fun b?!0 () Int ((Start Int ((Constant Int)))))
(declare-fun genL!3 (Int) Int)

(constraint (not (and (and (and (<= (- n!0) (- 0)) (= (- res!23 ifres!0) (- 0))) (or (and (= n!0 (- 0)) (= (+ ifres!0 (- 2)) (- 0))) (and (and (and (and (and (and (or (<= (+ arg!0 1) (- 0)) (<= (- (+ r!3 (* (- (a?!0)) arg!0)) (b?!0)) (- 0))) (= (+ (- ifres!1) r!3) (- 0))) (or (and (= arg!0 (- 0)) (= (+ ifres!1 (- 2)) (- 0))) (and (and (and (or (<= (+ arg!0 1) (- 0)) (<= (+ (- arg!0) 1) (- 0))) (= (+ (+ (- r!4) ifres!1) (- 4)) (- 0))) (= (+ (- arg!1 arg!0) 1) (- 0))) (= r!4 (genL!3 arg!1))))) (or (<= (+ n!0 1) (- 0)) (<= (+ (- n!0) 1) (- 0)))) (= (+ (- ifres!0 r!3) (- 4)) (- 0))) (= (+ (+ (- n!0) arg!0) 1) (- 0))) (= r!3 (genL!3 arg!0))))) (< (+ (+ (- res!23) (* (a?!0) n!0)) (b?!0)) (- 0)))))

(check-synth)

