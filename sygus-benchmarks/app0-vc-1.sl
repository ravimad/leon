(set-logic LIA)

(declare-var a!0 Int)
(declare-var arg!2 Int)
(declare-var arg!1 Int)
(declare-var arg!3 Int)
(declare-var b!0 Int)
(declare-var arg!0 Int)
(declare-var r!4 Int)
(declare-var res!15 Int)
(declare-var ifres!0 Int)
(declare-var r!3 Int)
(declare-var ifres!1 Int)
(synth-fun p?!0 () Int ((Start Int ((Constant Int)))))
(synth-fun r?!0 () Int ((Start Int ((Constant Int)))))
(synth-fun q?!0 () Int ((Start Int ((Constant Int)))))
(declare-fun app0!3 (Int Int) Int)

(constraint (not (and (and (and (and (<= (- a!0) (- 0)) (<= (- b!0) (- 0))) (= (+ (- ifres!0) res!15) (- 0))) (or (and (<= a!0 (- 0)) (= (+ (- b!0) ifres!0) (- 0))) (and (and (and (and (and (and (and (or (or (<= (+ arg!0 1) (- 0)) (<= (+ arg!1 1) (- 0))) (and (= (+ (+ (* (p?!0) r!3) (* (r?!0) arg!1)) (* (q?!0) arg!0)) (- 0)) (or (< (q?!0) (- 0)) (< (- (q?!0)) (- 0))))) (= (- r!3 ifres!1) (- 0))) (or (and (<= arg!0 (- 0)) (= (+ (- arg!1) ifres!1) (- 0))) (and (and (and (and (<= (+ (- arg!0) 1) (- 0)) (= (+ (- r!4) ifres!1) (- 0))) (= (+ (- arg!2 arg!0) 1) (- 0))) (= (+ (- arg!3 arg!1) (- 1)) (- 0))) (= r!4 (app0!3 arg!2 arg!3))))) (<= (+ (- a!0) 1) (- 0))) (= (- ifres!0 r!3) (- 0))) (= (+ (+ (- a!0) arg!0) 1) (- 0))) (= (+ (+ (- b!0) arg!1) (- 1)) (- 0))) (= r!3 (app0!3 arg!0 arg!1))))) (or (or (< (+ (+ (* (r?!0) b!0) (* (q?!0) a!0)) (* (p?!0) res!15)) (- 0)) (< (+ (+ (* (- (r?!0)) b!0) (* (- (q?!0)) a!0)) (* (- (p?!0)) res!15)) (- 0))) (= (q?!0) (- 0))))))

(check-synth)

