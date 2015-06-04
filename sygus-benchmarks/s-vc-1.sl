(set-logic LIA)

(declare-var r!4 Int)
(declare-var ifres!0 Int)
(declare-var arg!1 Int)
(declare-var arg!0 Int)
(declare-var ifres!1 Int)
(declare-var x!1 Int)
(declare-var r!3 Int)
(declare-var res!10 Int)
(synth-fun a?!0 () Int ((Start Int ((Constant Int)))))
(synth-fun b?!0 () Int ((Start Int ((Constant Int)))))
(synth-fun c?!0 () Int ((Start Int ((Constant Int)))))
(declare-fun s!3 (Int) Int)

(constraint (not (and (and (= (+ (- ifres!0) res!10) (- 0)) (or (and (<= (+ x!1 1) (- 0)) (= ifres!0 (- 0))) (and (and (and (and (and (and (and (<= (+ (+ (* (a?!0) r!3) (* (b?!0) arg!0)) (c?!0)) (- 0)) (= (+ (- ifres!1) r!3) (- 0))) (or (and (<= (+ arg!0 1) (- 0)) (= ifres!1 (- 0))) (and (and (and (and (or (<= (+ r!4 2) (- 0)) (<= (- r!4) (- 0))) (<= (- arg!0) (- 0))) (= (+ (+ (- r!4) ifres!1) (- 1)) (- 0))) (= (+ (- arg!1 arg!0) 1) (- 0))) (= r!4 (s!3 arg!1))))) (or (<= (+ r!3 2) (- 0)) (<= (- r!3) (- 0)))) (<= (- x!1) (- 0))) (= (+ (+ (- r!3) ifres!0) (- 1)) (- 0))) (= (+ (+ (- x!1) arg!0) 1) (- 0))) (= r!3 (s!3 arg!0))))) (or (= (+ res!10 1) (- 0)) (< (- (+ (* (- (a?!0)) res!10) (* (- (b?!0)) x!1)) (c?!0)) (- 0))))))

(check-synth)

