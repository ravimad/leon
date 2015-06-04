(set-logic LIA)

(declare-var ifres!1 Int)
(declare-var r!3 Int)
(declare-var arg!1 Int)
(declare-var r!4 Int)
(declare-var l!6 Int)
(declare-var arg!0 Int)
(declare-var ifres!0 Int)
(declare-var res!31 Int)
(synth-fun a?!5 () Int ((Start Int ((Constant Int)))))
(synth-fun b?!5 () Int ((Start Int ((Constant Int)))))
(declare-fun removeLast!3 (Int) Int)

(constraint (not (and (and (and (<= (+ (- l!6) 1) (- 0)) (= (+ (- ifres!0) res!31) (- 0))) (or (and (= (+ l!6 (- 1)) (- 0)) (= (+ ifres!0 (- 4)) (- 0))) (and (and (and (and (and (and (or (<= arg!0 (- 0)) (<= (- (+ (* (- (a?!5)) arg!0) r!3) (b?!5)) (- 0))) (= (+ (- ifres!1) r!3) (- 0))) (or (and (= (+ arg!0 (- 1)) (- 0)) (= (+ ifres!1 (- 4)) (- 0))) (and (and (and (or (<= arg!0 (- 0)) (<= (+ (- arg!0) 2) (- 0))) (= (+ (+ (- r!4) ifres!1) (- 6)) (- 0))) (= (+ (+ (- arg!0) arg!1) 1) (- 0))) (= r!4 (removeLast!3 arg!1))))) (or (<= l!6 (- 0)) (<= (+ (- l!6) 2) (- 0)))) (= (+ (+ (- r!3) ifres!0) (- 6)) (- 0))) (= (+ (+ (- l!6) arg!0) 1) (- 0))) (= r!3 (removeLast!3 arg!0))))) (< (+ (- (* (a?!5) l!6) res!31) (b?!5)) (- 0)))))

(check-synth)

