(set-logic LIA)

(declare-var r!12 Int)
(declare-var res!29 Int)
(declare-var arg!10 Int)
(declare-var ifres!9 Int)
(declare-var l1!4 Int)
(declare-var arg!9 Int)
(declare-var l2!4 Int)
(declare-var ifres!8 Int)
(declare-var arg!12 Int)
(declare-var r!11 Int)
(declare-var arg!11 Int)
(synth-fun a?!1 () Int ((Start Int ((Constant Int)))))
(synth-fun b?!1 () Int ((Start Int ((Constant Int)))))
(declare-fun reverseRec!3 (Int Int) Int)

(constraint (not (and (and (and (and (<= (- l1!4) (- 0)) (<= (- l2!4) (- 0))) (= (+ (- ifres!8) res!29) (- 0))) (or (and (= l1!4 (- 0)) (= (+ ifres!8 (- 3)) (- 0))) (and (and (and (and (and (and (and (or (or (<= (+ arg!9 1) (- 0)) (<= (+ arg!10 1) (- 0))) (<= (- (+ (* (- (a?!1)) arg!9) r!11) (b?!1)) (- 0))) (= (+ (- ifres!9) r!11) (- 0))) (or (and (= arg!9 (- 0)) (= (+ ifres!9 (- 3)) (- 0))) (and (and (and (and (or (<= (+ arg!9 1) (- 0)) (<= (+ (- arg!9) 1) (- 0))) (= (+ (- ifres!9 r!12) (- 6)) (- 0))) (= (+ (- arg!11 arg!9) 1) (- 0))) (= (+ (- arg!12 arg!10) (- 1)) (- 0))) (= r!12 (reverseRec!3 arg!11 arg!12))))) (or (<= (+ l1!4 1) (- 0)) (<= (+ (- l1!4) 1) (- 0)))) (= (+ (- ifres!8 r!11) (- 6)) (- 0))) (= (+ (+ (- l1!4) arg!9) 1) (- 0))) (= (+ (- arg!10 l2!4) (- 1)) (- 0))) (= r!11 (reverseRec!3 arg!9 arg!10))))) (< (+ (- (* (a?!1) l1!4) res!29) (b?!1)) (- 0)))))

(check-synth)

