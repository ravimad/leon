(set-logic LIA)

(declare-var ifres!12 Int)
(declare-var arg!16 Int)
(declare-var ifres!10 Int)
(declare-var res!26 Int)
(declare-var arg!13 Int)
(declare-var arg!18 Int)
(declare-var arg!14 Int)
(declare-var l1!4 Int)
(declare-var arg!17 Int)
(declare-var l!5 Int)
(declare-var r!15 Int)
(declare-var l2!4 Int)
(declare-var arg!15 Int)
(declare-var arg!19 Int)
(declare-var ifres!11 Int)
(declare-var r!14 Int)
(declare-var r!16 Int)
(declare-var res!35 Int)
(declare-var r!13 Int)
(declare-fun reverseRec!3 (Int Int) Int)
(synth-fun a?!2 () Int ((Start Int ((Constant Int)))))
(synth-fun b?!2 () Int ((Start Int ((Constant Int)))))

(constraint (not (and (and (and (and (and (and (and (and (and (and (and (and (<= (- l!5) (- 0)) (= (+ (+ (- r!13) res!26) (- 1)) (- 0))) (= arg!13 (- 0))) (or (or (<= (+ l!5 1) (- 0)) (<= (+ arg!13 1) (- 0))) (<= (+ (+ r!13 (* (- 9) l!5)) (- 4)) (- 0)))) (= (- r!13 ifres!11) (- 0))) (or (and (= l!5 (- 0)) (= (+ ifres!11 (- 3)) (- 0))) (and (and (and (and (and (or (or (<= (+ arg!16 1) (- 0)) (<= (+ arg!17 1) (- 0))) (<= (+ (+ (* (- 9) arg!16) r!15) (- 4)) (- 0))) (or (<= (+ l!5 1) (- 0)) (<= (+ (- l!5) 1) (- 0)))) (= (+ (- ifres!11 r!15) (- 6)) (- 0))) (= (+ (- arg!16 l!5) 1) (- 0))) (= (+ (- arg!17 arg!13) (- 1)) (- 0))) (= r!15 (reverseRec!3 arg!16 arg!17))))) (= r!13 (reverseRec!3 l!5 arg!13))) (< (+ (- (* (a?!2) l!5) res!26) (b?!2)) (- 0))) (<= (- l1!4) (- 0))) (<= (- l2!4) (- 0))) (= (+ (- ifres!10) res!35) (- 0))) (or (and (= l1!4 (- 0)) (= (+ ifres!10 (- 3)) (- 0))) (and (and (and (and (and (and (and (or (or (<= (+ arg!14 1) (- 0)) (<= (+ arg!15 1) (- 0))) (<= (+ (+ r!14 (* (- 9) arg!14)) (- 4)) (- 0))) (= (- r!14 ifres!12) (- 0))) (or (and (= arg!14 (- 0)) (= (+ ifres!12 (- 3)) (- 0))) (and (and (and (and (or (<= (+ arg!14 1) (- 0)) (<= (+ (- arg!14) 1) (- 0))) (= (+ (+ (- r!16) ifres!12) (- 6)) (- 0))) (= (+ (+ (- arg!14) arg!18) 1) (- 0))) (= (+ (+ (- arg!15) arg!19) (- 1)) (- 0))) (= r!16 (reverseRec!3 arg!18 arg!19))))) (or (<= (+ l1!4 1) (- 0)) (<= (+ (- l1!4) 1) (- 0)))) (= (+ (+ (- r!14) ifres!10) (- 6)) (- 0))) (= (+ (+ (- l1!4) arg!14) 1) (- 0))) (= (+ (- arg!15 l2!4) (- 1)) (- 0))) (= r!14 (reverseRec!3 arg!14 arg!15))))) (<= (+ (- (* 9 l1!4) res!35) 5) (- 0)))))

(check-synth)

