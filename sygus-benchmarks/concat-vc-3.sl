(set-logic LIA)

(declare-var arg!8 Int)
(declare-var arg!5 Int)
(declare-var r!10 Int)
(declare-var ifres!6 Int)
(declare-var r!9 Int)
(declare-var ifres!7 Int)
(declare-var l1!3 Int)
(declare-var res!32 Int)
(declare-var arg!6 Int)
(declare-var l2!3 Int)
(declare-var arg!7 Int)
(synth-fun a?!0 () Int ((Start Int ((Constant Int)))))
(synth-fun b?!0 () Int ((Start Int ((Constant Int)))))
(declare-fun concat!3 (Int Int) Int)

(constraint (not (and (and (and (and (<= (- l1!3) (- 0)) (<= (- l2!3) (- 0))) (= (- res!32 ifres!6) (- 0))) (or (and (= l1!3 (- 0)) (= (+ ifres!6 (- 3)) (- 0))) (and (and (and (and (and (and (and (or (or (<= (+ arg!5 1) (- 0)) (<= (+ arg!6 1) (- 0))) (<= (- (+ (* (- (a?!0)) arg!5) r!9) (b?!0)) (- 0))) (= (+ (- ifres!7) r!9) (- 0))) (or (and (= arg!5 (- 0)) (= (+ ifres!7 (- 3)) (- 0))) (and (and (and (and (or (<= (+ arg!5 1) (- 0)) (<= (+ (- arg!5) 1) (- 0))) (= (+ (- ifres!7 r!10) (- 5)) (- 0))) (= (+ (- arg!7 arg!5) 1) (- 0))) (= (+ (+ (- arg!6) arg!8) (- 1)) (- 0))) (= r!10 (concat!3 arg!7 arg!8))))) (or (<= (+ l1!3 1) (- 0)) (<= (+ (- l1!3) 1) (- 0)))) (= (+ (- ifres!6 r!9) (- 5)) (- 0))) (= (+ (- arg!5 l1!3) 1) (- 0))) (= (+ (- arg!6 l2!3) (- 1)) (- 0))) (= r!9 (concat!3 arg!5 arg!6))))) (< (+ (+ (- res!32) (* (a?!0) l1!3)) (b?!0)) (- 0)))))

(check-synth)

