(set-logic LIA)

(declare-var x!2 Int)
(declare-var r!29 Int)
(declare-var ifres!3 Int)
(declare-var r!3 Int)
(declare-var r!10 Int)
(declare-var r!36 Int)
(declare-var mid!1 Int)
(declare-var r!5 Int)
(declare-var q!3 Int)
(declare-var q!7 Int)
(declare-var r!37 Int)
(declare-var res!12 Int)
(declare-var r!34 Int)
(declare-var q!1 Int)
(declare-var ifres!7 Int)
(declare-var q!6 Int)
(declare-var r!8 Int)
(declare-var arg!23 Int)
(declare-var ifres!12 Int)
(declare-var min!0 Int)
(declare-var r!16 Int)
(declare-var arg!25 Int)
(declare-var arg!4 Int)
(declare-var q!0 Int)
(declare-var q!5 Int)
(declare-var ifres!4 Int)
(declare-var r!17 Int)
(declare-var arg!1 Int)
(declare-var ifres!8 Int)
(declare-var ifres!11 Int)
(declare-var ifres!13 Int)
(declare-var r!27 Int)
(declare-var mid!0 Int)
(declare-var res!16 Int)
(declare-var r!35 Int)
(declare-var max!1 Int)
(declare-var ifres!10 Int)
(declare-var q!2 Int)
(declare-var r!23 Int)
(declare-var r!4 Int)
(declare-var arg!5 Int)
(declare-var ifres!14 Int)
(declare-var x!1 Int)
(declare-var q!4 Int)
(declare-var ifres!2 Int)
(declare-var mid!2 Int)
(declare-var arg!18 Int)
(declare-var ifres!0 Int)
(declare-var arg!0 Int)
(declare-var r!28 Int)
(declare-var q!8 Int)
(declare-var r!11 Int)
(declare-var r!9 Int)
(declare-var ifres!6 Int)
(declare-var ifres!9 Int)
(declare-var ifres!1 Int)
(declare-var ifres!5 Int)
(declare-var arg!24 Int)
(synth-fun a?!0 () Int ((Start Int ((Constant Int)))))
(synth-fun b?!0 () Int ((Start Int ((Constant Int)))))
(declare-fun log!3 (Int) Int)
(declare-fun binarySearchAbs!3 (Int Int Int) Int)
(synth-fun a?!3 () Int ((Start Int ((Constant Int)))))
(synth-fun a?!2 () Int ((Start Int ((Constant Int)))))
(synth-fun a?!1 () Int ((Start Int ((Constant Int)))))

(constraint (not (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (<= (- min!0 max!1) (- 0)) (= (+ (- ifres!0) res!12) (- 0))) (or (and (<= (+ (- min!0) max!1) (- 0)) (= (+ ifres!0 (- 2)) (- 0))) (and (and (and (and (<= (+ (- min!0 max!1) 1) (- 0)) (= (+ (- ifres!1) ifres!0) (- 0))) (or (= (+ (+ (+ min!0 max!1) (* (- 2) q!0)) (- 1)) (- 0)) (= (+ (+ min!0 max!1) (* (- 2) q!0)) (- 0)))) (= (- mid!0 q!0) (- 0))) (or (and (and (and (and (and (= (- r!3 ifres!9) (- 0)) (or (and (<= (+ (- min!0) mid!0) (- 0)) (= (+ ifres!9 (- 2)) (- 0))) (and (and (and (and (<= (+ (- min!0 mid!0) 1) (- 0)) (= (- ifres!9 ifres!10) (- 0))) (or (= (+ (+ (+ min!0 (* (- 2) q!7)) mid!0) (- 1)) (- 0)) (= (+ (+ min!0 (* (- 2) q!7)) mid!0) (- 0)))) (= (- mid!1 q!7) (- 0))) (or (and (and (and (or (<= (+ (+ (- min!0) mid!1) 1) (- 0)) (and (and (<= (- (+ r!28 (* (- (a?!0)) r!36)) (b?!0)) (- 0)) (= (+ (- min!0 mid!1) arg!24) (- 0))) (= r!36 (log!3 arg!24)))) (<= (+ (+ (- mid!1) x!2) 1) (- 0))) (= (+ (+ (- r!28) ifres!10) (- 5)) (- 0))) (= r!28 (binarySearchAbs!3 x!2 min!0 mid!1))) (and (and (<= (- mid!1 x!2) (- 0)) (= (+ (- ifres!11) ifres!10) (- 0))) (or (and (and (and (and (or (<= (+ (+ (- arg!18) mid!0) 1) (- 0)) (and (and (<= (- (+ r!29 (* (- (a?!0)) r!37)) (b?!0)) (- 0)) (= (- (+ arg!18 arg!25) mid!0) (- 0))) (= r!37 (log!3 arg!25)))) (<= (+ (- mid!1 x!2) 1) (- 0))) (= (+ (+ (- r!29) ifres!11) (- 7)) (- 0))) (= (+ (+ (- mid!1) arg!18) (- 1)) (- 0))) (= r!29 (binarySearchAbs!3 x!2 arg!18 mid!0))) (and (<= (+ (- mid!1) x!2) (- 0)) (= (+ ifres!11 (- 8)) (- 0))))))))) (or (<= (+ (+ (- min!0) mid!0) 1) (- 0)) (and (and (and (and (and (<= (- (+ (* (- (a?!0)) r!11) r!3) (b?!0)) (- 0)) (= (- (+ min!0 arg!5) mid!0) (- 0))) (or (<= (+ arg!5 1) (- 0)) (<= (+ (+ (* (a?!3) arg!5) (* (a?!2) r!11)) (a?!1)) (- 0)))) (= (+ (- ifres!7) r!11) (- 0))) (or (and (<= (+ arg!5 (- 1)) (- 0)) (= ifres!7 (- 0))) (and (and (and (and (or (<= (+ q!5 1) (- 0)) (<= (+ (+ (* (a?!3) q!5) (* (a?!2) r!23)) (a?!1)) (- 0))) (<= (+ (- arg!5) 2) (- 0))) (= (+ (- ifres!7 r!23) (- 1)) (- 0))) (= r!23 (log!3 q!5))) (or (= (+ (+ arg!5 (* (- 2) q!5)) (- 1)) (- 0)) (= (+ arg!5 (* (- 2) q!5)) (- 0)))))) (= r!11 (log!3 arg!5))))) (<= (+ (- x!2 mid!0) 1) (- 0))) (= (+ (- ifres!1 r!3) (- 5)) (- 0))) (= r!3 (binarySearchAbs!3 x!2 min!0 mid!0))) (and (and (<= (+ (- x!2) mid!0) (- 0)) (= (- ifres!1 ifres!2) (- 0))) (or (and (and (and (and (and (and (= (- r!4 ifres!12) (- 0)) (or (and (<= (+ (- arg!0) max!1) (- 0)) (= (+ ifres!12 (- 2)) (- 0))) (and (and (and (and (<= (+ (- arg!0 max!1) 1) (- 0)) (= (- ifres!12 ifres!13) (- 0))) (or (= (+ (+ (+ (* (- 2) q!8) arg!0) max!1) (- 1)) (- 0)) (= (+ (+ (* (- 2) q!8) arg!0) max!1) (- 0)))) (= (+ (- q!8) mid!2) (- 0))) (or (and (and (<= (+ (- x!2 mid!2) 1) (- 0)) (= (+ (+ (- r!34) ifres!13) (- 5)) (- 0))) (= r!34 (binarySearchAbs!3 x!2 arg!0 mid!2))) (and (and (<= (+ (- x!2) mid!2) (- 0)) (= (+ (- ifres!14) ifres!13) (- 0))) (or (and (and (and (<= (+ (+ (- x!2) mid!2) 1) (- 0)) (= (+ (- ifres!14 r!35) (- 7)) (- 0))) (= (+ (- arg!23 mid!2) (- 1)) (- 0))) (= r!35 (binarySearchAbs!3 x!2 arg!23 max!1))) (and (<= (- x!2 mid!2) (- 0)) (= (+ ifres!14 (- 8)) (- 0))))))))) (or (<= (+ (+ (- arg!0) max!1) 1) (- 0)) (and (and (and (and (and (<= (- (+ r!4 (* (- (a?!0)) r!10)) (b?!0)) (- 0)) (= (- (+ arg!4 arg!0) max!1) (- 0))) (or (<= (+ arg!4 1) (- 0)) (<= (+ (+ (* (a?!3) arg!4) (* (a?!2) r!10)) (a?!1)) (- 0)))) (= (+ (- ifres!5) r!10) (- 0))) (or (and (<= (+ arg!4 (- 1)) (- 0)) (= ifres!5 (- 0))) (and (and (and (and (or (<= (+ q!3 1) (- 0)) (<= (+ (+ (* (a?!3) q!3) (* (a?!2) r!16)) (a?!1)) (- 0))) (<= (+ (- arg!4) 2) (- 0))) (= (+ (- ifres!5 r!16) (- 1)) (- 0))) (= r!16 (log!3 q!3))) (or (= (+ (+ (* (- 2) q!3) arg!4) (- 1)) (- 0)) (= (+ (* (- 2) q!3) arg!4) (- 0)))))) (= r!10 (log!3 arg!4))))) (<= (+ (+ (- x!2) mid!0) 1) (- 0))) (= (+ (+ (- r!4) ifres!2) (- 7)) (- 0))) (= (+ (- arg!0 mid!0) (- 1)) (- 0))) (= r!4 (binarySearchAbs!3 x!2 arg!0 max!1))) (and (<= (- x!2 mid!0) (- 0)) (= (+ ifres!2 (- 8)) (- 0))))))))) (< (+ (- (* (a?!0) r!5) res!12) (b?!0)) (- 0))) (= (- (+ arg!1 min!0) max!1) (- 0))) (or (<= (+ arg!1 1) (- 0)) (<= (+ (+ (* (a?!3) arg!1) (* (a?!2) r!5)) (a?!1)) (- 0)))) (= (- r!5 ifres!4) (- 0))) (or (and (<= (+ arg!1 (- 1)) (- 0)) (= ifres!4 (- 0))) (and (and (and (and (and (and (= (- r!9 ifres!8) (- 0)) (or (and (<= (+ q!2 (- 1)) (- 0)) (= ifres!8 (- 0))) (and (and (and (and (or (<= (+ q!6 1) (- 0)) (<= (+ (+ (* (a?!3) q!6) (* (a?!2) r!27)) (a?!1)) (- 0))) (<= (+ (- q!2) 2) (- 0))) (= (+ (+ (- r!27) ifres!8) (- 1)) (- 0))) (= r!27 (log!3 q!6))) (or (= (+ (+ (* (- 2) q!6) q!2) (- 1)) (- 0)) (= (+ (* (- 2) q!6) q!2) (- 0)))))) (or (<= (+ q!2 1) (- 0)) (<= (+ (+ (* (a?!2) r!9) (* (a?!3) q!2)) (a?!1)) (- 0)))) (<= (+ (- arg!1) 2) (- 0))) (= (+ (+ (- r!9) ifres!4) (- 1)) (- 0))) (= r!9 (log!3 q!2))) (or (= (+ (+ arg!1 (* (- 2) q!2)) (- 1)) (- 0)) (= (+ arg!1 (* (- 2) q!2)) (- 0)))))) (= r!5 (log!3 arg!1))) (or (<= (+ (- arg!1 q!2) 1) (- 0)) (<= (+ (- r!5) r!9) (- 0)))) (or (<= (+ (+ (- arg!1) q!2) 1) (- 0)) (<= (- r!5 r!9) (- 0)))) (or (<= (+ (+ (- arg!1) q!3) 1) (- 0)) (<= (- r!5 r!16) (- 0)))) (or (<= (+ (- q!3 q!2) 1) (- 0)) (<= (- r!9 r!16) (- 0)))) (or (<= (+ (+ (- arg!5) q!3) 1) (- 0)) (<= (- r!11 r!16) (- 0)))) (or (<= (+ (- q!3 arg!4) 1) (- 0)) (<= (- r!10 r!16) (- 0)))) (or (<= (+ (+ (- arg!1) arg!4) 1) (- 0)) (<= (- r!5 r!10) (- 0)))) (or (<= (+ (+ (- arg!5) q!2) 1) (- 0)) (<= (+ (- r!9) r!11) (- 0)))) (or (<= (+ (+ (- q!3) arg!4) 1) (- 0)) (<= (+ (- r!10) r!16) (- 0)))) (or (<= (+ (+ (- q!2) arg!4) 1) (- 0)) (<= (- r!9 r!10) (- 0)))) (or (<= (+ (- arg!1 arg!5) 1) (- 0)) (<= (+ (- r!5) r!11) (- 0)))) (or (<= (+ (+ (- arg!1) arg!5) 1) (- 0)) (<= (- r!5 r!11) (- 0)))) (or (<= (+ (- arg!5 q!3) 1) (- 0)) (<= (+ (- r!11) r!16) (- 0)))) (or (<= (+ (- arg!5 arg!4) 1) (- 0)) (<= (- r!10 r!11) (- 0)))) (or (<= (+ (- arg!5 q!2) 1) (- 0)) (<= (- r!9 r!11) (- 0)))) (or (<= (+ (+ (- arg!4) q!2) 1) (- 0)) (<= (+ (- r!9) r!10) (- 0)))) (or (<= (+ (+ (- q!3) q!2) 1) (- 0)) (<= (+ (- r!9) r!16) (- 0)))) (or (<= (+ (+ (- arg!5) arg!4) 1) (- 0)) (<= (+ (- r!10) r!11) (- 0)))) (or (<= (+ (- arg!1 q!3) 1) (- 0)) (<= (+ (- r!5) r!16) (- 0)))) (or (<= (+ (- arg!1 arg!4) 1) (- 0)) (<= (+ (- r!5) r!10) (- 0)))) (or (<= (+ (+ (- arg!5) q!5) 1) (- 0)) (<= (- r!11 r!23) (- 0)))) (or (<= (+ (- arg!5 q!5) 1) (- 0)) (<= (+ (- r!11) r!23) (- 0)))) (or (<= (+ (- q!3 q!5) 1) (- 0)) (<= (+ (- r!16) r!23) (- 0)))) (or (<= (+ (+ (- q!2) q!5) 1) (- 0)) (<= (- r!9 r!23) (- 0)))) (or (<= (+ (- q!2 q!5) 1) (- 0)) (<= (+ (- r!9) r!23) (- 0)))) (or (<= (+ (- arg!1 q!5) 1) (- 0)) (<= (+ (- r!5) r!23) (- 0)))) (or (<= (+ (- arg!4 q!5) 1) (- 0)) (<= (+ (- r!10) r!23) (- 0)))) (or (<= (+ (+ (- arg!1) q!5) 1) (- 0)) (<= (- r!5 r!23) (- 0)))) (or (<= (+ (+ (- q!3) q!5) 1) (- 0)) (<= (- r!16 r!23) (- 0)))) (or (<= (+ (+ (- arg!4) q!5) 1) (- 0)) (<= (- r!10 r!23) (- 0)))) (or (<= (+ (- q!6 q!5) 1) (- 0)) (<= (- r!23 r!27) (- 0)))) (or (<= (+ (- q!3 q!6) 1) (- 0)) (<= (+ (- r!16) r!27) (- 0)))) (or (<= (+ (+ (- q!6) arg!4) 1) (- 0)) (<= (+ (- r!10) r!27) (- 0)))) (or (<= (+ (+ (- q!3) q!6) 1) (- 0)) (<= (- r!16 r!27) (- 0)))) (or (<= (+ (+ (- arg!5) q!6) 1) (- 0)) (<= (- r!11 r!27) (- 0)))) (or (<= (+ (+ (- arg!1) q!6) 1) (- 0)) (<= (- r!5 r!27) (- 0)))) (or (<= (+ (+ (- arg!4) q!6) 1) (- 0)) (<= (- r!10 r!27) (- 0)))) (or (<= (+ (+ (- q!6) q!5) 1) (- 0)) (<= (+ (- r!23) r!27) (- 0)))) (or (<= (+ (- arg!1 q!6) 1) (- 0)) (<= (+ (- r!5) r!27) (- 0)))) (or (<= (+ (+ (- q!2) q!6) 1) (- 0)) (<= (- r!9 r!27) (- 0)))) (or (<= (+ (- arg!5 q!6) 1) (- 0)) (<= (+ (- r!11) r!27) (- 0)))) (or (<= (+ (+ (- q!6) q!2) 1) (- 0)) (<= (+ (- r!9) r!27) (- 0)))) (<= (- x!1) (- 0))) (= (+ (- ifres!3) res!16) (- 0))) (or (and (<= (+ x!1 (- 1)) (- 0)) (= ifres!3 (- 0))) (and (and (and (and (and (and (or (<= (+ q!1 1) (- 0)) (<= (+ (+ (* (a?!3) q!1) (* (a?!2) r!8)) (a?!1)) (- 0))) (= (+ (- ifres!6) r!8) (- 0))) (or (and (<= (+ q!1 (- 1)) (- 0)) (= ifres!6 (- 0))) (and (and (and (and (or (<= (+ q!4 1) (- 0)) (<= (+ (+ (* (a?!2) r!17) (* (a?!3) q!4)) (a?!1)) (- 0))) (<= (+ (- q!1) 2) (- 0))) (= (+ (+ (- r!17) ifres!6) (- 1)) (- 0))) (= r!17 (log!3 q!4))) (or (= (+ (+ (* (- 2) q!4) q!1) (- 1)) (- 0)) (= (+ (* (- 2) q!4) q!1) (- 0)))))) (<= (+ (- x!1) 2) (- 0))) (= (+ (- ifres!3 r!8) (- 1)) (- 0))) (= r!8 (log!3 q!1))) (or (= (+ (+ x!1 (* (- 2) q!1)) (- 1)) (- 0)) (= (+ x!1 (* (- 2) q!1)) (- 0)))))) (< (- (+ (* (- (a?!2)) res!16) (* (- (a?!3)) x!1)) (a?!1)) (- 0))) (or (<= (+ (+ (- q!4) q!1) 1) (- 0)) (<= (- r!17 r!8) (- 0)))) (or (<= (+ (- q!4 q!1) 1) (- 0)) (<= (+ (- r!17) r!8) (- 0))))))

(check-synth)

