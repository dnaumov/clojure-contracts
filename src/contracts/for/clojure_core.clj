(ns contracts.for.clojure-core
  (:use [contracts.core :only [provide-contracts boolean?] :as c]))

(defmacro un-inline! [& syms]
  (cons `do (for [sym syms]
              `(alter-meta! (var ~sym) dissoc :inline))))

(un-inline! unchecked-inc-int object-array booleans == longs shorts
            unchecked-multiply bit-or unchecked-dec nil? boolean-array long-array
            short-array char-array long short bit-xor -' rem unchecked-inc
            unchecked-negate floats * identical? zero? bit-and / unchecked-add
            unchecked-remainder-int boolean float-array doubles int-array
            unchecked-long float +' double-array chars unchecked-short
            unchecked-double unchecked-int inc unchecked-subtract bit-not
            unchecked-multiply-int unchecked-dec-int aset inc' quot
            unchecked-negate-int count unchecked-subtract-int nth byte < =
            unchecked-float *' unchecked-divide-int > max ints unchecked-add-int
            >= bit-shift-left dec' compare get <= bytes char byte-array int
            unchecked-char dec aget aclone bit-shift-right num double
            unchecked-byte alength bit-and-not min)

(def numbers? (c/every? number?))


;;; Numbers
(provide-contracts
  (+ (c/=> ([] [number?] [number? number?] [number? number? & numbers?])
           number?))
  (- (c/=> ([number?] [number? number?] [number? number? & numbers?])
           number?))
  (* (c/=> ([] [number?] [number? number?] [number? number? & numbers?])
           number?))
  (- (c/=> ([number?] [number? number?] [number? number? & numbers?])
           number?))

  (zero? [number? => boolean?])
  (pos? [number? => boolean?])
  (neg? [number? => boolean?])
  (even? [number? => boolean?])
  (odd? [number? => boolean?]))


;;; Collections
(provide-contracts ;; TODO: add `nil?` to collections' preds?
  (hash-map (c/=> [& (comp even? count)] map?))
  (assoc (c/=> [(c/or map? vector? nil?) & (comp even? count)]
               (c/or map? vector?)))
  (dissoc (c/=> [(c/or map? nil?) & c/any]
                (c/or map? nil?))))


;;; Sequences
(provide-contracts
  (repeat (c/=> ([c/any] [number? c/any])
                seq?))
  (repeatedly (c/=> ([fn?] [number? (c/=> [] c/any)])
                    seq?))) 
