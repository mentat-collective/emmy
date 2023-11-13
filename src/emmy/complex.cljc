#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.complex
  "This namespace provides a number of functions and constructors for working
  with [[Complex]] numbers in Clojure and ClojureScript, and
  installs [[Complex]] into the Emmy generic arithmetic system.

  For other numeric extensions, see [[emmy.ratio]]
  and [[emmy.numbers]]."
  (:require [emmy.complex.impl :as c]
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v]))

(def ZERO
  "A [[Complex]] value equal to 0 (south pole on the Riemann Sphere)."
  c/ZERO)

(def ONE
  "A [[Complex]] value equal to 1."
  c/ONE)

(def I
  "A [[Complex]] value equal to `i`."
  c/I)

(def -I
  "A [[Complex]] value equal to `-i`."
  c/-I)

(derive ::complex ::v/number)

(defn complex
  "Returns a [[Complex]] number with the supplied real part `re` and imaginary
  part `im`. `im` defaults to 0."
  ([c]
   (cond (string? c) (c/parse c)
         (vector? c) (do
                       (assert (= (count c) 2))
                       (let [[re im] c] (c/->Complex re im)))
         :else (c/->Complex c 0)))
  ([re im]
   (c/->Complex re im)))

(defn parse-complex
  "Used as a [data reader](https://www.clojurescript.org/guides/reader#_clojurescript_compilation).
  In Clojure, we could get away with just using the constructor above, but in
  ClojureScript we need to act as a macro to delay evaluation of the constructor."
  [c]
  `(complex ~c))

(defn complex?
  "Returns true if `a` is an instance of [[Complex]], false otherwise."
  [a]
  (instance? emmy.complex.impl.Complex a))

(defn ^:no-doc real
  [z]
  (c/real z))

(defn ^:no-doc imaginary
  [z]
  (c/imaginary z))

;; ## Gaussian Integers

(defn round
  "Generates a [Gaussian integer](https://en.wikipedia.org/wiki/Gaussian_integer)
  from the complex number `z` by rounding the real and imaginary components of
  `z` to their nearest integral values. (Note: the use of cast-to-double is
   unfortunate here, as complex numbers can now contain exact fractions, and
   we'd want a nearest integer generic function for those)"
  [z]
  (cond (complex? z) (complex (-> z real u/double Math/round u/int)
                              (-> z imaginary u/double Math/round u/int))
        (v/native-integral? z) z
        :else (Math/round (u/double z))))

;; ## Complex GCD

(defn ^:no-doc abs-real
  "Returns a complex or real number with a positive real component. (i.e., either z
  or (* -1 z)), whichever number has a positive real component."
  [z]
  (cond (complex? z)
        (if (g/negative? (real z))
          (g/negate z)
          z)

        (v/real? z)
        (g/abs z)

        :else (u/illegal "not supported!")))

(defn ^:no-doc gcd
  "Returns the complex gcd of two complex numbers using the euclidean algorithm.

  For more details on the algorithm, see [this post on Ask Dr
  Math](https://web.archive.org/web/20190720160400/http://mathforum.org/library/drmath/view/67068.html).

  NOTE that the GCD of two complex numbers is determined up to a factor of ±1
  and ±i."
  [l r]
  (cond (g/zero? l) r
        (g/zero? r) l
        (v/= l r)   (abs-real l)
        (not (and (g/exact? l)
                  (g/exact? r))) (u/illegal "gcd can only be computed for gaussian integers.")

        :else (let [[l r] (if (> (g/magnitude l)
                                 (g/magnitude r))
                            [l r] [r l])]
                (loop [a l
                       b r]
                  (if (g/zero? b)
                    (abs-real a)
                    (recur b (g/sub a (g/mul (round (g/div a b)) b))))))))

;; ## Generic Method Installation
(defmethod g/zero? [::complex] [z] (c/zero? z))
(defmethod g/one? [::complex] [c] (and (g/one? (real c))
                                       (g/zero? (imaginary c))))
(defmethod g/identity? [::complex] [c] (g/one? c))
(defmethod g/zero-like [::complex] [c] (c/zero-like c))
(defmethod g/one-like [::complex] [c] (c/one-like c))
(defmethod g/identity-like [::complex] [c] (c/one-like c))
(defmethod g/freeze [::complex] [c]
  (let [re (real c)
        im (imaginary c)]
    (if (g/zero? im)
      re
      (list 'complex re im))))
(defmethod g/exact? [::complex] [c]
  (and (g/exact? (real c)) (g/exact? (imaginary c))))
(defmethod g/gcd [::complex ::complex] [a b] (gcd a b))
(defmethod g/gcd [::complex ::v/real] [a b] (gcd a b))
(defmethod g/gcd [::v/real ::complex] [a b] (gcd a b))

(defmethod g/make-rectangular [::v/real ::v/real] [re im]
  (if (g/zero? im)
    re
    (complex re im)))

(defmethod g/make-polar [::v/real ::v/real] [radius angle]
  (cond (g/zero? radius) radius
        (g/zero? angle)  radius
        :else (complex (g/* radius (g/cos angle))
                       (g/* radius (g/sin angle)))))

(defmethod g/real-part [::complex] [a] (real a))
(defmethod g/imag-part [::complex] [a] (imaginary a))

(defmethod g/magnitude [::complex] [z] (c/abs z))
(defmethod g/angle [::complex] [z] (c/arg z))
(defmethod g/conjugate [::complex] [z] (c/conjugate z))

(defmethod g/dot-product [::complex ::complex] [a b]
  (g/+ (g/* (real a) (real b))
       (g/* (imaginary a) (imaginary b))))
(defmethod g/dot-product [::complex ::v/real] [a b] (g/* (real a) b))
(defmethod g/dot-product [::v/real ::complex] [a b] (g/* a (real b)))

(defmethod v/= [::complex ::complex] [a b] (c/equal? a b))
(defmethod v/= [::complex ::v/real] [a n]
  (and (g/zero? (imaginary a))
       (v/= (real a) n)))

(defmethod v/= [::v/real ::complex] [n a]
  (and (g/zero? (imaginary a))
       (v/= n (real a))))

(defmethod g/add [::complex ::complex] [a b] (c/add a b))
;; XXX consider making these methods in impl to avoid an allocation
(defmethod g/add [::complex ::v/real] [a n] (c/add a (complex n)))
(defmethod g/add [::v/real ::complex] [n a] (c/add (complex n) a))

(defmethod g/sub [::complex ::complex] [a b] (c/sub a b))
(defmethod g/sub [::complex ::v/real] [a n] (c/sub a (complex n)))
(defmethod g/sub [::v/real ::complex] [n a] (c/sub (complex n) a))

(defmethod g/mul [::complex ::complex] [a b] (c/mul a b))
(defmethod g/mul [::complex ::v/real] [a n] (c/mul a (complex n)))
(defmethod g/mul [::v/real ::complex] [n a] (c/mul (complex n) a))

(defmethod g/div [::complex ::complex] [a b] (c/div a b))
(defmethod g/div [::complex ::v/real] [a n] (c/div a (complex n)))
(defmethod g/div [::v/real ::complex] [n a] (c/div (complex n) a))

(defmethod g/invert [::complex] [z] (c/inverse z))
(defmethod g/negate [::complex] [z] (c/neg z))

(defmethod g/expt [::complex ::complex] [w z] (c/pow w z))
(defmethod g/expt [::complex ::v/real] [z r] (c/pow z (complex r)))
(defmethod g/expt [::v/real ::complex] [r z] (c/pow (complex r) z))
(defmethod g/square [::complex] [z] (g/* z z))
(defmethod g/cube [::complex] [z] (g/* z z z))

(defmethod g/abs [::complex] [z] (c/abs z))
(defmethod g/exp [::complex] [z] (c/exp z))
(defmethod g/log [::complex] [z] (c/log z))
(defmethod g/sqrt [::complex] [z] (c/sqrt z))

(defmethod g/sin [::complex] [z] (c/sin z))
(defmethod g/cos [::complex] [z] (c/cos z))
(defmethod g/tan [::complex] [z] (c/tan z))
(defmethod g/sec [::complex] [z] (c/sec z))
(defmethod g/csc [::complex] [z] (c/csc z))
(defmethod g/cot [::complex] [z] (c/cot z))

(defmethod g/asin [::complex] [z] (c/asin z))
(defmethod g/acos [::complex] [z] (c/acos z))
(defmethod g/atan [::complex] [z] (c/atan z))
(defmethod g/acsc [::complex] [z] (c/acsc z))
(defmethod g/asec [::complex] [z] (c/asec z))
(defmethod g/acot [::complex] [z] (c/acot z))

(defmethod g/asinh [::complex] [z] (c/asinh z))
(defmethod g/acosh [::complex] [z] (c/acosh z))
(defmethod g/atanh [::complex] [z] (c/atanh z))
(defmethod g/asech [::complex] [z] (c/asech z))
(defmethod g/acsch [::complex] [z] (c/acsch z))
(defmethod g/acoth [::complex] [z] (c/acoth z))

(defmethod g/floor [::complex] [z] (c/floor z))
(defmethod g/ceiling [::complex] [z] (c/ceil z))

(defmethod g/cosh [::complex] [z] (c/cosh z))
(defmethod g/sinh [::complex] [z] (c/sinh z))
(defmethod g/tanh [::complex] [z] (c/tanh z))
(defmethod g/sech [::complex] [z] (c/sech z))
(defmethod g/csch [::complex] [z] (c/csch z))
(defmethod g/coth [::complex] [z] (c/coth z))

(defmethod g/integer-part [::complex] [a]
  (let [re (g/integer-part (real a))
        im (g/integer-part (imaginary a))]
    (if (g/zero? im)
      re
      (complex re im))))

(defmethod g/fractional-part [::complex] [a]
  (let [re (g/fractional-part (real a))
        im (g/fractional-part (imaginary a))]
    (if (g/zero? im)
      re
      (complex re im))))

(defmethod g/negative? [::complex] [a]
  (and (g/zero? (imaginary a))
       (g/negative? (real a))))

(defmethod g/infinite? [::complex] [a]
  (or (g/infinite? (real a))
      (g/infinite? (imaginary a))))
