#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.matrix-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.clojure-test :as ct :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.function :as f]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.matrix :as m]
            [emmy.numsymb]
            [emmy.structure :as s]
            [emmy.structure-test :refer [<l|:inner:|r>]]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v]
            [same.core :refer [ish?]]))

(deftest value-protocol-tests
  (testing "zero?"
    (is (g/zero? (m/by-rows [0])))
    (is (g/zero?
         (m/by-rows [0 0 0]
                    [0 0 0]
                    [0 0 0])))
    (is (not (g/zero? (m/by-rows [1 2 3])))))

  (testing "zero-like"
    (is (= (m/by-rows [0 0 0])
           (g/zero-like (m/by-rows [1 2 3]))))
    (is (= (m/by-rows [0])
           (g/zero-like (m/by-rows [1]))))

    (is (= (m/by-rows [0.0] [0.0])
           (g/zero-like (m/by-rows [1.5] [2.5])))
        "zero-like preserves types"))

  (testing "one? vs identity?"
    (let [I10 (m/I 10)]
      (is (not (g/one? I10))
          "one? implies that multiplying by this acts as identity, which is only
          true for matrices of the correct shape (not for scalars!) so one? will
          always return false for a matrix.")

      (is (g/identity? I10)
          "identity? exists to check for an identity matrix.")))

  (testing "one-like"
    (is (= (m/I 3)
           (g/one-like
            (m/by-rows [1 2 3] [4 5 6] [7 8 9]))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/one-like (m/by-rows [1 2 3 4])))
        "one-like is only supported on square matrices."))

  (testing "identity-like"
    (is (= (m/I 3)
           (g/identity-like
            (m/by-rows [1 2 3] [4 5 6] [7 8 9]))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/identity-like (m/by-rows [1 2 3 4])))
        "identity-like is only supported on square matrices."))

  (testing "exact?"
    (is (g/exact? (m/by-rows [1] [2])))
    (is (not (g/exact? (m/by-rows [1.2] [3] [4]))))
    (is (not (g/exact? (m/by-rows [0] [0] [0.00001]))))
    (is (g/exact? (m/by-rows [0 1 (g// 3 2)]))))

  (testing "kind"
    (is (= ::m/row-matrix (v/kind (m/by-rows [1 2]))))
    (is (= ::m/column-matrix (v/kind (m/by-rows [1] [2]))))
    (is (= ::m/square-matrix (v/kind (m/by-rows [1 2] [3 4]))))
    (is (= ::m/matrix (v/kind (m/by-rows [1 2 3] [3 4 5])))))

  (testing "f/arity"
    (is (= [:exactly 2] (f/arity (m/by-rows [g/add g/sub])))
        "arity matches the arity of each element, if they all have the same
        arity.")

    (testing "f/arity narrows arity to the widest-compatible arity with each element."
      (is (= [:exactly 1] (f/arity (m/by-rows [g/sin]))))
      (is (= [:at-least 0] (f/arity (m/by-rows [g/+]))))
      (is (= [:exactly 1] (f/arity (m/by-rows [g/+ g/sin])))))

    (binding [f/*strict-arity-checks* true]
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (f/arity
                    (m/by-rows [g/add g/sin])))
          "If the matrix contains functions whose arities are totally
        incompatible, then `f/arity` will throw. `g/add` has arity [:exactly 2],
        `g/sin` has arity [:exactly 1]."))

    (binding [f/*strict-arity-checks* false]
      (is (= [:at-least 0]
             (f/arity
              (m/by-rows [g/add g/sin])))
          "If arity checks are disabled incompatible arities widen to the widest
          possible arity."))))

(deftest matrix-interfaces
  (testing "count"
    (is (= 1 (count (m/by-rows [1 2 3]))))
    (is (= 3 (count (m/by-rows [1] [2] [3])))))

  (testing "support take"
    (is (= [[1 2 3] [4 5 6]] (take 2 (m/by-rows [1 2 3] [4 5 6] [7 8 9])))))

  (testing "support drop"
    (is (= [[4 5 6] [7 8 9]] (drop 1 (m/by-rows [1 2 3] [4 5 6] [7 8 9])))))

  (testing "can be mapped"
    (is (= [2 2] (map count (m/by-rows [1 2] [3 4])))))

  (testing "a structure can produce a seq"
    (is (= [[1 2] [3 4]] (seq (m/by-rows [1 2] [3 4])))))

  (testing "seqable"
    (is (= [1 2 3 4] (into [] cat (m/by-rows [1 2] [3 4])))))

  (let [M (m/by-rows [1 2] [3 4])]
    (testing "a structure has a nth element (ILookup)"
      (is (= [1 2] (nth M 0)))
      (is (= [3 4] (nth M 1)))
      (is (= 2 (get-in M [0 1])))
      (is (thrown? #?(:clj IndexOutOfBoundsException :cljs js/Error)
                   (nth M 4)))))

  (testing "IFn"
    (is (= (m/by-rows [6 9] [1 0])
           ((m/by-rows [+ *] [/ -]) 3 3))))

  (testing "equality"
    (is (= [[1 2] [3 4]] (m/by-rows [1 2] [3 4])))
    (is (= (m/by-rows [1 2] [3 4])
           (m/by-rows [1 2] [3 4])))

    (testing "scalar equality on either side"
      (is (v/= 'x (m/make-diagonal 10 'x)))
      (is (v/= (m/make-diagonal 10 'x) 'x)))))

(deftest matrix-basics
  (checking "square? is false for numbers" 100
            [x sg/any-integral]
            (is (not (m/square? x))))

  (checking "square? is true for squares" 100
            [m (gen/let [n (gen/choose 1 10)]
                 (sg/square-matrix n))]
            (is (m/square? m)))

  (testing "some"
    (is (m/some even? (m/by-rows [1 2] [1 3])))
    (is (not (m/some even? (m/by-rows [1 5] [1 3])))))

  (checking "fmap-indexed" 100
            [M (gen/let [rows (gen/choose 1 5)
                         cols (gen/choose 1 5)]
                 (sg/matrix rows cols))]
            (is (= M (m/fmap-indexed (fn [_ i j]
                                       (get-in M [i j]))
                                     M))
                "fmap-indexed roundtrips"))

  (testing "*-like maintains type"
    (let [M (m/by-rows [identity g/cube]
                       [g/square identity])]
      (is (= (m/by-rows [2 8]
                        [4 2])
             (M 2)))

      (is (= (m/I 2)
             ((g/identity-like M) 2))
          "identity-like on a matrix of functions returns a new matrix of fns.")

      (is (= (m/I 2)
             ((g/one-like M) 2))
          "one-like on a matrix of functions returns a new matrix of fns.")

      (is (= (m/make-zero 2)
             ((g/zero-like M) 2))
          "one-like on a matrix of functions returns a new matrix of fns.")))

  (checking "by-rows == (comp transpose by-cols), vice versas" 100
            [vs (-> (gen/sized #(gen/vector sg/real %))
                    (gen/vector 1 20))]
            (is (= (m/by-rows vs)
                   (g/transpose (m/by-cols vs))))
            (is (= (m/by-cols vs)
                   (g/transpose (m/by-rows vs)))))

  (checking "row*==row, column*==column" 100
            [vs (gen/vector sg/real 1 20)]
            (is (= (m/row* vs)
                   (apply m/row vs)))

            (is (= (m/column* vs)
                   (apply m/column vs))))

  (checking "by-rows == row" 100
            [vs (gen/vector sg/real 1 20)]
            (let [row (m/row* vs)]
              (is (= (m/by-rows vs) row))
              (is (m/row? row))))

  (checking "by-cols == column" 100
            [vs (gen/vector sg/real 1 20)]
            (let [col (m/column* vs)]
              (is (= (m/by-cols vs) col))
              (is (m/column? col))))

  (testing "malformed by-cols, by-rows"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (m/by-cols [1] [2 3]))
        "counts aren't equal")

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (m/by-rows [1] [2 3]))
        "counts aren't equal"))

  (checking "with-substituted-row works" 100
            [[m new-row] (gen/let [n (gen/choose 1 10)]
                           (gen/tuple (sg/square-matrix n)
                                      (gen/vector sg/real n)))]
            (doseq [i (range (m/num-rows m))]
              (is (= new-row
                     (-> (m/with-substituted-row m i new-row)
                         (m/nth-row i)))
                  "swapping in a row should swap the row!")))

  (checking "submatrix matches without" 100
            [M (gen/let [n (gen/choose 1 10)]
                 (sg/square-matrix n))]
            (is (= (m/submatrix
                    M
                    1 (dec (m/num-rows M))
                    1 (dec (m/num-cols M)))
                   (m/without M 0 0))))

  (testing "submatrix"
    (let [M (m/by-rows [1 2 3]
                       [4 5 6]
                       [7 8 9])
          SM (s/down (s/up 1 4 7)
                     (s/up 2 5 8)
                     (s/up 3 6 9))]
      (is (= (m/by-rows [1 2]
                        [4 5])
             (m/submatrix M 0 1 0 1)
             (m/submatrix SM 0 1 0 1)))))

  (checking "make-zero" 100 [m (gen/choose 0 10)
                             n (gen/choose 0 10)]
            (let [M (m/make-zero m n)]
              (is (g/zero? M))
              (is (= m (m/num-rows M)))
              (is (= n (m/num-cols M)))))

  (testing "make-diagonal"
    (is (= (m/I 10) (m/make-diagonal 10 1))))

  (testing "symmetric?"
    (is (m/symmetric? (m/by-rows [1 2] [2 1])))
    (is (not (m/symmetric? (m/by-rows [1 2] [-2 1])))))

  (testing "empty"
    (is (g/zero? (empty (m/by-rows [1 2] [2 1])))))

  (checking "make-diagonal" 100
            [vs (gen/vector sg/real 1 20)]
            (let [M (m/make-diagonal vs)]
              (is (m/diagonal? M))

              (is (= (g/dimension M)
                     (g/dimension vs)))

              (is (= vs (m/diagonal M)))
              (is (= vs (m/diagonal M)))))

  (checking "make-diagonal, g/identity? g/one?" 100
            [v (gen/vector (gen/return 1) 1 20)]
            (let [M (m/make-diagonal v)]
              (is (g/identity? M))
              (is (not (g/identity? (g/* 2 M))))

              (is (not (g/one? M))
                  "matrices don't act as one; they need to maintain their
                  structure when multiplied by constants.")

              (is (not (g/one? (g/* 2 M))))))

  (let [M (m/by-rows (list 1 2 3)
                     (list 4 5 6))
        A (m/by-rows [11 12 13]
                     [21 22 23]
                     [31 32 33])
        v (m/column 7 8 9)]
    (is (= ::m/matrix (v/kind M)))
    (is (= '(matrix-by-rows
             [1 2 3]
             [4 5 6])
           (g/freeze M)))
    (is (= (m/by-rows [1 4] [2 5] [3 6]) (g/transpose M)))
    (is (= (m/by-rows [0 0 0] [0 0 0]) (g/zero-like M)))
    (is (= (m/by-rows [1 0 0] [0 1 0] [0 0 1]) (g/one-like A)))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (g/one-like M)))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (m/by-rows [1 2 3] [4 5])))
    (is (thrown? #?(:clj AssertionError :cljs js/Error) (m/by-rows)))
    (is (= 5 (m/get-in M [1 1])))
    (is (= 3 (m/get-in M [0 2])))
    (is (= [4 5 6] (m/get-in M [1])))
    (is (= 8 (m/get-in v [1])))
    (is (= (m/by-rows [2 3 4]
                      [5 6 7]) (m/fmap inc M)))
    (is (= (m/by-rows [22 23] [32 33])
           (m/without A 0 0)))
    (is (= (m/by-rows [21 23] [31 33])
           (m/without A 0 1)))
    (is (= (m/by-rows [21 22] [31 32])
           (m/without A 0 2)))
    (is (= (m/by-rows [12 13] [32 33])
           (m/without A 1 0)))
    (is (= (m/by-rows [11 13] [31 33])
           (m/without A 1 1)))
    (is (= (m/by-rows [11 12] [31 32])
           (m/without A 1 2)))
    (is (= (m/by-rows [12 13] [22 23])
           (m/without A 2 0)))
    (is (= (m/by-rows [11 13] [21 23])
           (m/without A 2 1)))
    (is (= (m/by-rows [11 12] [21 22])
           (m/without A 2 2)))
    (is (= (s/up 11 21 31) (m/nth-col A 0)))
    (is (= (s/up 12 22 32) (m/nth-col A 1)))
    (is (= (s/up 13 23 33) (m/nth-col A 2)))
    (is (thrown? #?(:clj IndexOutOfBoundsException :cljs js/Error) (m/nth-col A 3)))
    (let [matrix (m/by-rows [-2 2 -3]
                            [-1 1 3]
                            [2 0 -1])]
      (is (= 18 (g/determinant matrix)))
      (is (= -2 (g/trace matrix))))
    (is (= (m/by-rows [7 -3 -3]
                      [-1 1 0]
                      [-1 0 1])
           (m/invert (m/by-rows [1 3 3]
                                [1 4 3]
                                [1 3 4]))))
    (is (= (m/by-rows [0 1 2]
                      [1 2 3]
                      [2 3 4])
           (m/generate 3 3 +)))

    (is (= (m/generate 3 +)
           (m/generate 3 3 +))
        "Only filling in one dimension for generate returns a square matrix.")

    (is (not (m/diagonal?
              (m/generate 3 3 +))))

    (is (g/zero? (m/by-rows [0 0]
                            [0 0])))))

(deftest literal-matrix-creation
  (testing "literal-matrix"
    (let [M (m/literal-matrix 'x 3)]
      (is (m/matrix? M))
      (is (= M (m/by-rows ['x_0↑0 'x_1↑0 'x_2↑0]
                          ['x_0↑1 'x_1↑1 'x_2↑1]
                          ['x_0↑2 'x_1↑2 'x_2↑2]))
          "A literal matrix is a matrix populated by symbols with index refs
          baked in."))

    (is (= (m/literal-column-matrix 'x 3)
           (m/by-cols ['x↑0 'x↑1 'x↑2]))
        "literal column")

    (is (= (m/literal-row-matrix 'x 3)
           (m/by-rows ['x_0 'x_1 'x_2]))
        "literal row"))

  (checking "structure prototype matches matrix literal" 100
            [[sym M] (gen/let [sym    sg/symbol
                               rows  (gen/choose 1 10)
                               cols  (gen/choose 1 10)]
                       [sym (m/literal-matrix sym rows cols)])]
            (is (= (m/->structure M)
                   (s/structure->prototype
                    sym
                    (m/->structure M)))
                "literal matrices contain symbols that store their orientation
                properly. [[s/structure->prototype]] matches this behavior.")))

(deftest matrix-generic-operations
  (let [M (m/by-rows (list 1 2 3)
                     (list 4 5 6))]
    (is (= (m/by-rows (list -1 -2 -3)
                      (list -4 -5 -6)) (g/negate M)))
    (is (= (s/down (s/up -1 -2)
                   (s/up -3 -4))
           (m/two-tensor-operation
            (s/down (s/up 1 2) (s/up 3 4))
            #(m/fmap - %)))))

  (checking "dimension of row matrix is correct" 100
            [[r M] (gen/let [r (gen/fmap inc gen/nat)]
                     (gen/tuple (gen/return r)
                                (sg/matrix r 1)))]
            (is (= r (g/dimension M))))

  (checking "dimension of column matrix is correct" 100
            [[c M] (gen/let [c (gen/fmap inc gen/nat)]
                     (gen/tuple (gen/return c)
                                (sg/matrix 1 c)))]
            (is (= c (g/dimension M)))))

(deftest structure
  (checking "M^tM is always square" 100
            [M (gen/let [m (gen/choose 1 5)
                         n (gen/choose 1 5)]
                 (sg/matrix m n))]
            (is (m/square?
                 (g/* (g/transpose M) M))))

  (testing "s:transpose with rectangular"
    (is (= (s/down (s/up 'c 'e 'g)
                   (s/up 'd 'f 'h))
           (m/s:transpose-orientation
            (s/up (s/down 'c 'd)
                  (s/down 'e 'f)
                  (s/down 'g 'h))))
        "up of downs")

    (is (= (s/up (s/down 'c 'e 'g)
                 (s/down 'd 'f 'h))
           (m/s:transpose-orientation
            (s/down (s/up 'c 'd)
                    (s/up 'e 'f)
                    (s/up 'g 'h))))
        "down of ups")

    (is (= (s/down (s/down 'c 'e 'g)
                   (s/down 'd 'f 'h))
           (m/s:transpose-orientation
            (s/down (s/down 'c 'd)
                    (s/down 'e 'f)
                    (s/down 'g 'h))))
        "down of downs")

    (is (= (s/up (s/up 'c 'e 'g)
                 (s/up 'd 'f 'h))
           (m/s:transpose-orientation
            (s/up (s/up 'c 'd)
                  (s/up 'e 'f)
                  (s/up 'g 'h))))
        "up of ups"))

  (checking "(s:transpose <l|, inner, |r>)==(s/transpose-outer inner)"
            100 [[l inner r] (gen/let [rows (gen/choose 1 5)
                                       cols (gen/choose 1 5)]
                               (<l|:inner:|r> rows cols))]
            (is (g/zero?
                 (g/- (m/s:transpose l inner r)
                      (s/transpose-outer inner)))))

  ;; this binding is required because `s:transpose` is meant to return a scalar;
  ;; here, we're testing the relation in a case that returns an uncollapsed
  ;; empty structure.
  (binding [m/*careful-conversion* false]
    (checking "(s:transpose <l|, inner, |r>)==(s/transpose-outer inner) with
              either side empty returns an empty structure"
              100 [[l inner r] (gen/let [rows (gen/choose 0 5)
                                         cols (gen/choose 0 5)]
                                 (<l|:inner:|r> rows cols))]
              (if (empty? r)
                (testing "in this case, the right side is fully collapsed and
                empty and the left side contains a single empty structure."
                  (do (is (g/zero? (m/s:transpose l inner r)))
                      (is (empty? (s/transpose-outer inner)))))
                (is (g/zero?
                     (g/- (m/s:transpose l inner r)
                          (s/transpose-outer inner)))
                    "left side empty generates a compatible, zero entry"))))

  (letfn [(transpose-test [left-multiplier thing right-multiplier]
            ;; Should produce numerical zero and a zero structure
            [(g/- (g/* left-multiplier (g/* thing right-multiplier))
                  (g/* (g/* (m/s:transpose-orientation thing)
                            left-multiplier)
                       right-multiplier))
             (g/- (m/s:transpose left-multiplier thing right-multiplier)
                  (m/s:transpose thing right-multiplier))])]

    ;; down down
    (is (v/= [0 (s/down (s/down 0 0 0) (s/down 0 0 0))]
             (g/simplify
              (transpose-test
               (s/up 'a 'b)
               (s/down (s/down 'c 'd) (s/down 'e 'f) (s/down 'g 'h))
               (s/up 'i 'j 'k))))
        "down, down")

    ;; up up
    (is (v/= [0 (s/up (s/up 0 0 0) (s/up 0 0 0))]
             (g/simplify
              (transpose-test
               (s/down 'a 'b)
               (s/up (s/up 'c 'd) (s/up 'e 'f) (s/up 'g 'h))
               (s/down 'i 'j 'k))))
        "up, up")

    (is (v/= [0 (s/down (s/up 0 0 0) (s/up 0 0 0))]
             (g/simplify
              (transpose-test
               (s/up 'a 'b)
               (s/up (s/down 'c 'd) (s/down 'e 'f) (s/down 'g 'h))
               (s/down 'i 'j 'k))))
        "up, down")

    (is (v/= [0 (s/up (s/down 0 0 0) (s/down 0 0 0))]
             (g/simplify
              (transpose-test
               (s/down 'a 'b)
               (s/down (s/up 'c 'd) (s/up 'e 'f) (s/up 'g 'h))
               (s/up 'i 'j 'k))))))

  (let [A (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))
        M (m/by-rows [1 2 3]
                     [4 5 6])]
    (is (= 8 (g/dimension A))
        "The dimension of a structure is the total number of entries,
        disregarding structure.")

    (testing "->structure"
      (is (= (s/down (s/up 1 4)
                     (s/up 2 5)
                     (s/up 3 6))
             (m/->structure M)
             (m/->structure M ::s/down ::s/up true))
          "default behavior.")

      (is (= (s/up (s/down 1 2 3)
                   (s/down 4 5 6))
             (m/->structure M ::s/up ::s/down false)))
      (is (= (s/up (s/down 1 4)
                   (s/down 2 5)
                   (s/down 3 6))
             (m/->structure M ::s/up ::s/down true)))
      (is (= (s/up (s/down 1 2 3)
                   (s/down 4 5 6))
             (m/->structure M ::s/up ::s/down false)))))

  (checking "->structure laws" 100
            [outer      sg/orientation
             inner      sg/orientation
             transpose? gen/boolean
             M          (gen/let [rows (gen/choose 1 5)
                                  cols (gen/choose 1 5)]
                          (sg/matrix rows cols))]
            (is (= (m/->structure M outer inner transpose?)
                   (s/transpose-outer
                    (m/->structure M inner outer (not transpose?))))
                "s/transpose-outer flips the inner and outer structures.")

            (let [S (m/->structure M outer inner transpose?)]
              (is (= outer (s/orientation S))
                  "Does the outer structure have the correct orientation?")

              (is (every? (comp #{inner} s/orientation) S)
                  "Does every inner structure have the required orientation?")

              (if transpose?
                (is (= (map seq (g/transpose M))
                       (map seq S))
                    "the structure contains the matrix columns.")
                (is (= (map seq M)
                       (map seq S))
                    "the structure contains the matrix rows."))))

  (checking "seq-> is id for vectors without matrices" 100
            [xs (gen/vector sg/real)]
            (is (= (s/vector->up xs)
                   (m/seq-> xs))))

  (checking "seq-> converts internal matrices" 100
            [[l M r] (gen/let [rows (gen/choose 1 5)
                               cols (gen/choose 1 5)]
                       (gen/tuple (gen/vector sg/real)
                                  (sg/matrix rows cols)
                                  (gen/vector sg/real)))]
            (is (= (s/up*
                    (concat l [(m/->structure M)] r))
                   (m/seq-> (concat l [M] r)))))

  (testing "two-tensor->"
    (doseq [[S M] [[(s/down (s/up 11 12) (s/up 21 22))
                    (m/by-rows [11 21] [12 22])]
                   [(s/up (s/down 11 12) (s/down 21 22))
                    (m/by-rows [11 12] [21 22])]
                   [(s/up (s/up 11 12) (s/up 21 22))
                    (m/by-rows [11 21] [12 22])]
                   [(s/down (s/down 11 12) (s/down 21 22))
                    (m/by-rows [11 12] [21 22])]]]
      (m/two-tensor-> S (fn [m ->s]
                          (is (= M m))
                          (is (= S (->s m)))))))

  (testing "structure as matrix"
    (let [A (s/up (s/up 1 2)
                  (s/up 3 4))
          C (s/down (s/up 1 2 3)
                    (s/up 0 4 5)
                    (s/up 1 0 6))
          D (s/up (s/down 3))
          F (s/down (s/up 1 2)
                    (s/up 3 4))
          G (s/down (s/up 4 0 0 0)
                    (s/up 0 0 2 0)
                    (s/up 0 1 2 0)
                    (s/up 1 0 0 1))
          cof #(m/two-tensor-operation % m/cofactors)
          det #(m/two-tensor-> % (fn [m _] (g/determinant m)))]

      (testing "cofactors"
        (is (= (s/up (s/up 4 -3) (s/up -2 1)) (cof A)))
        (is (= (s/down (s/up 24 5 -4) (s/up -12 3 2) (s/up -2 -5 4)) (cof C)))
        (is (= (s/up (s/down 3)) (cof D))))

      (testing "determinant"
        (is (= -2 (det A)))
        (is (= 22 (det C)))
        (is (= 3 (det D)))
        (is (= -2 (det F)))
        (is (= -8 (det G))))

      (testing "trace"
        (is (= 5 (g/trace A)))
        (is (= 11 (g/trace C)))
        (is (= 3 (g/trace D)))
        (is (= 5 (g/trace F)))
        (is (= 7 (g/trace G))))))

  (testing "s->m->s"
    (is (= (m/by-rows [1 2] [2 3])
           (m/s->m (s/down 'x 'y)
                   (s/down (s/up 1 2) (s/up 2 3))
                   (s/up 'x 'y))))

    (is (= (m/by-rows [-3 2] [2 -1])
           (m/invert (m/s->m (s/down 'x 'y)
                             (s/down (s/up 1 2) (s/up 2 3))
                             (s/up 'x 'y)))))

    (is (= (s/up (s/down -3 2) (s/down 2 -1))
           (m/m->s
            (s/compatible-shape (s/down 1 2))
            (m/invert (m/s->m (s/down 'x 'y)
                              (s/down (s/up 1 2) (s/up 2 3))
                              (s/up 'x 'y)))
            (s/compatible-shape (s/up 2 3)))))

    (is (= (s/down (s/up -3 2) (s/up 2 -1))
           (m/s:inverse (s/down 'x 'y)
                        (s/down (s/up 1 2) (s/up 2 3))
                        (s/up 'x 'y))))))

(deftest matrix-mul-div
  (let [M (m/by-rows [1 2 3]
                     [2 3 4])
        S (m/by-rows [3 4]
                     [4 5]
                     [5 6])]
    (is (= (m/by-rows [26 32] [38 47]) (g/* M S))))

  (let [M (m/by-rows '[a b] '[c d])
        d (s/down 'x 'y)
        u (s/up 'x 'y)]
    (testing "mul"
      (is (= (s/up (g/+ (g/* 'a 'x) (g/* 'b 'y))
                   (g/+ (g/* 'c 'x) (g/* 'd 'y)))
             (g/* M u)))
      (is (= (s/down (g/+ (g/* 'x 'a) (g/* 'y 'c))
                     (g/+ (g/* 'x 'b) (g/* 'y 'd)))
             (g/* d M)))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (g/* u M)))

      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (g/* M d)))

      (testing "matrix, scalar mult"
        (is (= (m/by-rows [(g/* 'x 'a) (g/* 'x 'b)]
                          [(g/* 'x 'c) (g/* 'x 'd)])
               (g/* 'x M)))

        (is (= (m/by-rows [(g/* 'a 'x) (g/* 'b 'x)]
                          [(g/* 'c 'x) (g/* 'd 'x)])
               (g/* M 'x)))))

    (testing "matrix, scalar division"
      (is (= (g/* 'x (g// M))
             (g// 'x M))
          "matrix on right, matrix inverts")

      (is (= (m/by-rows [(g/* 'a (g// 'x)) (g/* 'b (g// 'x))]
                        [(g/* 'c (g// 'x)) (g/* 'd (g// 'x))])
             (g// M 'x))))

    (testing "matrix, scalar addition acts on the diagonal"
      (is (= (m/by-rows [(g/+ 'x 'a) 'b]
                        ['c (g/+ 'x 'd)])
             (g/+ 'x M)))

      (is (= (m/by-rows [(g/+ 'a 'x) 'b]
                        ['c (g/+ 'd 'x)])
             (g/+ M 'x))))

    (testing "matrix, scalar subtraction acts on the diagonal"
      (is (= (m/by-rows [(g/- 'x 'a) (g/- 'b)]
                        [(g/- 'c) (g/- 'x 'd)])
             (g/- 'x M))
          "matrix on right, matrix negates")

      (is (= (m/by-rows [(g/- 'a 'x) 'b]
                        ['c (g/- 'd 'x)])
             (g/- M 'x))))))

(deftest square-tests
  (checking "square matrices return true for square?"
            100 [matrix (gen/bind (gen/choose 1 10)
                                  sg/square-matrix)]
            (is (m/square? matrix))

            (is (= matrix
                   (m/by-rows*
                    (for [i (range (g/dimension matrix))]
                      (seq (m/nth-row matrix i)))))
                "fusing rows back together should yield the original.")

            (is (= matrix
                   (g/transpose
                    (m/by-rows*
                     (for [i (range (g/dimension matrix))]
                       (seq (m/nth-col matrix i))))))
                "fusing columns back together, then transposing, should yield
                the original.")

            (is (= (reduce g/+ (m/diagonal matrix))
                   (g/trace matrix))
                "the trace is the sum of the diagonal.")

            (if (= 1 (g/dimension matrix))
              (do (is (m/column? matrix))
                  (is (m/row? matrix)))
              (do (is (not (m/column? matrix)))
                  (is (not (m/row? matrix)))))))

(deftest complex-entry-tests
  (checking "g/conjugate with real entries acts as id" 100
            [a sg/real b sg/real
             c sg/real d sg/real]
            (is (= (m/by-rows [a b] [c d])
                   (g/conjugate
                    (m/by-rows [a b] [c d])))))

  (checking "g/conjugate conjugates entries" 100
            [a sg/complex b sg/complex
             c sg/complex d sg/complex]
            (is (= (m/by-rows [(g/conjugate a)
                               (g/conjugate b)]
                              [(g/conjugate c)
                               (g/conjugate d)])
                   (g/conjugate
                    (m/by-rows [a b] [c d])))))

  (checking "g/make-rectangular, g/make-polar" 100
            [a sg/real b sg/real
             c sg/real d sg/real]
            (let [M (m/by-rows [a b] [c d])]
              (is (= (m/fmap #(g/make-rectangular % %) M)
                     (g/make-rectangular M M)))

              (is (= (m/fmap #(g/make-polar % %) M)
                     (g/make-polar M M)))))

  (testing "incompatible shapes throw for g/make-*"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/make-rectangular (m/by-rows [1 2 3])
                                     (m/by-rows [1 2]))))

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/make-polar (m/by-rows [1 2 3])
                               (m/by-rows [1 2])))))

  (checking "g/real-part, g/imag-part pass through to values" 100
            [a sg/complex b sg/complex
             c sg/complex d sg/complex]
            (let [M (m/by-rows [a b] [c d])]
              (is (= (m/fmap g/real-part M)
                     (g/real-part M)))

              (is (= (m/fmap g/imag-part M)
                     (g/imag-part M)))

              (is (= M (g/make-rectangular
                        (g/real-part M)
                        (g/imag-part M)))
                  "g/make-rectangular rebuilds the original matrix from its
                  parts."))))

(defspec p+q=q+p
  (gen/let [n (gen/choose 1 10)]
    (prop/for-all [p (sg/square-matrix n)
                   q (sg/square-matrix n)]
                  (= (g/+ p q) (g/+ q p)))))

(defspec make-rectangular-imag-real-round-trip
  (gen/let [n (gen/choose 1 10)]
    (prop/for-all [M (sg/square-matrix n sg/complex)]
                  (= M (g/make-rectangular
                        (g/real-part M)
                        (g/imag-part M))))))

(defspec pq*r=p*qr
  (gen/let [n (gen/choose 1 10)]
    (prop/for-all [p (sg/square-matrix n)
                   q (sg/square-matrix n)
                   r (sg/square-matrix n)]
                  (= (g/* (g/* p q) r) (g/* p (g/* q r))))))

(defspec a*ainv=i
  (gen/let [n (gen/choose 1 5)]
    (prop/for-all [A (sg/square-matrix n)]
                  (or (g/zero? (g/determinant A))
                      (= (m/I n)
                         (g/* (g/invert A) A)
                         (g/* A (g/invert A)))))))

(defn naive-determinant [m]
  {:pre [(m/square? m)]}
  (condp = (m/num-rows m)
    0 m
    1 (get-in m [0 0])
    2 (let [[[a b] [c d]] m]
        (g/- (g/* a d)
             (g/* b c)))
    (ua/generic-sum
     (map g/*
          (cycle [1 -1])
          (nth m 0)
          (for [i (range (m/num-rows m))]
            (naive-determinant (m/without m 0 i)))))))

(deftest naive-vs-determinant-tests
  (let [M (m/literal-matrix 'x 6)]
    (is (g/zero?
         (g/simplify
          (g/- (m/determinant M)
               (naive-determinant M))))
        "the more complicated determinant routine in the matrix namespace
        matches the naive implementation above.")))

(defn naive-invert
  "Returns the inverse of the supplied square matrix `m`."
  [m]
  {:pre [(m/square? m)]}
  (let [r (m/num-rows m)]
    (condp = r
      0 m
      1 (m/->Matrix 1 1 [[(g/invert (get-in m [0 0]))]])
      (let [C (m/cofactors m)
            Δ (ua/generic-sum (map g/* (nth m 0) (nth C 0)))]
        (m/fmap #(g/divide % Δ)
                (m/transpose C))))))

(deftest naive-vs-invert-tests
  (let [M (m/literal-matrix 'x 4)]
    (is (g/zero?
         (g/simplify
          (g/- (m/invert M)
               (naive-invert M))))
        "the more complicated determinant routine in the matrix namespace
        matches the naive implementation above.")))

(deftest struct-matrix-conversion-tests
  (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
               (m/structure->matrix (s/up)))
      "without ANY inner structure we can't proceed.")

  (testing "almost-empty tensors"
    (is (= (m/literal-matrix 'x 1 0)
           (m/structure->matrix
            (s/up (s/down)))))

    (is (= (m/literal-matrix 'x 0 1)
           (m/structure->matrix
            (s/down (s/up))))))

  (is (= (into [] (s/literal-up 'x 4))
         (m/column-matrix->vector
          (m/literal-column-matrix 'x 4))))

  (is (= (into [] (s/literal-down 'x 4))
         (m/row-matrix->vector
          (m/literal-row-matrix 'x 4))))

  (is (= (m/by-cols ['a 'b] ['c 'd])
         (m/structure->matrix
          (s/down (s/up 'a 'b) (s/up 'c 'd)))
         (m/structure->matrix
          (s/up (s/up 'a 'b) (s/up 'c 'd))))
      "outer structure is ignored, and inner up signifies columns.")

  (is (= (m/by-rows ['a 'b] ['c 'd])
         (m/structure->matrix
          (s/up (s/down 'a 'b) (s/down 'c 'd)))
         (m/structure->matrix
          (s/down (s/down 'a 'b) (s/down 'c 'd))))
      "outer structure is ignored, and inner down signifies rows."))

(deftest product-tests
  ;; TODO - fix the case where we have a 1x1 matrix that can't reach these
  ;; methods, since it gets dispatched as square.
  (checking "dot-product" 100 [s (sg/up1 sg/real 2 10)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (= (g/dot-product s s)
                     (g/dot-product col col)))

              (is (= (g/dot-product s s)
                     (g/dot-product row row)))))

  (checking "inner-product" 100 [s (sg/up1 sg/complex 2 10)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (= (g/inner-product s s)
                     (g/inner-product col col)))

              (is (= (g/inner-product s s)
                     (g/inner-product row row)))))

  (checking "cross-product returns matrices" 100
            [s (sg/up1 sg/complex 3)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (ish? (m/up->column-matrix
                         (g/cross-product s s))
                        (g/cross-product col col)))

              (is (ish? (g/cross-product col col)
                        (g/transpose
                         (g/cross-product row row))))))

  (checking "outer-product" 100 [s (sg/up1 sg/complex 2 10)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (= (g/outer-product s s)
                     (g/outer-product col row))))))

(deftest division-tests
  (let [M (m/by-rows [1 2] [3 4])]
    (is (= (s/up 1 0)
           (g// (s/up 1 3) M))
        "up/M")

    (is (= (g// (s/down 5 -1) 2)
           (g// (s/down 1 3) M))
        "down/M")

    (is (= (m/column 1 0)
           (g// (m/column 1 3) M))
        "up/M matches column/M")

    (is (= (g// (m/row 5 -1) 2)
           (g// (m/row 1 3) M))
        "down/M matches row/M")))

(deftest matrices-from-structure
  (let [A (s/up (s/up 1 2)
                (s/up 3 4))
        C (s/down (s/up 1 2 3)
                  (s/up 0 4 5)
                  (s/up 1 0 6))
        D (s/up (s/down 3))
        F (s/down (s/up 1 2)
                  (s/up 3 4))
        G (s/down (s/up 4 0 0 0)
                  (s/up 0 0 2 0)
                  (s/up 0 1 2 0)
                  (s/up 1 0 0 1))]

    (checking "structure division returns a structure compatible for contraction
    with the denominator." 100
              [s (sg/up1 sg/any-integral 5)
               x (gen/fmap (fn [x]
                             (if (g/zero? x) 1 x))
                           sg/any-integral)]
              (is (= s (g/* x (g// s x)))))

    (testing "structural division unit test"
      (is (= (s/down
              (s/up #emmy/ratio 1/2
                    1
                    #emmy/ratio 3/2))
             (g/div (s/up 1 2 3)
                    (s/up 2)))
          "one compatible for contraction layer added")

      (testing "divide-by-structure from structs.scm"
        (let [a (s/up (s/down 'a 'b) (s/down 'c 'd))
              b (s/down 'e 'f)
              c (g/* a b)]
          (is (= (s/down 0 0)
                 (g/simplify
                  (g/- b (m/s:divide-by-structure c a))))
              "up-down, down"))

        (let [a (s/down (s/up 'a 'b) (s/up 'c 'd))
              b (s/up 'e 'f)
              c (g/* a b)]
          (is (= (s/up 0 0)
                 (g/simplify
                  (g/- b (m/s:divide-by-structure c a))))
              "down-up, up"))

        (testing "from GJS: 'the following are strange results', not sure why!"
          (let [a (s/down (s/down 'a 'b) (s/down 'c 'd))
                b (s/down 'e 'f)]
            (is (= (s/down 'e 'f)
                   (g/simplify
                    (g/* a (m/s:divide-by-structure b a))))))

          (let [a (s/up (s/up 'a 'b) (s/up 'c 'd))
                b (s/up 'e 'f)]
            (is (= (s/up 'e 'f)
                   (g/simplify
                    (g/* a (m/s:divide-by-structure b a))))))))

      (is (= (s/up 1 2 3)
             (g/* (s/up 2)
                  (g/div (s/up 1 2 3)
                         (s/up 2))))
          "s = x*(s/x)")

      (testing "structure multiplication, division are associative. Test by
      equation solving. All answers should be <0 0>."
        (let [a (s/down (s/down 'a 'b) (s/down 'c 'd))
              b (s/up 'e 'f)
              c (g/* a b)]
          (is (= (s/up 0 0)
                 (g/simplify
                  (g/- b (g/* (m/s:inverse a b) c))))
              "down of downs"))

        (let [a (s/up (s/up 'a 'b) (s/up 'c 'd))
              b (s/down 'e 'f)
              c (g/* a b)]
          (is (= (s/down 0 0)
                 (g/simplify
                  (g/- b (g/* (m/s:inverse a b) c))))
              "up of ups"))

        (let [a (s/up (s/down 'a 'b) (s/down 'c 'd))
              b (s/down 'e 'f)
              c (g/* a b)]
          (is (= (s/down 0 0)
                 (g/simplify
                  (g/- b (g/* (m/s:inverse a b) c))))
              "up of downs"))

        (let [a (s/down (s/up 'a 'b) (s/up 'c 'd))
              b (s/up 'e 'f)
              c (g/* a b)]
          (is (= (s/up 0 0)
                 (g/simplify
                  (g/- b (g/* (m/s:inverse a b) c))))
              "down of ups"))

        (let [a (s/up (s/down 'a 'b)
                      (s/down 'c 'd))
              b (s/down 'e 'f)]
          (is (= (s/down 0 0)
                 (g/simplify
                  (g/- b (g// (g/* a b) a))))
              "(ab)/a == b with up-down, down")

          (is (= (s/down 0 0)
                 (g/simplify
                  (g/- b (g/* a (g// b a)))))
              "a(b/a) == b with up-down, down")

          (is (= b (g/simplify
                    (g/solve-linear a (g/* a b))))
              "solve-linear works properly")

          (is (= (s/opposite b)
                 (-> (g/solve-linear-right (s/opposite b) a)
                     (g/* a)
                     (g/simplify)))
              "solve-linear-right contract"))

        (let [a (s/down (s/up 'a 'b)
                        (s/up 'c 'd))
              b (s/up 'e 'f)]
          (is (= (s/up 0 0)
                 (g/simplify
                  (g/- b (g// (g/* a b) a))))
              "(ab)/a == b with down-up, up")

          (is (= (s/up 0 0)
                 (g/simplify
                  (g/- b (g/* a (g// b a)))))
              "a(b/a) == b with down-up, up")

          (is (= b (g/simplify
                    (g/solve-linear a (g/* a b))))
              "solve-linear works properly")

          (is (= (s/opposite b)
                 (-> (g/solve-linear-right (s/opposite b) a)
                     (g/* a)
                     (g/simplify)))
              "solve-linear-right contract"))))

    (testing "inverse"
      (is (= (s/up (s/down 1))
             (g/* D (g/divide D))))

      (is (= (s/down (s/up 1 0)
                     (s/up 0 1))
             (g/* F (g/divide F))))

      (is (= (s/down (s/up 1 0)
                     (s/up 0 1))
             (g/* (g/divide F) F)))

      (is (= (s/down (s/up 1 0 0 0)
                     (s/up 0 1 0 0)
                     (s/up 0 0 1 0)
                     (s/up 0 0 0 1))
             (g/* (g/divide G) G))))

    (testing "ratio literals"
      (is (= (s/down (s/down -2 1)
                     (s/down #emmy/ratio 3/2 #emmy/ratio -1/2))
             (g/divide A)))
      (is (= #emmy/ratio 5/2 (g/* A (g/divide A))))
      (is (= #emmy/ratio 5/2 (g/* (g/divide A) A)))
      (is (= (g/* #emmy/ratio 1/22
                  (s/down (s/up 24 -12 -2)
                          (s/up 5 3 -5)
                          (s/up -4 2 4)))
             (g/divide C)))

      (is (= (s/up (s/down #emmy/ratio 1/3))
             (g/divide D)))

      (is (= (s/down (s/up #emmy/ratio 1/4 0 0 0)
                     (s/up 0 -1 1 0)
                     (s/up 0 #emmy/ratio 1/2 0 0)
                     (s/up #emmy/ratio -1/4 0 0 1))
             (g/divide G)))

      (is (= (s/down (s/up #emmy/ratio 1/4 0 0 0)
                     (s/up 0 -1 1 0)
                     (s/up 0 #emmy/ratio 1/2 0 0)
                     (s/up #emmy/ratio -1/4 0 0 1))
             (g/divide G)))
      (is (= (s/down (s/up #emmy/ratio 1/8))
             (g/divide (s/down (s/up 8))))))

    (testing "matrix ops, ratio literals"
      (is (= (m/by-rows [#emmy/ratio 1/2])
             (m/invert (m/by-rows [2])))))

    (testing "invert-hilbert-matrix"
      (let [N 3
            H (s/up* (for [i (range 1 (inc N))]
                       (s/up* (for [j (range 1 (inc N))]
                                (g/divide 1 (g/+ i j -1))))))]
        (is (= (s/down (s/down 9 -36 30)
                       (s/down -36 192 -180)
                       (s/down 30 -180 180))
               (g/divide H)))))))

(deftest matrix-times-structure-tests
  (let [M (m/by-rows ['a 'b 'c]
                     ['d 'e 'f]
                     ['g 'h 'i])
        v (s/up 'x 'y 'z)
        d (g/transpose v)]
    (is (= (m/column-matrix->up
            (g/* M (m/column* v)))
           (g/* M v))
        "multiplying by up directly matches conversion to matrix first.")

    (is (= (m/row-matrix->down
            (g/* (m/row* d) M))
           (g/* d M))
        "multiplying by down directly matches conversion to matrix first.")

    (is (g/zero?
         (g/simplify
          (g/- (g/transpose (g/* M v))
               (g/* (g/transpose v)
                    (g/transpose M)))))
        "transpose law works with structural multiplication")))

(deftest two-tensor-tests
  (testing "solve-linear right and left between scalar, 2-tensor"
    (is (= (s/up (s/down -24 12) (s/down 18 -6))
           (g/solve-linear-right
            12
            (s/up (s/down 1 2) (s/down 3 4)))

           (g/solve-linear-left
            (s/up (s/down 1 2) (s/down 3 4))
            12)))))

(deftest series-expansion-tests
  (is (= (m/by-rows [(g// Math/PI 2) 0]
                    [0 (g// Math/PI 2)])
         (first
          (g/acos
           (m/by-rows [1 2] [3 4]))))
      "series expansion works for g/acos")

  (is (= (m/by-rows [(g// Math/PI 2) 0]
                    [0 (g// Math/PI 2)])
         (first
          (g/acot
           (m/by-rows [1 2] [3 4]))))
      "series expansion works for g/acot"))

(deftest characteristic-poly-tests
  (let [M (m/by-rows [1 3 2] [4 5 2] [1 4 5])
        p (m/characteristic-polynomial M)]
    (is (= (m/characteristic-polynomial M 12)
           (p 12))
        "passing the arg directly has the same result as calling the returned
        polynomial.")

    (is (= 315 (p 12)))))
