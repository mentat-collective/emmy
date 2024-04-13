#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.value-test
  (:require #?(:cljs [cljs.reader :refer [read-string]])
            [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang PersistentVector))))

(deftest bigint-literal
  (testing "u/parse-bigint can round-trip Bigint instances in clj or cljs. "
    (is (= #?(:clj 10N
              :cljs '(emmy.util/bigint 10))
           (read-string {:readers {'emmy/bigint u/parse-bigint}}
                        (pr-str #emmy/bigint 10))))

    (let [one-e-40 (apply str "1" (repeat 40 "0"))]
      (is (= #?(:clj (bigint 1e40)
                :cljs (list 'emmy.util/bigint one-e-40))
             (read-string {:readers {'emmy/bigint u/parse-bigint}}
                          (pr-str #emmy/bigint one-e-40)))
          "Parsing #emmy/bigint works with big strings too."))))

(deftest vector-value-impl
  (testing "zero?"
    (is (g/zero? []))
    (is (g/zero? [0 0]))
    (is (not (g/zero? [1 2 3]))))

  (testing "zero-like"
    (is (= [0 0 0] (g/zero-like [1 2 3])))
    (is (= [] (g/zero-like [])))
    (is (= [0 [0 0] [0 0]] (g/zero-like [1 [2 3] [4 5]])))
    (is (= [(u/long 0) (u/int 0) 0]
           (g/zero-like [(u/long 1) (u/int 2) 3]))))

  (is (= 1 (g/one-like [1 2 3]))
      "1 is the multiplicative identity for vector spaces.")

  (testing "exact?"
    (is (g/exact? [1 2 3 4]))
    (is (not (g/exact? [1.2 3 4])))
    (is (g/exact? [0 1 #emmy/ratio 3/2]))
    (is (not (g/exact? [0 0 0.00001]))))

  (testing "freeze"
    (is (= '(up 1 2 3)
           (g/freeze [1 2 3]))))

  (testing "kind"
    (is (= PersistentVector (v/kind [1 2])))))

(deftest numeric-value-protocol-tests
  (checking "*-like properly coerce" 100
            [n sg/number]
            (is (g/zero? (g/zero-like n)))
            (is (not (g/zero? (g/one-like n))))

            (is (g/one? (g/one-like n)))
            (is (not (g/one? (g/zero-like n))))

            (is (g/identity? (g/identity-like n))))

  (let [n 50]
    (checking "all numbers act as hashmap keys" 100
              [ks (gen/set sg/real {:num-elements n})
               vs (gen/vector sg/real n)]
              ;; NOTE that test.check seems to have a bug where it will happily
              ;; generate a set containing 0 and (js/BigInt. 0), for example,
              ;; without distinct-ing.
              (let [ks (distinct (vec ks))
                    m  (zipmap ks vs)]
                (is (= (sort-by first v/compare (map vector ks vs))
                       (sort-by key v/compare m))
                    "Any numeric key works in a hash-map and round-trips."))))

  (testing "zero-like sticks with precision"
    (is (= 0 (g/zero-like 2)))
    (is (= 0.0 (g/zero-like 3.14))))

  (testing "one-like sticks with precision"
    (is (= 1 (g/one-like 1)))
    (is (= 1.0 (g/one-like 1.2))))

  (checking "on non-rational reals, g/freeze is identity" 100
            [n (gen/one-of [sg/any-integral (sg/reasonable-double)])]
            (is (= n (g/freeze n))))

  (is (isa? (v/kind 10) ::v/real))
  (is (g/exact? 10))
  (is (not (g/exact? 10.1))))

(deftest numeric-comparison-tests
  (checking "v/compare matches <, >, = behavior for reals" 100
            [[l r] (gen/vector sg/real-without-ratio 2)]
            (let [compare-bit (v/compare l r)]
              (cond (neg? compare-bit) (is (< l r))
                    (pos? compare-bit) (is (> l r))
                    :else (is (and (<= l r)
                                   ;; NOTE: Another strange observation. == is
                                   ;; supposed to call out to equiv, but it
                                   ;; seems like it gets inlined with the
                                   ;; current value of `-equiv` at call time.
                                   ;; Invoking the var gets around this.
                                   #?(:clj (== l r) :cljs (#'== l r))

                                   ;; NOTE: clojure can't compare float and int
                                   ;; with =, so this is special-cased to only
                                   ;; make this `=` check for cljs.
                                   #?(:cljs (= l r) :clj true)
                                   (>= l r))))))

  #?(:clj
     ;; This won't work in cljs because native `compare` can't handle the
     ;; `Ratio`, `BigInt`, `Long` and `Integer` types that we've pulled in to
     ;; the numeric tower. TODO: this is probably a bug in the latter two cases,
     ;; since cljs claims to interop with those types nicely. Report if you
     ;; like!
     (checking "v/compare matches core/compare for reals" 100
               [l sg/real, r sg/real]
               (is (= (v/compare l r)
                      (compare l r))))))

(deftest zero-tests
  (is (g/zero? 0))
  (is (g/zero? 0.0))
  (is (not (g/zero? 1)))
  (is (not (g/zero? 0.1))))

(deftest one-tests
  (is (g/one? 1))
  (is (g/one? 1.0))
  (is (not (g/one? 0)))
  (is (not (g/one? 0.0))))

(deftest kinds
  (is (= #?(:clj Long :cljs ::v/native-integral) (v/kind 1)))
  (is (= #?(:clj Double :cljs ::v/native-integral) (v/kind 1.0)))
  (is (= PersistentVector (v/kind [1 2]))))

(deftest exactness
  (is (g/exact? 1))
  (is (g/exact? 4N))
  (is (not (g/exact? 1.1)))
  (is (not (g/exact? :a)))
  (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (g/exact? "a")))
  (is (g/exact? #emmy/ratio 3/2))
  (is (g/exact? (u/biginteger 111))))

(deftest argument-kinds
  (let [L #?(:clj Long :cljs ::v/native-integral)
        V PersistentVector]
    (is (= [L] (v/argument-kind 1)))
    (is (= [L L L] (v/argument-kind 1 2 3)))
    (is (= [V] (v/argument-kind [2 3])))
    (is (= [V V] (v/argument-kind [1] [3 4]))))

  (checking "kind-predicate" 100 [l gen/any r gen/any]
            (let [l-kind  (v/kind l)
                  r-kind  (v/kind r)
                  l-kind? (v/kind-predicate l)
                  r-kind? (v/kind-predicate r)]
              ;; each item is its own kind.
              (is (l-kind? l))
              (is (r-kind? r))

              ;; they only respond true if they match kinds (or one inherits
              ;; from the other), false otherwise.
              (cond (= l-kind r-kind)
                    (do (is (l-kind? r))
                        (is (r-kind? l)))

                    (isa? l-kind r-kind)
                    (do (is (not (l-kind? r)))
                        (is (r-kind? l)))

                    (isa? r-kind l-kind)
                    (do (is (l-kind? r))
                        (is (not (r-kind? l))))

                    :else (do (is (not (l-kind? r)))
                              (is (not (r-kind? l))))))))
