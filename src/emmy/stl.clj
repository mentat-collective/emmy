(ns emmy.stl
  (:require [clojure.java.io :as io])
  (:import (java.nio ByteBuffer ByteOrder)))

(defn- float->bytes [f]
  (let [bb (ByteBuffer/allocate 4)]
    (.order bb ByteOrder/LITTLE_ENDIAN)
    (.putFloat bb f)
    (.array bb)))

(defn- vec3->bytes [[x y z]]
  (byte-array (concat (float->bytes (float x))
                      (float->bytes (float y))
                      (float->bytes (float z)))))

(defn- normal-vector [[x1 y1 z1] [x2 y2 z2] [x3 y3 z3]]
  (let [ux (- x2 x1)
        uy (- y2 y1)
        uz (- z2 z1)
        vx (- x3 x1)
        vy (- y3 y1)
        vz (- z3 z1)
        nx (- (* uy vz) (* uz vy))
        ny (- (* uz vx) (* ux vz))
        nz (- (* ux vy) (* uy vx))
        magnitude (Math/sqrt (+ (* nx nx) (* ny ny) (* nz nz)))]
    [(/ nx magnitude) (/ ny magnitude) (/ nz magnitude)]))

(defn- triangle->bytes [v1 v2 v3]
  (let [normal (normal-vector v1 v2 v3)]
    (byte-array (concat (vec3->bytes normal)
                        (vec3->bytes v1)
                        (vec3->bytes v2)
                        (vec3->bytes v3)
                        [0 0]))))  ; Attribute byte count (unused)

(defn- generate-triangles [points rows cols]
  (for [i (range rows)
        j (range cols)
        :let [next-i (mod (inc i) rows)
              next-j (mod (inc j) cols)
              v1 (nth points (+ (* i cols) j))
              v2 (nth points (+ (* i cols) next-j))
              v3 (nth points (+ (* next-i cols) j))
              v4 (nth points (+ (* next-i cols) next-j))]]
    [(triangle->bytes v1 v2 v3)
     (triangle->bytes v2 v4 v3)]))

(defn parametric-to-stl
  "Convert a parametric function to an STL file"
  [parametric-fn filename & {:keys [u-start u-end u-steps
                                    v-start v-end v-steps]
                             :or {u-start 0 u-end (* 2 Math/PI) u-steps 100
                                  v-start 0 v-end Math/PI v-steps 50}}]
  (let [u-range (range u-start u-end (/ (- u-end u-start) u-steps))
        v-range (range v-start v-end (/ (- v-end v-start) v-steps))
        points (vec (for [v v-range
                          u u-range]
                      (parametric-fn u v)))
        triangles (generate-triangles points v-steps u-steps)
        triangle-count (* 2 u-steps v-steps)]
    (with-open [out (io/output-stream filename)]
      ;; Write STL header
      (.write out (byte-array 80))
      ;; Write triangle count
      (.write out (.array (-> (.order (ByteBuffer/allocate 4) ByteOrder/LITTLE_ENDIAN)
                              (.putInt triangle-count))))
      ;; Write triangles
      (doseq [triangle (flatten triangles)]
        (.write out triangle)))
    filename))

;; Example usage:
(comment
  (require '[emmy.env :refer :all])

  (defn toroidal->rect [R r]
    (fn [theta phi]
      (*
       (rotate-z-matrix phi)
       (up (+ R (* r (cos theta)))
           0
           (* r (sin theta))))))

  (parametric-to-stl (toroidal->rect 50 10) "torus.stl"
                     :u-start 0
                     :u-end (* 2 Math/PI)
                     :v-start 0
                     :v-end (* 2 Math/PI)
                     :u-steps 200
                     :v-steps 100)

  (defn example-parametric-fn [u v]
    [(* 50 (Math/cos u) (Math/sin v))
     (* 50 (Math/sin u) (Math/sin v))
     (* 50 (Math/cos v))])

  (parametric-to-stl example-parametric-fn "output.stl"
                     :u-start 0
                     :u-end (* 2 Math/PI)
                     :v-start 0
                     :v-end Math/PI
                     :u-steps 20
                     :v-steps 20))
