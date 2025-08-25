(ns emmy.svg
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [emmy.env :as e :refer :all]))

(defn scale-points
  "Scale points to fit within the SVG viewbox"
  [points width height]
  (let [x-values (map first points)
        y-values (map second points)
        x-min (apply min x-values)
        x-max (apply max x-values)
        y-min (apply min y-values)
        y-max (apply max y-values)
        x-range (- x-max x-min)
        y-range (- y-max y-min)
        scale (min (/ width x-range) (/ height y-range))]
    (for [[x y] points]
      [(* scale (- x x-min))
       (* scale (- y y-min))])))

(defn points-to-path
  "Convert a sequence of points to an SVG path string"
  [points]
  (str "M " (str/join " L " (map #(str/join "," %) points))))

(defn generate-svg
  "Generate an SVG string from a sequence of points"
  [points width height]
  (let [scaled-points (scale-points points width height)
        path-data (points-to-path scaled-points)]
    (format
     "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 %d %d\">
        <path d=\"%s\" fill=\"none\" stroke=\"black\" stroke-width=\"1\"/>
      </svg>"
     width height path-data)))

(defn coordinates-to-svg
  "Convert a vector of [x y] coordinates to an SVG file"
  [coordinates filename & {:keys [width height]
                           :or {width 1000 height 1000}}]
  (let [svg-content (generate-svg coordinates width height)]
    (with-open [writer (io/writer filename)]
      (.write writer svg-content))
    filename))

(defn lorenz-attractor
  "Generate the derivatives (dx, dy, dz) for the Lorenz attractor given the current point (x, y, z) and parameters"
  [sigma beta rho]
  (fn [[x y z]]
    (let [dx (* sigma (- y x))
          dy (- (* x (- rho z)) y)
          dz (- (* x y) (* beta z))]
      [dx dy dz])))

;; Example usage:
(comment
  (def example-coordinates
    [[0 0] [100 100] [200 50] [300 200] [400 0]])

  (coordinates-to-svg example-coordinates "output.svg")

  (let [sigma 10
        beta 8/3
        rho 28
        t1 20
        dt 0.005 
        init-state [-8.0 8.0 27.0]]
    (-> (for [[x y] (integrate-state-derivative lorenz-attractor [sigma beta rho] init-state t1 dt)]
          [x y])
        (coordinates-to-svg "lorenz.svg"))))



(defn project-lorenz-xy
  "Project the Lorenz attractor onto the XY plane"
  [sigma beta rho dt steps]
  (let [initial-state [0.1 0 0]
        integrator (state-advancer lorenz-attractor sigma beta rho)]
    (mapv (fn [t]
            (let [[x y _] (integrator initial-state t)]
              [x y]))
          (range 0 (* dt steps) dt))))
