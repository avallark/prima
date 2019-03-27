(ns prima.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp]))

(defn gen-next-primes
  [lp]
  (let [largest-prime (last lp)]
    (loop [cnt (+ 1 largest-prime)]
      (if (let [divp (vec (for [x lp :when (<= x (int (Math/sqrt cnt)))] x))
                modu (vec (for [y divp] (mod cnt y)))
                is-not-prime (true? (some #(= 0 %) modu))]
            is-not-prime)
        (recur (inc cnt))
        (conj lp cnt)))))

(defn get-first-ten-primes
  []
  (loop [lp [2 3 5]]
    (if (>= (count lp) 10)
    lp
    (recur (gen-next-primes lp)))))

(defn mux
  [i j]g
  (vec (for [x i] (vec (for [y j] (* x y))))))

(defn main
  [& args]
  (let [primes (get-first-ten-primes)]
    (pp/pprint (mux primes primes))))
