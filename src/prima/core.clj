(ns prima.core
  (:require [clojure.pprint :as pp])
  (:gen-class :main true))

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

(defn get-first-n-primes
  [n]
  (loop [lp [2 3 5]]
    (if (>= (count lp) n)
    lp
    (recur (gen-next-primes lp)))))

(defn mux
  [i j]
  (vec (for [x i] (vec (for [y j] (* x y))))))

(defn -main
  [& args]
  (let [primes (get-first-n-primes (if (> (count args) 0)
                                     (read-string (nth args 0))
                                     10))
        multiplication-table (mux primes primes)]
    (pp/pprint multiplication-table)))
