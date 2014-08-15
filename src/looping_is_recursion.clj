(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (cond
                  (zero? n) 1
                  (== 1 n) acc
                  :else (recur (* acc base) (dec n))))]
    (helper base exp)))

(defn last-element [[n & more]]
  (if more
    (recur more)
    n))

(defn seq= [[n1 & more1] [n2 & more2]]
  (if (= n1 n2)
    (cond
     (and (nil? more1) (nil? more2)) true
     (or (nil? more1) (nil? more2)) false
     :else (recur more1 more2))
    false))

(defn find-first-index [pred a-seq]
  (loop [i 0
         coll a-seq]
    (if (empty? coll)
      nil
      (let [f (first coll)]
        (if (pred f)
          i
          (recur (inc i) (rest coll)))))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         coll a-seq]
    (if (empty? coll)
      (/ acc n)
      (recur (+ acc (first coll)) (inc n) (rest coll)))))

(defn toggle [set x]
  (if (contains? set x)
    (disj set x)
    (conj set x)))

(defn parity [a-seq]
  (loop [set #{}
         coll a-seq]
    (if (empty? coll)
      set
      (recur (toggle set (first coll)) (rest coll)))))

(defn fast-fibo [n]
  (loop [f0 0
         f1 1
         i (- n 2)]
    (cond
     (zero? i) (+ f0 f1)
     (== n 0) 0
     (== n 1) 1
     :else (recur f1 (+ f0 f1) (dec i)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         set #{}
         coll a-seq]
      (let [f (first coll)
            r (rest coll)]
        (cond (empty? coll) acc
              (contains? set f) acc
              :else (recur (conj acc f) (conj set f) r)))))
