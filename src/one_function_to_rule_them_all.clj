(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn str-cat [a-seq]
  (cond 
    (empty? a-seq) ""
    (singleton? a-seq) (first a-seq)
    :else (reduce #(str %1 " " %2) (first a-seq) (rest a-seq))))

(defn my-interpose [x a-seq]
  (cond 
    (empty? a-seq) '()
    (singleton? a-seq) (list (first a-seq))
    :else (list* (reduce #(conj (conj %1 x) %2) (vector (first a-seq)) (rest a-seq)))))
(my-interpose :a [1 2])

(defn my-count [a-seq]
  (reduce (fn [a b] (+ 1 a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (if (empty? a-seq)
    nil
  (reduce (fn [mm e]
            (let [[mn mx] mm]
              (cond
                (< e mn) [e mx]
                (> e mx) [mn e]
                :else mm))) [(first a-seq) (first a-seq)] (rest a-seq))))

(defn insert [sorted-seq n]
  (let [fst (take-while #(< % n) sorted-seq)
        snd (drop (count fst) sorted-seq)]
    (concat fst [n] snd)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set n]
  (if (contains? a-set n)
    (disj a-set n)
    (conj a-set n)))

(defn parity [a-seq]
  (reduce toggle #{} a-seq))

(defn minus 
  ([x] (- 0 x))
  ([x y] (- x y))
  ([x y & more] (reduce minus (minus x y) more)))

(defn count-params
  ([& more] (reduce (fn [a b] (+ 1 a)) 0 more)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x & more] (reduce * x more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] (fn [x] (p x)))
  ([p & more] (reduce (fn [p0 p1] (fn [x] (and (p0 x) (p1 x)))) p more)))

(defn join-vec [a b]
  (if (vector? a)
    (conj a b)
    (vector a b)))

(defn pair [a-seq b-seq]
  (loop [a a-seq
         b b-seq
         coll []]
    (if (or (empty? a) (empty? b))
      coll
      (recur (rest a) (rest b) (conj coll (join-vec (first a) (first b)))))))

(defn my-zip
  ([x] x)
  ([x y] (pair x y))
  ([x y & more] (reduce pair (pair x y) more)))

(my-zip [1 2])
(my-zip [1 2] [1 2])
(my-zip [1 2] [1 2] [1 2])
(apply + [1 1 1])
(defn my-map 
  ([f a-seq] (reduce #(conj %1 (f %2)) [] a-seq))
  ([f a-seq & more] 
   (reduce #(conj %1 (apply f %2)) [] (apply my-zip (conj more a-seq)))))
