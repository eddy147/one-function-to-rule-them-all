(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce
     (fn [first second]
       (str first " " second))
     a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    ()
    (reduce
     (fn [first second]
       (conj (conj first x) second))
     [(first a-seq)]
     (rest a-seq))))

(defn my-count [a-seq]
  (let [counter (fn [count not-needed]
                  (inc count))]
    (reduce counter 0 a-seq)))

(defn my-reverse [a-seq]
  (reduce conj '() a-seq))

(defn min-max-element [a-seq]
  (let [f-min (fn [min elem]
                (if (< elem min)
                  elem
                  min))
        f-max (fn [max elem]
                (if (> elem max)
                  elem
                  max))]
    [(reduce f-min a-seq)
     (reduce f-max a-seq)]))

(defn insert-helper [sorted-seq result-seq n]
  (cond
    (empty? sorted-seq) (conj result-seq n)
    (< n (first sorted-seq)) (into [] (concat (conj result-seq n) sorted-seq))
    :else (insert-helper (rest sorted-seq) (conj result-seq (first sorted-seq)) n)))

(defn insert [sorted-seq n]
  (insert-helper sorted-seq [] n))

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])