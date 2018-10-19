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
  (reduce insert nil a-seq))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [res #{}
         seq a-seq]
    (if (empty? seq) res
        (recur (toggle res (first seq)) (rest seq)))))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([p1 & more] (+ 1 (count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce my-* (my-* x y) more)))

(defn andp [& fns]
  (fn [& args]
    (every? #(apply % args) fns)))

(defn pred-and
  ([] (fn [x] true))
  ([x] x)
  ([x y] (andp x y))
  ([x y & more]
   (reduce pred-and (pred-and x y) more)))

(defn my-map
  ([f coll] (seq (reduce #(conj %1 (f %2)) [] coll)))
  ([f coll & colls]
   (let [colls (cons coll colls)]
     (my-map (partial apply f)
             (partition (count colls)
                        (apply interleave colls))))))